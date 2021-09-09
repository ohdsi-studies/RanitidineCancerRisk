#rm(list=ls())
library(ggplot2)
library(ggsci)
library(dplyr)

shinyFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"ShinyDeploy")
dataFolder <- file.path(shinyFolder,"data")
resultFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"output")
resultDiagFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"output_diag")

databaseIds <- c("IQVIA Ambulatory EMR", "CUIMC", "STARROMOP", 
                 "SIDIAP", "imrd1903", "IQVIA DA Germany",  "IQVIA France LPD", 
                 "NHIS-NSC", "AUSOM", "HUMIC", "KDH")
databaseIdsUs <- c("IQVIA Ambulatory EMR", "CUIMC", "STARROMOP")
databaseIdsEu <- c("IQVIA DA Germany",  "imrd1903", "IQVIA France LPD", "SIDIAP")
databaseIdsAsia <- c("NHIS-NSC", "AUSOM", "HUMIC", "KDH")

databaseIdsPrime <- c("IQVIA Ambulatory EMR", "CUIMC","SIDIAP","NHIS-NSC")
databaseIdAndMeta <- c(databaseIds, "Meta-analysis", "Meta-analysis_Full0")

source("./extras/FunctionsForReporting.R")


targetColor <- rgb(255/255,99/255,71/255, alpha = 0.8)
comparatorColor <- rgb(30/255,144/255,255/255, alpha = 0.8)
targetColorFill <- rgb(255/255,99/255,71/255, alpha = 0.3)
comparatorColorFill <- rgb(30/255,144/255,255/255, alpha = 0.3)

dayYr <- 365.25

####Data load####
#load files into the environment
for (i in seq(length(databaseIdAndMeta))){
  databaseId = databaseIdAndMeta[i]
  dataFolder <- file.path(shinyFolder,"data")
  files <- list.files(dataFolder, pattern = sprintf("%s.rds", databaseId))
  files<-files[!grepl("tNA_cNA",files)]
  if (i==1){
    connection <- NULL
    positiveControlOutcome <- NULL
    
    splittableTables <- c("covariate_balance")#c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
    
    # Remove data already in global environment:
    tableNames <- gsub("(_t[0-9]+_c[0-9]+)|(_)[^_]*\\.rds", "", files) 
    camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
    camelCaseNames <- unique(camelCaseNames)
    camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
    rm(list = camelCaseNames)
    
    #load files
    lapply(files, loadFile)
  }else {
    lapply(files, loadFile)
  }
}

####Primary configuration####
tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])

cohortList <- read.csv("./inst/settings/CohortsToCreate.csv") #;cohortList
method <- cohortMethodAnalysis %>% select(analysisId, description)#;method

targetIdPrime <- cohortList %>% filter(atlasName == "Ranitidine user") %>% pull(cohortId)
targetNamePrime <- "Ranitidine"
comparatorIdPrime <- cohortList %>% filter(atlasName == "Other H2 blocker user") %>% pull(cohortId)
comparatorNamePrime <- "Other HRAs"
comparatorIdCime <- cohortList %>% filter(atlasName == "Cimetidine user") %>% pull(cohortId)
outcomeNamePrime <- "Overall cancer except skin cancer"
outcomeIdPrime <- cohortList %>% filter(atlasName == "Overall cancer except skin cancer") %>% pull(cohortId)

analysisIdPrime <- method %>% unique() %>% 
  filter(description == "1:1 PS matching, ITT, with 1-year lag period") %>% pull(analysisId) #42
equipoiseBounds = c(0.3,0.7)


####Check Diagnostics####
resultDf <- cohortMethodResult %>% 
  select(targetId,comparatorId,databaseId) %>%
  filter(databaseId %in% databaseIds) %>%
  unique() #34

for(i in seq(nrow(resultDf))){
  targetId = resultDf$targetId[i]
  comparatorId = resultDf$comparatorId[i]
  databaseId = resultDf$databaseId[i]
  #Load balance
  pathToRds<-file.path(dataFolder,
                       sprintf("covariate_balance_t%d_c%d_%s.rds",targetId,comparatorId,databaseId))
  balance <- readRDS(pathToRds)
  colnames(balance)<-SqlRender::snakeCaseToCamelCase(colnames(balance))
  #max balance after applying PS model
  balMaxSingle <- balance %>% 
    group_by(targetId, comparatorId, databaseId, outcomeId, analysisId) %>%
    summarise(maxStdDiffAfter = max(abs(.data$stdDiffAfter)))
  
  if(i==1) balMax <- balMaxSingle 
  balMax <- rbind(balMax, balMaxSingle)
}
rm(balance)
nrow(balMax)==nrow(balMax%>%unique()) #F, there is duplicated rows 
balMax <- unique(balMax) %>% as_tibble()

if(is.null(equipoiseBounds)) equipoiseBounds = c(0.3,0.7)

psEqui <- preferenceScoreDist %>%
  group_by(targetId, comparatorId, databaseId, analysisId) %>%
  filter(preferenceScore>=equipoiseBounds[1]) %>% 
  filter(preferenceScore<=equipoiseBounds[2]) %>%
  summarise(targetDensityEqui = sum(targetDensity), comparatorDensityEqui = sum(comparatorDensity))

empEqui <- balMax %>%
  full_join(psEqui, by = c("targetId", "comparatorId", "databaseId", "analysisId"))

nrow(empEqui)==nrow(balMax) #TRUE

diagnosticsPassed <- empEqui %>% filter(maxStdDiffAfter < 0.1,
                                        targetDensityEqui>=50,
                                        comparatorDensityEqui>=50)
saveRDS(diagnosticsPassed, file.path(resultFolder,"diagnostics_passed.rds"))
####Load the result from the diagnostics####
diagnosticsPassed <- readRDS(file.path(resultFolder,"diagnostics_passed.rds"))

####Overall Number#####
##Number of patients in each database of 'No PS matching, ITT, with 1-year lag period'
analysisIdOfInt <- method %>% 
  filter(.data$description=="No PS matching, ITT, with 1-year lag period") %>% pull(analysisId)# 39
cohortNum<- cohortMethodResult %>% 
  filter(.data$analysisId == analysisIdOfInt) %>% 
  filter(.data$targetId == targetIdPrime) %>%
  filter(.data$comparatorId == comparatorIdPrime) %>%
  filter(.data$outcomeId == outcomeIdPrime) %>%
  filter(.data$databaseId %in% databaseIds) %>%
  select(targetSubjects, comparatorSubjects, targetDays, comparatorDays, databaseId)

# France LPD does not have patients with other H2 blockers except cimetidine
# cohortMethodResult %>% 
#   filter(.data$databaseId == "IQVIA France LPD") %>% 
#   filter(.data$targetId == targetIdPrime) %>%
#   filter(.data$outcomeId == outcomeIdPrime) 

cohortSumUs <- cohortNum %>% 
  filter(.data$databaseId %in% databaseIdsUs) %>%
  select(-databaseId) %>% 
  apply(2, sum) %>% t() %>% as.data.frame()
# cohortSumUs$targetYear <- round(cohortSumUs$targetDays/365.25, 2)
# cohortSumUs$comparatorYear <- round(cohortSumUs$comparatorDays/365.25, 2)
cohortSumUs$targetSubjects + cohortSumUs$comparatorSubjects

cohortSumEu <- cohortNum %>% 
  filter(.data$databaseId %in% databaseIdsEu) %>%
  select(-databaseId) %>% 
  apply(2, sum) %>% t() %>% as.data.frame()

cohortSumEu$targetSubjects + cohortSumEu$comparatorSubjects

cohortSumAsia <- cohortNum %>% 
  filter(.data$databaseId %in% databaseIdsAsia) %>%
  select(-databaseId) %>% 
  apply(2, sum) %>% t() %>% as.data.frame()

cohortSumAsia$targetSubjects + cohortSumAsia$comparatorSubjects

#Total number of target and comparator
cohortSumUs$targetSubjects + cohortSumUs$comparatorSubjects + 
  cohortSumEu$targetSubjects + cohortSumEu$comparatorSubjects +
  cohortSumAsia$targetSubjects + cohortSumAsia$comparatorSubjects

#Total number of target
cohortSumUs$targetSubjects + cohortSumEu$targetSubjects + cohortSumAsia$targetSubjects

#Total number of comparator
cohortSumUs$comparatorSubjects + cohortSumEu$comparatorSubjects + cohortSumAsia$comparatorSubjects


##Number of patients in each database of primary analysis
analysisIdOfInt <- analysisIdPrime
cohortNum<- cohortMethodResult %>% 
  filter(.data$analysisId == analysisIdOfInt) %>% 
  filter(.data$targetId == targetIdPrime) %>%
  filter(.data$comparatorId == comparatorIdPrime) %>%
  filter(.data$outcomeId == outcomeIdPrime) %>%
  filter(.data$databaseId %in% databaseIds) %>%
  select(targetSubjects, comparatorSubjects, targetDays, comparatorDays, databaseId)
cohortNum

#Number of cohorts after PS matching
cohortSum <- cohortNum %>% 
  # filter(.data$databaseId %in% databaseIdsUs) %>%
  select(-databaseId) %>% 
  apply(2, sum) %>% t() %>% as.data.frame()
cohortSum

#Number of cohorts after PS matching and diagnostics
cohortSumPrime <- cohortNum %>% 
  filter(.data$databaseId %in% databaseIdsPrime) %>%
  select(-databaseId) %>% 
  apply(2, sum) %>% t() %>% as.data.frame()
cohortSumPrime


cohortSumAsia$targetSubjects + cohortSumAsia$comparatorSubjects

#Total number of target and comparator
cohortSumUs$targetSubjects + cohortSumUs$comparatorSubjects + 
  cohortSumEu$targetSubjects + cohortSumEu$comparatorSubjects +
  cohortSumAsia$targetSubjects + cohortSumAsia$comparatorSubjects

#Total number of target
cohortSumUs$targetSubjects + cohortSumEu$targetSubjects + cohortSumAsia$targetSubjects

#Total number of comparator
cohortSumUs$comparatorSubjects + cohortSumEu$comparatorSubjects + cohortSumAsia$comparatorSubjects

cohortSumPrime <- cohortNum %>% 
  filter(.data$databaseId %in% databaseIdsPrime) %>%
  select(-databaseId) %>% 
  apply(2, sum) %>% t() %>% as.data.frame()


####Balance####
targetId = targetIdPrime
comparatorId = comparatorIdPrime
analysisId = analysisIdPrime
outcomeId = outcomeIdPrime
targetName = targetNamePrime
comparatorName = comparatorNamePrime

for(databaseId in databaseIds){
  if(databaseId == "IQVIA France LPD") next
  balance<-getBalance(databaseId = databaseId,
                      dataFolder = dataFolder,
                      targetId = targetId,
                      comparatorId = comparatorId,
                      analysisId = analysisId,
                      outcomeId = outcomeId)
  
  ##Table 1
  Table1 <- prepareTable1(balance,
                          beforeLabel = "Before matching",
                          afterLabel = "After matching",
                          targetLabel = targetName,
                          comparatorLabel = comparatorName,
                          percentDigits = 1,
                          stdDiffDigits = 2,
                          output = "latex",
                          pathToCsv = file.path("inst/shiny/EvidenceExplorer","Table1Specs.csv"))
  Table1[,1]<-as.character(Table1[,1])
  Table1[,1]<-gsub(" \\(Unknown unit\\)","",Table1[,1])
  
  # Table1<-labFormmating(Table1,percentDigits=1)
  if(!file.exists(file.path(resultFolder,"Table1"))) dir.create(file.path(resultFolder,"Table1"))
  
  write.csv(Table1,file.path(resultFolder, "Table1",sprintf("Table1_%s_t%d_c%d_o%d_a%d.csv",
                                                            databaseId,
                                                            targetId,
                                                            comparatorId,
                                                            outcomeId,
                                                            analysisId
  )))
  
  #covariate balance scatter plot
  balancePlot<-plotCovariateBalanceScatterPlot(balance, beforeLabel = "Before matching", afterLabel = "After matching",
                                               limits= c(0,0.4))
  #balancePlot<-CohortMethod::plotCovariateBalanceScatterPlot(balance, title = databaseId, showCovariateCountLabel = TRUE, showMaxLabel = TRUE)
  #add number of coaviates in the plot
  
  balancePlot<-balancePlot+ annotate("label", label = sprintf(" Number of covariates: %s\nAfter matching max(absolute):%s",
                                                              format(nrow(balance), big.mark=",", scientific=FALSE), 
                                                              format(round(max(balance$absAfterMatchingStdDiff),2),nsmall=2)), 
                                     x = -Inf, y = Inf, hjust=-0.2,vjust=2, color = "black");balancePlot
  balancePlot+ggtitle(databaseId)
  
  assign(paste0("balancePlot","_",gsub("-","_",gsub(" ","",databaseId))), balancePlot, envir = .GlobalEnv)
  if(!file.exists(file.path(resultFolder,"balance_scatter_plot"))) dir.create(file.path(resultFolder,"balance_scatter_plot"))
  ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%d_c%d_o%d_a%d.eps",
                                                                        databaseId,
                                                                        targetId,
                                                                        comparatorId,
                                                                        outcomeId,
                                                                        analysisId)), 
                  balancePlot, device = "eps", width = 10, height = 10, units = "cm", dpi = 320)
  ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%d_c%d_o%d_a%d.pdf",
                                                                        databaseId,
                                                                        targetId,
                                                                        comparatorId,
                                                                        outcomeId,
                                                                        analysisId)), 
                  balancePlot, device = "pdf", width = 10, height = 10, units = "cm", dpi = 320)
  ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%d_c%d_o%d_a%d.tiff",
                                                                        databaseId,
                                                                        targetId,
                                                                        comparatorId,
                                                                        outcomeId,
                                                                        analysisId)), 
                  balancePlot, device = "tiff", width = 10, height = 10, units = "cm", dpi = 320)
  
  rm(balance)
}



####Preference Score, Equipoise####
targetId = targetIdPrime
comparatorId = comparatorIdPrime
analysisId = analysisIdPrime
outcomeId = outcomeIdPrime
targetName = targetNamePrime
comparatorName = comparatorNamePrime

for(databaseId in databaseIds){
  ps = preferenceScoreDist %>% filter(databaseId == !!databaseId,
                                      targetId == !!targetId,
                                      comparatorId == !!comparatorId,
                                      analysisId == !!analysisId)
  if(nrow(ps)==0) next
  
  if(!file.exists(file.path(resultFolder,"ps"))) dir.create(file.path(resultFolder,"ps"))
  plotPs(ps, 
         targetName= targetName, 
         comparatorName = comparatorName,
         showEquiposeLabel = TRUE, 
         equipoiseBounds = equipoiseBounds, 
         fileName = file.path(resultFolder,"ps",
                              sprintf("ps_t_%s_c_%s_a_%s_%s.tiff",targetId, comparatorId, analysisId,databaseId)))
}


####Survival analysis####
for (databaseId in databaseIds){
  #for (outcomeId in unique(outcomeOfInterest$outcomeId) ){
  outcomeId = outcomeIdPrime
  outcomeName = outcomeNamePrime
  analysisId = analysisIdPrime
  targetId = targetIdPrime
  comparatorId = comparatorIdPrime
  outcomeId = outcomeIdPrime
  targetName = targetNamePrime
  comparatorName = comparatorNamePrime
  targetColor = targetColor
  comparatorColor = comparatorColor
  targetColorFill = targetColorFill
  comparatorColorFill = comparatorColorFill
  ylims = c(0,0.25)
  xBreaks = seq(from = 0,to = 2500, by = 250)
  
  kaplanMeier <- kaplanMeierDist[kaplanMeierDist$databaseId==databaseId&
                                   kaplanMeierDist$analysisId==analysisId&
                                   kaplanMeierDist$outcomeId==outcomeId&
                                   kaplanMeierDist$targetId == targetId&
                                   kaplanMeierDist$comparatorId == comparatorId
                                 ,]
  if(nrow(kaplanMeier)==0) next
  kpResult <- cohortMethodResult[cohortMethodResult$databaseId==databaseId&
                                   cohortMethodResult$analysisId==analysisId&
                                   cohortMethodResult$outcomeId==outcomeId&
                                   cohortMethodResult$targetId == targetId&
                                   cohortMethodResult$comparatorId == comparatorId
                                 ,]
  
  kaplanMeier$targetSurvival <- 1-kaplanMeier$targetSurvival
  kaplanMeier$targetSurvivalLb <-1-kaplanMeier$targetSurvivalLb
  kaplanMeier$targetSurvivalUb <-1-kaplanMeier$targetSurvivalUb
  kaplanMeier$comparatorSurvival <-1-kaplanMeier$comparatorSurvival
  kaplanMeier$comparatorSurvivalLb <-1-kaplanMeier$comparatorSurvivalLb
  kaplanMeier$comparatorSurvivalUb <-1-kaplanMeier$comparatorSurvivalUb
  if(is.na(unique(kpResult$p))) next
  if(length(unique(kpResult$p))>1) next
  
  if(unique(kpResult$p)<0.001){
    pValue =  sprintf("italic(P) < 0.001")
    if (journalTheme=="jama") pValue = sprintf("italic(P)<.001")
    pValue = sprintf("italic(P)<%s",".001")
  }else {
    #if (journalTheme=="jama") pNum <- sub("^(-?)0.", "\\1.", sprintf("%.3f",unique(kpResult$p))) 
    pValue =  sprintf("italic(P)==%#.3f",
                      unique(kpResult$p))
    
  }
  p<-plotKaplanMeier(kaplanMeier, 
                     targetName, 
                     comparatorName,
                     ylims = ylims,#c(0,round(max(kaplanMeier$comparatorSurvivalUb,kaplanMeier$targetSurvivalUb),2)+0.02),
                     xBreaks = seq(from = 0,to = 2000, by = 250),#NULL,#c(0,100,200,300),
                     targetColor = targetColor,
                     comparatorColor = comparatorColor,
                     targetColorFill = targetColorFill,
                     comparatorColorFill = comparatorColorFill,
                     pValue = pValue,
                     title = paste0(databaseId,"_",outcomeName))
  
  if(!file.exists(file.path(resultFolder,"kmplot"))) dir.create(file.path(resultFolder,"kmplot"))
  ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%d_c%d_o%d_a%d.eps",
                                                          databaseId,
                                                          targetId,
                                                          comparatorId,
                                                          outcomeId,
                                                          analysisId)), p, device = "eps", width = 16, height = 12, units = "cm", dpi = 400)
  ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%d_c%d_o%d_a%d.pdf",
                                                          databaseId,
                                                          targetId,
                                                          comparatorId,
                                                          outcomeId,
                                                          analysisId)), p, device = "pdf", width = 16, height = 12, units = "cm", dpi = 400)
  ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%d_c%d_o%d_a%d.tiff",
                                                          databaseId,
                                                          targetId,
                                                          comparatorId,
                                                          outcomeId,
                                                          analysisId)), p, device = "tiff", width = 16, height = 12, units = "cm", dpi = 400)
  #}
}

####Meta-analysis####
#FULL DATA
targetId = targetIdPrime
comparatorId = comparatorIdPrime
outcomeId = outcomeIdPrime
analysisId = analysisIdPrime
targetName = "Ranitidine"
comparatorName = "Other HRAs"
outcomeName <- cohortList %>% filter(cohortId == outcomeId) %>% pull(atlasName)

metaResult <- doMeta(cohortMethodResult,
                     targetId = targetId,
                     comparatorId = comparatorId,
                     outcomeId = outcomeId,
                     analysisId = analysisId,
                     targetName = targetName,
                     comparatorName = comparatorName,
                     outcomeName = outcomeName,
                     calibration = F)
tiff(file.path(resultFolder,"meta",sprintf("meta_t%d_c%d_a%d_o%d_%s.tiff",targetId,
                                           comparatorId,analysisId,outcomeId,outcomeName) ),
     width = 900*5,height = 720*5,
     res = 500)
forestPlotGenerator(metaResult)
dev.off()

#Only those passed diagnostics
cohortMethodResultPassed <-  cohortMethodResult %>% 
  right_join(diagnosticsPassed %>% 
               select(targetId,comparatorId,#outcomeId,
                      analysisId,databaseId),
             by = c("targetId","comparatorId",#"outcomeId",
                    "analysisId","databaseId")) %>%
  unique()

metaResult <- doMeta(cohortMethodResultPassed,
                     targetId = targetId,
                     comparatorId = comparatorId,
                     outcomeId = outcomeId,
                     analysisId = analysisId,
                     targetName = targetName,
                     comparatorName = comparatorName,
                     outcomeName = outcomeName,
                     calibration = F)

tiff(file.path(resultFolder,"meta",sprintf("meta_passed_t%d_c%d_a%d_o%d_%s.tiff",targetId,
                                           comparatorId,analysisId,outcomeId,outcomeName) ),
     width = 900*5,height = 720*5,
     res = 500)
forestPlotGenerator(metaResult)
dev.off()

####Systematic error####
targetId = targetIdPrime
comparatorId = comparatorIdPrime
outcomeId = outcomeIdPrime
analysisId = analysisIdPrime
databaseId = "Meta-analysis"

controlResults <- cohortMethodResult[cohortMethodResult$targetId == targetId &
                                       cohortMethodResult$comparatorId == comparatorId &
                                       cohortMethodResult$analysisId == analysisId &
                                       cohortMethodResult$databaseId == databaseId, ]
controlResults$effectSize <- NA
idx <- controlResults$outcomeId %in% negativeControlOutcome$outcomeId
controlResults$effectSize[idx] <- 1
positiveControlOutcome = NULL
if (!is.null(positiveControlOutcome)) {
  idx <- controlResults$outcomeId %in% positiveControlOutcome$outcomeId
  controlResults$effectSize[idx] <- positiveControlOutcome$effectSize[match(results$outcomeId[idx],
                                                                            positiveControlOutcome$outcomeId)]
}
controlResults <- controlResults[!is.na(controlResults$effectSize), ]

sum(controlResults$p<0.05, na.rm = T) #5
sum(controlResults$p>=0.05, na.rm = T) #108

sum(controlResults$calibratedP<0.05, na.rm = T) #2
sum(controlResults$calibratedP>=0.05, na.rm = T) #111

tiff(file.path(resultFolder,"meta",sprintf("meta_systematic_error.tiff") ),
     width = 750*5,height = 800*5,
     res = 500)
plotScatter(controlResults)
dev.off()

####Distribution of the risk estimates####
targetId = targetIdPrime
comparatorId = comparatorIdPrime
outcomeId = outcomeIdPrime

outcome<- results %>% 
  #filter(databaseId %in% c("Meta-analysis")) %>%
  filter(outcomeId %in% c(1202,1240)) %>%
  filter(analysisId %in% c(1,2,3,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21))

outcome$databaseId <- factor(outcome$databaseId, level = c("OptumPanTher","IQVIA - Hospital", "HIRA", "Meta-analysis"))
outcomeBeforeCal <- outcome %>% select(targetId, comparatorId, outcomeId, analysisId,rr,ci95Lb,ci95Ub,p, databaseId) %>%
  mutate(Calibration="Before calibration")
outcomeAfterCal <- outcome %>% 
  mutate(rr=calibratedRr, ci95Lb=calibratedCi95Lb, ci95Ub=calibratedCi95Ub,p=calibratedP) %>%
  select(targetId, comparatorId, outcomeId, analysisId,rr,ci95Lb,ci95Ub,p, databaseId) %>%
  mutate(Calibration="After calibration")
outcomeCal <- rbind(outcomeBeforeCal,outcomeAfterCal)

outcomeCal$Calibration <- factor(outcomeCal$Calibration,level = c("Before calibration", "After calibration"))

primaryRr<-outcomeCal %>% filter(targetId == targetIdPrime, 
                                 comparatorId == comparatorIdPrime,
                                 analysisId == analysisIdPrime,
                                 outcomeId == outcomeIdPrime#, calibration == "before calibration"
)
customLimit = c(0.60,1.7)
customBreaks = c(0.60,0.75,0.9,1.0,1.1, 1.3,1.7)

RrDistr<-ggplot(outcomeCal, aes(x=rr,fill = Calibration, color = Calibration)) +
  geom_histogram(#fill="white",
    alpha = 0.3, position="identity", bins=50) +
  geom_vline(data = primaryRr, aes(xintercept=rr, color = Calibration)) +
  geom_vline(aes(xintercept=1.0), linetype="dashed") +
  facet_grid(databaseId~.)+
  ggplot2::theme_bw()+
  scale_x_continuous(trans=log10_trans(), limits= customLimit,breaks =customBreaks
  )+
  xlab('Hazard ratio')+ ylab("Count")
outcomeName="outcome"
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("RrDistr_%s.eps",
                                                         outcomeName)), RrDistr, device = "eps" ,
                width = 24, height = 20, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("RrDistr_%s.pdf",
                                                         outcomeName)), RrDistr, device = "pdf" ,
                width = 24, height = 20, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("RrDistr_%s.tiff",
                                                         outcomeName)), RrDistr, device = "tiff" ,
                width = 24, height = 20, units = "cm", dpi = 320)