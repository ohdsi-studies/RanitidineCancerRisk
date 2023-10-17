#rm(list=ls())
# renv::deactivate()
# install.packages("ggsci")
# install.packages("meta")
library(ggplot2)
library(ggsci)
library(dplyr)

shinyFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"ShinyDeploy")
dataFolder <- file.path(shinyFolder,"data")
resultFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"output")
resultDiagFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"output_diag")

# databaseIds <- c("IQVIA Ambulatory EMR", "CUIMC", "STARROMOP", 
#                  "IQVIA DA Germany",  "IMRD", "IQVIA France LPD", "SIDIAP",
#                  "NHIS-NSC", "AUSOM", "HUMIC", "KDH", "TMUCDR")
databaseIdsUs <- c("AmbEMR", "CUIMC", "STARR")
databaseIdsEu <- c("DA Germany",  "IMRD", "France LPD", "SIDIAP")
databaseIdsAsia <- c("NHIS-NSC", "AUSOM", "HUMIC", "KDH", "TMUCDR")
databaseIds <- c(databaseIdsUs,
                 databaseIdsEu,
                 databaseIdsAsia)

databaseIdsPrime <- c("AmbEMR", "CUIMC","SIDIAP","NHIS-NSC")
databaseIdAndMeta <- c(databaseIds, "Meta-analysis", "Meta-analysis-Full")

source("./extras/FunctionsForReporting.R")


targetColor <- rgb(255/255,99/255,71/255, alpha = 0.8)
comparatorColor <- rgb(30/255,144/255,255/255, alpha = 0.8)
targetColorFill <- rgb(255/255,99/255,71/255, alpha = 0.3)
comparatorColorFill <- rgb(30/255,144/255,255/255, alpha = 0.3)

dayYr <- 365 #365.25

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
cohortNumAfterMatch<- cohortMethodResult %>% 
  filter(.data$analysisId == analysisIdOfInt) %>% 
  filter(.data$targetId == targetIdPrime) %>%
  filter(.data$comparatorId == comparatorIdPrime) %>%
  filter(.data$outcomeId == outcomeIdPrime) %>%
  filter(.data$databaseId %in% databaseIds) %>%
  select(targetSubjects, comparatorSubjects, targetDays, comparatorDays, databaseId)
cohortNum

#Number of cohorts after PS matching
cohortSum <- cohortNumAfterMatch %>% 
  # filter(.data$databaseId %in% databaseIdsUs) %>%
  select(-databaseId) %>% 
  apply(2, sum) %>% t() %>% as.data.frame()
cohortSum

#Number of cohorts after PS matching and diagnostics
cohortSumPrime <- cohortNumAfterMatch %>% 
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

####Table cohort numbers before/after matching and follow-up duration####
cohortNumBeforeMatch <- cohortNum %>%
  rename(targetSubjectsBefore=targetSubjects,
         comparatorSubjectsBefore = comparatorSubjects,
         targetDaysBefore = targetDays,
         comparatorDaysBefore = comparatorDays)
cohortTable <- inner_join(cohortNumBeforeMatch, cohortNumAfterMatch, by = "databaseId")
write.csv(cohortTable, file.path(resultFolder, "cohort_table.csv"))

noMatchingAnalysisId <- method %>% 
  filter(.data$description=="No PS matching, ITT, with 1-year lag period") %>% pull(analysisId)# 39
matchingAnalysisId <- method %>% unique() %>% 
  filter(description == "1:1 PS matching, ITT, with 1-year lag period") %>% pull(analysisId) #42

cohortMethodResult %>% 
  filter(analysisId == noMatchingAnalysisId) %>%
  select (databaseId, targetSubjects, comparatorSubjects, targetDays, comparatorDays)

cohortMethodResult %>% filter(analysisId == matchingAnalysisId)



####Balance####
targetId = targetIdPrime
comparatorId = comparatorIdPrime
analysisId = analysisIdPrime
outcomeId = outcomeIdPrime
targetName = targetNamePrime
comparatorName = comparatorNamePrime

for(databaseId in databaseIds){
  if(databaseId == "France LPD") next
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

####Aggregation of Balance####
dbsPrime <- 
  diagnosticsPassed %>% 
  filter(targetId == !!targetIdPrime,
         comparatorId == !!comparatorIdPrime,
         outcomeId == !!outcomeIdPrime,
         analysisId == !!analysisIdPrime) %>%
  pull(databaseId) %>% unique()

covDbNum <- covariate %>% 
  filter(databaseId %in% dbsPrime) %>%
  filter(analysisId == !!analysisIdPrime) %>%
  group_by(covariateId) %>% 
  summarise(n = n()) %>%
  filter(n == length(dbsPrime))

covOverlap <- covariate %>% 
  select(covariateId, covariateName, covariateAnalysisId) %>% 
  unique() %>% 
  filter(covariateId %in% pull(covDbNum, covariateId))
##Target covariate Ids
#Medical conditions
targetCovIds1 <- c(4212540210,255573210,201606210,4182210210,440383210,201820210,439727210,432867210,316866210,80180210,4030518210,80809210,4279309210,81893210,197494210,
                   1901) #1901 for Charlson Comorbidity Index
# covOverlap %>% filter(covariateId %in% targetCovIds)
#GI
targetCovIds2 <- c(318800210, 4027663210, 201340210)
# covOverlap %>% filter(covariateId %in% targetCovIds2)
#Drug
targetCovIds3 <- c(21604686410, 21603890410, 21600960410, 21601853410, 21604489410, 21603932410, 21602723410, 21602472410)

covIdsTarget <- covariate %>%
  filter(
    (covariateId %in% c(targetCovIds1,targetCovIds2,targetCovIds3))|
      covariateAnalysisId %in% c(1,2,3,4,5,6)
  ) %>%
  pull(covariateId) %>% unique()

##Categorical values
beforeMatchingSeq <- max(attrition$sequenceNumber) - 1
afterMatchingSeq <- max(attrition$sequenceNumber)
for(i in seq(length(dbsPrime))){
  if(i==1) aggBals <- data.frame()
  print(i)
  singleBal <- 
    getBalance(databaseId = dbsPrime[i],
               dataFolder = dataFolder,
               targetId = targetIdPrime,
               comparatorId = comparatorIdPrime,
               analysisId = analysisIdPrime,
               outcomeId = outcomeIdPrime) %>%
    filter(covariateId %in% covIdsTarget)
  
  singleAttr <- attrition %>% filter(databaseId == dbsPrime[i],
                                     targetId == targetIdPrime,
                                     comparatorId == comparatorIdPrime,
                                     analysisId == analysisIdPrime,
                                     outcomeId == outcomeIdPrime)
  
  singleBal <- singleBal %>% mutate(
    databaseId = dbsPrime[i], 
    beforeTargetPop = singleAttr %>% 
      filter(sequenceNumber == beforeMatchingSeq, exposureId == unique(targetId)) %>%
      pull(subjects),
    beforeComparatorPop = singleAttr %>% 
      filter(sequenceNumber == beforeMatchingSeq, exposureId == unique(comparatorId)) %>%
      pull(subjects),
    afterTargetPop = singleAttr %>% 
      filter(sequenceNumber == afterMatchingSeq, exposureId == unique(targetId)) %>%
      pull(subjects),
    afterComparatorPop = singleAttr %>% 
      filter(sequenceNumber == afterMatchingSeq, exposureId == unique(comparatorId)) %>%
      pull(subjects)
  )
  aggBals <- rbind(aggBals, singleBal)
}

aggBals$beforeTargetPopAll <- aggBals %>% 
  group_by(databaseId) %>% 
  summarise(popMax = max(beforeTargetPop)) %>% 
  pull(popMax) %>% sum()
aggBals$beforeComparatorPopAll <- aggBals %>% 
  group_by(databaseId) %>% 
  summarise(popMax = max(beforeComparatorPop)) %>% 
  pull(popMax) %>% sum()
aggBals$afterTargetPopAll <- aggBals %>% 
  group_by(databaseId) %>% 
  summarise(popMax = max(afterTargetPop)) %>% 
  pull(popMax) %>% sum()
aggBals$afterComparatorPopAll <- aggBals %>% 
  group_by(databaseId) %>% 
  summarise(popMax = max(afterComparatorPop)) %>% 
  pull(popMax) %>% sum()

aggBalResult<- aggBals %>% 
  mutate(groupId = covariateId) %>% 
  group_by(groupId) %>%
  group_map(~aggregateCategoricalBalance(.x))

aggBalResultDf <- do.call(rbind.data.frame, aggBalResult) 
if(!file.exists(file.path(resultFolder,"Table1"))) dir.create(file.path(resultFolder,"Table1"))
write.csv(aggBalResultDf,file.path(resultFolder, "Table1","AggregatedBalance2.csv"
))

###aggregated StdDiff for non-categorical values should be disregarded.
# This table should be interpreted with caution, because there are missing values in several covariates.
Table1 <- prepareTable1(aggBalResultDf,
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

write.csv(Table1,file.path(resultFolder, "Table1",sprintf("AggregatedBalance.csv",
                                                          databaseId,
                                                          targetId,
                                                          comparatorId,
                                                          outcomeId,
                                                          analysisId
)))

# Only overlapped covariate
Table1 <- prepareTable1(aggBalResultDf %>% filter(covariateId %in% covOverlap$covariateId),
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

write.csv(Table1,file.path(resultFolder, "Table1",sprintf("AggregatedBalanceOverlap.csv",
                                                          databaseId,
                                                          targetId,
                                                          comparatorId,
                                                          outcomeId,
                                                          analysisId
)))

####Aggregation of balance for all cohorts####
covDbNum <- covariate %>% 
  filter(databaseId %in% databaseIds) %>%
  filter(analysisId == !!analysisIdPrime) %>%
  group_by(covariateId) %>% 
  summarise(n = n()) %>%
  filter(n == length(databaseIds))

covOverlap <- covariate %>% 
  select(covariateId, covariateName, covariateAnalysisId) %>% 
  unique() %>% 
  filter(covariateId %in% pull(covDbNum, covariateId))
##Target covariate Ids
#Medical conditions
targetCovIds1 <- c(4212540210,255573210,201606210,4182210210,440383210,201820210,439727210,432867210,316866210,80180210,4030518210,80809210,4279309210,81893210,197494210,
                   1901) #1901 for Charlson Comorbidity Index
# covOverlap %>% filter(covariateId %in% targetCovIds)
#GI
targetCovIds2 <- c(318800210, 4027663210, 201340210)
# covOverlap %>% filter(covariateId %in% targetCovIds2)
#Drug
targetCovIds3 <- c(21604686410, 21603890410, 21600960410, 21601853410, 21604489410, 21603932410, 21602723410, 21602472410)

covIdsTarget <- covariate %>%
  filter(
    covariateAnalysisId %in% c(1,2,3,4,5,6)
  ) %>%
  pull(covariateId) %>% unique()

##Categorical values
beforeMatchingSeq <- max(attrition$sequenceNumber) - 1
afterMatchingSeq <- max(attrition$sequenceNumber)
for(i in seq(length(databaseIds))){
  if(i==1) aggBals <- data.frame()
  print(i)
  if(databaseIds[i]=="France LPD") next
  singleBal <- 
    getBalance(databaseId = databaseIds[i],
               dataFolder = dataFolder,
               targetId = targetIdPrime,
               comparatorId = comparatorIdPrime,
               analysisId = analysisIdPrime,
               outcomeId = outcomeIdPrime) %>%
    filter(covariateId %in% covIdsTarget)
  
  singleAttr <- attrition %>% filter(databaseId == databaseIds[i],
                                     targetId == targetIdPrime,
                                     comparatorId == comparatorIdPrime,
                                     analysisId == analysisIdPrime,
                                     outcomeId == outcomeIdPrime)
  
  singleBal <- singleBal %>% mutate(
    databaseId = databaseIds[i], 
    beforeTargetPop = singleAttr %>% 
      filter(sequenceNumber == beforeMatchingSeq, exposureId == unique(targetId)) %>%
      pull(subjects),
    beforeComparatorPop = singleAttr %>% 
      filter(sequenceNumber == beforeMatchingSeq, exposureId == unique(comparatorId)) %>%
      pull(subjects),
    afterTargetPop = singleAttr %>% 
      filter(sequenceNumber == afterMatchingSeq, exposureId == unique(targetId)) %>%
      pull(subjects),
    afterComparatorPop = singleAttr %>% 
      filter(sequenceNumber == afterMatchingSeq, exposureId == unique(comparatorId)) %>%
      pull(subjects)
  )
  aggBals <- rbind(aggBals, singleBal)
}

aggBals$beforeTargetPopAll <- aggBals %>% 
  group_by(databaseId) %>% 
  summarise(popMax = max(beforeTargetPop)) %>% 
  pull(popMax) %>% sum()
aggBals$beforeComparatorPopAll <- aggBals %>% 
  group_by(databaseId) %>% 
  summarise(popMax = max(beforeComparatorPop)) %>% 
  pull(popMax) %>% sum()
aggBals$afterTargetPopAll <- aggBals %>% 
  group_by(databaseId) %>% 
  summarise(popMax = max(afterTargetPop)) %>% 
  pull(popMax) %>% sum()
aggBals$afterComparatorPopAll <- aggBals %>% 
  group_by(databaseId) %>% 
  summarise(popMax = max(afterComparatorPop)) %>% 
  pull(popMax) %>% sum()

aggBalResult<- aggBals %>% 
  mutate(groupId = covariateId) %>% 
  group_by(groupId) %>%
  group_map(~aggregateCategoricalBalance(.x))

aggBalResultDf <- do.call(rbind.data.frame, aggBalResult) 
if(!file.exists(file.path(resultFolder,"Table1"))) dir.create(file.path(resultFolder,"Table1"))
write.csv(aggBalResultDf,file.path(resultFolder, "Table1","AggregatedBalance_all.csv"))

###aggregated StdDiff for non-categorical values should be disregarded.
# This table should be interpreted with caution, because there are missing values in several covariates.
Table1 <- prepareTable1(aggBalResultDf,
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

write.csv(Table1,file.path(resultFolder, "Table1",sprintf("AggregatedBalance_all.csv",
                                                          databaseId,
                                                          targetId,
                                                          comparatorId,
                                                          outcomeId,
                                                          analysisId
)))

# Only overlapped covariate
Table1 <- prepareTable1(aggBalResultDf %>% filter(covariateId %in% covOverlap$covariateId),
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

write.csv(Table1,file.path(resultFolder, "Table1",sprintf("AggregatedBalanceOverlap.csv",
                                                          databaseId,
                                                          targetId,
                                                          comparatorId,
                                                          outcomeId,
                                                          analysisId
)))

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

####Meta-analysis for primary outcome####
#FULL DATA
targetId = targetIdPrime
comparatorId = comparatorIdPrime
outcomeId = outcomeIdPrime
analysisId = analysisIdPrime
targetName = "Ranitidine"
comparatorName = "Other HRAs"
outcomeName <- cohortList %>% filter(cohortId == outcomeId) %>% pull(atlasName)

metaResult <- doMeta(data = cohortMethodResult %>% 
                       filter(databaseId != "Meta-analysis") %>% 
                       filter(databaseId != "Meta-analysis-Full"),
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
setEPS()
postscript(file.path(resultFolder,"meta",sprintf("meta_t%d_c%d_a%d_o%d_%s.eps",targetId,
                                                 comparatorId,analysisId,outcomeId,outcomeName) ),
           width = 9,height = 7.2)
forestPlotGenerator(metaResult)
dev.off()


####Regional subgroup meta-analysis####
metaRegion <- metaResult
region = c('US','US','US','Europe','Europe','Europe','Asia','Asia','Asia','Asia','Asia')
metaRegion$meta <- meta::update.meta(metaResult$meta,
                                     byvar = region,
                                     bylab = "Region",
                                     comb.random=T,
                                     comb.fixed =T)
tiff(file.path(resultFolder,"meta",sprintf("meta_regional_t%d_c%d_a%d_o%d_%s.tiff",targetId,
                                           comparatorId,analysisId,outcomeId,outcomeName) ),
     width = 900*5,height = 720*5,
     res = 500)
forestPlotGenerator(metaRegion,
                    limited = T,
                    subgroup = T)
dev.off()

setEPS()
postscript(file.path(resultFolder,"meta",sprintf("meta_regional_t%d_c%d_a%d_o%d_%s.eps",targetId,
                                                 comparatorId,analysisId,outcomeId,outcomeName) ),
width = 9,height = 7.2)
forestPlotGenerator(metaRegion,
                    limited = T,
                    subgroup = T)
dev.off()


####Subgroup meta-analysis grouped by follow-up duration####
cohortMethodResultPrimary <- cohortMethodResult %>% 
  filter(databaseId != "Meta-analysis") %>% 
  filter(databaseId != "Meta-analysis-Full") %>% 
  filter(targetId == !!targetId) %>%
  filter(comparatorId == !!comparatorId) %>%
  filter(outcomeId == !!outcomeId) %>%
  filter(analysisId == !!analysisId)

cohortMethodResultPrimary$targetDuration <- cohortMethodResultPrimary$targetDays / cohortMethodResultPrimary$targetSubjects
hist(cohortMethodResultPrimary$targetDuration)
cohortMethodResultPrimary$comparatorDuration <- cohortMethodResultPrimary$comparatorDays / cohortMethodResultPrimary$comparatorSubjects
hist(cohortMethodResultPrimary$comparatorDuration)

summary(cohortMethodResultPrimary$targetDuration)
summary(cohortMethodResultPrimary$comparatorDuration)

metaResult$targetDuration <- cohortMethodResultPrimary$targetDuration # Order of database of metaResult is identical to that of cohortMethodResultPrimary
metaResult$comparatorDuration <- cohortMethodResultPrimary$comparatorDuration 

metaPeriod <- metaResult
period = ifelse(metaPeriod$targetDuration >= 1825, '>=5 year', '<5 year')
metaRegion$meta <- meta::update.meta(metaResult$meta,
                                     byvar = period,
                                     bylab = "Follow-up period",
                                     comb.random=T,
                                     comb.fixed =T)
tiff(file.path(resultFolder,"meta",sprintf("meta_period_t%d_c%d_a%d_o%d_%s.tiff",targetId,
                                           comparatorId,analysisId,outcomeId,outcomeName) ),
     width = 900*5,height = 720*5,
     res = 500)
forestPlotGenerator(metaRegion,
                    limited = T,
                    subgroup = T)
dev.off()

setEPS()
postscript(file.path(resultFolder,"meta",sprintf("meta_period_t%d_c%d_a%d_o%d_%s.eps",targetId,
                                                 comparatorId,analysisId,outcomeId,outcomeName) ),
           width = 9,height = 7.2)
forestPlotGenerator(metaRegion,
                    limited = T,
                    subgroup = T)
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

setEPS()
postscript(file.path(resultFolder,"meta",sprintf("meta_passed_t%d_c%d_a%d_o%d_%s.eps",targetId,
                                                 comparatorId,analysisId,outcomeId,outcomeName)
                     ),
           width = 9,height = 7.2)
forestPlotGenerator(metaResult)
dev.off()

##Meta result for PS stratification
metaResult <- doMeta(data = cohortMethodResultPassed %>% 
                       filter(databaseId != "Meta-analysis") %>% 
                       filter(databaseId != "Meta-analysis-Full"),
                     targetId = targetId,
                     comparatorId = comparatorId,
                     outcomeId = outcomeId,
                     analysisId = 48, #PS stratification
                     targetName = targetName,
                     comparatorName = comparatorName,
                     outcomeName = outcomeName,
                     calibration = F)
tiff(file.path(resultFolder,sprintf("meta_passed_strat_t%d_c%d_a%d_o%d_%s.tiff",targetId,
                                           comparatorId,analysisId,outcomeId,outcomeName) ),
     width = 900*5,height = 720*5,
     res = 500)
forestPlotGenerator(metaResult)
dev.off()

####Meta-analysis for secondary outcomes####
secondaryOutcomeIds <- cohortList %>% filter(.data$cohortId<=100) %>% pull(cohortId)
# outcomeId = secondaryOutcomeIds[5]
targetId = targetIdPrime
comparatorId = comparatorIdPrime
analysisId = analysisIdPrime
targetName = "Ranitidine"
comparatorName = "Other HRAs"

cohortMethodResultPassed <-  cohortMethodResult %>% 
  right_join(diagnosticsPassed %>% 
               select(targetId,comparatorId,#outcomeId,
                      analysisId,databaseId),
             by = c("targetId","comparatorId",#"outcomeId",
                    "analysisId","databaseId")) %>%
  unique()
#Plotting each outcome (total outcome number should be revised)
for (outcomeId in secondaryOutcomeIds){
  outcomeName <- cohortList %>% filter(.data$cohortId==!!outcomeId) %>% pull(atlasName)
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
}

####Table for Secondary outcomes####
cohortMethodResultTemp <- cohortMethodResult %>% 
  filter(.data$databaseId=="Meta-analysis") %>%
  filter(.data$outcomeId %in% 2:19) %>%
  filter(.data$targetId == !!targetId) %>%
  filter(.data$comparatorId == !!comparatorId) %>%
  filter(.data$analysisId == !!analysisId) 


cm2nd <- cohortMethodResultTemp %>%
  left_join(cohortList %>% select(cohortId, atlasName),
            by = c("outcomeId" = "cohortId"))

#List correction methods
p.adjust.methods
#Bonferroni correction
cm2nd$bonCorrectedP <- p.adjust(cm2nd$p)
#add incidence
cm2nd$targetIncidence <- cm2nd$targetOutcomes/(cm2nd$targetDays/365)*1000
cm2nd$comparatorIncidence <- cm2nd$comparatorOutcomes/(cm2nd$comparatorDays/365)*1000

cm2nd <- cm2nd %>% 
  dplyr::mutate_if(is.numeric, round, 2)
write.csv(cm2nd, file.path(resultFolder, "secondary_outcome.csv"))

#Summary plot
#https://cran.r-project.org/web/packages/forestplot/vignettes/forestplot.html
cohortMethodResultTemp <- cohortMethodResult %>% 
  filter(.data$databaseId=="Meta-analysis") %>%
  filter(.data$outcomeId<=100) %>%
  filter(.data$targetId == !!targetId) %>%
  filter(.data$comparatorId == !!comparatorId) %>%
  filter(.data$analysisId == !!analysisId) 

unique(cohortMethodResult$databaseId)
cohortMethodResultTemp <- cohortMethodResult %>% 
  filter(.data$databaseId=="Meta-analysis") %>%
  filter(.data$outcomeId<=100) %>%
  filter(.data$targetId == !!targetId) %>%
  filter(.data$comparatorId == !!comparatorId) %>%
  filter(.data$analysisId == !!analysisId) 



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
  controlResults$effectSize[idx] <- positiveControlOutcome$effectSize[match(cohortMethodResult$outcomeId[idx],
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
analysisIdList = 1:48

outcome<- cohortMethodResult %>% 
  #filter(databaseId %in% c("Meta-analysis")) %>%
  filter(outcomeId %in% !!outcomeId) %>%
  filter(targetId %in% !!targetId) %>%
  filter(comparatorId %in% !!comparatorId) %>%
  filter(analysisId %in% !!analysisIdList)

outcome$databaseId <- factor(outcome$databaseId, level = c(databaseIds, "Meta-analysis", "Meta-analysis-Full"))
outcomeBeforeCal <- outcome %>% select(targetId, comparatorId, outcomeId, analysisId,rr,ci95Lb,ci95Ub,p, databaseId) %>%
  mutate(Calibration="Before calibration")
outcomeAfterCal <- outcome %>% 
  mutate(rr=calibratedRr, ci95Lb=calibratedCi95Lb, ci95Ub=calibratedCi95Ub,p=calibratedP) %>%
  select(targetId, comparatorId, outcomeId, analysisId,rr,ci95Lb,ci95Ub,p, databaseId) %>%
  mutate(Calibration="After calibration")
outcomeCal <- rbind(outcomeBeforeCal,outcomeAfterCal)

outcomeCal$Calibration <- factor(outcomeCal$Calibration,level = c("Before calibration", "After calibration"))
#Only Meta-analysis
outcomeCal <- outcomeCal %>% filter(databaseId %in% c("Meta-analysis", "Meta-analysis-Full"))
outcomeCal$databaseId
outcomeCal %>% filter(ci95Lb>1)
outcomeCal %>% filter(ci95Ub<1) #none

primaryRr <- outcomeCal %>% filter(targetId == targetIdPrime,
                                   comparatorId == comparatorIdPrime,
                                   analysisId == analysisIdPrime,
                                   outcomeId == outcomeIdPrime#, calibration == "before calibration"
)

customLimit = c(0.85,1.2)
customBreaks = c(0.85,0.90,0.95,0.98,1.0,1.02,1.05,1.1,1.15, 1.2)

RrDistr<-ggplot(outcomeCal, aes(x=rr,fill = Calibration, color = Calibration)) +
  geom_histogram(#fill="white",
    alpha = 0.3, position="identity", bins=50) +
  geom_vline(data = primaryRr, aes(xintercept=rr, color = Calibration)) +
  geom_vline(aes(xintercept=1.0), linetype="dashed") +
  facet_grid(databaseId~.)+
  ggplot2::theme_bw()+
  scale_x_continuous(trans=scales::log10_trans(), limits= customLimit,breaks =customBreaks
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


####Two-dimensional Plotting####
#target analysis IDs
analysisIdList <- method %>% 
  filter(analysisId %in% 1:48) %>% 
  pull(analysisId) %>% 
  unique()

# method$description
#analysisId = analysisIdList[1]
results <- data.frame()
for (analysisId in (analysisIdList)){
  analysisName = unique(method$description[cohortMethodAnalysis$analysisId==analysisId])
  
  # no-matching analysis will not be included
  if (grepl("No PS matching",analysisName))next
  
  result <- cohortMethodResult %>% filter(analysisId==!!analysisId)
  
  if (grepl("[Oo]n-treatment",analysisName)) result$TAR <- "OT"
  if (grepl("ITT",analysisName)) result$TAR <- "ITT"
  
  if (grepl("1-year lag period",analysisName)) result$TAR <- paste0(result$TAR, " (1 yr lag)")
  
  # if (grepl("No PS matching",analysisName)) result$Adjustment <- "No PS adjustment"
  if (grepl("1:1",analysisName)) result$Adjustment <- "1-to-1 PS matching"
  if (grepl("[Vv]ariable-ratio",analysisName)) result$Adjustment <- "Variable-ratio PS matching"
  if (grepl("[Ss]tratification",analysisName)) result$Adjustment <- "PS stratification"
  results <- rbind(results, result)
}

## merge uncal and calibrated results
estimatesVar <- c("p", "rr", "ci95Lb", "ci95Ub", "logRr", "seLogRr")
calVar <- c("calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub", "calibratedP", "calibratedLogRr", "calibratedSeLogRr")
unCalResults <- results %>% 
  select (targetId, comparatorId, outcomeId, analysisId, 
          rr, ci95Lb, ci95Ub, p, i2, logRr, seLogRr,
          targetSubjects, comparatorSubjects, targetDays, comparatorDays, targetOutcomes, comparatorOutcomes,
          databaseId, TAR, Adjustment
  )
calResults <- results %>% 
  select (targetId, comparatorId, outcomeId, analysisId, 
          calibratedRr, calibratedCi95Lb, calibratedCi95Ub, calibratedP, i2, calibratedLogRr, calibratedSeLogRr,
          targetSubjects, comparatorSubjects, targetDays, comparatorDays, targetOutcomes, comparatorOutcomes,
          databaseId, TAR, Adjustment
  )

unCalResults$Calibration <- "not calibrated"
calResults$Calibration <- "calibrated"
colnames(calResults)<-colnames(unCalResults)

results <- rbind(unCalResults, calResults)
results$Adjustment <- ifelse(results$Calibration=="calibrated", paste0(results$Adjustment, " (calibrated)"), as.character(results$Adjustment) )

###Plotting for Primary endpoint
outcomeIds = outcomeIdPrime
outcomeName = outcomeNamePrime
targetId = targetIdPrime
comparatorId = comparatorIdPrime


results <- results %>% 
  filter(outcomeId %in% !!outcomeIds) %>%
  filter(targetId %in% !!targetIdPrime) %>%
  filter(comparatorId %in% !!comparatorIdPrime) %>%
  filter(databaseId %in% c(databaseIdsPrime, "Meta-analysis") )

## Leveling of factors
results$TAR<-factor(results$TAR,levels = c("OT","OT (1 yr lag)",
                                           "ITT", "ITT (1 yr lag)"))
results$Adjustment<-factor(results$Adjustment,levels = c("1-to-1 PS matching",
                                                         "1-to-1 PS matching (calibrated)",
                                                         "Variable-ratio PS matching",
                                                         "Variable-ratio PS matching (calibrated)",
                                                         "PS stratification",
                                                         "PS stratification (calibrated)"))
results$databaseId <- factor(results$databaseId, levels = c(databaseIdsPrime, "Meta-analysis"))

results$outcomeName <- outcomeName

xLim = max(ceiling(median(results$ci95Ub, na.rm=TRUE)),ceiling(1/median(results$ci95Lb, na.rm=TRUE)),2, na.rm = TRUE)
xLim<-ifelse(xLim< (max( 1/results$rr,results$rr,na.rm=TRUE)),ceiling(max( 1/results$rr,results$rr,na.rm=TRUE)),xLim)
limits = c(1/xLim,xLim)
xLimits = c(0.70,1.8)

results <- results %>% mutate(#ci95Lb =ifelse(ci95Lb < limits[1], ci95Lb-1, ci95Lb), 
  #ci95Ub = ifelse(ci95Lb > limits[2], ci95Ub+1, ci95Ub),
  ci95LbOut = ifelse(ci95Lb < limits[1], rr - limits[1], NA), 
  ci95UbOut = ifelse(ci95Ub > limits[2], rr - limits[2], NA))

results$Significance<-factor(ifelse(results$p<0.05,"P<.05","Not significant"),
                             levels = c("P<.05","Not significant")
)

summaryP <- gridForest(results, 
                       breaks = c(0.75, 1, 1.5), 
                       outlierMoverLower= 0.03,
                       outlierMoverUpper= 0.03, 
                       xLimits=xLimits,
                       varX = "databaseId",
                       varY = "TAR",
                       # cols = NULL,
                       xLab = "Database")

summaryP

outcomeName = "primary"
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)


####Subgroup Analysis####
cmDose <- cmInteractionResult %>% 
  filter(databaseId %in% c("AmbEMR", "CUIMC", "SIDIAP", "NHIS-NSC")) %>% 
  filter(analysisId %in% c(3998,4998,5998)) %>%
  filter(targetId == !!targetIdPrime,
         comparatorId == !!comparatorIdPrime,
         outcomeId == !!outcomeIdPrime
         )
write.csv(cmDose,file.path(resultFolder,"summary","cm_dose.csv"))

cohortMethodResult %>% filter(databaseId %in% c("AmbEMR")) %>%#, "CUIMC", "SIDIAP", "NHIS-NSC")) %>% 
  filter(analysisId %in% c(3998,4998,5998)) %>%
  filter(targetId == !!targetIdPrime,
         comparatorId == !!comparatorIdPrime,
         outcomeId == !!outcomeIdPrime
  )

head(cmDose)

subgroupMethod <- method %>% 
  filter(grepl("interaction",description))
subgroupMethod

cmPassedSubgroup <- cohortMethodResult %>% 
  filter(databaseId %in% c("AmbEMR", "CUIMC", "SIDIAP", "NHIS-NSC")) %>% 
  filter(analysisId %in% c(3998,4998,5998)) %>% 
  filter(databaseId != "Meta-analysis")

cmPassedSubgroup

## 365 unit
doseId <- 3998
metaResult <- doMeta(cmPassedSubgroup,
                     targetId = targetIdPrime,
                     comparatorId = comparatorIdPrime,
                     outcomeId = outcomeIdPrime,
                     analysisId = doseId,
                     targetName = targetNamePrime,
                     comparatorName = comparatorNamePrime,
                     outcomeName = outcomeNamePrime,
                     calibration = F)
tiff(file.path(resultFolder,"meta",sprintf("subgroup_meta_passed_365_t%d_c%d_a%d_o%d_%s.tiff",targetIdPrime,
                                           comparatorIdPrime,doseId,outcomeIdPrime,outcomeNamePrime) ),
     width = 900*5,height = 720*5,
     res = 500)
forestPlotGenerator(metaResult)
dev.off()

## 730 unit
doseId <- 4998
metaResult <- doMeta(cmPassedSubgroup,
                     targetId = targetIdPrime,
                     comparatorId = comparatorIdPrime,
                     outcomeId = outcomeIdPrime,
                     analysisId = doseId,
                     targetName = targetNamePrime,
                     comparatorName = comparatorNamePrime,
                     outcomeName = outcomeNamePrime,
                     calibration = F)
tiff(file.path(resultFolder,"meta",sprintf("subgroup_meta_passed_730_t%d_c%d_a%d_o%d_%s.tiff",targetIdPrime,
                                           comparatorIdPrime,doseId,outcomeIdPrime,outcomeNamePrime) ),
     width = 900*5,height = 720*5,
     res = 500)
forestPlotGenerator(metaResult)
dev.off()

## 1095 unit
doseId <- 5998
metaResult <- doMeta(cmPassedSubgroup,
                     targetId = targetIdPrime,
                     comparatorId = comparatorIdPrime,
                     outcomeId = outcomeIdPrime,
                     analysisId = doseId,
                     targetName = targetNamePrime,
                     comparatorName = comparatorNamePrime,
                     outcomeName = outcomeNamePrime,
                     calibration = F)
tiff(file.path(resultFolder,"meta",sprintf("subgroup_meta_passed_1095_t%d_c%d_a%d_o%d_%s.tiff",targetIdPrime,
                                           comparatorIdPrime,doseId,outcomeIdPrime,outcomeNamePrime) ),
     width = 900*5,height = 720*5,
     res = 500)
forestPlotGenerator(metaResult)
dev.off()


# method
# 18 1:1 PS matching, on-treatment, with 1-year lag period
# 4 1:1 PS matching, on-treatment

