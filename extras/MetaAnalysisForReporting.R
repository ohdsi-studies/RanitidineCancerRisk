####Re-write the result to add calibrated CIs####
library(ggplot2)
library(ggsci)
library(dplyr)
source("./extras/FunctionsForReporting.R")
#tcs<-unique(tcos[,c("targetId","comparatorId")])

shinyFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"ShinyDeploy")
resultFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"output")

# databaseIds <- c("IQVIA Ambulatory EMR", "CUIMC", "STARROMOP", 
#                  "IQVIA DA Germany",  "IMRD", "IQVIA France LPD", "SIDIAP",
#                  "NHIS-NSC", "AUSOM", "HUMIC", "KDH", "TMUCDR")
databaseIds <- c("AmbEMR", "CUIMC", "STARROMOP", 
                 "DA Germany",  "IMRD", "France LPD", "SIDIAP",
                 "NHIS-NSC", "AUSOM", "HUMIC", "KDH", "TMUCDR")
databaseIdAndMeta <- c(databaseIds, "Meta-analysis", "Meta-analysis-Full")

equipoiseBounds <- c(0.3,0.7)

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


####Add calibrated CI to the data####
# databaseId <- "IMRD"
for(databaseId in databaseIds){
    singleCohortMethodResult<-readRDS(file.path(shinyFolder,"data",sprintf("cohort_method_result_%s.rds",databaseId)))
    colnames(singleCohortMethodResult)<-SqlRender::snakeCaseToCamelCase(colnames(singleCohortMethodResult))
    tcos <- unique(singleCohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
    tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
    tcs <- unique(tcos[,c("targetId","comparatorId")])

    for (analysisId in unique(cohortMethodAnalysis$analysisId)){
        for (i in seq(nrow(tcs))){
            tc<- tcs[i,]
            index<-singleCohortMethodResult$targetId==tc$targetId&
                singleCohortMethodResult$comparatorId==tc$comparatorId&
                singleCohortMethodResult$analysisId==analysisId&
                singleCohortMethodResult$databaseId==databaseId&
                !is.na(singleCohortMethodResult$logRr) &
                !is.na(singleCohortMethodResult$seLogRr)

            if(sum(index, na.rm=T)==0) next
            negativeData<-singleCohortMethodResult[index &
                                                 singleCohortMethodResult$outcomeId %in% unique(negativeControlOutcome$outcomeId),]
            null<-EmpiricalCalibration::fitNull(negativeData$logRr,
                                                negativeData$seLogRr)

            model<-EmpiricalCalibration::convertNullToErrorModel(null)

            calibratedCi<-EmpiricalCalibration::calibrateConfidenceInterval(logRr=singleCohortMethodResult[index,]$logRr,
                                                                            seLogRr=singleCohortMethodResult[index,]$seLogRr,
                                                                            model=model,
                                                                            ciWidth = 0.95)

            singleCohortMethodResult[index,]$calibratedLogRr<-calibratedCi$logRr
            singleCohortMethodResult[index,]$calibratedSeLogRr<-calibratedCi$seLogRr
            singleCohortMethodResult[index,]$calibratedCi95Lb<-exp(calibratedCi$logLb95Rr)
            singleCohortMethodResult[index,]$calibratedCi95Ub<-exp(calibratedCi$logUb95Rr)
            singleCohortMethodResult[index,]$calibratedRr<-exp(calibratedCi$logRr)

        }


    }
    colnames(singleCohortMethodResult)<-SqlRender::camelCaseToSnakeCase(colnames(singleCohortMethodResult))
    saveRDS(singleCohortMethodResult,file.path(shinyFolder,"data",sprintf("cohort_method_result_%s.rds",databaseId)))
}

####Meta-analysis####
maExportFolder <- "/Users/chan/data/ranitidine3/results/MetaAnalysis"

RanitidineCancerRisk::doMetaAnalysis(shinyFolder = file.path(shinyFolder,"data"),
                                     maExportFolder = maExportFolder,
                                     maxCores = 1,
                                     interactions=FALSE,
                                     positiveControlOutcome = FALSE,
                                     diagnostics = diagnosticsPassed,
                                     databaseId = "Meta-analysis")
# debug(doMetaAnalysis)
RanitidineCancerRisk::doMetaAnalysis(shinyFolder = file.path(shinyFolder,"data"),
                                     maExportFolder = maExportFolder,
                                     maxCores = 1,
                                     interactions=FALSE,
                                     positiveControlOutcome = FALSE,
                                     diagnostics = NULL,
                                     databaseId = "Meta-analysis-Full")
####Save the result from the meta-analysis ####

#To identify whole study population and outcomes
cmMetaFull<-readRDS(file.path(shinyFolder,"data","cohort_method_result_Meta-analysis-Full.rds"))
colnames(cmMetaFull)<-SqlRender::snakeCaseToCamelCase(colnames(cmMetaFull))
analysisIdTemp <- method %>% unique() %>% 
  filter(description == "No PS matching, ITT, with 1-year lag period") %>% pull(analysisId) #42

cmMetaFull %>% 
  filter(.data$outcomeId==!!outcomeId) %>%
  filter(.data$targetId == !!targetId) %>%
  filter(.data$comparatorId == !!comparatorId) %>%
  filter(.data$analysisId == !!analysisIdTemp) 

#Adding CI for meta-analysis
# databaseId <- "Meta-analysis"
# singleCohortMethodResult<-readRDS(file.path(shinyFolder,"data",sprintf("cohort_method_result_%s.rds",databaseId)))
# colnames(singleCohortMethodResult)<-SqlRender::snakeCaseToCamelCase(colnames(singleCohortMethodResult))
# tcos <- unique(singleCohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
# tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
# tcs <- unique(tcos[,c("targetId","comparatorId")])
# 
# for (analysisId in unique(cohortMethodAnalysis$analysisId)){
#   for (i in seq(nrow(tcs))){
#     tc<- tcs[i,]
#     index<-singleCohortMethodResult$targetId==tc$targetId&
#       singleCohortMethodResult$comparatorId==tc$comparatorId&
#       singleCohortMethodResult$analysisId==analysisId&
#       singleCohortMethodResult$databaseId==databaseId&
#       !is.na(singleCohortMethodResult$logRr) &
#       !is.na(singleCohortMethodResult$seLogRr)
#     
#     if(sum(index, na.rm=T)==0) next
#     negativeData<-singleCohortMethodResult[index &
#                                              singleCohortMethodResult$outcomeId %in% unique(negativeControlOutcome$outcomeId),]
#     null<-EmpiricalCalibration::fitNull(negativeData$logRr,
#                                         negativeData$seLogRr)
#     
#     model<-EmpiricalCalibration::convertNullToErrorModel(null)
#     
#     calibratedCi<-EmpiricalCalibration::calibrateConfidenceInterval(logRr=singleCohortMethodResult[index,]$logRr,
#                                                                     seLogRr=singleCohortMethodResult[index,]$seLogRr,
#                                                                     model=model,
#                                                                     ciWidth = 0.95)
#     
#     singleCohortMethodResult[index,]$calibratedLogRr<-calibratedCi$logRr
#     singleCohortMethodResult[index,]$calibratedSeLogRr<-calibratedCi$seLogRr
#     singleCohortMethodResult[index,]$calibratedCi95Lb<-exp(calibratedCi$logLb95Rr)
#     singleCohortMethodResult[index,]$calibratedCi95Ub<-exp(calibratedCi$logUb95Rr)
#     singleCohortMethodResult[index,]$calibratedRr<-exp(calibratedCi$logRr)
#     
#   }
#   
#   
# }
# colnames(singleCohortMethodResult)<-SqlRender::camelCaseToSnakeCase(colnames(singleCohortMethodResult))
# saveRDS(singleCohortMethodResult,file.path(shinyFolder,"data",sprintf("cohort_method_result_%s.rds",databaseId)))


####Regional meta-analysis####
shinyFolderMeta <- file.path(shinyFolder,"data")
cohortMethodResultList <- list.files(shinyFolderMeta, "^cohort_method_result.*.rds$", full.names = TRUE)
if(sum(grepl("Meta-analysis", cohortMethodResultList))){
  ParallelLogger::logWarn(cohortMethodResultList[grep("Meta-analysis", cohortMethodResultList)]," is excluded from meta-analysis since it seems a result of meta-analysis")
  cohortMethodResultList <- cohortMethodResultList[-grep("Meta-analysis", cohortMethodResultList)]
}
allResults <- lapply(cohortMethodResultList,loadShinyResults)
allResults <- do.call(rbind, allResults)

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

mainResults <- allResults %>% 
  filter(targetId == !!targetIdPrime,
         comparatorId == !!comparatorIdPrime,
         outcomeId == !! outcomeIdPrime,
         analysisId == !! analysisIdPrime)

res <- metafor::rma(logRr, sei = seLogRr, data = mainResults, method = "DL")
predict(res, transf=exp, digits=2)
