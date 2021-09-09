####Re-write the result to add calibrated CIs####

#tcs<-unique(tcos[,c("targetId","comparatorId")])

shinyFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"ShinyDeploy")
resultFolder <- file.path(Sys.getenv("ranitidineCancerDir"),"output")

databaseIds <- c("IQVIA Ambulatory EMR", "CUIMC", "STARROMOP", 
                 "IQVIA DA Germany",  "imrd1903", "IQVIA France LPD", "SIDIAP",
                 "NHIS-NSC", "AUSOM", "HUMIC", "KDH")

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

####Prepare data for meta-analysis####

##Meta-analysis
maExportFolder <- "/Users/chan/data/ranitidine3/results/MetaAnalysis"

RanitidineCancerRisk::doMetaAnalysis(shinyFolder = file.path(shinyFolder,"data"),
                                     maExportFolder = maExportFolder,
                                     maxCores = 1,
                                     interactions=FALSE,
                                     positiveControlOutcome = FALSE,
                                     diagnostics = diagnosticsPassed,
                                     databaseId = "Meta-analysis")
debug(doMetaAnalysis)
RanitidineCancerRisk::doMetaAnalysis(shinyFolder = file.path(shinyFolder,"data"),
                                     maExportFolder = maExportFolder,
                                     maxCores = 1,
                                     interactions=FALSE,
                                     positiveControlOutcome = FALSE,
                                     diagnostics = NULL,
                                     databaseId = "Meta-analysis_Full")
#Adding CI for meta-analysis
databaseId <- "Meta-analysis"
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
