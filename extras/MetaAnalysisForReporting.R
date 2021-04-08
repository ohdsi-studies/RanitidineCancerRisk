####Prepare data for meta-analysis####

##Meta-analysis
shinyFolder <- ""
maExportFolder <- ""

RanitidineCancerRisk::doMetaAnalysis(shinyFolder = shinyFolder,
                                     maExportFolder = maExportFolder,
                                     maxCores = 1,
                                     interactions=FALSE,
                                     positiveControlOutcome = FALSE)

####Adding calibrated confidential intervals and re-write RDS####
tcs<-unique(tcos[,c("targetId","comparatorId")])

for(databaseId in databaseIds){
  singleCohortMethodResult <- readRDS(file.path(studyFolder,"shinyData",sprintf("cohort_method_result_%s.rds",databaseId)))
  colnames(singleCohortMethodResult) <- SqlRender::snakeCaseToCamelCase(colnames(singleCohortMethodResult))
  tcos <- unique(singleCohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
  tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
  tcs <- unique(tcos[,c("targetId","comparatorId")])
  
  for (analysisId in unique(cohortMethodAnalysis$analysisId)){
    for (i in seq(nrow(tcs))){
      tc<- tcs[i,]
      index <- singleCohortMethodResult$targetId==tc$targetId&
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
  colnames(singleCohortMethodResult) <- SqlRender::camelCaseToSnakeCase(colnames(singleCohortMethodResult))
  saveRDS(singleCohortMethodResult,file.path(studyFolder,"shinyData",sprintf("cohort_method_result_%s.rds",databaseId)))
}