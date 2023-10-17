# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of RanitidineCancerRisk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create figures and tables for report
#'
#' @details
#' This function generates tables and figures for the report on the study results.
#'
#' @param shinyFolder          A folder name containing shiny result. make sure to use forward slashes (/). D
#' @param maExportFolder       A local folder where the meta-analysis results will be written.
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#' @param interactions
#' @param positiveControlOutcome
#' @param diagnostics
#' @param databaseId                             
#'
#' @export
doMetaAnalysis <- function(shinyFolder, 
                           maExportFolder, 
                           maxCores,
                           interactions,
                           positiveControlOutcome = FALSE,
                           diagnostics = NULL,
                           databaseId = "Meta-analysis") {
  if (!file.exists(maExportFolder)) {
    dir.create(maExportFolder, recursive = TRUE)
  }
  ParallelLogger::addDefaultFileLogger(file.path(maExportFolder, "metaAnalysisLog.txt"))
  ParallelLogger::logInfo("Performing meta-analysis for main effects")
  doMaEffectType(shinyFolder = shinyFolder,
                 maExportFolder = maExportFolder,
                 maxCores = maxCores,
                 interactions = FALSE,
                 positiveControlOutcome = FALSE,
                 diagnostics = diagnostics,
                 databaseId = databaseId)
  if(interactions){
    ParallelLogger::logInfo("Performing meta-analysis for interaction effects")
    doMaEffectType(exportFolders = exportFolders,
                   maExportFolder = maExportFolder,
                   maxCores = maxCores,
                   interactions = TRUE,
                   positiveControlOutcome = FALSE)
  }
  
  ParallelLogger::logInfo("Creating database table")
  database <- data.frame(database_id = databaseId,
                         database_name = "Random effects meta-analysis",
                         description = "Random effects meta-analysis using the DerSimonian-Laird estimator.",
                         is_meta_analysis = 1)
  fileName <- file.path(maExportFolder, "database.csv")
  write.csv(database, fileName, row.names = FALSE)
  saveRDS(database, file.path(shinyFolder, sprintf("database_%s.rds", databaseId)))
  
}

#' Additional meta-analysis
#'
#' @details
#' This function generates tables and figures for the report on the study results.
#'
#' @param shinyFolder          A folder name containing shiny result. make sure to use forward slashes (/). D
#' @param maExportFolder       A local folder where the meta-anlysis results will be written.
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#' @param databaseId           desginated databaseId (default = 'Meta-analysis')
#'
#' @export
doAddtionalMetaAnalysis <- function(shinyFolder, 
                                    maExportFolder, 
                                    maxCores,
                                    interactions = F,
                                    positiveControlOutcome = FALSE, 
                                    databaseId = "Meta-analysis") {
  if (!file.exists(maExportFolder)) {
    dir.create(maExportFolder, recursive = TRUE)
  }
  ParallelLogger::addDefaultFileLogger(file.path(maExportFolder, "metaAnalysisLog.txt"))
  ParallelLogger::logInfo("Performing meta-analysis for main effects")
  doAddMaEffectType(shinyFolder = shinyFolder,
                    maExportFolder = maExportFolder,
                    maxCores = maxCores,
                    interactions = FALSE,
                    positiveControlOutcome = FALSE)
  if(interactions){
    ParallelLogger::logInfo("Performing meta-analysis for interaction effects")
    doMaEffectType(exportFolders = exportFolders,
                   maExportFolder = maExportFolder,
                   maxCores = maxCores,
                   interactions = TRUE,
                   positiveControlOutcome = FALSE)
  }
  
  ParallelLogger::logInfo("Creating database table")
  database <- data.frame(database_id = databaseId,
                         database_name = "Random effects meta-analysis",
                         description = "Random effects meta-analysis using the DerSimonian-Laird estimator.",
                         is_meta_analysis = 1)
  #fileName <- file.path(maExportFolder, "database.csv")
  #write.csv(database, fileName, row.names = FALSE)
  #saveRDS(database, file.path(shinyFolder, "database_Meta-analysis.rds"))
  
}

doMaEffectType <- function(shinyFolder,
                           maExportFolder,
                           maxCores,
                           interactions,
                           positiveControlOutcome=FALSE,
                           diagnostics = NULL, 
                           databaseId = "Meta-analysis") {
  cohortMethodResultList <- list.files(shinyFolder, "^cohort_method_result.*.rds$", full.names = TRUE)
  loadShinyResults <- function(shinyFile){
    ParallelLogger::logInfo("Loading main results from ", shinyFile, " for meta-analysis")
    results<-readRDS(shinyFile)
    colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
    colnames(results)[colnames(results) == "ci95lb"] <- "ci95Lb"
    colnames(results)[colnames(results) == "ci95ub"] <- "ci95Ub"
    
    ncs <- readRDS(gsub("cohort_method_result","negative_control_outcome",shinyFile))
    colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
    results$trueEffectSize <- NA
    idx <- results$outcomeId %in% ncs$outcomeId
    results$trueEffectSize[idx] <- 1
    return(results)
  }
  loadMainResults <- function(exportFolder) {
    ParallelLogger::logInfo("Loading main results from ", exportFolder, " for meta-analysis")
    zipFile <- list.files(exportFolder, "^Results.*.zip$", full.names = TRUE)[1]
    utils::unzip(zipfile = zipFile,
                 files = c("cohort_method_result.csv",
                           "negative_control_outcome.csv",
                           "positive_control_outcome.csv"),
                 exdir = exportFolder)
    results <- read.csv(file.path(exportFolder, "cohort_method_result.csv"))
    colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
    colnames(results)[colnames(results) == "ci95lb"] <- "ci95Lb"
    colnames(results)[colnames(results) == "ci95ub"] <- "ci95Ub"
    ncs <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
    colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
    results$trueEffectSize <- NA
    idx <- results$outcomeId %in% ncs$outcomeId
    results$trueEffectSize[idx] <- 1
    if(positiveControlOutcome){
      pcs <- read.csv(file.path(exportFolder, "positive_control_outcome.csv"))
      colnames(pcs) <- SqlRender::snakeCaseToCamelCase(colnames(pcs))
      idx <- results$outcomeId %in% pcs$outcomeId
      results$trueEffectSize[idx] <- pcs$effectSize[match(results$outcomeId[idx],
                                                          pcs$outcomeId)]
    }
    
    return(results)
  }
  loadInteractionResults <- function(exportFolder) {
    ParallelLogger::logInfo("Loading interaction results from ", exportFolder, " for meta-analysis")
    zipFile <- list.files(exportFolder, "^Results.*.zip$", full.names = TRUE)[1]
    utils::unzip(zipfile = zipFile,
                 files = c("cm_interaction_result.csv",
                           "negative_control_outcome.csv"),
                 exdir = exportFolder)
    results <- read.csv(file.path(exportFolder, "cm_interaction_result.csv"))
    colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
    ncs <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
    colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
    results$trueEffectSize <- NA
    idx <- results$outcomeId %in% ncs$outcomeId
    results$trueEffectSize[idx] <- 1
    results$rr <- results$rrr
    results$logRr <- results$logRrr
    results$seLogRr <- results$seLogRrr
    return(results)
  }
  if (interactions) {
    allResults <- lapply(exportFolder, loadInteractionResults)
  } else {
    #allResults <- lapply(exportFolders, loadMainResults)
    allResults <- lapply(cohortMethodResultList,loadShinyResults)
  }
  allResults <- do.call(rbind, allResults)
  if(!is.null(diagnostics)){
    allResults <- unique(
      dplyr::inner_join(allResults, diagnostics[,c("targetId","comparatorId","analysisId","databaseId")],
                      by = c("targetId","comparatorId","analysisId","databaseId"))
      )
  }
  
  groups <- split(allResults, paste(allResults$targetId, allResults$comparatorId, allResults$analysisId))
  rm(allResults)
  cluster <- ParallelLogger::makeCluster(min(maxCores, 10))
  results <- ParallelLogger::clusterApply(cluster, groups, computeGroupMetaAnalysis, interactions = interactions, databaseId = databaseId)
  ParallelLogger::stopCluster(cluster)
  # results <- plyr::compact(results)
  results <- do.call(rbind, results)
  results$trueEffectSize <- NULL
  if (interactions) {
    results$rrr <- results$rr
    results$logRrr <- results$logRr
    results$seLogRrr <- results$seLogRr
    results$rr <- NULL
    results$logRr <- NULL
    results$seLogRr <- NULL
    colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
    fileName <-  file.path(maExportFolder, paste0("cm_interaction_result.csv"))
    write.csv(results, fileName, row.names = FALSE)
    saveRDS(results, file.path(shinyFolder, sprintf("m_interaction_result_%s.rds", databaseId)))
  } else {
    colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
    fileName <-  file.path(maExportFolder, paste0("cohort_method_result.csv"))
    write.csv(results, fileName, row.names = FALSE)
    saveRDS(results, file.path(shinyFolder, sprintf("cohort_method_result_%s.rds", databaseId)))
  }
}

doAddMaEffectType <- function(shinyFolder,
                              maExportFolder,
                              maxCores,
                              interactions,
                              positiveControlOutcome=FALSE){
  cohortMethodResultList <- list.files(shinyFolder, "^cohort_method_result.*.rds$", full.names = TRUE)
  if(grep("Meta-analysis", cohortMethodResultList)){
    ParallelLogger::logWarn(cohortMethodResultList[grep("Meta-analysis", cohortMethodResultList)]," is excluded from meta-analysis since it seems a result of meta-analysis")
    cohortMethodResultList <- cohortMethodResultList[-grep("Meta-analysis", cohortMethodResultList)]
  }
  
  loadShinyResults <- function(shinyFile){
    ParallelLogger::logInfo("Loading main results from ", shinyFile, " for meta-analysis")
    results<-readRDS(shinyFile)
    colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
    colnames(results)[colnames(results) == "ci95lb"] <- "ci95Lb"
    colnames(results)[colnames(results) == "ci95ub"] <- "ci95Ub"
    
    ncs <- readRDS(gsub("cohort_method_result","negative_control_outcome",shinyFile))
    colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
    results$trueEffectSize <- NA
    idx <- results$outcomeId %in% ncs$outcomeId
    results$trueEffectSize[idx] <- 1
    return(results)
  }
  loadMainResults <- function(exportFolder) {
    ParallelLogger::logInfo("Loading main results from ", exportFolder, " for meta-analysis")
    zipFile <- list.files(exportFolder, "^Results.*.zip$", full.names = TRUE)[1]
    utils::unzip(zipfile = zipFile,
                 files = c("cohort_method_result.csv",
                           "negative_control_outcome.csv",
                           "positive_control_outcome.csv"),
                 exdir = exportFolder)
    results <- read.csv(file.path(exportFolder, "cohort_method_result.csv"))
    colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
    colnames(results)[colnames(results) == "ci95lb"] <- "ci95Lb"
    colnames(results)[colnames(results) == "ci95ub"] <- "ci95Ub"
    ncs <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
    colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
    results$trueEffectSize <- NA
    idx <- results$outcomeId %in% ncs$outcomeId
    results$trueEffectSize[idx] <- 1
    if(positiveControlOutcome){
      pcs <- read.csv(file.path(exportFolder, "positive_control_outcome.csv"))
      colnames(pcs) <- SqlRender::snakeCaseToCamelCase(colnames(pcs))
      idx <- results$outcomeId %in% pcs$outcomeId
      results$trueEffectSize[idx] <- pcs$effectSize[match(results$outcomeId[idx],
                                                          pcs$outcomeId)]
    }
    
    return(results)
  }
  loadInteractionResults <- function(exportFolder) {
    ParallelLogger::logInfo("Loading interaction results from ", exportFolder, " for meta-analysis")
    zipFile <- list.files(exportFolder, "^Results.*.zip$", full.names = TRUE)[1]
    utils::unzip(zipfile = zipFile,
                 files = c("cm_interaction_result.csv",
                           "negative_control_outcome.csv"),
                 exdir = exportFolder)
    results <- read.csv(file.path(exportFolder, "cm_interaction_result.csv"))
    colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
    ncs <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
    colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
    results$trueEffectSize <- NA
    idx <- results$outcomeId %in% ncs$outcomeId
    results$trueEffectSize[idx] <- 1
    results$rr <- results$rrr
    results$logRr <- results$logRrr
    results$seLogRr <- results$seLogRrr
    return(results)
  }
  allResults <- lapply(cohortMethodResultList,loadShinyResults)
  allResults <- do.call(rbind, allResults)
  groups <- split(allResults, paste(allResults$targetId, allResults$comparatorId, allResults$analysisId))
  rm(allResults)
  cluster <- ParallelLogger::makeCluster(min(maxCores, 10))
  results <- ParallelLogger::clusterApply(cluster, groups, computeGroupAddMetaAnalysis, interactions = F)
  ParallelLogger::stopCluster(cluster)
  # results <- plyr::compact(results)
  results <- do.call(rbind, results)
  colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
  fileName <-  file.path(maExportFolder, paste0("add_meta_result_revision.csv"))
  write.csv(results, fileName, row.names = FALSE)
  #saveRDS(results, file.path(shinyFolder, "cohort_method_result_Meta-analysis.rds"))
}

computeGroupMetaAnalysis <- function(group, interactions, databaseId = "Meta-analysis") {
  # group <- groups[[1]]
  if (nrow(group) == 0) {
    return(NULL)
  }
  analysisId <- group$analysisId[1]
  targetId <- group$targetId[1]
  comparatorId <- group$comparatorId[1]
  ParallelLogger::logTrace("Performing meta-analysis for target ", targetId, ", comparator ", comparatorId, ", analysis ", analysisId)
  outcomeGroups <- split(group, group$outcomeId)
  outcomeGroupResults <- lapply(outcomeGroups, computeSingleMetaAnalysis,databaseId)
  groupResults <- do.call(rbind, outcomeGroupResults)
  ncs <- groupResults[groupResults$trueEffectSize == 1, ]
  validNcs <- ncs[!is.na(ncs$seLogRr), ]
  if (nrow(validNcs) >= 5) {
    null <- EmpiricalCalibration::fitMcmcNull(validNcs$logRr, validNcs$seLogRr)
    calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                    logRr = groupResults$logRr,
                                                    seLogRr = groupResults$seLogRr)
    groupResults$calibratedP <- calibratedP$p
  } else {
    groupResults$calibratedP <- NA
  }
  if (!interactions) {
    pcs <- groupResults[!is.na(groupResults$trueEffectSize) &
                          groupResults$trueEffectSize != 1, ]
    validPcs <- pcs[!is.na(pcs$seLogRr), ]
    if (nrow(validPcs) > 5) {
      model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = c(validNcs$logRr, validPcs$logRr),
                                                             seLogRr = c(validNcs$seLogRr,
                                                                         validPcs$seLogRr),
                                                             trueLogRr = c(rep(0, nrow(validNcs)),
                                                                           log(validPcs$trueEffectSize)),
                                                             estimateCovarianceMatrix = FALSE)
      calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = groupResults$logRr,
                                                                        seLogRr = groupResults$seLogRr,
                                                                        model = model)
      groupResults$calibratedRr <- exp(calibratedCi$logRr)
      groupResults$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
      groupResults$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
      groupResults$calibratedLogRr <- calibratedCi$logRr
      groupResults$calibratedSeLogRr <- calibratedCi$seLogRr
    } else if(nrow(validNcs) >= 5){
      model <- EmpiricalCalibration::convertNullToErrorModel(null)
      calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr=groupResults$logRr,
                                                                        seLogRr=groupResults$seLogRr,
                                                                        model=model,
                                                                        ciWidth = 0.95)
      groupResults$calibratedRr <- exp(calibratedCi$logRr)
      groupResults$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
      groupResults$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
      groupResults$calibratedLogRr <- calibratedCi$logRr
      groupResults$calibratedSeLogRr <- calibratedCi$seLogRr
    } else{
      groupResults$calibratedRr <- rep(NA, nrow(groupResults))
      groupResults$calibratedCi95Lb <- rep(NA, nrow(groupResults))
      groupResults$calibratedCi95Ub <- rep(NA, nrow(groupResults))
      groupResults$calibratedLogRr <- rep(NA, nrow(groupResults))
      groupResults$calibratedSeLogRr <- rep(NA, nrow(groupResults))
    }
  }
  return(groupResults)
}

computeGroupAddMetaAnalysis <- function(group, interactions){
  if (nrow(group) == 0) {
    return(NULL)
  }
  analysisId <- group$analysisId[1]
  targetId <- group$targetId[1]
  comparatorId <- group$comparatorId[1]
  ParallelLogger::logTrace("Performing meta-analysis for target ", targetId, ", comparator ", comparatorId, ", analysis ", analysisId)
  outcomeGroups <- split(group, group$outcomeId)
  outcomeGroupResults <- lapply(outcomeGroups, computeSingleAddMetaAnalysis)
  groupResults <- do.call(rbind, outcomeGroupResults)
  
  return(groupResults)
}

sumMinCellCount <- function(counts) {
  total <- sum(abs(counts))
  if (any(counts < 0)) {
    total <- -total
  }
  return(total)
}

computeSingleMetaAnalysis <- function(outcomeGroup,
                                      databaseId = "Meta-analysis") {
  # outcomeGroup <- outcomeGroups[[1]]
  maRow <- outcomeGroup[1, ]
  outcomeGroup <- outcomeGroup[!is.na(outcomeGroup$seLogRr), ]
  if (nrow(outcomeGroup) == 0) {
    maRow$targetSubjects <- 0
    maRow$comparatorSubjects <- 0
    maRow$targetDays <- 0
    maRow$comparatorDays <- 0
    maRow$targetOutcomes <- 0
    maRow$comparatorOutcomes <- 0
    maRow$rr <- NA
    maRow$ci95Lb <- NA
    maRow$ci95Ub <- NA
    maRow$p <- NA
    maRow$logRr <- NA
    maRow$seLogRr <- NA
    maRow$i2 <- NA
  } else if (nrow(outcomeGroup) == 1) {
    maRow <- outcomeGroup[1, ]
    maRow$i2 <- 0
  } else {
    maRow$targetSubjects <- sumMinCellCount(outcomeGroup$targetSubjects)
    maRow$comparatorSubjects <- sumMinCellCount(outcomeGroup$comparatorSubjects)
    maRow$targetDays <- sum(outcomeGroup$targetDays)
    maRow$comparatorDays <- sum(outcomeGroup$comparatorDays)
    maRow$targetOutcomes <- sumMinCellCount(outcomeGroup$targetOutcomes)
    maRow$comparatorOutcomes <- sumMinCellCount(outcomeGroup$comparatorOutcomes)
    meta <- meta::metagen(TE = outcomeGroup$logRr,
                          seTE = outcomeGroup$seLogRr,
                          sm = "RR",
                          hakn = FALSE)
    s <- summary(meta)
    maRow$i2 <- s$I2$TE
    rnd <- s$random
    maRow$rr <- exp(rnd$TE)
    maRow$ci95Lb <- exp(rnd$lower)
    maRow$ci95Ub <- exp(rnd$upper)
    maRow$p <- rnd$p
    maRow$logRr <- rnd$TE
    maRow$seLogRr <- rnd$seTE
  }
  maRow$databaseId <- databaseId
  return(maRow)
}

computeSingleAddMetaAnalysis <- function(outcomeGroup,
                                         databaseId = "Meta-analysis") {
  # outcomeGroup <- outcomeGroups[[1]]
  maRow <- outcomeGroup[1, ]
  outcomeGroup <- outcomeGroup[!is.na(outcomeGroup$seLogRr), ]
  if (nrow(outcomeGroup) == 0) {
    maRow$targetSubjects <- 0
    maRow$comparatorSubjects <- 0
    maRow$targetDays <- 0
    maRow$comparatorDays <- 0
    maRow$targetOutcomes <- 0
    maRow$comparatorOutcomes <- 0
    
    maRow$rr <- NA
    maRow$ci95Lb <- NA
    maRow$ci95Ub <- NA
    maRow$p <- NA
    maRow$logRr <- NA
    maRow$seLogRr <- NA
    maRow$i2 <- NA
    
    maRow$fixRr <- NA
    maRow$fixCi95Lb <- NA
    maRow$fixCi95Ub <- NA
    maRow$fixP <- NA
    maRow$fixLogRr <- NA
    maRow$fixSeLogRr <- NA
    
    maRow$irdI2 <- NA
    maRow$ird <- NA
    maRow$irdLowerCi <- NA
    maRow$irdUpperCi <- NA
    maRow$irdPval <- NA
    
    maRow$irdFixed <- NA
    maRow$irdLowerCiFixed <- NA
    maRow$irdUpperCiFixed <- NA
    maRow$irdPvalFixed <- NA
  } else if (nrow(outcomeGroup) == 1) {
    maRow <- outcomeGroup[1, ]
    maRow$i2 <- 0
    maRow$fixRr <- NA
    maRow$fixCi95Lb <- NA
    maRow$fixCi95Ub <- NA
    maRow$fixP <- NA
    maRow$fixLogRr <- NA
    maRow$fixSeLogRr <- NA
    
    maRow$irdI2 <- NA
    maRow$ird <- NA
    maRow$irdLowerCi <- NA
    maRow$irdUpperCi <- NA
    maRow$irdPval <- NA
    
    maRow$irdFixed <- NA
    maRow$irdLowerCiFixed <- NA
    maRow$irdUpperCiFixed <- NA
    maRow$irdPvalFixed <- NA
  } else {
    maRow$targetSubjects <- sumMinCellCount(outcomeGroup$targetSubjects)
    maRow$comparatorSubjects <- sumMinCellCount(outcomeGroup$comparatorSubjects)
    maRow$targetDays <- sum(outcomeGroup$targetDays)
    maRow$comparatorDays <- sum(outcomeGroup$comparatorDays)
    maRow$targetOutcomes <- sumMinCellCount(outcomeGroup$targetOutcomes)
    maRow$comparatorOutcomes <- sumMinCellCount(outcomeGroup$comparatorOutcomes)
    meta <- meta::metagen(TE = outcomeGroup$logRr,
                          seTE = outcomeGroup$seLogRr,
                          sm = "RR",
                          hakn = FALSE)
    s <- summary(meta)
    maRow$i2 <- s$I2$TE
    rnd <- s$random
    maRow$rr <- exp(rnd$TE)
    maRow$ci95Lb <- exp(rnd$lower)
    maRow$ci95Ub <- exp(rnd$upper)
    maRow$p <- rnd$p
    maRow$logRr <- rnd$TE
    maRow$seLogRr <- rnd$seTE
    
    fixed <- s$fixed
    maRow$fixRr <- exp(fixed$TE)
    maRow$fixCi95Lb <- exp(fixed$lower)
    maRow$fixCi95Ub <- exp(fixed$upper)
    maRow$fixP <- fixed$p
    maRow$fixLogRr <- fixed$TE
    maRow$fixSeLogRr <- fixed$seTE
    
    metaIrd <- meta::metainc(event.e = abs(outcomeGroup$targetOutcomes),
                             time.e = outcomeGroup$targetDays/(365*1000),
                             event.c = abs(outcomeGroup$comparatorOutcomes),
                             time.c = outcomeGroup$comparatorDays/(365*1000),
                             method = "MH", #Haenszel
                             sm = "IRD"
    )
    maRow$irdI2 = metaIrd$I2
    maRow$ird = metaIrd$TE.random
    maRow$irdLowerCi = metaIrd$lower.random
    maRow$irdUpperCi = metaIrd$upper.random
    maRow$irdPval = metaIrd$pval.random
    
    maRow$irdFixed = metaIrd$TE.fixed
    maRow$irdLowerCiFixed = metaIrd$lower.fixed
    maRow$irdUpperCiFixed = metaIrd$upper.fixed
    maRow$irdPvalFixed = metaIrd$pval.fixed
    # if((min(outcomeGroup$targetOutcomes)!=0) & (min(outcomeGroup$comparatorOutcomes)!=0) ){
    #     
    # }else{
    #     maRow$irdI2 = NA
    #     maRow$ird = NA
    #     maRow$irdLowerCi = NA
    #     maRow$irdUpperCi = NA
    #     maRow$irdPval = NA
    #     
    #     maRow$irdFixed = NA
    #     maRow$irdLowerCiFixed = NA
    #     maRow$irdUpperCiFixed = NA
    #     maRow$irdPvalFixed = NA
    # }
  }
  maRow$databaseId <- databaseId
  return(maRow)
}