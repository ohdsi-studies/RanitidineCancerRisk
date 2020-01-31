# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' Execute Feasibility test for the study
#'
#'
#' @details
#' This function executes the feasibility test for RanitidineCancerRisk Study.
#' 
#' The \code{createCohorts}, \code{synthesizePositiveControls}, \code{runAnalyses}, and \code{runDiagnostics} arguments
#' are intended to be used to run parts of the full study at a time, but none of the parts are considerd to be optional.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the exposure and outcome cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param databaseName         The full name of the database (e.g. 'Medicare Claims
#'                             Synthetic Public Use Files (SynPUFs)').
#' @param databaseDescription  A short description (several sentences) of the database.
#' @param createCohorts        Create the cohortTable table with the exposure and outcome cohorts?
#' @param runFeasibility        Perform feasibility test?
#' @param runDiagnostics       Compute study diagnostics?
#' @param feasibilityResults   Should results of feasibility test be packaged for later sharing?     
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'
#' execute(connectionDetails,
#'         cdmDatabaseSchema = "cdm_data",
#'         cohortDatabaseSchema = "study_results",
#'         cohortTable = "cohort",
#'         oracleTempSchema = NULL,
#'         outputFolder = "c:/temp/study_results",
#'         maxCores = 4)
#' }
#'
#' @export
runFeasibility <- function(connectionDetails,
                           cdmDatabaseSchema,
                           cohortDatabaseSchema = cdmDatabaseSchema,
                           cohortTable = "cohort",
                           oracleTempSchema = cohortDatabaseSchema,
                           outputFolder,
                           databaseId = "Unknown",
                           databaseName = "Unknown",
                           databaseDescription = "Unknown",
                           createCohorts = TRUE,
                           runFeasibility = TRUE,
                           runFeasibilityDiagnostics = TRUE,
                           feasibilityResults = TRUE,
                           maxCores = 4,
                           minCellCount= 5) {
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  
  if (!is.null(getOption("fftempdir")) && !file.exists(getOption("fftempdir"))) {
    warning("fftempdir '", getOption("fftempdir"), "' not found. Attempting to create folder")
    dir.create(getOption("fftempdir"), recursive = TRUE)
  }
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "feasibilityLog.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT"))
  
  if (createCohorts) {
    ParallelLogger::logInfo("Creating exposure and outcome cohorts")
    createCohorts(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable,
                  oracleTempSchema = oracleTempSchema,
                  outputFolder = outputFolder)
  }
  #chage the outputFolder to the feasiblityExport
  feasibilityOutputFolder = file.path(outputFolder,
                                      "feasibilityExport")
  if (!file.exists(feasibilityOutputFolder))
    dir.create(feasibilityOutputFolder, recursive = TRUE)
  
  if(runFeasibility) {
    ParallelLogger::logInfo("Running CohortMethod analyses for Feasibility")
    runCohortMethod(connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable,
                    oracleTempSchema = oracleTempSchema,
                    outputFolder = feasibilityOutputFolder,
                    maxCores = maxCores,
                    cmAnalysisListFileName = "cmAnalysisFeasibilityList.json")
  }
  
  if (runFeasibilityDiagnostics) {
    ParallelLogger::logInfo("Running diagnostics for feasibility test")
    generateFeasibilityDiagnostics(outputFolder = feasibilityOutputFolder,
                                   maxCores = maxCores)
  }
  
  if (feasibilityResults) {
    ParallelLogger::logInfo("Packaging results for feasibility test")
    exportResultFeas(outputFolder = feasibilityOutputFolder,
                     databaseId = databaseId,
                     databaseName = databaseName,
                     databaseDescription = databaseDescription,
                     minCellCount = minCellCount,
                     maxCores = maxCores)
  }
  
  invisible(NULL)
  
}

#' Generate diagnostics for feasibility test
#'
#' @details
#' This function generates analyses diagnostics. Requires the study to be executed first.
#'
#' @param outputFolder         Name of local folder where the results were generated; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param maxCores              How many parallel cores should be used? If more cores are made
#'                              available this can speed up the analyses.
#'
#' @export
generateFeasibilityDiagnostics <- function(outputFolder, maxCores) {
  cmOutputFolder <- file.path(outputFolder, "cmOutput")
  diagnosticsFolder <- file.path(outputFolder, "diagnostics")
  if (!file.exists(diagnosticsFolder)) {
    dir.create(diagnosticsFolder)
  }
  reference <- readRDS(file.path(cmOutputFolder, "outcomeModelReference.rds"))
  reference <- addCohortNames(reference, "targetId", "targetName")
  reference <- addCohortNames(reference, "comparatorId", "comparatorName")
  reference <- addCohortNames(reference, "outcomeId", "outcomeName")
  reference <- addAnalysisDescription(reference, "analysisId", "analysisDescription")
  analysisSummary <- read.csv(file.path(outputFolder, "analysisSummary.csv"))
  reference <- merge(reference, analysisSummary[, c("targetId", "comparatorId", "outcomeId", "analysisId", "logRr", "seLogRr")])
  allControls <- getAllControls(outputFolder)
  subsets <- split(reference, list(reference$targetId, reference$comparatorId, reference$analysisId))
  #remove empty row in subsets
  subsets<-subsets[lapply(subsets,nrow)>0]
  # subset <- subsets[[1]]
  cluster <- ParallelLogger::makeCluster(min(4, maxCores))
  ParallelLogger::clusterApply(cluster = cluster, 
                               x = subsets, 
                               fun = createDiagnosticsForSubset, 
                               allControls = allControls, 
                               outputFolder = outputFolder, 
                               cmOutputFolder = cmOutputFolder, 
                               diagnosticsFolder = diagnosticsFolder)
  ParallelLogger::stopCluster(cluster)
}

createDiagnosticsForSubset <- function(subset, allControls, outputFolder, cmOutputFolder, diagnosticsFolder) {
  targetId <- subset$targetId[1]
  comparatorId <- subset$comparatorId[1]
  analysisId <- subset$analysisId[1]
  ParallelLogger::logTrace("Generating diagnostics for target ", targetId, ", comparator ", comparatorId, ", analysis ", analysisId)
  ParallelLogger::logDebug("Subset has ", nrow(subset)," entries with ", sum(!is.na(subset$seLogRr)), " valid estimates")
  title <- paste(paste(subset$targetName[1], subset$comparatorName[1], sep = " - "), 
                 subset$analysisDescription[1], sep = "\n")
  controlSubset <- merge(subset,
                         allControls[, c("targetId", "comparatorId", "outcomeId", "oldOutcomeId", "targetEffectSize")])
  
  # Empirical calibration ----------------------------------------------------------------------------------
  
  # Negative controls
  negControlSubset <- controlSubset[controlSubset$targetEffectSize == 1, ]
  validNcs <- sum(!is.na(negControlSubset$seLogRr))
  ParallelLogger::logDebug("Subset has ", validNcs, " valid negative control estimates")
  null <- NULL
  # if (validNcs >= 5) {
  #   null <- EmpiricalCalibration::fitMcmcNull(negControlSubset$logRr, negControlSubset$seLogRr)
  #   fileName <-  file.path(diagnosticsFolder, paste0("nullDistribution_a", analysisId, "_t", targetId, "_c", comparatorId, ".png"))
  #   EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = negControlSubset$logRr,
  #                                               seLogRrNegatives = negControlSubset$seLogRr,
  #                                               null = null,
  #                                               showCis = TRUE,
  #                                               title = title,
  #                                               fileName = fileName)
  # } else {
  #   null <- NULL
  # }
  
  # Positive and negative controls
  # validPcs <- sum(!is.na(controlSubset$seLogRr[controlSubset$targetEffectSize != 1]))
  # ParallelLogger::logDebug("Subset has ", validPcs, " valid positive control estimates")
  # if (validPcs >= 10) {
  #   model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controlSubset$logRr, 
  #                                                          seLogRr = controlSubset$seLogRr, 
  #                                                          trueLogRr = log(controlSubset$targetEffectSize), 
  #                                                          estimateCovarianceMatrix = FALSE)
  #   
  #   fileName <-  file.path(diagnosticsFolder, paste0("controls_a", analysisId, "_t", targetId, "_c", comparatorId, ".png"))
  #   EmpiricalCalibration::plotCiCalibrationEffect(logRr = controlSubset$logRr, 
  #                                                 seLogRr = controlSubset$seLogRr, 
  #                                                 trueLogRr = log(controlSubset$targetEffectSize),
  #                                                 model = model,
  #                                                 title = title,
  #                                                 fileName = fileName)
  #   
  #   fileName <-  file.path(diagnosticsFolder, paste0("ciCoverage_a", analysisId, "_t", targetId, "_c", comparatorId, ".png"))
  #   evaluation <- EmpiricalCalibration::evaluateCiCalibration(logRr = controlSubset$logRr, 
  #                                                             seLogRr = controlSubset$seLogRr, 
  #                                                             trueLogRr = log(controlSubset$targetEffectSize),
  #                                                             crossValidationGroup = controlSubset$oldOutcomeId)
  #   EmpiricalCalibration::plotCiCoverage(evaluation = evaluation,
  #                                        title = title,
  #                                        fileName = fileName)
  # } 
  
  # Statistical power --------------------------------------------------------------------------------------
  outcomeIdsOfInterest <- subset$outcomeId[!(subset$outcomeId %in% controlSubset$outcomeId)]
  mdrrs <- data.frame()
  for (outcomeId in outcomeIdsOfInterest) {
    strataFile <- subset$strataFile[subset$outcomeId == outcomeId]
    if (strataFile == "") {
      strataFile <- subset$studyPopFile[subset$outcomeId == outcomeId]
    }
    population <- readRDS(file.path(cmOutputFolder, strataFile))
    modelFile <- subset$outcomeModelFile[subset$outcomeId == outcomeId]
    model <- readRDS(file.path(cmOutputFolder, modelFile))
    mdrr <- CohortMethod::computeMdrr(population = population, 
                                      alpha = 0.05, 
                                      power = 0.8, 
                                      twoSided = TRUE, 
                                      modelType =  model$outcomeModelType)
    
    mdrr$outcomeId <- outcomeId
    mdrr$outcomeName <- subset$outcomeName[subset$outcomeId == outcomeId]
    mdrrs <- rbind(mdrrs, mdrr)
  }
  mdrrs$analysisId <- analysisId
  mdrrs$analysisDescription <- subset$analysisDescription[1]
  mdrrs$targetId <- targetId
  mdrrs$targetName <- subset$targetName[1]
  mdrrs$comparatorId <- comparatorId
  mdrrs$comparatorName <- subset$comparatorName[1]
  fileName <-  file.path(diagnosticsFolder, paste0("mdrr_a", analysisId, "_t", targetId, "_c", comparatorId, ".csv"))
  write.csv(mdrrs, fileName, row.names = FALSE)
  
  # Covariate balance --------------------------------------------------------------------------------------
  outcomeIdsOfInterest <- subset$outcomeId[!(subset$outcomeId %in% controlSubset$outcomeId)]
  outcomeId = outcomeIdsOfInterest[1]
  for (outcomeId in outcomeIdsOfInterest) {
    balanceFileName <- file.path(outputFolder,
                                 "balance",
                                 sprintf("bal_t%s_c%s_o%s_a%s.rds", targetId, comparatorId, outcomeId, analysisId))
    if (file.exists(balanceFileName)) {
      balance <- readRDS(balanceFileName)
      fileName = file.path(diagnosticsFolder, 
                           sprintf("bal_t%s_c%s_o%s_a%s.csv", targetId, comparatorId, outcomeId, analysisId))
      write.csv(balance, fileName, row.names = FALSE)
      
      outcomeTitle <- paste(paste(subset$targetName[1], subset$comparatorName[1], sep = " - "), 
                            subset$outcomeName[subset$outcomeId == outcomeId], 
                            subset$analysisDescription[1], sep = "\n")
      fileName = file.path(diagnosticsFolder, 
                           sprintf("balanceScatter_t%s_c%s_o%s_a%s.png", targetId, comparatorId, outcomeId, analysisId))
      balanceScatterPlot <- CohortMethod::plotCovariateBalanceScatterPlot(balance = balance,
                                                                          beforeLabel = "Before PS adjustment",
                                                                          afterLabel =  "After PS adjustment",
                                                                          showCovariateCountLabel = TRUE,
                                                                          showMaxLabel = TRUE,
                                                                          title = outcomeTitle,
                                                                          fileName = fileName)
      
      fileName = file.path(diagnosticsFolder, 
                           sprintf("balanceTop_t%s_c%s_o%s_a%s.png", targetId, comparatorId, outcomeId, analysisId))
      balanceTopPlot <- CohortMethod::plotCovariateBalanceOfTopVariables(balance = balance,
                                                                         beforeLabel = "Before PS adjustment",
                                                                         afterLabel =  "After PS adjustment",
                                                                         title = outcomeTitle,
                                                                         fileName = fileName)
    }
  }
  # Propensity score distribution --------------------------------------------------------------------------
  psFile <- subset$sharedPsFile[1]
  if (psFile != "") {
    ps <- readRDS(file.path(cmOutputFolder, psFile))
    fileName <-  file.path(diagnosticsFolder, paste0("ps_a", analysisId, "_t", targetId, "_c", comparatorId, ".png"))
    CohortMethod::plotPs(data = ps,
                         targetLabel = subset$targetName[1],
                         comparatorLabel = subset$comparatorName[1],
                         showCountsLabel = TRUE,
                         showAucLabel = TRUE,
                         showEquiposeLabel = TRUE,
                         title = subset$analysisDescription[1],
                         fileName = fileName)
  }
}


#' Export all results to tables in feasibility test
#'
#' @description
#' Outputs all results to a folder called 'export', and zips them.
#'
#' @param outputFolder          Name of local folder to place results; make sure to use forward slashes
#'                              (/). Do not use a folder on a network drive since this greatly impacts
#'                              performance.
#' @param databaseId            A short string for identifying the database (e.g. 'Synpuf').
#' @param databaseName          The full name of the database.
#' @param databaseDescription   A short description (several sentences) of the database.
#' @param minCellCount          The minimum cell count for fields contains person counts or fractions.
#' @param maxCores              How many parallel cores should be used? If more cores are made
#'                              available this can speed up the analyses.
#'
#' @export
exportResultFeas<- function(outputFolder,
                            databaseId,
                            databaseName,
                            databaseDescription,
                            minCellCount = 5,
                            maxCores) {
  exportFolder <- file.path(outputFolder, "export")
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  
  exportAnalyseFeas(outputFolder = outputFolder,
                    exportFolder = exportFolder)
  
  exportExposures(outputFolder = outputFolder,
                  exportFolder = exportFolder)
  
  exportOutcomes(outputFolder = outputFolder,
                 exportFolder = exportFolder)
  
  exportMetadata(outputFolder = outputFolder,
                 exportFolder = exportFolder,
                 databaseId = databaseId,
                 databaseName = databaseName,
                 databaseDescription = databaseDescription,
                 minCellCount = minCellCount)
  
  exportMainResultFeas(outputFolder = outputFolder,
                       exportFolder = exportFolder,
                       databaseId = databaseId,
                       minCellCount = minCellCount,
                       maxCores = maxCores)
  
  exportDiagnosticsFeas(outputFolder = outputFolder,
                      exportFolder = exportFolder,
                      databaseId = databaseId,
                      minCellCount = minCellCount,
                      maxCores = maxCores)
  
  
  # Add all to zip file -------------------------------------------------------------------------------
  ParallelLogger::logInfo("Adding results to zip file")
  zipName <- file.path(exportFolder, paste0("Results", databaseId, ".zip"))
  files <- list.files(exportFolder, pattern = ".*\\.csv$")
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd))
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
}

exportAnalyseFeas <- function(outputFolder, exportFolder) {
  ParallelLogger::logInfo("Exporting analyses")
  ParallelLogger::logInfo("- cohort_method_analysis table")
  
  tempFileName <- tempfile()
  
  cmAnalysisListFile <- system.file("settings",
                                    "cmAnalysisFeasibilityList.json",
                                    package = "RanitidineCancerRisk")
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
  cmAnalysisToRow <- function(cmAnalysis) {
    ParallelLogger::saveSettingsToJson(cmAnalysis, tempFileName)
    row <- data.frame(analysisId = cmAnalysis$analysisId,
                      description = cmAnalysis$description,
                      definition = readChar(tempFileName, file.info(tempFileName)$size))
    return(row)
  }
  cohortMethodAnalysis <- lapply(cmAnalysisList, cmAnalysisToRow)
  cohortMethodAnalysis <- do.call("rbind", cohortMethodAnalysis)
  cohortMethodAnalysis <- unique(cohortMethodAnalysis)
  unlink(tempFileName)
  colnames(cohortMethodAnalysis) <- SqlRender::camelCaseToSnakeCase(colnames(cohortMethodAnalysis))
  fileName <- file.path(exportFolder, "cohort_method_analysis.csv")
  write.csv(cohortMethodAnalysis, fileName, row.names = FALSE)
  
  
  ParallelLogger::logInfo("- covariate_analysis table")
  reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  getCovariateAnalyses <- function(cmDataFolder) {
    cmData <- CohortMethod::loadCohortMethodData(file.path(outputFolder, "cmOutput", cmDataFolder), readOnly = TRUE)
    covariateAnalysis <- ff::as.ram(cmData$analysisRef)
    covariateAnalysis <- covariateAnalysis[, c("analysisId", "analysisName")]
    return(covariateAnalysis)
  }
  covariateAnalysis <- lapply(unique(reference$cohortMethodDataFolder), getCovariateAnalyses)
  covariateAnalysis <- do.call("rbind", covariateAnalysis)
  covariateAnalysis <- unique(covariateAnalysis)
  colnames(covariateAnalysis) <- c("covariate_analysis_id", "covariate_analysis_name")
  fileName <- file.path(exportFolder, "covariate_analysis.csv")
  write.csv(covariateAnalysis, fileName, row.names = FALSE)
}

exportMainResultFeas <- function(outputFolder,
                                 exportFolder,
                                 databaseId,
                                 minCellCount,
                                 maxCores) {
  ParallelLogger::logInfo("Exporting main results")
  
  
  ParallelLogger::logInfo("- cohort_method_result table")
  analysesSum <- read.csv(file.path(outputFolder, "analysisSummary.csv"))
  allControls <- getAllControls(outputFolder)
  ParallelLogger::logInfo("  Performing empirical calibration on main effects")
  cluster <- ParallelLogger::makeCluster(min(4, maxCores))
  subsets <- split(analysesSum,
                   paste(analysesSum$targetId, analysesSum$comparatorId, analysesSum$analysisId))
  rm(analysesSum)  # Free up memory
  results <- ParallelLogger::clusterApply(cluster,
                                          subsets,
                                          calibrateFeas,
                                          allControls = allControls)
  ParallelLogger::stopCluster(cluster)
  rm(subsets)  # Free up memory
  results <- do.call("rbind", results)
  results$databaseId <- databaseId
  results <- enforceMinCellValue(results, "targetSubjects", minCellCount)
  results <- enforceMinCellValue(results, "comparatorSubjects", minCellCount)
  results <- enforceMinCellValue(results, "targetOutcomes", minCellCount)
  results <- enforceMinCellValue(results, "comparatorOutcomes", minCellCount)
  colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
  fileName <- file.path(exportFolder, "cohort_method_result.csv")
  write.csv(results, fileName, row.names = FALSE)
  rm(results)  # Free up memory
  
  ParallelLogger::logInfo("- cm_interaction_result table")
  reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  loadInteractionsFromOutcomeModel <- function(i) {
    outcomeModel <- readRDS(file.path(outputFolder,
                                      "cmOutput",
                                      reference$outcomeModelFile[i]))
    if (!is.null(outcomeModel$subgroupCounts)) {
      rows <- data.frame(targetId = reference$targetId[i],
                         comparatorId = reference$comparatorId[i],
                         outcomeId = reference$outcomeId[i],
                         analysisId = reference$analysisId[i],
                         interactionCovariateId = outcomeModel$subgroupCounts$subgroupCovariateId,
                         rrr = NA,
                         ci95Lb = NA,
                         ci95Ub = NA,
                         p = NA,
                         i2 = NA,
                         logRrr = NA,
                         seLogRrr = NA,
                         targetSubjects = outcomeModel$subgroupCounts$targetPersons,
                         comparatorSubjects = outcomeModel$subgroupCounts$comparatorPersons,
                         targetDays = outcomeModel$subgroupCounts$targetDays,
                         comparatorDays = outcomeModel$subgroupCounts$comparatorDays,
                         targetOutcomes = outcomeModel$subgroupCounts$targetOutcomes,
                         comparatorOutcomes = outcomeModel$subgroupCounts$comparatorOutcomes)
      if (!is.null(outcomeModel$outcomeModelInteractionEstimates)) {
        idx <- match(outcomeModel$outcomeModelInteractionEstimates$covariateId,
                     rows$interactionCovariateId)
        rows$rrr[idx] <- exp(outcomeModel$outcomeModelInteractionEstimates$logRr)
        rows$ci95Lb[idx] <- exp(outcomeModel$outcomeModelInteractionEstimates$logLb95)
        rows$ci95Ub[idx] <- exp(outcomeModel$outcomeModelInteractionEstimates$logUb95)
        rows$logRrr[idx] <- outcomeModel$outcomeModelInteractionEstimates$logRr
        rows$seLogRrr[idx] <- outcomeModel$outcomeModelInteractionEstimates$seLogRr
        z <- rows$logRrr[idx]/rows$seLogRrr[idx]
        rows$p[idx] <- 2 * pmin(pnorm(z), 1 - pnorm(z))
      }
      return(rows)
    } else {
      return(NULL)
    }
    
  }
  interactions <- plyr::llply(1:nrow(reference),
                              loadInteractionsFromOutcomeModel,
                              .progress = "text")
  interactions <- do.call("rbind", interactions)
  if (!is.null(interactions)) {
    ParallelLogger::logInfo("  Performing empirical calibration on interaction effects")
    allControls <- getAllControls(outputFolder)
    negativeControls <- allControls[allControls$targetEffectSize == 1, ]
    cluster <- ParallelLogger::makeCluster(min(4, maxCores))
    subsets <- split(interactions,
                     paste(interactions$targetId, interactions$comparatorId, interactions$analysisId))
    interactions <- ParallelLogger::clusterApply(cluster,
                                                 subsets,
                                                 calibrateInteractions,
                                                 negativeControls = negativeControls)
    ParallelLogger::stopCluster(cluster)
    rm(subsets)  # Free up memory
    interactions <- do.call("rbind", interactions)
    interactions$databaseId <- databaseId
    
    interactions <- enforceMinCellValue(interactions, "targetSubjects", minCellCount)
    interactions <- enforceMinCellValue(interactions, "comparatorSubjects", minCellCount)
    interactions <- enforceMinCellValue(interactions, "targetOutcomes", minCellCount)
    interactions <- enforceMinCellValue(interactions, "comparatorOutcomes", minCellCount)
    colnames(interactions) <- SqlRender::camelCaseToSnakeCase(colnames(interactions))
    fileName <- file.path(exportFolder, "cm_interaction_result.csv")
    write.csv(interactions, fileName, row.names = FALSE)
    rm(interactions)  # Free up memory
  }
}

calibrateFeas <- function(subset, allControls) {
  ncs <- subset[subset$outcomeId %in% allControls$outcomeId[allControls$targetEffectSize == 1], ]
  ncs <- ncs[!is.na(ncs$seLogRr), ]
  # if (nrow(ncs) > 5) {
  #   null <- EmpiricalCalibration::fitMcmcNull(ncs$logRr, ncs$seLogRr)
  #   calibratedP <- EmpiricalCalibration::calibrateP(null = null,
  #                                                   logRr = subset$logRr,
  #                                                   seLogRr = subset$seLogRr)
  #   subset$calibratedP <- calibratedP$p
  # } else {
  #   subset$calibratedP <- rep(NA, nrow(subset))
  # }
  subset$calibratedP <- rep(NA, nrow(subset))
  
  pcs <- subset[subset$outcomeId %in% allControls$outcomeId[allControls$targetEffectSize != 1], ]
  pcs <- pcs[!is.na(pcs$seLogRr), ]
  # if (nrow(pcs) > 5) {
  #   controls <- merge(subset, allControls[, c("targetId", "comparatorId", "outcomeId", "targetEffectSize")])
  #   model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controls$logRr,
  #                                                          seLogRr = controls$seLogRr,
  #                                                          trueLogRr = log(controls$targetEffectSize),
  #                                                          estimateCovarianceMatrix = FALSE)
  #   calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = subset$logRr,
  #                                                                     seLogRr = subset$seLogRr,
  #                                                                     model = model)
  #   subset$calibratedRr <- exp(calibratedCi$logRr)
  #   subset$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
  #   subset$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
  #   subset$calibratedLogRr <- calibratedCi$logRr
  #   subset$calibratedSeLogRr <- calibratedCi$seLogRr
  # } else {
  #   subset$calibratedRr <- rep(NA, nrow(subset))
  #   subset$calibratedCi95Lb <- rep(NA, nrow(subset))
  #   subset$calibratedCi95Ub <- rep(NA, nrow(subset))
  #   subset$calibratedLogRr <- rep(NA, nrow(subset))
  #   subset$calibratedSeLogRr <- rep(NA, nrow(subset))
  # }
  subset$calibratedRr <- rep(NA, nrow(subset))
  subset$calibratedCi95Lb <- rep(NA, nrow(subset))
  subset$calibratedCi95Ub <- rep(NA, nrow(subset))
  subset$calibratedLogRr <- rep(NA, nrow(subset))
  subset$calibratedSeLogRr <- rep(NA, nrow(subset))
  subset$i2 <- rep(NA, nrow(subset))
  subset <- subset[, c("targetId",
                       "comparatorId",
                       "outcomeId",
                       "analysisId",
                       "rr",
                       "ci95lb",
                       "ci95ub",
                       "p",
                       "i2",
                       "logRr",
                       "seLogRr",
                       "target",
                       "comparator",
                       "targetDays",
                       "comparatorDays",
                       "eventsTarget",
                       "eventsComparator",
                       "calibratedP",
                       "calibratedRr",
                       "calibratedCi95Lb",
                       "calibratedCi95Ub",
                       "calibratedLogRr",
                       "calibratedSeLogRr")]
  colnames(subset) <- c("targetId",
                        "comparatorId",
                        "outcomeId",
                        "analysisId",
                        "rr",
                        "ci95Lb",
                        "ci95Ub",
                        "p",
                        "i2",
                        "logRr",
                        "seLogRr",
                        "targetSubjects",
                        "comparatorSubjects",
                        "targetDays",
                        "comparatorDays",
                        "targetOutcomes",
                        "comparatorOutcomes",
                        "calibratedP",
                        "calibratedRr",
                        "calibratedCi95Lb",
                        "calibratedCi95Ub",
                        "calibratedLogRr",
                        "calibratedSeLogRr")
  return(subset)
}

exportDiagnosticsFeas <- function(outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount,
                              maxCores) {
  ParallelLogger::logInfo("Exporting diagnostics")
  ParallelLogger::logInfo("- covariate_balance table")
  fileName <- file.path(exportFolder, "covariate_balance.csv")
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  first <- TRUE
  balanceFolder <- file.path(outputFolder, "balance")
  files <- list.files(balanceFolder, pattern = "bal_.*.rds", full.names = TRUE)
  if (length(files) !=0){
    pb <- txtProgressBar(style = 3)
    for (i in 1:length(files)) {
      ids <- gsub("^.*bal_t", "", files[i])
      targetId <- as.numeric(gsub("_c.*", "", ids))
      ids <- gsub("^.*_c", "", ids)
      comparatorId <- as.numeric(gsub("_[aso].*$", "", ids))
      if (grepl("_s", ids)) {
        subgroupId <- as.numeric(gsub("^.*_s", "", gsub("_a[0-9]*.rds", "", ids)))
      } else {
        subgroupId <- NA
      }
      if (grepl("_o", ids)) {
        outcomeId <- as.numeric(gsub("^.*_o", "", gsub("_a[0-9]*.rds", "", ids)))
      } else {
        outcomeId <- NA
      }
      ids <- gsub("^.*_a", "", ids)
      analysisId <- as.numeric(gsub(".rds", "", ids))
      balance <- readRDS(files[i])
      inferredTargetBeforeSize <- mean(balance$beforeMatchingSumTarget/balance$beforeMatchingMeanTarget,
                                       na.rm = TRUE)
      inferredComparatorBeforeSize <- mean(balance$beforeMatchingSumComparator/balance$beforeMatchingMeanComparator,
                                           na.rm = TRUE)
      inferredTargetAfterSize <- mean(balance$afterMatchingSumTarget/balance$afterMatchingMeanTarget,
                                      na.rm = TRUE)
      inferredComparatorAfterSize <- mean(balance$afterMatchingSumComparator/balance$afterMatchingMeanComparator,
                                          na.rm = TRUE)
      
      balance$databaseId <- databaseId
      balance$targetId <- targetId
      balance$comparatorId <- comparatorId
      balance$outcomeId <- outcomeId
      balance$analysisId <- analysisId
      balance$interactionCovariateId <- subgroupId
      balance <- balance[, c("databaseId",
                             "targetId",
                             "comparatorId",
                             "outcomeId",
                             "analysisId",
                             "interactionCovariateId",
                             "covariateId",
                             "beforeMatchingMeanTarget",
                             "beforeMatchingMeanComparator",
                             "beforeMatchingStdDiff",
                             "afterMatchingMeanTarget",
                             "afterMatchingMeanComparator",
                             "afterMatchingStdDiff")]
      colnames(balance) <- c("databaseId",
                             "targetId",
                             "comparatorId",
                             "outcomeId",
                             "analysisId",
                             "interactionCovariateId",
                             "covariateId",
                             "targetMeanBefore",
                             "comparatorMeanBefore",
                             "stdDiffBefore",
                             "targetMeanAfter",
                             "comparatorMeanAfter",
                             "stdDiffAfter")
      balance$targetMeanBefore[is.na(balance$targetMeanBefore)] <- 0
      balance$comparatorMeanBefore[is.na(balance$comparatorMeanBefore)] <- 0
      balance$stdDiffBefore <- round(balance$stdDiffBefore, 3)
      balance$targetMeanAfter[is.na(balance$targetMeanAfter)] <- 0
      balance$comparatorMeanAfter[is.na(balance$comparatorMeanAfter)] <- 0
      balance$stdDiffAfter <- round(balance$stdDiffAfter, 3)
      balance <- enforceMinCellValue(balance,
                                     "targetMeanBefore",
                                     minCellCount/inferredTargetBeforeSize,
                                     TRUE)
      balance <- enforceMinCellValue(balance,
                                     "comparatorMeanBefore",
                                     minCellCount/inferredComparatorBeforeSize,
                                     TRUE)
      balance <- enforceMinCellValue(balance,
                                     "targetMeanAfter",
                                     minCellCount/inferredTargetAfterSize,
                                     TRUE)
      balance <- enforceMinCellValue(balance,
                                     "comparatorMeanAfter",
                                     minCellCount/inferredComparatorAfterSize,
                                     TRUE)
      balance$targetMeanBefore <- round(balance$targetMeanBefore, 3)
      balance$comparatorMeanBefore <- round(balance$comparatorMeanBefore, 3)
      balance$targetMeanAfter <- round(balance$targetMeanAfter, 3)
      balance$comparatorMeanAfter <- round(balance$comparatorMeanAfter, 3)
      balance <- balance[balance$targetMeanBefore != 0 & balance$comparatorMeanBefore != 0 & balance$targetMeanAfter !=
                           0 & balance$comparatorMeanAfter != 0 & balance$stdDiffBefore != 0 & balance$stdDiffAfter !=
                           0, ]
      balance <- balance[!is.na(balance$targetId), ]
      colnames(balance) <- SqlRender::camelCaseToSnakeCase(colnames(balance))
      write.table(x = balance,
                  file = fileName,
                  row.names = FALSE,
                  col.names = first,
                  sep = ",",
                  dec = ".",
                  qmethod = "double",
                  append = !first)
      first <- FALSE
      setTxtProgressBar(pb, i/length(files))
    }
    close(pb)
  }
  
  ParallelLogger::logInfo("- preference_score_dist table")
  preparePlot <- function(i, reference) {
    psFileName <- file.path(outputFolder,
                            "cmOutput",
                            reference$sharedPsFile[i])
    if (file.exists(psFileName)) {
      ps <- readRDS(psFileName)
      if (min(ps$propensityScore) < max(ps$propensityScore)) {
        ps <- CohortMethod:::computePreferenceScore(ps)
        
        d1 <- density(ps$preferenceScore[ps$treatment == 1], from = 0, to = 1, n = 100)
        d0 <- density(ps$preferenceScore[ps$treatment == 0], from = 0, to = 1, n = 100)
        
        result <- data.frame(databaseId = databaseId,
                             targetId = reference$targetId[i],
                             comparatorId = reference$comparatorId[i],
                             preferenceScore = d1$x,
                             targetDensity = d1$y,
                             comparatorDensity = d0$y)
        return(result)
      }
    }
    return(NULL)
  }
  reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  reference <- reference[order(reference$sharedPsFile), ]
  reference <- reference[!duplicated(reference$sharedPsFile), ]
  reference <- reference[reference$sharedPsFile != "", ]
  if(nrow(reference) != 0 ){
    data <- plyr::llply(1:nrow(reference),
                        preparePlot,
                        reference = reference,
                        .progress = "text")
    data <- do.call("rbind", data)
    fileName <- file.path(exportFolder, "preference_score_dist.csv")
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    write.csv(data, fileName, row.names = FALSE)
  }
  
  ParallelLogger::logInfo("- propensity_model table")
  getPsModel <- function(i, reference) {
    psFileName <- file.path(outputFolder,
                            "cmOutput",
                            reference$sharedPsFile[i])
    if (file.exists(psFileName)) {
      ps <- readRDS(psFileName)
      metaData <- attr(ps, "metaData")
      if (is.null(metaData$psError)) {
        cmDataFile <- file.path(outputFolder,
                                "cmOutput",
                                reference$cohortMethodDataFolder[i])
        cmData <- CohortMethod::loadCohortMethodData(cmDataFile)
        model <- CohortMethod::getPsModel(ps, cmData)
        model$covariateId[is.na(model$covariateId)] <- 0
        ff::close.ffdf(cmData$covariates)
        ff::close.ffdf(cmData$covariateRef)
        ff::close.ffdf(cmData$analysisRef)
        model$databaseId <- databaseId
        model$targetId <- reference$targetId[i]
        model$comparatorId <- reference$comparatorId[i]
        model <- model[, c("databaseId", "targetId", "comparatorId", "covariateId", "coefficient")]
        return(model)
      }
    }
    return(NULL)
  }
  reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  reference <- reference[order(reference$sharedPsFile), ]
  reference <- reference[!duplicated(reference$sharedPsFile), ]
  reference <- reference[reference$sharedPsFile != "", ]
  
  if(nrow(reference) != 0 ){
    data <- plyr::llply(1:nrow(reference),
                        getPsModel,
                        reference = reference,
                        .progress = "text")
    data <- do.call("rbind", data)
    fileName <- file.path(exportFolder, "propensity_model.csv")
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    write.csv(data, fileName, row.names = FALSE)
  }
  
  ParallelLogger::logInfo("- kaplan_meier_dist table")
  ParallelLogger::logInfo("  Computing KM curves")
  reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  outcomesOfInterest <- getOutcomesOfInterest()
  reference <- reference[reference$outcomeId %in% outcomesOfInterest, ]
  reference <- reference[, c("strataFile",
                             "studyPopFile",
                             "targetId",
                             "comparatorId",
                             "outcomeId",
                             "analysisId")]
  tempFolder <- file.path(exportFolder, "temp")
  if (!file.exists(tempFolder)) {
    dir.create(tempFolder)
  }
  cluster <- ParallelLogger::makeCluster(min(4, maxCores))
  tasks <- split(reference, seq(nrow(reference)))
  ParallelLogger::clusterApply(cluster,
                               tasks,
                               prepareKm,
                               outputFolder = outputFolder,
                               tempFolder = tempFolder,
                               databaseId = databaseId,
                               minCellCount = minCellCount)
  ParallelLogger::stopCluster(cluster)
  ParallelLogger::logInfo("  Writing to single csv file")
  saveKmToCsv <- function(file, first, outputFile) {
    data <- readRDS(file)
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    write.table(x = data,
                file = outputFile,
                row.names = FALSE,
                col.names = first,
                sep = ",",
                dec = ".",
                qmethod = "double",
                append = !first)
  }
  outputFile <- file.path(exportFolder, "kaplan_meier_dist.csv")
  files <- list.files(tempFolder, "km_.*.rds", full.names = TRUE)
  saveKmToCsv(files[1], first = TRUE, outputFile = outputFile)
  if (length(files) > 1) {
    plyr::l_ply(files[2:length(files)], saveKmToCsv, first = FALSE, outputFile = outputFile, .progress = "text")
  }  
  unlink(tempFolder, recursive = TRUE)
}