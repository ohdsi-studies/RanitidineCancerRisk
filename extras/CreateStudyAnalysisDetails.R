# Copyright 2019 Observational Health Data Sciences and Informatics
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

createAnalysesDetails <- function(workFolder) {
  maxCohortSizeForFitting = 2000 #250000
  
  covarSettings <- FeatureExtraction::createDefaultCovariateSettings(excludedCovariateConceptIds = c(19011685,
                                                                                                     961047,
                                                                                                     950696, 
                                                                                                     43009003,
                                                                                                     953076,
                                                                                                     997276),
                                                                     addDescendantsToExclude = TRUE)
  
  getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(washoutPeriod = 365,
                                                                   restrictToCommonPeriod = TRUE,
                                                                   firstExposureOnly = FALSE,
                                                                   removeDuplicateSubjects = "keep first",
                                                                   studyStartDate = "",
                                                                   studyEndDate = "",
                                                                   excludeDrugsFromCovariates = FALSE,
                                                                   covariateSettings = covarSettings)
  
  createOnTreatmentStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                      minDaysAtRisk = 1,
                                                                      riskWindowStart = 0,
                                                                      startAnchor  = "cohort start",
                                                                      riskWindowEnd = 30,
                                                                      endAnchor = "cohort end")
  
  unConditionedCox <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                              modelType = "cox",
                                                              stratified = FALSE)
  
  conditionedCox <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                            modelType = "cox",
                                                            stratified = TRUE)
  
  cmAnalysis1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                                description = "On-treatment, no matching",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createOnTreatmentStudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  
  createPsArgs <- CohortMethod::createCreatePsArgs(control = Cyclops::createControl(cvType = "auto",
                                                                                    startingVariance = 0.01,
                                                                                    noiseLevel = "quiet",
                                                                                    tolerance = 2e-07,
                                                                                    cvRepetitions = 10),
                                                   errorOnHighCorrelation = TRUE,
                                                   stopOnError = FALSE, 
                                                   maxCohortSizeForFitting = maxCohortSizeForFitting)
  
  matchOnPsArgs1 <- CohortMethod::createMatchOnPsArgs(maxRatio = 1)
  
  cmAnalysis2 <- CohortMethod::createCmAnalysis(analysisId = 2,
                                                description = "On-treatment, One-on-one matching",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createOnTreatmentStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = matchOnPsArgs1,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  
  matchOnPsArgs2 <- CohortMethod::createMatchOnPsArgs(maxRatio = 100)
  
  cmAnalysis3 <- CohortMethod::createCmAnalysis(analysisId = 3,
                                                description = "On-treatment, Variable ratio matching",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createOnTreatmentStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = matchOnPsArgs2,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 5)
  
  cmAnalysis4 <- CohortMethod::createCmAnalysis(analysisId = 4,
                                                description = "On-treatment, Stratification",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createOnTreatmentStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratifyByPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  
  createAfterOneYearStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                 minDaysAtRisk = 365,
                                                                                 riskWindowStart = 0,
                                                                                 startAnchor = "cohort start",
                                                                                 riskWindowEnd = 30,
                                                                                 endAnchor = "cohort end")
  
  cmAnalysis11 <- CohortMethod::createCmAnalysis(analysisId = 11,
                                                description = "On-treatment after one year, no matching",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createAfterOneYearStudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  
  cmAnalysis12 <- CohortMethod::createCmAnalysis(analysisId = 12,
                                                description = "On-treatment after one year, One-on-one matching",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createAfterOneYearStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = matchOnPsArgs1,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis13 <- CohortMethod::createCmAnalysis(analysisId = 13,
                                                description = "On-treatment after one year, Variable ratio matching",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createAfterOneYearStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = matchOnPsArgs2,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  cmAnalysis14 <- CohortMethod::createCmAnalysis(analysisId = 14,
                                                description = "On-treatment after one year, Stratification",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createAfterOneYearStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratifyByPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  # interactionCovariateIds <- c(8532001, 201826210, 21600960413) # Female, T2DM, concurent use of antithrombotic agents
  # 
  # fitOutcomeModelArgs3 <- CohortMethod::createFitOutcomeModelArgs(modelType = "cox",
  #                                                                 stratified = TRUE,
  #                                                                 useCovariates = FALSE,
  #                                                                 interactionCovariateIds = interactionCovariateIds)
  # 
  # cmAnalysis5 <- CohortMethod::createCmAnalysis(analysisId = 5,
  #                                               description = "Stratification with interaction terms",
  #                                               getDbCohortMethodDataArgs = getDbCmDataArgs,
  #                                               createStudyPopArgs = createOnTreatmentStudyPopArgs,
  #                                               createPs = TRUE,
  #                                               createPsArgs = createPsArgs,
  #                                               stratifyByPs = TRUE,
  #                                               stratifyByPsArgs = stratifyByPsArgs,
  #                                               fitOutcomeModel = TRUE,
  #                                               fitOutcomeModelArgs = fitOutcomeModelArgs3)
  
  cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4
                         , cmAnalysis11, cmAnalysis12, cmAnalysis13, cmAnalysis14
                         )
  
  CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(workFolder, "cmAnalysisList.json"))
}

createPositiveControlSynthesisArgs <- function(workFolder) {
  settings <- list(
    outputIdOffset = 10000,
    firstExposureOnly = TRUE,
    firstOutcomeOnly = TRUE,
    removePeopleWithPriorOutcomes = TRUE,
    modelType = "survival",
    washoutPeriod = 365,
    riskWindowStart = 0,
    riskWindowEnd = 30,
    addExposureDaysToEnd = TRUE,
    effectSizes = c(1.5, 2, 4),
    precision = 0.01,
    prior = Cyclops::createPrior("laplace", exclude = 0, useCrossValidation = TRUE),
    control = Cyclops::createControl(cvType = "auto",
                                     startingVariance = 0.01,
                                     noiseLevel = "quiet",
                                     cvRepetitions = 1,
                                     threads = 1),
    maxSubjectsForModel = 250000,
    minOutcomeCountForModel = 50,
    minOutcomeCountForInjection = 25,
    covariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAgeGroup = TRUE,
                                                                   useDemographicsGender = TRUE,
                                                                   useDemographicsIndexYear = TRUE,
                                                                   useDemographicsIndexMonth = TRUE,
                                                                   useConditionGroupEraLongTerm = TRUE,
                                                                   useDrugGroupEraLongTerm = TRUE,
                                                                   useProcedureOccurrenceLongTerm = TRUE,
                                                                   useMeasurementLongTerm = TRUE,
                                                                   useObservationLongTerm = TRUE,
                                                                   useCharlsonIndex = TRUE,
                                                                   useDcsi = TRUE,
                                                                   useChads2Vasc = TRUE,
                                                                   longTermStartDays = -365,
                                                                   endDays = 0) 
  )
  ParallelLogger::saveSettingsToJson(settings, file.path(workFolder, "positiveControlSynthArgs.json"))
}

