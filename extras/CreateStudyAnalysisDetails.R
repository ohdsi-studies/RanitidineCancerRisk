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
  maxCohortSizeForFitting = 250000
  
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
  
  OnTreatment1to0StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                 minDaysAtRisk = 1,
                                                                                 riskWindowStart = 1,
                                                                                 startAnchor  = "cohort start",
                                                                                 riskWindowEnd = 0,
                                                                                 endAnchor = "cohort end")
  
  OnTreatment30to0StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                  minDaysAtRisk = 1,
                                                                                  riskWindowStart = 30,
                                                                                  startAnchor  = "cohort start",
                                                                                  riskWindowEnd = 0,
                                                                                  endAnchor = "cohort end")
  
  OnTreatment365to0StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                   minDaysAtRisk = 1,
                                                                                   riskWindowStart = 365,
                                                                                   startAnchor  = "cohort start",
                                                                                   riskWindowEnd = 0,
                                                                                   endAnchor = "cohort end")
  
  OnTreatment1to365StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                  minDaysAtRisk = 1,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor  = "cohort start",
                                                                                  riskWindowEnd = 365,
                                                                                  endAnchor = "cohort end")
  
  OnTreatment30to365StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                   minDaysAtRisk = 1,
                                                                                   riskWindowStart = 30,
                                                                                   startAnchor  = "cohort start",
                                                                                   riskWindowEnd = 365,
                                                                                   endAnchor = "cohort end")
  
  OnTreatment365to365StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                    minDaysAtRisk = 1,
                                                                                    riskWindowStart = 365,
                                                                                    startAnchor  = "cohort start",
                                                                                    riskWindowEnd = 365,
                                                                                    endAnchor = "cohort end")
  
  OnTreatment1to1095StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                               minDaysAtRisk = 1,
                                                                                               riskWindowStart = 1,
                                                                                               startAnchor  = "cohort start",
                                                                                               riskWindowEnd = 1095,
                                                                                               endAnchor = "cohort end")
  
  OnTreatment30to1095StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                  minDaysAtRisk = 1,
                                                                                  riskWindowStart = 30,
                                                                                  startAnchor  = "cohort start",
                                                                                  riskWindowEnd = 1095,
                                                                                  endAnchor = "cohort end")
  
  OnTreatment365to1095StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                   minDaysAtRisk = 1,
                                                                                   riskWindowStart = 365,
                                                                                   startAnchor  = "cohort start",
                                                                                   riskWindowEnd = 1095,
                                                                                   endAnchor = "cohort end")
  
  ITTBlankingOf1StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                              minDaysAtRisk = 1,
                                                                              riskWindowStart = 1,
                                                                              startAnchor  = "cohort start",
                                                                              riskWindowEnd = 9999,
                                                                              endAnchor = "cohort end")
  
  ITTBlankingOf30StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                              minDaysAtRisk = 1,
                                                                              riskWindowStart = 30,
                                                                              startAnchor  = "cohort start",
                                                                              riskWindowEnd = 9999,
                                                                              endAnchor = "cohort end")
  
  ITTBlankingOf365StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                               minDaysAtRisk = 1,
                                                                               riskWindowStart = 365,
                                                                               startAnchor  = "cohort start",
                                                                               riskWindowEnd = 9999,
                                                                               endAnchor = "cohort end")
  
  createPsArgs <- CohortMethod::createCreatePsArgs(control = Cyclops::createControl(cvType = "auto",
                                                                                    startingVariance = 0.01,
                                                                                    noiseLevel = "quiet",
                                                                                    tolerance = 2e-07,
                                                                                    cvRepetitions = 10),
                                                   errorOnHighCorrelation = TRUE,
                                                   stopOnError = F, 
                                                   maxCohortSizeForFitting = maxCohortSizeForFitting)
  
  
  unConditionedCox <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                              modelType = "cox",
                                                              stratified = FALSE)
  
  conditionedCox <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                            modelType = "cox",
                                                            stratified = TRUE)
  
  oneToOneMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 1)
  
  variableRatioMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 100)
  
  stratificationMatchOnPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10)
  
  description1 = "No PS matching, On-treatment"
  description2 = "No PS matching, On-treatment, with blanking period of 30 days"
  description3 = "No PS matching, On-treatment, with blanking period of 1 year"
  
  description4 = "1:1 PS matching, On-treatment"
  description5 = "1:1 PS matching, On-treatment, with blanking period of 30 days"
  description6 = "1:1 PS matching, On-treatment, with blanking period of 1 year"
  
  description7 = "Variable-ratio PS matching, On-treatment"
  description8 = "Variable-ratio PS matching, with blanking period of 30 days"
  description9 = "Variable-ratio PS matching, with blanking period of 1 year"
  
  description10 = "PS stratification, On-treatment"
  description11 = "PS stratification, On-treatment, with blanking period of 30 days"
  description12 = "PS stratification, On-treatment, with blanking period of 1 year"
  
  description13 = "No PS matching, 1 Year added to on-treatment"
  description14 = "No PS matching, 1 Year added to on-treatment, with blanking period of 30 days"
  description15 = "No PS matching, 1 Year added to on-treatment, with blanking period of 1 year"
  
  description16 = "1:1 PS matching, 1 Year added to on-treatment"
  description17 = "1:1 PS matching, 1 Year added to on-treatment, with blanking period of 30 days"
  description18 = "1:1 PS matching, 1 Year added to on-treatment, with blanking period of 1 year"
  
  description19 = "Variable-ratio PS matching, 1 Year added to on-treatment"
  description20 = "Variable-ratio PS matching, 1 Year added to on-treatment, with blanking period of 30 days"
  description21 = "Variable-ratio PS matching, 1 Year added to on-treatment, with blanking period of 1 year"
  
  description22 = "PS stratification, 1 Year added to on-treatment"
  description23 = "PS stratification, 1 Year added to on-treatment, with blanking period of 30 days"
  description24 = "PS stratification, 1 Year added to on-treatment, with blanking period of 1 year"
  
  description25 = "No PS matching, 3 Year added to on-treatment"
  description26 = "No PS matching, 3 Year added to on-treatment, with blanking period of 30 days"
  description27 = "No PS matching, 3 Year added to on-treatment, with blanking period of 1 year"
  
  description28 = "1:1 PS matching, 3 Year added to on-treatment"
  description29 = "1:1 PS matching, 3 Year added to on-treatment, with blanking period of 30 days"
  description30 = "1:1 PS matching, 3 Year added to on-treatment, with blanking period of 1 year"
  
  description31 = "Variable-ratio PS matching, 3 Year added to on-treatment"
  description32 = "Variable-ratio PS matching, 3 Year added to on-treatment, with blanking period of 30 days"
  description33 = "Variable-ratio PS matching, 3 Year added to on-treatment, with blanking period of 1 year"
  
  description34 = "PS stratification, 3 Year added to on-treatment"
  description35 = "PS stratification, 3 Year added to on-treatment, with blanking period of 30 days"
  description36 = "PS stratification, 3 Year added to on-treatment, with blanking period of 1 year"
  
  description37 = "No PS matching, ITT"
  description38 = "No PS matching, ITT, with blanking period of 30 days"
  description39 = "No PS matching, ITT, with blanking period of 1 year"
  
  description40 = "1:1 PS matching, ITT"
  description41 = "1:1 PS matching, ITT, with blanking period of 30 days"
  description42 = "1:1 PS matching, ITT, with blanking period of 1 year"
  
  description43 = "Variable-ratio PS matching, ITT"
  description44 = "Variable-ratio PS matching, ITT, with blanking period of 30 days"
  description45 = "Variable-ratio PS matching, ITT, with blanking period of 1 year"
  
  description46 = "PS stratification, ITT"
  description47 = "PS stratification, ITT, with blanking period of 30 days"
  description48 = "PS stratification, ITT, with blanking period of 1 year"
  
  
  ####1-12####
  #Without matching
  cmAnalysis1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                                description = description1,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to0StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis2 <- CohortMethod::createCmAnalysis(analysisId = 2,
                                                description = description2,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to0StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis3 <- CohortMethod::createCmAnalysis(analysisId = 3,
                                                description = description3,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to0StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  #1:1 PS matching
  cmAnalysis4 <- CohortMethod::createCmAnalysis(analysisId = 4,
                                                description = description4,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis5 <- CohortMethod::createCmAnalysis(analysisId = 5,
                                                description = description5,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis6 <- CohortMethod::createCmAnalysis(analysisId = 6,
                                                description = description6,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  #Variable-ratio PS matching
  cmAnalysis7 <- CohortMethod::createCmAnalysis(analysisId = 7,
                                                description = description7,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis8 <- CohortMethod::createCmAnalysis(analysisId = 8,
                                                description = description8,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis9 <- CohortMethod::createCmAnalysis(analysisId = 9,
                                                description = description9,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  #PS stratification
  cmAnalysis10 <- CohortMethod::createCmAnalysis(analysisId = 10,
                                                description = description10,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis11 <- CohortMethod::createCmAnalysis(analysisId = 11,
                                                description = description11,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis12 <- CohortMethod::createCmAnalysis(analysisId = 12,
                                                description = description12,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to0StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)

  
  ####13-24####
  #Without matching
  cmAnalysis13 <- CohortMethod::createCmAnalysis(analysisId = 13,
                                                description = description13,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to365StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis14 <- CohortMethod::createCmAnalysis(analysisId = 14,
                                                description = description14,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to365StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis15 <- CohortMethod::createCmAnalysis(analysisId = 15,
                                                description = description15,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to365StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  #1:1 PS matching
  cmAnalysis16 <- CohortMethod::createCmAnalysis(analysisId = 16,
                                                description = description16,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis17 <- CohortMethod::createCmAnalysis(analysisId = 17,
                                                description = description17,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis18 <- CohortMethod::createCmAnalysis(analysisId = 18,
                                                description = description18,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  #Variable-ratio PS matching
  cmAnalysis19 <- CohortMethod::createCmAnalysis(analysisId = 19,
                                                description = description19,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis20 <- CohortMethod::createCmAnalysis(analysisId = 20,
                                                description = description20,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis21 <- CohortMethod::createCmAnalysis(analysisId = 21,
                                                description = description21,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  #PS stratification
  cmAnalysis22 <- CohortMethod::createCmAnalysis(analysisId = 22,
                                                description = description22,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis23 <- CohortMethod::createCmAnalysis(analysisId = 23,
                                                description = description23,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis24 <- CohortMethod::createCmAnalysis(analysisId = 24,
                                                description = description24,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  ####25-36####
  #Without matching
  cmAnalysis25 <- CohortMethod::createCmAnalysis(analysisId = 25,
                                                description = description25,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to1095StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis26 <- CohortMethod::createCmAnalysis(analysisId = 26,
                                                description = description26,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to1095StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis27 <- CohortMethod::createCmAnalysis(analysisId = 27,
                                                description = description27,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to1095StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  #1:1 PS matching
  cmAnalysis28 <- CohortMethod::createCmAnalysis(analysisId = 28,
                                                description = description28,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis29 <- CohortMethod::createCmAnalysis(analysisId = 29,
                                                description = description29,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis30 <- CohortMethod::createCmAnalysis(analysisId = 30,
                                                description = description30,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  #Variable-ratio PS matching
  cmAnalysis31 <- CohortMethod::createCmAnalysis(analysisId = 31,
                                                description = description31,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis32 <- CohortMethod::createCmAnalysis(analysisId = 32,
                                                description = description32,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis33 <- CohortMethod::createCmAnalysis(analysisId = 33,
                                                description = description33,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  #PS stratification
  cmAnalysis34 <- CohortMethod::createCmAnalysis(analysisId = 34,
                                                description = description34,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment1to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis35 <- CohortMethod::createCmAnalysis(analysisId = 35,
                                                description = description35,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment30to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis36 <- CohortMethod::createCmAnalysis(analysisId = 36,
                                                description = description36,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = OnTreatment365to1095StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  
  ####37-48####
  #Without matching
  cmAnalysis37 <- CohortMethod::createCmAnalysis(analysisId = 37,
                                                description = description37,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf1StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis38 <- CohortMethod::createCmAnalysis(analysisId = 38,
                                                description = description38,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf30StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis39 <- CohortMethod::createCmAnalysis(analysisId = 39,
                                                description = description39,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf365StudyPopArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  #1:1 PS matching
  cmAnalysis40 <- CohortMethod::createCmAnalysis(analysisId = 40,
                                                description = description40,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf1StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis41 <- CohortMethod::createCmAnalysis(analysisId = 41,
                                                description = description41,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf30StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis42 <- CohortMethod::createCmAnalysis(analysisId = 42,
                                                description = description42,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = oneToOneMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = unConditionedCox)
  #Variable-ratio PS matching
  cmAnalysis43 <- CohortMethod::createCmAnalysis(analysisId = 43,
                                                description = description43,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf1StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis44 <- CohortMethod::createCmAnalysis(analysisId = 44,
                                                description = description44,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf30StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis45 <- CohortMethod::createCmAnalysis(analysisId = 45,
                                                description = description45,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = variableRatioMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  
  #PS stratification
  cmAnalysis46 <- CohortMethod::createCmAnalysis(analysisId = 46,
                                                description = description46,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf1StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis47 <- CohortMethod::createCmAnalysis(analysisId = 47,
                                                description = description47,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf30StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = conditionedCox)
  cmAnalysis48 <- CohortMethod::createCmAnalysis(analysisId = 48,
                                                description = description48,
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = ITTBlankingOf365StudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratificationMatchOnPsArgs,
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
  
  
  #paste0("cmAnalysis",1:48,collapse=", ")
  
  cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5, cmAnalysis6, 
                         cmAnalysis7, cmAnalysis8, cmAnalysis9, cmAnalysis10, cmAnalysis11, cmAnalysis12, 
                         cmAnalysis13, cmAnalysis14, cmAnalysis15, cmAnalysis16, cmAnalysis17, cmAnalysis18, 
                         cmAnalysis19, cmAnalysis20, cmAnalysis21, cmAnalysis22, cmAnalysis23, cmAnalysis24, 
                         cmAnalysis25, cmAnalysis26, cmAnalysis27, cmAnalysis28, cmAnalysis29, cmAnalysis30, 
                         cmAnalysis31, cmAnalysis32, cmAnalysis33, cmAnalysis34, cmAnalysis35, cmAnalysis36, 
                         cmAnalysis37, cmAnalysis38, cmAnalysis39, cmAnalysis40, cmAnalysis41, cmAnalysis42, 
                         cmAnalysis43, cmAnalysis44, cmAnalysis45, cmAnalysis46, cmAnalysis47, cmAnalysis48)
  
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

createFeasibilityAnalysesDetails <- function(workFolder) {
  maxCohortSizeForFitting = 1000
  minCovarSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                 useDemographicsAge = TRUE,
                                                                 useDemographicsAgeGroup = TRUE,
                                                                 useDemographicsRace = TRUE, 
                                                                 useDemographicsEthnicity = TRUE,
                                                                 useDemographicsIndexYear = TRUE,
                                                                 useDemographicsPriorObservationTime = TRUE,
                                                                 useDemographicsPostObservationTime = TRUE,
                                                                 useDemographicsTimeInCohort = TRUE)
  
  minDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(washoutPeriod = 365,
                                                                   restrictToCommonPeriod = TRUE,
                                                                   firstExposureOnly = FALSE,
                                                                   removeDuplicateSubjects = "keep first",
                                                                   studyStartDate = "",
                                                                   studyEndDate = "",
                                                                   excludeDrugsFromCovariates = FALSE,
                                                                   covariateSettings = minCovarSettings)
  
  OnTreatment1to0StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                               minDaysAtRisk = 1,
                                                                               riskWindowStart = 1,
                                                                               startAnchor  = "cohort start",
                                                                               riskWindowEnd = 0,
                                                                               endAnchor = "cohort end")
  
  OnTreatment30to0StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                minDaysAtRisk = 1,
                                                                                riskWindowStart = 30,
                                                                                startAnchor  = "cohort start",
                                                                                riskWindowEnd = 0,
                                                                                endAnchor = "cohort end")
  
  OnTreatment365to0StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                 minDaysAtRisk = 1,
                                                                                 riskWindowStart = 365,
                                                                                 startAnchor  = "cohort start",
                                                                                 riskWindowEnd = 0,
                                                                                 endAnchor = "cohort end")
  
  OnTreatment1to365StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                 minDaysAtRisk = 1,
                                                                                 riskWindowStart = 1,
                                                                                 startAnchor  = "cohort start",
                                                                                 riskWindowEnd = 365,
                                                                                 endAnchor = "cohort end")
  
  OnTreatment30to365StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                  minDaysAtRisk = 1,
                                                                                  riskWindowStart = 30,
                                                                                  startAnchor  = "cohort start",
                                                                                  riskWindowEnd = 365,
                                                                                  endAnchor = "cohort end")
  
  OnTreatment365to365StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                   minDaysAtRisk = 1,
                                                                                   riskWindowStart = 365,
                                                                                   startAnchor  = "cohort start",
                                                                                   riskWindowEnd = 365,
                                                                                   endAnchor = "cohort end")
  
  OnTreatment1to1095StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                  minDaysAtRisk = 1,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor  = "cohort start",
                                                                                  riskWindowEnd = 1095,
                                                                                  endAnchor = "cohort end")
  
  OnTreatment30to1095StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                   minDaysAtRisk = 1,
                                                                                   riskWindowStart = 30,
                                                                                   startAnchor  = "cohort start",
                                                                                   riskWindowEnd = 1095,
                                                                                   endAnchor = "cohort end")
  
  OnTreatment365to1095StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                    minDaysAtRisk = 1,
                                                                                    riskWindowStart = 365,
                                                                                    startAnchor  = "cohort start",
                                                                                    riskWindowEnd = 1095,
                                                                                    endAnchor = "cohort end")
  
  ITTBlankingOf1StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                              minDaysAtRisk = 1,
                                                                              riskWindowStart = 1,
                                                                              startAnchor  = "cohort start",
                                                                              riskWindowEnd = 9999,
                                                                              endAnchor = "cohort end")
  
  ITTBlankingOf30StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                               minDaysAtRisk = 1,
                                                                               riskWindowStart = 30,
                                                                               startAnchor  = "cohort start",
                                                                               riskWindowEnd = 9999,
                                                                               endAnchor = "cohort end")
  
  ITTBlankingOf365StudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                                minDaysAtRisk = 1,
                                                                                riskWindowStart = 365,
                                                                                startAnchor  = "cohort start",
                                                                                riskWindowEnd = 9999,
                                                                                endAnchor = "cohort end")
  
  # createPsArgs <- CohortMethod::createCreatePsArgs(control = Cyclops::createControl(cvType = "auto",
  #                                                                                   startingVariance = 0.01,
  #                                                                                   noiseLevel = "quiet",
  #                                                                                   tolerance = 2e-07,
  #                                                                                   cvRepetitions = 10),
  #                                                  errorOnHighCorrelation = TRUE,
  #                                                  stopOnError = F, 
  #                                                  maxCohortSizeForFitting = maxCohortSizeForFitting)
  
  
  unConditionedCox <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                              modelType = "cox",
                                                              stratified = FALSE)
  
  conditionedCox <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                            modelType = "cox",
                                                            stratified = TRUE)
  
  oneToOneMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 1)
  
  variableRatioMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 100)
  
  stratificationMatchOnPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10)
  
  description1 = "No PS matching, On-treatment"
  description2 = "No PS matching, On-treatment, with blanking period of 30 days"
  description3 = "No PS matching, On-treatment, with blanking period of 1 year"
  
  description13 = "No PS matching, 1 Year added to on-treatment"
  description14 = "No PS matching, 1 Year added to on-treatment, with blanking period of 30 days"
  description15 = "No PS matching, 1 Year added to on-treatment, with blanking period of 1 year"
  
  description25 = "No PS matching, 3 Year added to on-treatment"
  description26 = "No PS matching, 3 Year added to on-treatment, with blanking period of 30 days"
  description27 = "No PS matching, 3 Year added to on-treatment, with blanking period of 1 year"
  
  description37 = "No PS matching, ITT"
  description38 = "No PS matching, ITT, with blanking period of 30 days"
  description39 = "No PS matching, ITT, with blanking period of 1 year"
  
  
  ####1-12####
  #Without matching
  cmAnalysis991 <- CohortMethod::createCmAnalysis(analysisId = 991,
                                                  description = description1,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment1to0StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis992 <- CohortMethod::createCmAnalysis(analysisId = 992,
                                                  description = description2,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment30to0StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis993 <- CohortMethod::createCmAnalysis(analysisId = 993,
                                                  description = description3,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment365to0StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  
  ####13-24####
  #Without matching
  cmAnalysis913 <- CohortMethod::createCmAnalysis(analysisId = 913,
                                                  description = description13,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment1to365StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis914 <- CohortMethod::createCmAnalysis(analysisId = 914,
                                                  description = description14,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment30to365StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis915 <- CohortMethod::createCmAnalysis(analysisId = 915,
                                                  description = description15,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment365to365StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  ####25-36####
  #Without matching
  cmAnalysis925 <- CohortMethod::createCmAnalysis(analysisId = 925,
                                                  description = description25,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment1to1095StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis926 <- CohortMethod::createCmAnalysis(analysisId = 926,
                                                  description = description26,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment30to1095StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis927 <- CohortMethod::createCmAnalysis(analysisId = 927,
                                                  description = description27,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = OnTreatment365to1095StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  ####37-48####
  #Without matching
  cmAnalysis937 <- CohortMethod::createCmAnalysis(analysisId = 937,
                                                  description = description37,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = ITTBlankingOf1StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis938 <- CohortMethod::createCmAnalysis(analysisId = 938,
                                                  description = description38,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = ITTBlankingOf30StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  cmAnalysis939 <- CohortMethod::createCmAnalysis(analysisId = 939,
                                                  description = description39,
                                                  getDbCohortMethodDataArgs = minDbCmDataArgs,
                                                  createStudyPopArgs = ITTBlankingOf365StudyPopArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = unConditionedCox)
  
  cmAnalysisFeasibilityList <- list(cmAnalysis991, cmAnalysis992, cmAnalysis993, 
                                    cmAnalysis913, cmAnalysis914, cmAnalysis915, 
                                    cmAnalysis925, cmAnalysis926, cmAnalysis927, 
                                    cmAnalysis937, cmAnalysis938, cmAnalysis939
  )
  
  CohortMethod::saveCmAnalysisList(cmAnalysisFeasibilityList, file.path(workFolder, "cmAnalysisFeasibilityList.json"))
}