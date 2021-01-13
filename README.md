Cancer Risk between H2 blockers
==============================

<img src="https://img.shields.io/badge/Study%20Status-Design%20Finalized-brightgreen.svg" alt="Study Status: Design Finalized">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Clinical Application**
- Tags: **OHDSI-Korea, FEEDER-NET**
- Study lead: **Seng Chan You, Seung In Seo, Chan Hyuk Park**
- Study lead forums tag: **[SCYou](https://forums.ohdsi.org/u/SCYou)**
- Study start date: **January 31, 2020**
- Study end date:
- Protocol: [Protocol](https://github.com/ohdsi-studies/RanitidineCancerRisk/blob/master/documents/)
- Publications:
- Results explorer:

This study aims to compare the risk of cancer between H2 blockers

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, or Microsoft APS.
- R version 3.5.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

See [this video](https://youtu.be/K9_0s2Rchbo) for instructions on how to set up the R environment on Windows.

- Required fields of CDM include:
	- (Mandatory) CONCEPT, CONCEPT_RELATIONSHIP, CONCEPT_ANCESTOR (You can download these tables from the [ATHENA](http://athena.ohdsi.org/))
	- (Mandatory) PERSON, OBSERVATION_PERIOD, VISIT_OCCURRENCE, CONDITION_OCCURRENCE, DRUG_EXPOSURE, DRUG_ERA
	- (Optioinal) PROCEDURE_OCCURRENCE, OBSERVATION, DEATH

How to run
==========
1. We strongly recommend using `renv` to build project-specific R environment as guided [here](StudyPackageSetup.md)
 	 Otherwise, in `R`, use the following code to install the dependencies:

	```r
	install.packages("devtools")
	library(devtools)
	install_github("ohdsi/SqlRender")
	install_github("ohdsi/DatabaseConnector")
	install_github("ohdsi/OhdsiSharing")
	install_github("ohdsi/FeatureExtraction")
	install_github("ohdsi/CohortMethod")
	install_github("ohdsi/EmpiricalCalibration")
	install_github("ohdsi/MethodEvaluation")
	```

	If you experience problems on Windows where rJava can't find Java, one solution may be to add `args = "--no-multiarch"` to each `install_github` call, for example:

	```r
	install_github("ohdsi/SqlRender", args = "--no-multiarch")
	```

	Alternatively, ensure that you have installed both 32-bit and 64-bit JDK versions, as mentioned in the [video tutorial](https://youtu.be/K9_0s2Rchbo).

2. In 'R', use the following code to install the RanitidineCancerRisk package:

  ```r
	install_github("ohdsi-studies/RanitidineCancerRisk", args = "--no-multiarch")
  ```

3. Once installed, please execute the following code. Then Send the file ```./export/Results_<DatabaseId>.zip``` in the output folder to the study coordinator (SCYou, applegna@gmail.com)

	```r
	library(RanitidineCancerRisk)

	# Optional: specify where the temporary files (used by the ff package) will be created:
	options(fftempdir = "c:/FFtemp")

	# Maximum number of cores to be used:
	maxCores <- parallel::detectCores()

	# Minimum cell count when exporting data:
	minCellCount <- 5

	# The folder where the study intermediate and result files will be written:
	outputFolder <- "c:/RanitidineCancerRisk"

	# Details for connecting to the server:
	# See ?DatabaseConnector::createConnectionDetails for help
	connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
									server = "some.server.com/ohdsi",
									user = "joe",
									password = "secret")

	# The name of the database schema where the CDM data can be found:
	cdmDatabaseSchema <- "cdm_synpuf"

	# The name of the database schema and table where the study-specific cohorts will be instantiated:
	cohortDatabaseSchema <- "scratch.dbo"
	cohortTable <- "my_study_cohorts"

	# Some meta-information that will be used by the export function:
	databaseId <- "Synpuf"
	databaseName <- "Medicare Claims Synthetic Public Use Files (SynPUFs)"
	databaseDescription <- "Medicare Claims Synthetic Public Use Files (SynPUFs) were created to allow interested parties to gain familiarity using Medicare claims data while protecting beneficiary privacy. These files are intended to promote development of software and applications that utilize files in this format, train researchers on the use and complexities of Centers for Medicare and Medicaid Services (CMS) claims, and support safe data mining innovations. The SynPUFs were created by combining randomized information from multiple unique beneficiaries and changing variable values. This randomization and combining of beneficiary information ensures privacy of health information."

	# For Oracle: define a schema that can be used to emulate temp tables:
	oracleTempSchema <- NULL

	execute(connectionDetails = connectionDetails,
	        cdmDatabaseSchema = cdmDatabaseSchema,
	        cohortDatabaseSchema = cohortDatabaseSchema,
	        cohortTable = cohortTable,
	        oracleTempSchema = oracleTempSchema,
	        outputFolder = outputFolder,
	        databaseId = databaseId,
	        databaseName = databaseName,
	        databaseDescription = databaseDescription,
	        createCohorts = TRUE,
	        synthesizePositiveControls = TRUE,
	        runAnalyses = TRUE,
	        packageResults = TRUE,
	        maxCores = maxCores)
	```

4. Please send the file ```export/Results<DatabaseId>.zip``` in the output folder to the study coordinator (SCYou, seng.chan.you@ohdsi.org or applegna@gmail.com):

5. To view the results, use the Shiny app:

	```r
	prepareForEvidenceExplorer("Result<databaseId>.zip", "/shinyData")
	launchEvidenceExplorer("/shinyData", blind = TRUE)
	```

  Note that you can save plots from within the Shiny app. It is possible to view results from more than one database by applying `prepareForEvidenceExplorer` to the Results file from each database, and using the same data folder. Set `blind = FALSE` if you wish to be unblinded to the final results.


License
=======
The RanitidineCancerRisk package is licensed under Apache License 2.0


Development
===========
RanitidineCancerRisk was developed in ATLAS and R Studio.

### Development status

Under development
