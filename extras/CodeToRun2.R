library(RanitidineCancerRisk)

# Optional: specify where the temporary files (used by the ff package) will be created:
options(fftempdir = "/home/chandryou/temp")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# The folder where the study intermediate and result files will be written:
outputFolder <- "/home/chandryou/output/RanitidineAjouV4T1"

# Details for connecting to the server:

# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",
#                                                                 server = "128.1.99.58",
#                                                                 user = "chandryou",
#                                                                 password = "dbtmdcks12#")

# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
#                                                                 server = "128.1.99.58/evidnet",
#                                                                 user = "postgres",
#                                                                 password = "ajoumed01!@",
#                                                                 port = "5433")

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                server = "128.1.99.58/evidnet",
                                                                user = "chandryou",
                                                                password = "dbtmdcks12#",
                                                                port = "5433")


# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",
#                                                                 server = "128.1.99.53",
#                                                                 user = "chandryou",
#                                                                 password = "dbtmdcks12#")

# The name of the database schema where the CDM data can be found:
#cdmDatabaseSchema <- "CDMPv1.dbo"
#cdmDatabaseSchema <- "NHIS_NSC.dbo"
cdmDatabaseSchema<- "cdmpv531"
# cdmDatabaseSchema<- "AUSOMv5_3.dbo" #AUSOMv5_3_1

# The name of the database schema and table where the study-specific cohorts will be instantiated:
#cohortDatabaseSchema <- "NHIS_NSC_Result.dbo"
#cohortDatabaseSchema <- "CDMPv1_Result.dbo"
cohortDatabaseSchema <- "cdmpv531_result"
cohortTable <- "ranitidine"

# Some meta-information that will be used by the export function:
databaseId <- "AUSOM"
databaseName <- "AUSOM"
databaseDescription <- "EHR data of Ajou university"

# For Oracle: define a schema that can be used to emulate temp tables:
oracleTempSchema <- NULL

runFeasibility(connectionDetails,
               cdmDatabaseSchema = cdmDatabaseSchema,
               cohortDatabaseSchema = cohortDatabaseSchema,
               cohortTable = cohortTable,
               oracleTempSchema = NULL,
               outputFolder,
               databaseId = databaseId,
               databaseName = databaseName,
               databaseDescription = databaseDescription,
               createCohorts = F,
               runFeasibility = T,
               runFeasibilityDiagnostics = F,
               feasibilityResults = F,
               maxCores = maxCores,
               minCellCount= 5)

runFeasibility(connectionDetails,
               cdmDatabaseSchema = cdmDatabaseSchema,
               cohortDatabaseSchema = cohortDatabaseSchema,
               cohortTable = cohortTable,
               oracleTempSchema = NULL,
               outputFolder,
               databaseId = databaseId,
               databaseName = databaseName,
               databaseDescription = databaseDescription,
               createCohorts = F,
               runFeasibility = F,
               runFeasibilityDiagnostics = T,
               feasibilityResults = TRUE,
               maxCores = 1,
               minCellCount= 5)



resultsZipFile <- file.path(outputFolder, 'feasibility',"export", paste0("FeasibilityResults", databaseId, ".zip"))
dataFolder <- file.path(outputFolder,'feasibility', "shinyData")

prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)
launchEvidenceExplorer(dataFolder = dataFolder, blind = FALSE, launch.browser = FALSE)

launchEvidenceExplorer(dataFolder = dataFolder, blind = F, launch.browser = FALSE)


execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        oracleTempSchema = oracleTempSchema,
        outputFolder = outputFolder,
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        createCohorts = F,
        synthesizePositiveControls = T,
        runAnalyses = T,
        runDiagnostics = T, 
        packageResults = TRUE,
        maxCores = maxCores)

resultsZipFile <- file.path(outputFolder, "export", paste0("Results", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")

prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)


#launchEvidenceExplorer(dataFolder = dataFolder, blind = TRUE, launch.browser = FALSE)

launchEvidenceExplorer(dataFolder = dataFolder, blind = F, launch.browser = FALSE)
