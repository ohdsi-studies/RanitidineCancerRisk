/************************************************************************
  Copyright 2020 Observational Health Data Sciences and Informatics

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/
  --subgroups of interest
--1.  femal
--2.  elderly (age >= 65)
--3.  Cumulative drug dose than 365 unit
--4.  Cumulative drug dose than 730 unit
--5.  Cumulative drug dose than 1095 unit

{DEFAULT @cdm_database_schema = 'cdm.dbo'}
{DEFAULT @window_end = 0}
{DEFAULT @window_start = -365}
{DEFAULT @short_term_window_start = -7}
{DEFAULT @maintenance_window_end = 365}
{DEFAULT @analysis_id = 998}
{DEFAULT @cohort_id = -1}
{DEFAULT @row_id_field = 'row_id'}
{DEFAULT @cohort_temp_table = '#cohort'}

IF OBJECT_ID('tempdb..#cov_1', 'U') IS NOT NULL
DROP TABLE #cov_1;

IF OBJECT_ID('tempdb..#cov_2', 'U') IS NOT NULL
DROP TABLE #cov_2;

IF OBJECT_ID('tempdb..#cov_3', 'U') IS NOT NULL
DROP TABLE #cov_3;

IF OBJECT_ID('tempdb..#cov_4', 'U') IS NOT NULL
DROP TABLE #cov_4;

IF OBJECT_ID('tempdb..#cov_5', 'U') IS NOT NULL
DROP TABLE #cov_5;


CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 999 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (43009003, 953076, 950696, 19011685, 997276, 961047)and invalid_reason is null
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (43009003, 953076, 950696, 19011685, 997276, 961047)
  and c.invalid_reason is null

) I
) C;


--1. female
SELECT DISTINCT row_id,
CAST(1000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_1	
FROM (
  SELECT @row_id_field AS row_id
  FROM @cohort_temp_table c
  INNER JOIN @cdm_database_schema.person p
  ON c.subject_id = p.person_id
  AND p.gender_concept_id = 8532
  {@cohort_id != -1} ? {		AND c.cohort_definition_id = @cohort_id}	
) tmp;

--2.  elderly (age >= 65)
SELECT DISTINCT row_id,
CAST(2000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_2
FROM (
  SELECT @row_id_field AS row_id
  FROM @cohort_temp_table c
  INNER JOIN @cdm_database_schema.person p
  ON c.subject_id = p.person_id
  WHERE YEAR(c.cohort_start_date) - p.year_of_birth >= 65
  {@cohort_id != -1} ? {		AND c.cohort_definition_id = @cohort_id}	
) tmp;

--3. Cumulative drug dose than 365 unit
SELECT DISTINCT row_id,
CAST(3000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_3
FROM
(
  SELECT DISTINCT @row_id_field AS row_id, SUM(ex.quantity) AS cumulative_amount, SUM(ex.days_supply) AS days_supply_sum
    FROM @cdm_database_schema.DRUG_EXPOSURE ex
    INNER JOIN @cohort_temp_table c
    ON C.subject_id = ex.person_id
    AND drug_exposure_start_date >= c.cohort_start_date
    --AND drug_exposure_end_date <= DATEADD(DAY, @maintenance_window_end, c.cohort_start_date)
    AND drug_exposure_start_date <= c.cohort_end_date
    AND ex.drug_concept_id IN (SELECT concept_id FROM #Codesets WHERE codeset_id = 999) 
    AND ex.quantity > 0
    {@cohort_id != -1} ? {AND c.cohort_definition_id = @cohort_id}
    GROUP BY @row_id_field
  
) AS Y
WHERE cumulative_amount > 365;

--4. Cumulative drug dose than 730 unit
SELECT DISTINCT row_id,
CAST(4000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_4
FROM
(
  SELECT DISTINCT @row_id_field AS row_id, SUM(ex.quantity) AS cumulative_amount, SUM(ex.days_supply) AS days_supply_sum
    FROM @cdm_database_schema.DRUG_EXPOSURE EX
    INNER JOIN @cohort_temp_table c
    ON C.subject_id = ex.person_id
    AND drug_exposure_start_date >= c.cohort_start_date
    --AND drug_exposure_end_date <= DATEADD(DAY, @maintenance_window_end, c.cohort_start_date)
    AND drug_exposure_start_date <= c.cohort_end_date
    AND ex.drug_concept_id IN (SELECT concept_id FROM #Codesets WHERE codeset_id = 999) 
    AND ex.quantity > 0
    {@cohort_id != -1} ? {AND c.cohort_definition_id = @cohort_id}
    GROUP BY @row_id_field
  
) AS Y
WHERE cumulative_amount > 730;

--5. Cumulative drug dose than 1095 unit
SELECT DISTINCT row_id,
CAST(5000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_5
FROM
(
  SELECT DISTINCT @row_id_field AS row_id, SUM(ex.quantity) AS cumulative_amount, SUM(ex.days_supply) AS days_supply_sum
    FROM @cdm_database_schema.DRUG_EXPOSURE EX
    INNER JOIN @cohort_temp_table c
    ON C.subject_id = ex.person_id
    AND drug_exposure_start_date >= c.cohort_start_date
    --AND drug_exposure_end_date <= DATEADD(DAY, @maintenance_window_end, c.cohort_start_date)
    AND drug_exposure_start_date <= c.cohort_end_date
    AND ex.drug_concept_id IN (SELECT concept_id FROM #Codesets WHERE codeset_id = 999) 
    AND ex.quantity > 0
    {@cohort_id != -1} ? {AND c.cohort_definition_id = @cohort_id}
    GROUP BY @row_id_field
  
) AS Y
WHERE cumulative_amount > 1095;

TRUNCATE TABLE #codesets;

DROP TABLE #codesets;

/*
--3.  black or african american
SELECT DISTINCT row_id,
CAST(3000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_3
FROM (
  SELECT @row_id_field AS row_id
  FROM @cohort_temp_table c
  INNER JOIN @cdm_database_schema.person p
  ON c.subject_id = p.person_id
  AND p.race_concept_id IN (8516, 38003598, 38003599, 38003600)
) tmp;


--4. Concomitant Myocardial infarction
SELECT DISTINCT row_id,
CAST(4000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_4
FROM (
  SELECT DISTINCT @row_id_field AS row_id
  FROM @cohort_temp_table c
  INNER JOIN @cdm_database_schema.condition_occurrence co
  ON c.subject_id = co.person_id
  AND co.condition_start_date <= DATEADD(DAY, @window_end, c.cohort_start_date)
  AND co.condition_start_date >= DATEADD(DAY, @short_term_window_start, c.cohort_start_date)
  {@cohort_id != -1} ? {			AND c.cohort_definition_id = @cohort_id}
  AND co.condition_concept_id IN (
    SELECT descendant_concept_id
    FROM @cdm_database_schema.concept_ancestor
    WHERE ancestor_concept_id IN (312327,434376,438170,444406) --concepts for myocardial infarction; 444406: Acute subendocardial infarction
  )
  
) tmp;

--5. Concomitant PPI use
SELECT DISTINCT row_id,
CAST(5000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_5
FROM (
  SELECT DISTINCT @row_id_field AS row_id
  FROM @cohort_temp_table c
  INNER JOIN @cdm_database_schema.drug_exposure co
  ON c.subject_id = co.person_id
  AND co.drug_exposure_start_date <= DATEADD(DAY, @window_end, c.cohort_start_date)
  AND co.drug_exposure_end_date >= DATEADD(DAY, @short_term_window_start, c.cohort_start_date)
  {@cohort_id != -1} ? {			AND c.cohort_definition_id = @cohort_id}
  AND co.drug_concept_id IN (
    SELECT descendant_concept_id
    FROM @cdm_database_schema.concept_ancestor
    WHERE ancestor_concept_id IN (21600095) --concepts for proton pump inhibitor (ATC 4th:A02BC) 
  )
) tmp;
*/

/*
--6. High aspirin maintenance dose(>=300mg)
SELECT DISTINCT row_id,
CAST(6000 + @analysis_id AS BIGINT) AS covariate_id,
1 AS covariate_value
INTO #cov_6
FROM
(
  SELECT row_id, daily_dose, days_supply_sum, ROW_NUMBER() OVER (PARTITION BY row_id ORDER BY days_supply_sum DESC )AS dose_order
  FROM
  (SELECT DISTINCT @row_id_field AS row_id, SUM(ex.days_supply) AS days_supply_sum, ex.quantity*st.amount_value/ex.days_supply AS daily_dose
    FROM @cdm_database_schema.DRUG_EXPOSURE EX
    INNER JOIN @cdm_database_schema.DRUG_STRENGTH ST
    ON EX.drug_concept_id = ST.drug_concept_id
    INNER JOIN @cohort_temp_table c
    ON C.subject_id = EX.person_id
    AND drug_exposure_start_date >= DATEADD(DAY, 2, c.cohort_start_date)
    AND drug_exposure_start_date <= DATEADD(DAY, @maintenance_window_end, c.cohort_start_date)
    AND drug_exposure_start_date <= c.cohort_end_date
    AND st.ingredient_concept_id = 1112807
    AND st.amount_value is not null 
    AND CAST(st. amount_value AS VARCHAR)  != ''
    AND ex.days_supply > 0
    {@cohort_id != -1} ? {                AND c.cohort_definition_id = @cohort_id}
    GROUP BY @row_id_field, ex.quantity*st.amount_value/ex.days_supply
  ) AS X
) AS Y
WHERE dose_order = 1 AND daily_dose >= 300;	
*/


--TRUNCATE TABLE #initial_cohort;

--DROP TABLE #initial_cohort;
