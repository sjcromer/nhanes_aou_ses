library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "person" and was generated for All of Us Registered Tier Dataset v7
dataset_55630848_person_sql <- paste("
    SELECT
        person.person_id,
        person.birth_datetime as date_of_birth,
        p_race_concept.concept_name as race,
        p_ethnicity_concept.concept_name as ethnicity,
        p_sex_at_birth_concept.concept_name as sex_at_birth 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_race_concept 
            ON person.race_concept_id = p_race_concept.concept_id 
    LEFT JOIN
        `concept` p_ethnicity_concept 
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id  
    WHERE
        person.PERSON_ID IN (
            SELECT
                distinct person_id  
            FROM
                `cb_search_person` cb_search_person  
            WHERE
                cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `cb_search_person` p 
                    WHERE
                        age_at_consent BETWEEN 18 AND 120 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        gender_concept_id IN (45878463, 45880669, 2000000002) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                    UNION
                    ALL SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585945) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585946) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000006) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000007)
                            )) criteria 
                    UNION
                    ALL SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585376) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585377) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585378) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585379) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585380) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585381) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585382) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585383) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585384)
                            )) criteria ) 
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_55630848_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_55630848",
  "person_55630848_*.csv")
message(str_glue('The data will be written to {person_55630848_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
#bq_table_save(
# bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55630848_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
#person_55630848_path,
#destination_format = "CSV")

person_55630848_path <- 'gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240220/person_55630848/person_55630848_*.csv'

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_55630848_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character(), sex_at_birth = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_55630848_person_df <- read_bq_export_from_workspace_bucket(person_55630848_path)

dim(dataset_55630848_person_df)

head(dataset_55630848_person_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "survey" and was generated for All of Us Registered Tier Dataset v7
dataset_55630848_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.question,
        answer.answer  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (
                1585375, 1585386, 1585389, 1585838, 1585845, 1585857, 1585860, 1585873, 1585940, 1585952, 1586140, 1586159, 1586162, 1586166, 43528428, 43530562, 43530593, 43530595
            )
        )  
        AND (
            answer.PERSON_ID IN (
                SELECT
                    distinct person_id  
                FROM
                    `cb_search_person` cb_search_person  
                WHERE
                    cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `cb_search_person` p 
                        WHERE
                            age_at_consent BETWEEN 18 AND 120 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            gender_concept_id IN (45878463, 45880669, 2000000002) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                        UNION
                        ALL SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585945) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585946) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000006) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000007)
                                )) criteria 
                        UNION
                        ALL SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585376) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585377) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585378) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585379) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585380) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585381) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585382) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585383) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585384)
                                )) criteria ) 
                    )
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_55630848_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_55630848",
  "survey_55630848_*.csv")
message(str_glue('The data will be written to {survey_55630848_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55630848_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_55630848_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_55630848_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(question = col_character(), answer = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_55630848_survey_df <- read_bq_export_from_workspace_bucket(survey_55630848_path)

dim(dataset_55630848_survey_df)

head(dataset_55630848_survey_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "condition" and was generated for All of Us Registered Tier Dataset v7
dataset_55630848_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    201254, 201826, 37018196, 433736
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    c_occurrence.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                ALL SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                ALL SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) c_occurrence 
            LEFT JOIN
                `concept` c_standard_concept 
                    ON c_occurrence.condition_concept_id = c_standard_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_55630848_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_55630848",
  "condition_55630848_*.csv")
message(str_glue('The data will be written to {condition_55630848_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55630848_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_55630848_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_55630848_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_55630848_condition_df <- read_bq_export_from_workspace_bucket(condition_55630848_path)

dim(dataset_55630848_condition_df)

head(dataset_55630848_condition_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "measurement" and was generated for All of Us Registered Tier Dataset v7
dataset_55630848_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        m_standard_concept.concept_name as standard_concept_name,
        measurement.measurement_datetime,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high 
    FROM
        ( SELECT
            * 
        FROM
            `measurement` measurement 
        WHERE
            (
                measurement_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    1002597, 3004410, 3005673, 3025315, 3034639, 3036277, 3038553, 37065054, 40759207, 40762636, 40762637, 40765148, 40795740, 4184637, 4197971, 4245997
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    measurement.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                ALL SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                ALL SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) measurement 
            LEFT JOIN
                `concept` m_standard_concept 
                    ON measurement.measurement_concept_id = m_standard_concept.concept_id 
            LEFT JOIN
                `concept` m_value 
                    ON measurement.value_as_concept_id = m_value.concept_id 
            LEFT JOIN
                `concept` m_unit 
                    ON measurement.unit_concept_id = m_unit.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_55630848_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "measurement_55630848",
  "measurement_55630848_*.csv")
message(str_glue('The data will be written to {measurement_55630848_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55630848_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_55630848_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {measurement_55630848_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), value_as_concept_name = col_character(), unit_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_55630848_measurement_df <- read_bq_export_from_workspace_bucket(measurement_55630848_path)

dim(dataset_55630848_measurement_df)

head(dataset_55630848_measurement_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "person" and was generated for All of Us Registered Tier Dataset v7
dataset_71898682_person_sql <- paste("
    SELECT
        person.person_id,
        person.birth_datetime as date_of_birth,
        p_race_concept.concept_name as race,
        p_ethnicity_concept.concept_name as ethnicity,
        p_sex_at_birth_concept.concept_name as sex_at_birth 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_race_concept 
            ON person.race_concept_id = p_race_concept.concept_id 
    LEFT JOIN
        `concept` p_ethnicity_concept 
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id  
    WHERE
        person.PERSON_ID IN (
            SELECT
                distinct person_id  
            FROM
                `cb_search_person` cb_search_person  
            WHERE
                cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `cb_search_person` p 
                    WHERE
                        age_at_consent BETWEEN 18 AND 120 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        gender_concept_id IN (45878463, 45880669, 2000000002) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                    UNION
                    ALL SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585945) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585946) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000006) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000007)
                            )) criteria 
                    UNION
                    ALL SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585376) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585377) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585378) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585379) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585380) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585381) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585382) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585383) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585384)
                            )) criteria ) 
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_71898682_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_71898682",
  "person_71898682_*.csv")
message(str_glue('The data will be written to {person_71898682_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_71898682_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_71898682_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_71898682_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character(), sex_at_birth = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_71898682_person_df <- read_bq_export_from_workspace_bucket(person_71898682_path)

dim(dataset_71898682_person_df)

head(dataset_71898682_person_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "survey" and was generated for All of Us Registered Tier Dataset v7
dataset_71898682_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.question,
        answer.answer  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (
                1585375, 1585386, 1585389, 1585838, 1585845, 1585857, 1585860, 1585873, 1585940, 1585952, 1586140, 1586159, 1586162, 1586166, 43528428, 43530562, 43530593, 43530595
            )
        )  
        AND (
            answer.PERSON_ID IN (
                SELECT
                    distinct person_id  
                FROM
                    `cb_search_person` cb_search_person  
                WHERE
                    cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `cb_search_person` p 
                        WHERE
                            age_at_consent BETWEEN 18 AND 120 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            gender_concept_id IN (45878463, 45880669, 2000000002) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                        UNION
                        ALL SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585945) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585946) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000006) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000007)
                                )) criteria 
                        UNION
                        ALL SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585376) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585377) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585378) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585379) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585380) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585381) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585382) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585383) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585384)
                                )) criteria ) 
                    )
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_71898682_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_71898682",
  "survey_71898682_*.csv")
message(str_glue('The data will be written to {survey_71898682_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_71898682_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_71898682_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_71898682_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_71898682_survey_df <- read_bq_export_from_workspace_bucket(survey_71898682_path)

dim(dataset_71898682_survey_df)

head(dataset_71898682_survey_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "condition" and was generated for All of Us Registered Tier Dataset v7
dataset_71898682_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    201254, 201826, 37018196, 433736
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    c_occurrence.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                ALL SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                ALL SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) c_occurrence 
            LEFT JOIN
                `concept` c_standard_concept 
                    ON c_occurrence.condition_concept_id = c_standard_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_71898682_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_71898682",
  "condition_71898682_*.csv")
message(str_glue('The data will be written to {condition_71898682_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_71898682_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_71898682_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_71898682_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_71898682_condition_df <- read_bq_export_from_workspace_bucket(condition_71898682_path)

dim(dataset_71898682_condition_df)

head(dataset_71898682_condition_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "measurement" and was generated for All of Us Registered Tier Dataset v7
dataset_71898682_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        m_standard_concept.concept_name as standard_concept_name,
        measurement.measurement_datetime,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high 
    FROM
        ( SELECT
            * 
        FROM
            `measurement` measurement 
        WHERE
            (
                measurement_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    3025315, 3036277, 3038553, 40759207, 40762636, 40762637, 40765148, 4245997
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    measurement.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                ALL SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                ALL SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) measurement 
            LEFT JOIN
                `concept` m_standard_concept 
                    ON measurement.measurement_concept_id = m_standard_concept.concept_id 
            LEFT JOIN
                `concept` m_value 
                    ON measurement.value_as_concept_id = m_value.concept_id 
            LEFT JOIN
                `concept` m_unit 
                    ON measurement.unit_concept_id = m_unit.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_71898682_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "measurement_71898682",
  "measurement_71898682_*.csv")
message(str_glue('The data will be written to {measurement_71898682_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_71898682_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_71898682_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {measurement_71898682_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), value_as_concept_name = col_character(), unit_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_71898682_measurement_df <- read_bq_export_from_workspace_bucket(measurement_71898682_path)

dim(dataset_71898682_measurement_df)

head(dataset_71898682_measurement_df, 5)



#import and name data sets
#use me to access earlier generated files/without having to regenerate using sql code above
aou_person <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/person_55630848/person_55630848_000000000000.csv')
aou_survey <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/survey_55630848/survey_55630848_000000000000.csv')
aou_condition <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/condition_55630848/condition_55630848_000000000000.csv')
aou_measurement <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/measurement_71898682/measurement_71898682_000000000000.csv') #_000000000000 through _000000000009

#import and name data sets
aou_person <- dataset_55630848_person_df #read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/person_55630848/person_55630848_000000000000.csv')
aou_survey <- dataset_55630848_survey_df #read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/survey_55630848/survey_55630848_000000000000.csv')
aou_condition <- dataset_55630848_condition_df #read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/condition_55630848/condition_55630848_000000000000.csv')
aou_measurement <- dataset_71898682_measurement_df #read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/measurement_71898682/measurement_71898682_000000000000.csv') #_000000000000 through _000000000009

#df basics
dim(aou_person)
length(unique(aou_person$person_id))
colnames(aou_person)

dim(aou_survey)
length(unique(aou_condition$person_id))
colnames (aou_survey)

dim(aou_condition)
length(unique(aou_condition$person_id))
colnames(aou_condition)

dim(aou_measurement)
length(unique(aou_measurement$person_id))
colnames(aou_measurement)

#####clean person file: #####
#clean person file: 
aou_person <- dataset_55630848_person_df
#aou_person <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240220/person_55630848/person_55630848_000000000000.csv')

dim(aou_person)
length(unique(aou_person$person_id))
colnames(aou_person)

aou_person$date_of_birth[1:10]
#aou_person$race2 <- aou_person$race
table(aou_person$race2, aou_person$ethnicity, exclude=NULL)

#aou_person$age <- as.date(visit_start_date, %YYYY-%mm-%dd) - as.date(date_of_birth, %YYYY-%mm-%dd) #visit_start_date only available in visit file - need to request
aou_person$sex <- aou_person$sex_at_birth
aou_person$race <- ifelse(aou_person$ethnicity=='Hispanic or Latino','Hispanic',
                          ifelse(aou_person$race2=='White', 'NHW',
                                 ifelse(aou_person$race2=='Black or African American', 'NHB',
                                        ifelse(aou_person$race2=='Asian', 'NHAsian',
                                               ifelse(aou_person$race2=='Another single population', 'Other',
                                                      ifelse(aou_person$race2=='More than one population', 'Multiracial',
                                                             ifelse(aou_person$race2=='None of these', 'None of these',
                                                                    ifelse(aou_person$race2=='None Indicated', 'Missing','Missing'))))))))
table(aou_person$race2, aou_person$race, exclude=NULL)

aou_person_final <- aou_person[,c('person_id','date_of_birth','sex','race')]

#write.csv(aou_person_final, file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/AoU_RExSES_person_final.csv', row.names=F)

#name_to_save <- "AoU_RExSES_person"
#dataset_to_csv_to_bucket(name_to_save = name_to_save, df=aou_person_final)

#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
write.csv(aou_person_final, file='AoU_RExSES_person_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_person_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_person_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_person_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)


#####clean survey#####
#clean aou_survey
aou_survey <- dataset_55630848_survey_df
#table(aou_survey$question)
#aou_survey$answer[1:40]
#aou_survey <- aou_survey[,c('person_id','question','answer')]
aou_survey_concat <- aou_survey %>% group_by(person_id, survey_datetime, question) %>% dplyr::mutate(answers_all=paste0(answer, collapse=" & "))
#mutate(answers_all = paste0(answer, collapse = ""))
table(aou_survey_concat$answers_all[aou_survey_concat$question=='Health Insurance: Health Insurance Type'])
dim(aou_survey_concat)

aou_survey_concat$survey_date <- aou_survey_concat %>% group_by(person_id) %>% arrange(survey_datetime) %>% slice(1L)
length(unique(aou_survey_concat$survey_datetime))
length(unique(aou_survey_concat$survey_date))
aou_survey_concat$survey_date <- format(as.POSIXct(aou_survey_concat$survey_date,format='%Y-%m%d %H:%M:%S'),format='%m/%d/%Y')
aou_survey_concat$survey_date[1:20]
#mutate(survey_date=paste0(answer, collapse=" & "))

aou_survey_concat$survey_date <- format(as.POSIXct(aou_survey_concat$survey_date,format='%Y-%m%d %H:%M:%S'),format='%m/%d/%Y')
aou_survey_concat$survey_date[1:20]

aou_survey_short <- aou_survey_concat %>% filter(str_detect(question, 'Education Level: Highest Grade|Employment: Employment Status|Health Advice: Place for Health Advice|Health Advice: What Kind Of Place|Health Insurance: Health Insurance Type|Income: Annual Income|Insurance: Health Insurance|Smoking: 100 Cigs Lifetime|Smoking: Current Daily Cigarette Number'))
aou_survey_short <- aou_survey_short[,c('person_id','survey_datetime','question','answers_all')]
#aou_survey_short <- aou_survey[aou_survey$question %in% c('Education Level: Highest Grade','Employment: Employment Status',
#'Health Advice: Place for Health Advice','Health Advice: What Kind Of Place',
#'Health Insurance: Health Insurance Type','Income: Annual Income','Insurance: Health Insurance',
#'Smoking: 100 Cigs Lifetime','Smoking: Current Daily Cigarette Number')
aou_survey_wide <- aou_survey_short %>% pivot_wider(id_cols=c(person_id,survey_datetime), names_from = question, values_from = answers_all, values_fn=max)
dim(aou_survey_wide)
colnames(aou_survey_wide)
length(unique(aou_survey_wide$person_id))

table(aou_survey_wide$'Education Level: Highest Grade')
table(aou_survey_wide$'Income: Annual Income')
table(aou_survey_wide$'Employment: Employment Status')

table(aou_survey_wide$'Smoking: 100 Cigs Lifetime')
table(aou_survey_wide$'Smoking: Current Daily Cigarette Number')

table(aou_survey_wide$'Insurance: Health Insurance')
table(aou_survey_wide$'Health Insurance: Health Insurance Type')
table(aou_survey_wide$'Health Advice: Place for Health Advice')
table(aou_survey_wide$'Health Advice: What Kind Of Place')

#clean SES data

aou_survey_wide$hreduc <- aou_survey_wide$'Education Level: Highest Grade'
aou_survey_wide$educ_simple <- ifelse(aou_survey_wide$hreduc=='College graduate or advanced degree','College Degree', 
                                      ifelse(aou_survey_wide$hreduc=='Highest Grade: College One to Three', 'Some College',
                                             ifelse(aou_survey_wide$hreduc=='Highest Grade: Twelve Or GED', 'HS Degree',
                                                    ifelse(aou_survey_wide$hreduc=='Less than a high school degree or equivalent','Less than HS', NA))))
table(aou_survey_wide$hreduc, aou_survey_wide$educ_simple, exclude=NULL)
aou_survey_wide$educ_num <- ifelse(aou_survey_wide$hreduc=='College graduate or advanced degree',7, 
                                   ifelse(aou_survey_wide$hreduc=='Highest Grade: College One to Three', 6,
                                          ifelse(aou_survey_wide$hreduc=='Highest Grade: Twelve Or GED', 4,
                                                 ifelse(aou_survey_wide$hreduc=='Less than a high school degree or equivalent',2, NA))))
boxplot_indiveduc <- ggplot(aou_survey_wide, aes(factor(x=racecat), y=educ_num, fill=factor(diab)), exclude=TRUE) + geom_boxplot(notch=T)

aou_survey_wide$hhinc <- aou_survey_wide$'Income: Annual Income'
aou_survey_wide$inc_simple <- ifelse(aou_survey_wide$hhinc=="Annual Income: less 10k" | aou_survey_wide$hhinc=="Annual Income: 10k 25k", 'Under $25,000',
                                     ifelse(aou_survey_wide$hhinc=="Annual Income: 25k 35k" | aou_survey_wide$hhinc=="Annual Income: 35k 50k", '$25,000-49,999', 
                                            ifelse(aou_survey_wide$hhinc=="Annual Income: 50k 75k", '$50,000-74,999',
                                                   ifelse(aou_survey_wide$hhinc=="Annual Income: 75k 100k" | aou_survey_wide$hhinc=="Annual Income: 100k 150k"| 
                                                            aou_survey_wide$hhinc=='Annual Income: 150k 200k'| aou_survey_wide$hhinc=='Annual Income: more 200k', "More than $75,000", NA))))

aou_survey_wide$inc_simple2 <- ifelse(aou_survey_wide$hhinc=="Annual Income: less 10k" | aou_survey_wide$hhinc=="Annual Income: 10k 25k", 'Under $25,000',
                                      ifelse(aou_survey_wide$hhinc=="Annual Income: 25k 35k" | aou_survey_wide$hhinc=="Annual Income: 35k 50k", '$25,000-49,999', 
                                             ifelse(aou_survey_wide$hhinc=="Annual Income: 50k 75k", '$50,000-74,999',
                                                    ifelse(aou_survey_wide$hhinc=="Annual Income: 75k 100k" | aou_survey_wide$hhinc=="Annual Income: 100k 150k",'$75,000-149,999',
                                                           ifelse(aou_survey_wide$hhinc=='Annual Income: 150k 200k'| aou_survey_wide$hhinc=='Annual Income: more 200k', "More than $150,000", NA)))))
table(aou_survey_wide$hhinc, aou_survey_wide$inc_simple, exclude=NULL)
table(aou_survey_wide$inc_simple, exclude=NULL)

aou_survey_wide$educ_simple <- factor(aou_survey_wide$educ_simple, levels=c("College Degree","Some College","HS Degree","Less than HS"))
aou_survey_wide$hreduc <- factor(aou_survey_wide$hreduc, levels=c('Less than a high school degree or equivalent', 'Highest Grade: Twelve Or GED',  'Highest Grade: College One to Three', 'College graduate or advanced degree', 'PMI: Prefer Not To Answer','PMI: Skip'))
aou_survey_wide$hhinc <- factor(aou_survey_wide$hhinc, levels=c("Annual Income: more 200k","Annual Income: 150k 200k","Annual Income: 100k 150k","Annual Income: 75k 100k","Annual Income: 50k 75k","Annual Income: 35k 50k","Annual Income: 25k 35k","Annual Income: 10k 25k","Annual Income: less 10k","PMI: Prefer Not To Answer","PMI: Skip"))
aou_survey_wide$inc_simple <- factor(aou_survey_wide$inc_simple, levels=c("More than $75,000","$45,000-74,999","$20,000-44,999","Under $20,000"))
aou_survey_wide$inc_simple2 <- factor(aou_survey_wide$inc_simple2, levels=c("More than $150,000","$75,000-149,999","$45,000-74,999","$20,000-44,999","Under $20,000"))
table(aou_survey_wide$hhinc)

aou_survey_wide$all_ses_na <- ifelse(is.na(aou_survey_wide$educ_num) & is.na(aou_survey_wide$hhinc),1,0)
addmargins(table(aou_survey_wide$all_ses_na, exclude=NULL))


aou_survey_wide$work_yn <- ifelse(aou_survey_wide$'Employment: Employment Status'=='Employed for wages or self-employed' | 
                                    aou_survey_wide$'Employment: Employment Status'=='Employed for wages or self-employed & Not currently employed for wages' | 
                                    aou_survey_wide$'Employment: Employment Status'=='Not currently employed for wages & Employed for wages or self-employed', 'Working', 
                                  ifelse(aou_survey_wide$'Employment: Employment Status'=='Not currently employed for wages', 'Not Working', NA))
table(aou_survey_wide$'Employment: Employment Status', aou_survey_wide$work_yn, exclude=NULL)

#clean smoking
aou_survey_wide$smoke_ever <- ifelse(is.na(aou_survey_wide$'Smoking: 100 Cigs Lifetime'),'Missing',
                                     ifelse(aou_survey_wide$'Smoking: 100 Cigs Lifetime'=='100 Cigs Lifetime: Yes', 1,
                                            ifelse(aou_survey_wide$'Smoking: 100 Cigs Lifetime'=='100 Cigs Lifetime: No',0,'Missing')))
aou_survey_wide$smoke_current<- ifelse(aou_survey_wide$'Smoking: Current Daily Cigarette Number'=='PMI: Skip' |
                                         aou_survey_wide$'Smoking: Current Daily Cigarette Number'=='PMI: Prefer Not To Answer' |
                                         aou_survey_wide$'Smoking: Current Daily Cigarette Number'=='PMI: Dont Know'|
                                         is.na(aou_survey_wide$'Smoking: Current Daily Cigarette Number'), "Missing",
                                       ifelse(aou_survey_wide$'Smoking: Current Daily Cigarette Number'==0, 0,1))
table(aou_survey_wide$smoke_ever, aou_survey_wide$smoke_current, exclude=NULL)
aou_survey_wide$smoke_status <- ifelse(aou_survey_wide$smoke_ever==0, 'Never Smoker', 
                                       ifelse(aou_survey_wide$smoke_current==1, 'Current Smoker',
                                              ifelse(aou_survey_wide$smoke_ever==1 & aou_survey_wide$smoke_current==0, 'Former Smoker',
                                                     ifelse(aou_survey_wide$smoke_ever==1,'Current or Former Smoker NOS','Missing'))))
table(aou_survey_wide$smoke_ever, aou_survey_wide$smoke_status, exclude=NULL)
table(aou_survey_wide$smoke_current, aou_survey_wide$smoke_status, exclude=NULL)
aou_survey_wide$smoke_status <- factor(aou_survey_wide$smoke_status, levels=c("Never Smoker","Former Smoker","Current or Former Smoker NOS","Current Smoker","Missing"))
table(aou_survey_wide$smoke_status, exclude=NULL)

#clean insurance
table(aou_survey_wide$'Insurance: Health Insurance',exclude=NULL)#aou_survey_wide$'Health Insurance: Health Insurance Type', exclude=NULL)
aou_survey_wide$insur_curr_yn <- ifelse(is.na(aou_survey_wide$'Insurance: Health Insurance'),'Missing',
                                        ifelse(aou_survey_wide$'Insurance: Health Insurance'=='Health Insurance: Yes',1,
                                               ifelse(aou_survey_wide$'Insurance: Health Insurance'=='Health Insurance: No'| 
                                                        aou_survey_wide$'Health Insurance: Health Insurance Type'=='Health Insurance Type: No Coverage',0,'Missing')))
table(aou_survey_wide$insur_curr_yn, exclude=NULL)
aou_survey_wide$insur_private <- ifelse(grepl('Private',aou_survey_wide$'Health Insurance: Health Insurance Type'),1,0)
aou_survey_wide$insur_mcare <- ifelse(grepl('Medicare|Medi GAP',aou_survey_wide$'Health Insurance: Health Insurance Type'),1,0)
aou_survey_wide$insur_lowincgov <- ifelse(grepl('Medicaid|Schip|State',aou_survey_wide$'Health Insurance: Health Insurance Type'),1,0)
aou_survey_wide$insur_othgovmil <- ifelse(grepl('Other Gov|Military',aou_survey_wide$'Health Insurance: Health Insurance Type'),1,0)
aou_survey_wide$insur_ihs <- ifelse(grepl('Indian',aou_survey_wide$'Health Insurance: Health Insurance Type'),1,0)
aou_survey_wide$insur_singserv <- ifelse(grepl('Single Service',aou_survey_wide$'Health Insurance: Health Insurance Type'),1,0)
aou_survey_wide$insur_curr <- ifelse(aou_survey_wide$insur_curr_yn==1|aou_survey_wide$insur_private==1|aou_survey_wide$insur_mcare==1|
                                       aou_survey_wide$insur_lowincgov==1|aou_survey_wide$insur_othgovmil==1|aou_survey_wide$insur_ihs==1|
                                       aou_survey_wide$insur_singserv==1,1,0)
#table(aou_survey_wide$insur_curr, aou_survey_wide$insur_private, exclude=NULL)
#insur_stability variable will be different than NHANES as no uninsur_pastyr data available; insur_complex rounds up (if you have both private and ihs, you are listed as private)
aou_survey_wide$insur_stability <- ifelse(aou_survey_wide$insur_curr==1 & aou_survey_wide$insur_private==1, 'Insured-Private',
                                          ifelse(aou_survey_wide$insur_curr==1 & !aou_survey_wide$insur_private==1, 'Insured-Other', 
                                                 ifelse(aou_survey_wide$insur_curr==0, 'Uninsured', 'Missing')))
aou_survey_wide$insur_complex <- ifelse(aou_survey_wide$insur_curr==0, 'Uninsured',
                                        ifelse(aou_survey_wide$insur_private==1, 'Private',
                                               ifelse(aou_survey_wide$insur_mcare==1, 'Medicare', 
                                                      ifelse(aou_survey_wide$insur_othgovmil==1, 'Other Gov/Military', 
                                                             ifelse(aou_survey_wide$insur_ihs==1, 'IHS', 
                                                                    ifelse(aou_survey_wide$insur_lowincgov==1, 'Medicaid/Low Inc Insurance', 
                                                                           ifelse(aou_survey_wide$insur_singserv==1, 'Single Service', 'Missing')))))))
table(aou_survey_wide$insur_stability, aou_survey_wide$insur_complex, exclude=NULL)
addmargins(table(aou_survey_wide$insur_curr, aou_survey_wide$insur_stability, exclude=NULL))
addmargins(table(aou_survey_wide$insur_curr, aou_survey_wide$insur_private))
aou_survey_wide$insur_stability <- factor(aou_survey_wide$insur_stability, levels=c("Insured-Private","Insured-Other","Uninsured","Missing"))


#clean routine place of healthcare - only counting longitudinal outpatient (not ER, UC, etc.)
aou_survey_wide$hc_routine_place <- ifelse(is.na(aou_survey_wide$'Health Advice: Place for Health Advice'),'Missing',
                                           ifelse(aou_survey_wide$'Health Advice: Place for Health Advice'=='Place for Health Advice: Yes'|
                                                    aou_survey_wide$'Health Advice: Place for Health Advice'=='Place for Health Advice: More Than One',1,
                                                  ifelse(aou_survey_wide$'Health Advice: Place for Health Advice'=='Place for Health Advice: No',0,'Missing')))
table(aou_survey_wide$hc_routine_place, exclude=NULL)
aou_survey_wide$hc_routine_place_typenonemerg <- ifelse(is.na(aou_survey_wide$'Health Advice: What Kind Of Place'),'Missing',
                                                        ifelse(aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: Doctors Office'|
                                                                 aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: Some Other Place'|
                                                                 aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: No One Place Most Often',1,
                                                               ifelse(aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: Urgent Care'|
                                                                        aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: Emergency Room',0,'Missing')))
addmargins(table(aou_survey_wide$hc_routine_place, aou_survey_wide$hc_routine_place_typenonemerg, exclude=NULL))
aou_survey_wide$hc_routine_place_yn <- ifelse(aou_survey_wide$hc_routine_place==1 | aou_survey_wide$hc_routine_place_typenonemerg==1,1,0)
table(aou_survey_wide$hc_routine_place_yn, exclude=NULL)

dim(aou_survey_wide)
colnames(aou_survey_wide)
aou_survey_final <- aou_survey_wide[,c(1:2,11:18,21,29:30,33)]
colnames(aou_survey_final)

#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
write.csv(aou_survey_final, file='AoU_RExSES_survey_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_survey_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_survey_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_survey_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)

aou_surv <- read.csv(file='***gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/AoU_RExSES/AoU_RExSES_aou_survey_final.csv')
#aou_surv <- read_bq_export_from_workspace_bucket(AoU_RExSES_filepath)

dim(aou_surv)

head(aou_surv, 5)































library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "person" and was generated for All of Us Registered Tier Dataset v7
dataset_55630848_person_sql <- paste("
    SELECT
        person.person_id,
        person.birth_datetime as date_of_birth,
        p_race_concept.concept_name as race,
        p_ethnicity_concept.concept_name as ethnicity,
        p_sex_at_birth_concept.concept_name as sex_at_birth 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_race_concept 
            ON person.race_concept_id = p_race_concept.concept_id 
    LEFT JOIN
        `concept` p_ethnicity_concept 
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id  
    WHERE
        person.PERSON_ID IN (
            SELECT
                distinct person_id  
            FROM
                `cb_search_person` cb_search_person  
            WHERE
                cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `cb_search_person` p 
                    WHERE
                        age_at_consent BETWEEN 18 AND 120 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        gender_concept_id IN (45878463, 45880669, 2000000002) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                    UNION
                    ALL SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585945) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585946) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000006) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000007)
                            )) criteria 
                    UNION
                    ALL SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585376) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585377) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585378) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585379) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585380) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585381) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585382) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585383) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585384)
                            )) criteria ) 
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_55630848_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_55630848",
  "person_55630848_*.csv")
message(str_glue('The data will be written to {person_55630848_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55630848_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_55630848_path,
  destination_format = "CSV")



# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_55630848_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character(), sex_at_birth = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_55630848_person_df <- read_bq_export_from_workspace_bucket(person_55630848_path)

dim(dataset_55630848_person_df)

head(dataset_55630848_person_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "survey" and was generated for All of Us Registered Tier Dataset v7
dataset_55630848_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.question,
        answer.answer  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (
                1585375, 1585386, 1585389, 1585838, 1585845, 1585857, 1585860, 1585873, 1585940, 1585952, 1586140, 1586159, 1586162, 1586166, 43528428, 43530562, 43530593, 43530595
            )
        )  
        AND (
            answer.PERSON_ID IN (
                SELECT
                    distinct person_id  
                FROM
                    `cb_search_person` cb_search_person  
                WHERE
                    cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `cb_search_person` p 
                        WHERE
                            age_at_consent BETWEEN 18 AND 120 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            gender_concept_id IN (45878463, 45880669, 2000000002) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                        UNION
                        ALL SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585945) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585946) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000006) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000007)
                                )) criteria 
                        UNION
                        ALL SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585376) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585377) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585378) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585379) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585380) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585381) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585382) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585383) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585384)
                                )) criteria ) 
                    )
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_55630848_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_55630848",
  "survey_55630848_*.csv")
message(str_glue('The data will be written to {survey_55630848_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55630848_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_55630848_path,
  destination_format = "CSV")



# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_55630848_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_55630848_survey_df <- read_bq_export_from_workspace_bucket(survey_55630848_path)

dim(dataset_55630848_survey_df)

head(dataset_55630848_survey_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "condition" and was generated for All of Us Registered Tier Dataset v7
dataset_55630848_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    201254, 201826, 37018196, 433736
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    c_occurrence.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                ALL SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                ALL SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) c_occurrence 
            LEFT JOIN
                `concept` c_standard_concept 
                    ON c_occurrence.condition_concept_id = c_standard_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_55630848_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_55630848",
  "condition_55630848_*.csv")
message(str_glue('The data will be written to {condition_55630848_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55630848_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_55630848_path,
  destination_format = "CSV")



# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_55630848_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_55630848_condition_df <- read_bq_export_from_workspace_bucket(condition_55630848_path)

dim(dataset_55630848_condition_df)

head(dataset_55630848_condition_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "measurement" and was generated for All of Us Registered Tier Dataset v7
dataset_55630848_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        m_standard_concept.concept_name as standard_concept_name,
        measurement.measurement_datetime,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high 
    FROM
        ( SELECT
            * 
        FROM
            `measurement` measurement 
        WHERE
            (
                measurement_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    1002597, 3004410, 3005673, 3025315, 3034639, 3036277, 3038553, 37065054, 40759207, 40762636, 40762637, 40765148, 40795740, 4184637, 4197971, 4245997
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    measurement.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                ALL SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                ALL SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) measurement 
            LEFT JOIN
                `concept` m_standard_concept 
                    ON measurement.measurement_concept_id = m_standard_concept.concept_id 
            LEFT JOIN
                `concept` m_value 
                    ON measurement.value_as_concept_id = m_value.concept_id 
            LEFT JOIN
                `concept` m_unit 
                    ON measurement.unit_concept_id = m_unit.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_55630848_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "measurement_55630848",
  "measurement_55630848_*.csv")
message(str_glue('The data will be written to {measurement_55630848_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55630848_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_55630848_path,
  destination_format = "CSV")



# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {measurement_55630848_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), value_as_concept_name = col_character(), unit_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_55630848_measurement_df <- read_bq_export_from_workspace_bucket(measurement_55630848_path)

dim(dataset_55630848_measurement_df)

head(dataset_55630848_measurement_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "person" and was generated for All of Us Registered Tier Dataset v7
dataset_71898682_person_sql <- paste("
    SELECT
        person.person_id,
        person.birth_datetime as date_of_birth,
        p_race_concept.concept_name as race,
        p_ethnicity_concept.concept_name as ethnicity,
        p_sex_at_birth_concept.concept_name as sex_at_birth 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_race_concept 
            ON person.race_concept_id = p_race_concept.concept_id 
    LEFT JOIN
        `concept` p_ethnicity_concept 
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id  
    WHERE
        person.PERSON_ID IN (
            SELECT
                distinct person_id  
            FROM
                `cb_search_person` cb_search_person  
            WHERE
                cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `cb_search_person` p 
                    WHERE
                        age_at_consent BETWEEN 18 AND 120 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        gender_concept_id IN (45878463, 45880669, 2000000002) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                    UNION
                    ALL SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585945) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585946) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000006) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000007)
                            )) criteria 
                    UNION
                    ALL SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585376) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585377) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585378) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585379) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585380) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585381) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585382) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585383) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585384)
                            )) criteria ) 
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_71898682_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_71898682",
  "person_71898682_*.csv")
message(str_glue('The data will be written to {person_71898682_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_71898682_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_71898682_path,
  destination_format = "CSV")



# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_71898682_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character(), sex_at_birth = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_71898682_person_df <- read_bq_export_from_workspace_bucket(person_71898682_path)

dim(dataset_71898682_person_df)

head(dataset_71898682_person_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "survey" and was generated for All of Us Registered Tier Dataset v7
dataset_71898682_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.question,
        answer.answer  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (
                1585375, 1585386, 1585389, 1585838, 1585845, 1585857, 1585860, 1585873, 1585940, 1585952, 1586140, 1586159, 1586162, 1586166, 43528428, 43530562, 43530593, 43530595
            )
        )  
        AND (
            answer.PERSON_ID IN (
                SELECT
                    distinct person_id  
                FROM
                    `cb_search_person` cb_search_person  
                WHERE
                    cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `cb_search_person` p 
                        WHERE
                            age_at_consent BETWEEN 18 AND 120 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            gender_concept_id IN (45878463, 45880669, 2000000002) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                        UNION
                        ALL SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585945) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585946) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000006) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000007)
                                )) criteria 
                        UNION
                        ALL SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585376) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585377) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585378) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585379) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585380) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585381) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585382) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585383) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585384)
                                )) criteria ) 
                    )
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_71898682_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_71898682",
  "survey_71898682_*.csv")
message(str_glue('The data will be written to {survey_71898682_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_71898682_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_71898682_path,
  destination_format = "CSV")



# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_71898682_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_71898682_survey_df <- read_bq_export_from_workspace_bucket(survey_71898682_path)

dim(dataset_71898682_survey_df)

head(dataset_71898682_survey_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "condition" and was generated for All of Us Registered Tier Dataset v7
dataset_71898682_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    201254, 201826, 37018196, 433736
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    c_occurrence.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                ALL SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                ALL SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) c_occurrence 
            LEFT JOIN
                `concept` c_standard_concept 
                    ON c_occurrence.condition_concept_id = c_standard_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_71898682_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_71898682",
  "condition_71898682_*.csv")
message(str_glue('The data will be written to {condition_71898682_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_71898682_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_71898682_path,
  destination_format = "CSV")



# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_71898682_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_71898682_condition_df <- read_bq_export_from_workspace_bucket(condition_71898682_path)

dim(dataset_71898682_condition_df)

head(dataset_71898682_condition_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES" for domain "measurement" and was generated for All of Us Registered Tier Dataset v7
dataset_71898682_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        m_standard_concept.concept_name as standard_concept_name,
        measurement.measurement_datetime,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high 
    FROM
        ( SELECT
            * 
        FROM
            `measurement` measurement 
        WHERE
            (
                measurement_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    3025315, 3036277, 3038553, 40759207, 40762636, 40762637, 40765148, 4245997
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    measurement.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                ALL SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                ALL SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) measurement 
            LEFT JOIN
                `concept` m_standard_concept 
                    ON measurement.measurement_concept_id = m_standard_concept.concept_id 
            LEFT JOIN
                `concept` m_value 
                    ON measurement.value_as_concept_id = m_value.concept_id 
            LEFT JOIN
                `concept` m_unit 
                    ON measurement.unit_concept_id = m_unit.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_71898682_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "measurement_71898682",
  "measurement_71898682_*.csv")
message(str_glue('The data will be written to {measurement_71898682_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_71898682_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_71898682_path,
  destination_format = "CSV")



# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {measurement_71898682_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), value_as_concept_name = col_character(), unit_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_71898682_measurement_df <- read_bq_export_from_workspace_bucket(measurement_71898682_path)

dim(dataset_71898682_measurement_df)

head(dataset_71898682_measurement_df, 5)



#import and name data sets
#use me to access earlier generated files/without having to regenerate using sql code above
aou_person <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/person_55630848/person_55630848_000000000000.csv')
aou_survey <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/survey_55630848/survey_55630848_000000000000.csv')
aou_condition <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/condition_55630848/condition_55630848_000000000000.csv')
aou_measurement <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/measurement_71898682/measurement_71898682_000000000000.csv') #_000000000000 through _000000000009
#import and name data sets
aou_person <- dataset_55630848_person_df #read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/person_55630848/person_55630848_000000000000.csv')
aou_survey <- dataset_55630848_survey_df #read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/survey_55630848/survey_55630848_000000000000.csv')
aou_condition <- dataset_55630848_condition_df #read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/condition_55630848/condition_55630848_000000000000.csv')
aou_measurement <- dataset_71898682_measurement_df #read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/measurement_71898682/measurement_71898682_000000000000.csv') #_000000000000 through _000000000009
#df basics
dim(aou_person)
length(unique(aou_person$person_id))
colnames(aou_person)

dim(aou_survey)
length(unique(aou_condition$person_id))
colnames (aou_survey)

dim(aou_condition)
length(unique(aou_condition$person_id))
colnames(aou_condition)

dim(aou_measurement)
length(unique(aou_measurement$person_id))
colnames(aou_measurement)

#clean person file: 
aou_person <- dataset_55630848_person_df
#aou_person <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240220/person_55630848/person_55630848_000000000000.csv')

dim(aou_person)
length(unique(aou_person$person_id))
colnames(aou_person)

aou_person$date_of_birth[1:10]
#aou_person$race2 <- aou_person$race
table(aou_person$race2, aou_person$ethnicity, exclude=NULL)

#aou_person$age <- as.date(visit_start_date, %YYYY-%mm-%dd) - as.date(date_of_birth, %YYYY-%mm-%dd) #visit_start_date only available in visit file - need to request
aou_person$sex <- aou_person$sex_at_birth
aou_person$race <- ifelse(aou_person$ethnicity=='Hispanic or Latino','Hispanic',
                          ifelse(aou_person$race2=='White', 'NHW',
                                 ifelse(aou_person$race2=='Black or African American', 'NHB',
                                        ifelse(aou_person$race2=='Asian', 'NHAsian',
                                               ifelse(aou_person$race2=='Another single population', 'Other',
                                                      ifelse(aou_person$race2=='More than one population', 'Multiracial',
                                                             ifelse(aou_person$race2=='None of these', 'None of these',
                                                                    ifelse(aou_person$race2=='None Indicated', 'Missing','Missing'))))))))
table(aou_person$race2, aou_person$race, exclude=NULL)

aou_person_final <- aou_person[,c('person_id','date_of_birth','sex','race')]

#write.csv(aou_person_final, file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/AoU_RExSES_person_final.csv', row.names=F)

#name_to_save <- "AoU_RExSES_person"
#dataset_to_csv_to_bucket(name_to_save = name_to_save, df=aou_person_final)

#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
#save aou_person_final
write.csv(aou_person_final, file='AoU_RExSES_person_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_person_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_person_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_person_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)


#clean aou_survey
aou_survey <- dataset_55630848_survey_df
#aou_survey <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240205/survey_55630848/survey_55630848_000000000000.csv')
#table(aou_survey$question)
#aou_survey$answer[1:40]
#aou_survey <- aou_survey[,c('person_id','question','answer')]
aou_survey_concat <- aou_survey %>% group_by(person_id, survey_datetime, question) %>% dplyr::mutate(answers_all=paste0(answer, collapse=" & "))
#mutate(answers_all = paste0(answer, collapse = ""))
table(aou_survey_concat$answers_all[aou_survey_concat$question=='Health Insurance: Health Insurance Type'])
dim(aou_survey_concat)

#colnames(aou_survey)
#table(aou_survey$question)
table(aou_survey$answer[aou_survey$question=='Health Insurance: Insurance Type Update'])

aou_survey_concat$survey_datetime[1:20]
aou_survey_concat$survey_datetime <- as.Date(aou_survey_concat$survey_datetime,format='%Y-%m-%d %H:%M:%S')
class(aou_survey_concat$survey_datetime)
length(unique(aou_survey_concat$survey_datetime))

#aou_survey_concat$survey_date <- aou_survey_concat %>% group_by(person_id) %>% mutate(survey_date = min(survey_datetime))
#length(unique(aou_survey_concat$survey_date))

aou_survey_concat <- aou_survey_concat %>% group_by(person_id) %>% mutate(survey_date = min(survey_datetime))
length(unique(aou_survey_concat$survey_date))

aou_survey_concat$survey_date[1:100]
#length(unique(aou_survey_concat$survey_date))
dim(aou_survey_concat)
aou_survey_concat <- select(aou_survey_concat, -survey_datetime)
dim(aou_survey_concat)

aou_survey_short <- aou_survey_concat %>% filter(str_detect(question, 'Education Level: Highest Grade|Employment: Employment Status|Health Advice: Place for Health Advice|Health Advice: What Kind Of Place|Health Insurance: Health Insurance Type|Health Insurance: Insurance Type Update|Income: Annual Income|Insurance: Health Insurance|Smoking: 100 Cigs Lifetime|Smoking: Current Daily Cigarette Number'))
aou_survey_short <- aou_survey_short[,c('person_id','survey_date','question','answers_all')]
#aou_survey_short <- aou_survey[aou_survey$question %in% c('Education Level: Highest Grade','Employment: Employment Status',
#'Health Advice: Place for Health Advice','Health Advice: What Kind Of Place',
#'Health Insurance: Health Insurance Type','Income: Annual Income','Insurance: Health Insurance',
#'Smoking: 100 Cigs Lifetime','Smoking: Current Daily Cigarette Number')
aou_survey_wide <- aou_survey_short %>% pivot_wider(id_cols=c(person_id,survey_date), names_from = question, values_from = answers_all, values_fn=max)
dim(aou_survey_wide)
colnames(aou_survey_wide)
length(unique(aou_survey_wide$person_id))
aou_survey_wide[1:20,]
aou_survey_wide[aou_survey_wide$person_id==1000004,]

sum(is.na(aou_survey_wide$person_id))
aou_survey_wide2 <- aou_survey_wide[with(aou_survey_wide,order(person_id)),]
dup_check <- aou_survey_wide2[duplicated(aou_survey_wide2$person_id),1:3]
dup_check[with(dup_check,order(person_id)),][1:20,]
dim(dup_check)

table(aou_survey_wide$'Education Level: Highest Grade')
table(aou_survey_wide$'Income: Annual Income')
table(aou_survey_wide$'Employment: Employment Status')

table(aou_survey_wide$'Smoking: 100 Cigs Lifetime')
table(aou_survey_wide$'Smoking: Current Daily Cigarette Number')

table(aou_survey_wide$'Insurance: Health Insurance')
table(aou_survey_wide$'Health Insurance: Health Insurance Type')
table(aou_survey_wide$'Health Insurance: Insurance Type Update')
table(aou_survey_wide$'Health Advice: Place for Health Advice')
table(aou_survey_wide$'Health Advice: What Kind Of Place')

#####clean SES data#####
#clean SES data

aou_survey_wide$hreduc <- aou_survey_wide$'Education Level: Highest Grade'
aou_survey_wide$educ_simple <- ifelse(aou_survey_wide$hreduc=='College graduate or advanced degree','College Degree', 
                                      ifelse(aou_survey_wide$hreduc=='Highest Grade: College One to Three', 'Some College',
                                             ifelse(aou_survey_wide$hreduc=='Highest Grade: Twelve Or GED', 'HS Degree',
                                                    ifelse(aou_survey_wide$hreduc=='Less than a high school degree or equivalent','Less than HS', NA))))
table(aou_survey_wide$hreduc, aou_survey_wide$educ_simple, exclude=NULL)
aou_survey_wide$educ_num <- ifelse(aou_survey_wide$hreduc=='College graduate or advanced degree',7, 
                                   ifelse(aou_survey_wide$hreduc=='Highest Grade: College One to Three', 6,
                                          ifelse(aou_survey_wide$hreduc=='Highest Grade: Twelve Or GED', 4,
                                                 ifelse(aou_survey_wide$hreduc=='Less than a high school degree or equivalent',2, NA))))
boxplot_indiveduc <- ggplot(aou_survey_wide, aes(factor(x=racecat), y=educ_num, fill=factor(diab)), exclude=TRUE) + geom_boxplot(notch=T)

aou_survey_wide$hhinc <- aou_survey_wide$'Income: Annual Income'
aou_survey_wide$inc_simple <- ifelse(aou_survey_wide$hhinc=="Annual Income: less 10k" | aou_survey_wide$hhinc=="Annual Income: 10k 25k", 'Under $25,000',
                                     ifelse(aou_survey_wide$hhinc=="Annual Income: 25k 35k" | aou_survey_wide$hhinc=="Annual Income: 35k 50k", '$25,000-49,999', 
                                            ifelse(aou_survey_wide$hhinc=="Annual Income: 50k 75k", '$50,000-74,999',
                                                   ifelse(aou_survey_wide$hhinc=="Annual Income: 75k 100k" | aou_survey_wide$hhinc=="Annual Income: 100k 150k"| 
                                                            aou_survey_wide$hhinc=='Annual Income: 150k 200k'| aou_survey_wide$hhinc=='Annual Income: more 200k', "More than $75,000", NA))))

aou_survey_wide$inc_simple2 <- ifelse(aou_survey_wide$hhinc=="Annual Income: less 10k" | aou_survey_wide$hhinc=="Annual Income: 10k 25k", 'Under $25,000',
                                      ifelse(aou_survey_wide$hhinc=="Annual Income: 25k 35k" | aou_survey_wide$hhinc=="Annual Income: 35k 50k", '$25,000-49,999', 
                                             ifelse(aou_survey_wide$hhinc=="Annual Income: 50k 75k", '$50,000-74,999',
                                                    ifelse(aou_survey_wide$hhinc=="Annual Income: 75k 100k" | aou_survey_wide$hhinc=="Annual Income: 100k 150k",'$75,000-149,999',
                                                           ifelse(aou_survey_wide$hhinc=='Annual Income: 150k 200k'| aou_survey_wide$hhinc=='Annual Income: more 200k', "More than $150,000", NA)))))
table(aou_survey_wide$hhinc, aou_survey_wide$inc_simple, exclude=NULL)
table(aou_survey_wide$inc_simple, exclude=NULL)

aou_survey_wide$educ_simple <- factor(aou_survey_wide$educ_simple, levels=c("College Degree","Some College","HS Degree","Less than HS"))
aou_survey_wide$hreduc <- factor(aou_survey_wide$hreduc, levels=c('Less than a high school degree or equivalent', 'Highest Grade: Twelve Or GED',  'Highest Grade: College One to Three', 'College graduate or advanced degree', 'PMI: Prefer Not To Answer','PMI: Skip'))
aou_survey_wide$hhinc <- factor(aou_survey_wide$hhinc, levels=c("Annual Income: more 200k","Annual Income: 150k 200k","Annual Income: 100k 150k","Annual Income: 75k 100k","Annual Income: 50k 75k","Annual Income: 35k 50k","Annual Income: 25k 35k","Annual Income: 10k 25k","Annual Income: less 10k","PMI: Prefer Not To Answer","PMI: Skip"))
aou_survey_wide$inc_simple <- factor(aou_survey_wide$inc_simple, levels=c("More than $75,000","$45,000-74,999","$20,000-44,999","Under $20,000"))
aou_survey_wide$inc_simple2 <- factor(aou_survey_wide$inc_simple2, levels=c("More than $150,000","$75,000-149,999","$45,000-74,999","$20,000-44,999","Under $20,000"))
table(aou_survey_wide$hhinc)

aou_survey_wide$all_ses_na <- ifelse(is.na(aou_survey_wide$educ_num) & is.na(aou_survey_wide$hhinc),1,0)
addmargins(table(aou_survey_wide$all_ses_na, exclude=NULL))


aou_survey_wide$work_yn <- ifelse(aou_survey_wide$'Employment: Employment Status'=='Employed for wages or self-employed' | 
                                    aou_survey_wide$'Employment: Employment Status'=='Employed for wages or self-employed & Not currently employed for wages' | 
                                    aou_survey_wide$'Employment: Employment Status'=='Not currently employed for wages & Employed for wages or self-employed', 'Working', 
                                  ifelse(aou_survey_wide$'Employment: Employment Status'=='Not currently employed for wages', 'Not Working', NA))
table(aou_survey_wide$'Employment: Employment Status', aou_survey_wide$work_yn, exclude=NULL)

dim(aou_survey_wide)

#clean smoking
aou_survey_wide$smoke_ever <- ifelse(is.na(aou_survey_wide$'Smoking: 100 Cigs Lifetime'),'Missing',
                                     ifelse(aou_survey_wide$'Smoking: 100 Cigs Lifetime'=='100 Cigs Lifetime: Yes', 1,
                                            ifelse(aou_survey_wide$'Smoking: 100 Cigs Lifetime'=='100 Cigs Lifetime: No',0,'Missing')))
aou_survey_wide$smoke_current<- ifelse(aou_survey_wide$'Smoking: Current Daily Cigarette Number'=='PMI: Skip' |
                                         aou_survey_wide$'Smoking: Current Daily Cigarette Number'=='PMI: Prefer Not To Answer' |
                                         aou_survey_wide$'Smoking: Current Daily Cigarette Number'=='PMI: Dont Know'|
                                         is.na(aou_survey_wide$'Smoking: Current Daily Cigarette Number'), "Missing",
                                       ifelse(aou_survey_wide$'Smoking: Current Daily Cigarette Number'==0, 0,1))
table(aou_survey_wide$smoke_ever, aou_survey_wide$smoke_current, exclude=NULL)
aou_survey_wide$smoke_status <- ifelse(aou_survey_wide$smoke_ever==0, 'Never Smoker', 
                                       ifelse(aou_survey_wide$smoke_current==1, 'Current Smoker',
                                              ifelse(aou_survey_wide$smoke_ever==1 & aou_survey_wide$smoke_current==0, 'Former Smoker',
                                                     ifelse(aou_survey_wide$smoke_ever==1,'Current or Former Smoker NOS','Missing'))))
table(aou_survey_wide$smoke_ever, aou_survey_wide$smoke_status, exclude=NULL)
table(aou_survey_wide$smoke_current, aou_survey_wide$smoke_status, exclude=NULL)
aou_survey_wide$smoke_status <- factor(aou_survey_wide$smoke_status, levels=c("Never Smoker","Former Smoker","Current or Former Smoker NOS","Current Smoker","Missing"))
table(aou_survey_wide$smoke_status, exclude=NULL)

#sum(stringr::str_count(aou_survey_wide$'Health Insurance: Health Insurance Type', pattern = "rivate"))
#sum(stringr::str_count(aou_survey_wide$'Health Insurance: Insurance Type Update', pattern = "mployer"))
#colnames(aou_survey_wide)
aou_survey_wide[1:5,]
aou_survey_wide$healthinsurtype_combined <- paste(aou_survey_wide$'Health Insurance: Health Insurance Type', " plus UPDATE column: ", aou_survey_wide$'Health Insurance: Insurance Type Update')
aou_survey_wide[1:5,]
sum(stringr::str_count(aou_survey_wide$healthinsurtype_combined, pattern = "rivate"))
sum(stringr::str_count(aou_survey_wide$'Health Insurance: Insurance Type Update', pattern = "mployer"))

#aou_survey_wide$insur_type_none <- ifelse(aou_survey_wide$'Health Insurance: Health Insurance Type'=='Health Insurance Type: No Coverage',1,0)
aou_survey_wide$insur_type_none <- ifelse(aou_survey_wide$'Health Insurance: Insurance Type Update'=='Insurance Type Update: None',1,0)
table(aou_survey_wide$insur_type_none, exclude=NULL)

sum(stringr::str_count(aou_survey_wide$'Health Insurance: Health Insurance Type', pattern = "rivate"))
sum(stringr::str_count(aou_survey_wide$healthinsurtype_combined, pattern = "mployer"))
#clean insurance
table(aou_survey_wide$'Insurance: Health Insurance',exclude=NULL)#aou_survey_wide$'Health Insurance: Health Insurance Type', exclude=NULL)
aou_survey_wide$insur_type_none <- ifelse(aou_survey_wide$'Health Insurance: Health Insurance Type'=='Health Insurance Type: No Coverage',1,0)
aou_survey_wide$insur_curr_yn <- ifelse(aou_survey_wide$'Insurance: Health Insurance'=='Health Insurance: Yes',1,
                                        ifelse(aou_survey_wide$'Insurance: Health Insurance'=='Health Insurance: No'| 
                                                 aou_survey_wide$'Health Insurance: Health Insurance Type'=='Health Insurance Type: No Coverage'|
                                                 aou_survey_wide$'Health Insurance: Insurance Type Update'=='Insurance Type Update: None',0,
                                               ifelse(is.na(aou_survey_wide$'Insurance: Health Insurance'),'Missing','Missing')))
table(aou_survey_wide$insur_curr_yn, exclude=NULL)
#classify 'Health Insurance: Health Insurance Type'
aou_survey_wide$insur_private <- ifelse(grepl('Private|Purchased',aou_survey_wide$healthinsurtype_combined),1,0)
aou_survey_wide$insur_employer <- ifelse(grepl('Employer',aou_survey_wide$healthinsurtype_combined),1,0)
aou_survey_wide$insur_mcare <- ifelse(grepl('Medicare|Medi GAP',aou_survey_wide$healthinsurtype_combined),1,0)
aou_survey_wide$insur_lowincgov <- ifelse(grepl('Medicaid|Schip|State',aou_survey_wide$healthinsurtype_combined),1,0)
aou_survey_wide$insur_othgovmil <- ifelse(grepl('Other Gov|VA|Military',aou_survey_wide$healthinsurtype_combined),1,0)
aou_survey_wide$insur_ihs <- ifelse(grepl('Indian',aou_survey_wide$healthinsurtype_combined),1,0)
aou_survey_wide$insur_singserv <- ifelse(grepl('Single Service',aou_survey_wide$healthinsurtype_combined),1,0)
aou_survey_wide$insur_other <- ifelse(grepl('Other Health',aou_survey_wide$healthinsurtype_combined),1,0)
aou_survey_wide$insur_curr <- ifelse(aou_survey_wide$insur_curr_yn==1|aou_survey_wide$insur_private==1|aou_survey_wide$insur_employer==1|
                                       aou_survey_wide$insur_mcare==1|aou_survey_wide$insur_lowincgov==1|aou_survey_wide$insur_othgovmil==1|
                                       aou_survey_wide$insur_ihs==1|aou_survey_wide$insur_singserv==1,1,0)

#table(aou_survey_wide$insur_curr, aou_survey_wide$insur_private, exclude=NULL)
#insur_stability variable will be different than NHANES as no uninsur_pastyr data available; insur_complex rounds up (if you have both private and ihs, you are listed as private)
aou_survey_wide$insur_stability <- ifelse(aou_survey_wide$insur_curr==1 & aou_survey_wide$insur_private==1, 'Insured-Private',
                                          ifelse(aou_survey_wide$insur_curr==1 & aou_survey_wide$insur_employer==1, 'Insured-Private',
                                                 ifelse(aou_survey_wide$insur_curr==1 & !aou_survey_wide$insur_private==1, 'Insured-Other', 
                                                        ifelse(aou_survey_wide$insur_curr==0, 'Uninsured', 'Missing'))))
aou_survey_wide$insur_complex <- ifelse(aou_survey_wide$insur_curr==0, 'Uninsured',
                                        ifelse(aou_survey_wide$insur_private==1, 'Private',
                                               ifelse(aou_survey_wide$insur_employer==1, 'Employer',
                                                      ifelse(aou_survey_wide$insur_mcare==1, 'Medicare', 
                                                             ifelse(aou_survey_wide$insur_othgovmil==1, 'Other Gov/Military', 
                                                                    ifelse(aou_survey_wide$insur_ihs==1, 'IHS', 
                                                                           ifelse(aou_survey_wide$insur_lowincgov==1, 'Medicaid/Low Inc Insurance', 
                                                                                  ifelse(aou_survey_wide$insur_singserv==1, 'Single Service', 
                                                                                         ifelse(aou_survey_wide$insur_other==1, 'Other', 'Missing')))))))))
table(aou_survey_wide$insur_stability, aou_survey_wide$insur_complex, exclude=NULL)
addmargins(table(aou_survey_wide$insur_curr, aou_survey_wide$insur_stability, exclude=NULL))
addmargins(table(aou_survey_wide$insur_curr, aou_survey_wide$insur_private))
aou_survey_wide$insur_stability <- factor(aou_survey_wide$insur_stability, levels=c("Insured-Private","Insured-Other","Uninsured","Missing"))

#clean routine place of healthcare - only counting longitudinal outpatient (not ER, UC, etc.)
aou_survey_wide$hc_routine_place <- ifelse(is.na(aou_survey_wide$'Health Advice: Place for Health Advice'),'Missing',
                                           ifelse(aou_survey_wide$'Health Advice: Place for Health Advice'=='Place for Health Advice: Yes'|
                                                    aou_survey_wide$'Health Advice: Place for Health Advice'=='Place for Health Advice: More Than One',1,
                                                  ifelse(aou_survey_wide$'Health Advice: Place for Health Advice'=='Place for Health Advice: No',0,'Missing')))
table(aou_survey_wide$hc_routine_place, exclude=NULL)
aou_survey_wide$hc_routine_place_typenonemerg <- ifelse(is.na(aou_survey_wide$'Health Advice: What Kind Of Place'),'Missing',
                                                        ifelse(aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: Doctors Office'|
                                                                 aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: Some Other Place'|
                                                                 aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: No One Place Most Often',1,
                                                               ifelse(aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: Urgent Care'|
                                                                        aou_survey_wide$'Health Advice: What Kind Of Place'=='What Kind Of Place: Emergency Room',0,'Missing')))
addmargins(table(aou_survey_wide$hc_routine_place, aou_survey_wide$hc_routine_place_typenonemerg, exclude=NULL))
aou_survey_wide$hc_routine_place_yn <- ifelse(aou_survey_wide$hc_routine_place=='Missing' & aou_survey_wide$hc_routine_place_typenonemerg=='Missing','Missing',
                                              ifelse(aou_survey_wide$hc_routine_place==1 | aou_survey_wide$hc_routine_place_typenonemerg==1,1,0))
table(aou_survey_wide$hc_routine_place_yn, exclude=NULL)

dim(aou_survey_wide)
colnames(aou_survey_wide)
aou_survey_final <- aou_survey_wide[,c(1:2,13:23,25,33:36,40)]
colnames(aou_survey_final)

#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
#save aou_survey_final
write.csv(aou_survey_final, file='AoU_RExSES_survey_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_survey_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_survey_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_survey_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)


#####clean condition data set#####
#clean condition data set
aou_cond <- dataset_55630848_condition_df
dim(aou_cond)
head(aou_cond)
length(unique(aou_cond$person_id))

#table(aou_cond$standard_concept_name)
aou_cond$t1d <- ifelse(grepl('type 1|latent autoimmune',aou_cond$standard_concept_name, ignore.case=T),1,0)
aou_cond$ob <- ifelse(grepl('obes',aou_cond$standard_concept_name, ignore.case=T),1,0)
aou_cond$predm <- ifelse(grepl('Prediabetes',aou_cond$standard_concept_name, ignore.case=T) & aou_cond$t1d==0,1,0)
aou_cond$t2d <- ifelse(grepl('diabetes mellitus|type 2',aou_cond$standard_concept_name, ignore.case=T) & aou_cond$t1d==0,1,0)
addmargins(table(aou_cond$t1d, aou_cond$t2d, exclude=NULL))
addmargins(table(aou_cond$predm, aou_cond$t2d, exclude=NULL))
addmargins(table(aou_cond$ob, aou_cond$t2d, exclude=NULL))

aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(sum_t1d = sum(t1d))
aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(sum_ob = sum(ob))
aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(sum_predm = sum(predm))
aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(sum_t2d = sum(t2d))
aou_cond[1:40,c(1,4:11)]

dim(aou_cond)
aou_cond_short <- aou_cond[!duplicated(aou_cond$person_id),c('person_id','t1d','ob','predm','t2d','sum_t1d','sum_ob','sum_predm','sum_t2d')]
dim(aou_cond_short)
head(aou_cond_short)

aou_cond_short$t1d <- ifelse(aou_cond_short$sum_t1d >0,1,0)
aou_cond_short$ob <- ifelse(aou_cond_short$sum_ob >0,1,0)
aou_cond_short$predm <- ifelse(aou_cond_short$sum_predm >0,1,0)
aou_cond_short$t2d <- ifelse(aou_cond_short$sum_t2d >0 & aou_cond_short$t1d==0,1,0)
addmargins(table(aou_cond_short$t1d, aou_cond_short$t2d, exclude=NULL))
addmargins(table(aou_cond_short$predm, aou_cond_short$t2d, exclude=NULL))
addmargins(table(aou_cond_short$ob, aou_cond_short$t2d, exclude=NULL))

aou_cond_final <- aou_cond_short[,c('person_id','t1d','ob','predm','t2d')]
head(aou_cond_final)

#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
#save aou_cond_final
write.csv(aou_cond_final, file='AoU_RExSES_cond_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_cond_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_survey_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_survey_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)


#####clean measurement data set#####
#clean measurement data set
aou_meas <- dataset_55630848_measurement_df
dim(aou_meas)
head(aou_meas)
length(unique(aou_meas$person_id))

table(aou_meas$standard_concept_name)

aou_meas$type <- ifelse(grepl('urine|periton|cerebral|CSF|pleur|synov|body fluid|of Dose|estimated from glycated|method correlation|narrative', aou_meas$standard_concept_name, ignore.case=T),'other',
                        ifelse(grepl('bmi|body mass index', aou_meas$standard_concept_name, ignore.case=T),'bmi',
                               ifelse(grepl('height', aou_meas$standard_concept_name, ignore.case=T),'height',
                                      ifelse(grepl('weight', aou_meas$standard_concept_name, ignore.case=T),'weight',
                                             ifelse(grepl('fasting glucose|hours fasting', aou_meas$standard_concept_name, ignore.case=T),'fbg',
                                                    ifelse(grepl('estimated from glycated hyemoglobin', aou_meas$standard_concept_name, ignore.case=T),'meanbg_a1c',
                                                           ifelse(grepl('glucose', aou_meas$standard_concept_name, ignore.case=T),'rbg',
                                                                  ifelse(grepl('HbA1c|hemoglobin A1c', aou_meas$standard_concept_name, ignore.case=T),'a1c',
                                                                         ifelse(grepl('waist circum', aou_meas$standard_concept_name, ignore.case=T),'waist',
                                                                                ifelse(grepl('hip circum', aou_meas$standard_concept_name, ignore.case=T),'hip','other'))))))))))
table(aou_meas$type)

addmargins(table(aou_meas$standard_concept_name, aou_meas$type, exclude=NULL))

colnames(aou_meas)
aou_meas$measurement_datetime[1:20]
aou_meas$measurement_datetime <- as.Date(aou_meas$measurement_datetime,format='%Y-%m-%d %H:%M:%S')
class(aou_meas$measurement_datetime)
length(unique(aou_meas$measurement_datetime))
colnames(aou_meas) <- c('person_id','standard_concept_name','date','value','value_as_concept_id','value_as_concept_name',
                        'units','LLN','ULN','type')
head(aou_meas)

aou_meas_bmi <- aou_meas[aou_meas$type=='bmi',c('person_id','type','date','value','units','LLN','ULN')]
dim(aou_meas_bmi)
length(unique(aou_meas_bmi$person_id))

aou_meas_fbg <- aou_meas[aou_meas$type=='fbg',c('person_id','type','date','value','units','LLN','ULN')]
dim(aou_meas_fbg)
length(unique(aou_meas_fbg$person_id))

aou_meas_rbg <- aou_meas[aou_meas$type=='rbg',c('person_id','type','date','value','units','LLN','ULN')]
dim(aou_meas_rbg)
length(unique(aou_meas_rbg$person_id))

aou_meas_a1c <- aou_meas[aou_meas$type=='a1c',c('person_id','type','date','value','units','LLN','ULN')]
dim(aou_meas_a1c)
length(unique(aou_meas_a1c$person_id))

#clean BMI
aou_meas_bmi <- aou_meas_bmi[!is.na(aou_meas_bmi$value),]
length(unique(aou_meas_bmi$person_id))
table(aou_meas_bmi$units, exclude=NULL)
class(aou_meas_bmi$value)
hist(aou_meas_bmi$value, breaks=20, ylim=c(0,100), freq=T)

table(aou_meas$value[aou_meas$type=='bmi' & is.na(aou_meas$units)])
summary(aou_meas_bmi$value[is.na(aou_meas_bmi$units)])
summary(aou_meas_bmi$value[aou_meas_bmi$units=='kg/sq. m'])
summary(aou_meas_bmi$value[aou_meas_bmi$units=='kilogram per square meter'])
summary(aou_meas_bmi$value[aou_meas_bmi$units=="link for Gunter's chain (US)"])
summary(aou_meas_bmi$value[aou_meas_bmi$units=='No matching concept'])
summary(aou_meas_bmi$value[aou_meas_bmi$units=='no value'])
summary(aou_meas_bmi$value[aou_meas_bmi$units=='percent'])
summary(aou_meas_bmi$value[aou_meas_bmi$units=='ratio'])
summary(aou_meas_bmi$value[aou_meas_bmi$units=='square meter'])
summary(aou_meas_bmi$value[aou_meas_bmi$units=='unit'])

aou_meas_bmi <- aou_meas_bmi[aou_meas_bmi$value >=10 & aou_meas_bmi$value <= 100,]
dim(aou_meas_bmi)
length(unique(aou_meas_bmi$person_id))
aou_meas_bmi <- aou_meas_bmi %>% group_by(person_id) %>% mutate(bmi_max=max(value))
summary(aou_meas_bmi$bmi_max)

aou_meas_bmi <- aou_meas_bmi %>% group_by(person_id) %>% mutate(bmi_date_earliest=min(date))
aou_meas_bmi <- aou_meas_bmi %>% group_by(person_id) %>% mutate(bmi_date_latest=max(date))
aou_meas_bmi <- aou_meas_bmi %>% group_by(person_id) %>% mutate(bmi_count = n())
head(aou_meas_bmi)

aou_meas_bmi_final <- aou_meas_bmi[, c('person_id','bmi_max','bmi_date_earliest','bmi_date_latest','bmi_count')]
aou_meas_bmi_final <- aou_meas_bmi_final[!duplicated(aou_meas_bmi_final),]
aou_meas_bmi_final$ob_bmi <- ifelse(aou_meas_bmi_final$bmi_max >= 30,1,0)
dim(aou_meas_bmi_final)
length(unique(aou_meas_bmi_final$person_id))

#clean FBG
aou_meas_fbg <- aou_meas_fbg[!is.na(aou_meas_fbg$value),]
length(unique(aou_meas_fbg$person_id))
table(aou_meas_fbg$units, exclude=NULL)
#class(aou_meas_fbg$value)
#hist(aou_meas_fbg$value, breaks=20, ylim=c(0,500), freq=T)

table(aou_meas$value[aou_meas$type=='fbg' & is.na(aou_meas$units)])
summary(aou_meas_fbg$value[is.na(aou_meas_fbg$units)])
#summary(aou_meas_fbg$value[aou_meas_fbg$units=='mg/dL'])
#summary(aou_meas_fbg$value[aou_meas_fbg$units=='milligram per deciliter'])
#summary(aou_meas_fbg$value[aou_meas_fbg$units=='millimole per liter']) # median IQR is 95 (86,113) so this is actually mg/dL
#summary(aou_meas_fbg$value[aou_meas_fbg$units=='No matching concept'])
#summary(aou_meas_fbg$value[aou_meas_fbg$units=='no value'])

aou_meas_fbg <- aou_meas_fbg[aou_meas_fbg$value >=30 & aou_meas_fbg$value <= 500,]
dim(aou_meas_fbg)
length(unique(aou_meas_fbg$person_id))
aou_meas_fbg <- aou_meas_fbg %>% group_by(person_id) %>% mutate(fbg_max=max(value))
summary(aou_meas_fbg$fbg_max)

aou_meas_fbg <- aou_meas_fbg %>% group_by(person_id) %>% mutate(fbg_date_earliest=min(date))
aou_meas_fbg <- aou_meas_fbg %>% group_by(person_id) %>% mutate(fbg_date_latest=max(date))
aou_meas_fbg <- aou_meas_fbg %>% group_by(person_id) %>% mutate(fbg_count = n())
head(aou_meas_fbg)

aou_meas_fbg$fbg_hi <- ifelse(is.na(aou_meas_fbg$ULN),'missing',ifelse(aou_meas_fbg$fbg_max >= aou_meas_fbg$ULN,1,0))
aou_meas_fbg$fbg_gt100 <- ifelse(is.na(aou_meas_fbg$ULN) & aou_meas_fbg$fbg_max >= 100,1,0)
aou_meas_fbg$dm_fbg <- ifelse(aou_meas_fbg$fbg_max >= 30,1,0)
table(aou_meas_fbg$fbg_hi, exclude=NULL)
summary(aou_meas_fbg$fbg_max[aou_meas_fbg$fbg_hi==1])
table(aou_meas_fbg$fbg_gt100, exclude=NULL)
summary(aou_meas_fbg$fbg_max[aou_meas_fbg$fbg_gt100==1])
table(aou_meas_fbg$fbg_hi, aou_meas_fbg$fbg_gt100, exclude=NULL)

aou_meas_fbg_final <- aou_meas_fbg[, c('person_id','fbg_max','fbg_date_earliest','fbg_date_latest','fbg_count','fbg_hi','fbg_gt100')]
aou_meas_fbg_final <- aou_meas_fbg_final[!duplicated(aou_meas_fbg_final),]
aou_meas_fbg_final$dm_fbg <- ifelse(aou_meas_fbg_final$fbg_hi==1 | aou_meas_fbg_final$fbg_gt100==1,1,0)
table(aou_meas_fbg_final$dm_fbg, exclude=NULL)
dim(aou_meas_fbg_final)
length(unique(aou_meas_fbg_final$person_id))

#clean RBG
aou_meas_rbg <- aou_meas_rbg[!is.na(aou_meas_rbg$value),]
colnames(aou_meas_rbg)
length(unique(aou_meas_rbg$person_id))
#table(aou_meas_rbg$units, exclude=NULL)
#class(aou_meas_rbg$value)
#hist(aou_meas_rbg$value, breaks=20, ylim=c(0,500), freq=T)

#table(aou_meas$standard_concept_name[aou_meas$type=='rbg'])
#table(aou_meas$standard_concept_name[aou_meas$type=='rbg' & aou_meas$units=='calculated'])
#table(aou_meas$standard_concept_name[aou_meas$type=='rbg' & aou_meas$units=='Gauss'])
#table(aou_meas$standard_concept_name[aou_meas$type=='rbg' & aou_meas$units=='gram'])
#table(aou_meas$standard_concept_name[aou_meas$type=='rbg' & aou_meas$units=='gram per deciliter'])
#table(aou_meas$standard_concept_name[aou_meas$type=='rbg' & aou_meas$units=='gram per liter'])
#table(aou_meas$standard_concept_name[aou_meas$type=='rbg' & aou_meas$units=='milligram per deciliter calculated'])
#table(aou_meas$standard_concept_name[aou_meas$type=='rbg' & aou_meas$units=='milligram per gram of creatinine'])

table(aou_meas$value[aou_meas$type=='rbg' & is.na(aou_meas$units)])
summary(aou_meas_rbg$value[is.na(aou_meas_rbg$units)])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='calculated']) #from A1c - not an actual glucose measure
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='Gauss'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='gram'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='gram per deciliter'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='gram per liter'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='mg/dL'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='microgram per deciliter'])# median IQR is 140 (115, 163) so this is actually mg/dL
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='milligram per deciliter'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='milligram per deciliter calculated']) #from A1c - not an actual glucose measure
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='milligram per milliliter'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='millimole per liter'])# median IQR is 128 (114,157) so this is actually mg/dL
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='mmol/L']) #actually mmol/L
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='No matching concept'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='no value'])
#summary(aou_meas_rbg$value[aou_meas_rbg$units=='thousand per microliter'])

aou_meas_rbg <- aou_meas_rbg[grepl('mg/dL|microgram per deciliter|milligram per deciliter|millimole per liter|mmol/L|
                                #No matching concept|no value',aou_meas_rbg$units, ignore.case=T),]
dim(aou_meas_rbg)
length(unique(aou_meas_rbg$person_id))
summary(aou_meas_rbg$value)
aou_meas_rbg$value <- ifelse(is.na(aou_meas_rbg$units), aou_meas_rbg$value, ifelse(aou_meas_rbg$units=='mmol/L', aou_meas_rbg$value*18.0182, aou_meas_rbg$value))
summary(aou_meas_rbg$value)

aou_meas_rbg <- aou_meas_rbg[aou_meas_rbg$value >=30 & aou_meas_rbg$value <= 999,]
dim(aou_meas_rbg)
length(unique(aou_meas_rbg$person_id))
aou_meas_rbg <- aou_meas_rbg %>% group_by(person_id) %>% mutate(rbg_max=max(value))
summary(aou_meas_rbg$rbg_max)

aou_meas_rbg <- aou_meas_rbg %>% group_by(person_id) %>% mutate(rbg_date_earliest=min(date))
aou_meas_rbg <- aou_meas_rbg %>% group_by(person_id) %>% mutate(rbg_date_latest=max(date))
aou_meas_rbg <- aou_meas_rbg %>% group_by(person_id) %>% mutate(rbg_count = n())
head(aou_meas_rbg)

aou_meas_rbg_final <- aou_meas_rbg[, c('person_id','rbg_max','rbg_date_earliest','rbg_date_latest','rbg_count')]
aou_meas_rbg_final <- aou_meas_rbg_final[!duplicated(aou_meas_rbg_final),]
aou_meas_rbg_final$dm_rbg <- ifelse(aou_meas_rbg_final$rbg_max >= 200,1,0)
table(aou_meas_rbg_final$dm_rbg, exclude=NULL)
dim(aou_meas_rbg_final)
length(unique(aou_meas_rbg_final$person_id))


#clean A1c
aou_meas_a1c <- aou_meas_a1c[!is.na(aou_meas_a1c$value),]
colnames(aou_meas_a1c)
length(unique(aou_meas_a1c$person_id))
#table(aou_meas_a1c$units, exclude=NULL)
#class(aou_meas_a1c$value)
#hist(aou_meas_a1c$value, breaks=20, ylim=c(0,500), freq=T)

#table(aou_meas$value[aou_meas$type=='a1c' & is.na(aou_meas$units)])
#table(aou_meas$standard_concept_name[aou_meas$type=='a1c' & aou_meas$units=='gram per deciliter'])
#table(aou_meas$value[aou_meas$type=='a1c' & aou_meas$units=='gram per deciliter'])
#table(aou_meas$standard_concept_name[aou_meas$type=='a1c' & aou_meas$units=='gram per deciliter'])
#table(aou_meas$standard_concept_name[aou_meas$type=='a1c' & aou_meas$units=='percent'])
#table(aou_meas$value[aou_meas$type=='a1c' & aou_meas$units=='Percentage unit'])
#table(aou_meas$standard_concept_name[aou_meas$type=='a1c' & aou_meas$units=='gram per deciliter'])
#table(aou_meas$standard_concept_name[aou_meas$type=='a1c' & aou_meas$units=='gram per liter'])
#table(aou_meas$standard_concept_name[aou_meas$type=='a1c' & aou_meas$units=='milligram per deciliter calculated'])
#table(aou_meas$standard_concept_name[aou_meas$type=='a1c' & aou_meas$units=='milligram per gram of creatinine'])

#summary(aou_meas_a1c$value[is.na(aou_meas_a1c$units)])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='% of total']) 
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='gram per deciliter']) #738 entries, range 0.3-2 mostly with outliers up to 77
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='mg/dL']) #estimated gluc from a1c
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='milligram per deciliter']) #estimated gluc from a1c
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='nanogram per milliliter']) #just 1, with value 197
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='No matching concept'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='no value'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='Of Total H']) #from A1c - not an actual glucose measure
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='per gram of hemoglobin'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='percent'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='Percent'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='percent hemoglobin'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='percent hemoglobin A1c'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='Percentage unit'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='second'])
#summary(aou_meas_a1c$value[aou_meas_a1c$units=='Seconds per CO']) #range mostly <1, only 13 entries


aou_meas_a1c <- aou_meas_a1c[!grepl('nanogram per milliliter|gram per deciliter|Seconds per CO',aou_meas_a1c$units, ignore.case=T),]
dim(aou_meas_a1c)
length(unique(aou_meas_a1c$person_id))
summary(aou_meas_a1c$value)
aou_meas_a1c$value <- ifelse(is.na(aou_meas_a1c$units), aou_meas_a1c$value, ifelse(aou_meas_a1c$units=='mg/dL' | aou_meas_a1c$units=='milligram per deciliter', ((aou_meas_a1c$value+46.7)/28.7),aou_meas_a1c$value))
summary(aou_meas_a1c$value)

aou_meas_a1c <- aou_meas_a1c[aou_meas_a1c$value >=4 & aou_meas_a1c$value <= 20,]
dim(aou_meas_a1c)
length(unique(aou_meas_a1c$person_id))
aou_meas_a1c <- aou_meas_a1c %>% group_by(person_id) %>% mutate(a1c_max=max(value))
summary(aou_meas_a1c$a1c_max)

aou_meas_a1c <- aou_meas_a1c %>% group_by(person_id) %>% mutate(a1c_date_earliest=min(date))
aou_meas_a1c <- aou_meas_a1c %>% group_by(person_id) %>% mutate(a1c_date_latest=max(date))
aou_meas_a1c <- aou_meas_a1c %>% group_by(person_id) %>% mutate(a1c_count = n())
head(aou_meas_a1c)

aou_meas_a1c_final <- aou_meas_a1c[, c('person_id','a1c_max','a1c_date_earliest','a1c_date_latest','a1c_count')]
aou_meas_a1c_final <- aou_meas_a1c_final[!duplicated(aou_meas_a1c_final),]
aou_meas_a1c_final$dm_a1c <- ifelse(aou_meas_a1c_final$a1c_max >= 6.5,1,0)
table(aou_meas_a1c_final$dm_a1c, exclude=NULL)
dim(aou_meas_a1c_final)
length(unique(aou_meas_a1c_final$person_id))

#combine meas_X_final dfs
aou_meas_bmifbg <- merge(aou_meas_bmi_final, aou_meas_fbg_final, by='person_id', all.x=T, all.y=T, sort=T)
dim(aou_meas_bmifbg)
aou_meas_bmifbgrbg <- merge(aou_meas_bmifbg, aou_meas_rbg_final, by='person_id', all.x=T, all.y=T, sort=T)
dim(aou_meas_bmifbgrbg)
aou_meas_final <- merge(aou_meas_bmifbgrbg, aou_meas_a1c_final, by='person_id', all.x=T, all.y=T, sort=T)
dim(aou_meas_final)

#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
#save aou_meas_final
write.csv(aou_meas_final, file='AoU_RExSES_meas_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_meas_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_survey_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_survey_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)

#####END!#####









#####AoU_RExSES_Cleaning_2#####
library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES_2" for domain "condition" and was generated for All of Us Registered Tier Dataset v7
dataset_81533400_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_occurrence.condition_start_datetime,
        c_type.concept_name as condition_type_concept_name,
        c_source_concept.concept_code as source_concept_code 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            SELECT
                                CAST(cr.id as string) AS id       
                            FROM
                                `cb_criteria` cr       
                            WHERE
                                concept_id IN (
                                    4274025
                                )       
                                AND full_text LIKE '%_rank1]%'      
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )  
                AND (
                    c_occurrence.PERSON_ID IN (
                        SELECT
                            distinct person_id  
                        FROM
                            `cb_search_person` cb_search_person  
                        WHERE
                            cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_person` p 
                                WHERE
                                    age_at_consent BETWEEN 18 AND 120 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    gender_concept_id IN (45878463, 45880669, 2000000002) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                UNION
                                DISTINCT SELECT
                                    person_id 
                                FROM
                                    `person` p 
                                WHERE
                                    ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                            ) 
                            AND cb_search_person.person_id IN (
                                SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585945) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585946) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000006) 
                                            OR  concept_id IN (1585940) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (2000000007)
                                        )) criteria 
                                UNION
                                DISTINCT SELECT
                                    criteria.person_id 
                                FROM
                                    (SELECT
                                        DISTINCT person_id,
                                        entry_date,
                                        concept_id 
                                    FROM
                                        `cb_search_all_events` 
                                    WHERE
                                        (
                                            concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585376) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585377) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585378) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585379) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585380) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585381) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585382) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585383) 
                                            OR  concept_id IN (1585375) 
                                            AND is_standard = 0  
                                            AND  value_source_concept_id IN (1585384)
                                        )) criteria ) 
                            )
                        )
                ) c_occurrence 
            LEFT JOIN
                `concept` c_standard_concept 
                    ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
            LEFT JOIN
                `concept` c_type 
                    ON c_occurrence.condition_type_concept_id = c_type.concept_id 
            LEFT JOIN
                `concept` c_source_concept 
                    ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_81533400_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_81533400",
  "condition_81533400_*.csv")
message(str_glue('The data will be written to {condition_81533400_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_81533400_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_81533400_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_81533400_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), condition_type_concept_name = col_character(), source_concept_code = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_81533400_condition_df <- read_bq_export_from_workspace_bucket(condition_81533400_path)

dim(dataset_81533400_condition_df)

head(dataset_81533400_condition_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES_2" for domain "drug" and was generated for All of Us Registered Tier Dataset v7
dataset_81533400_drug_sql <- paste("
    SELECT
        d_exposure.person_id,
        d_exposure.drug_concept_id,
        d_standard_concept.concept_name as standard_concept_name,
        d_standard_concept.concept_code as standard_concept_code,
        d_exposure.refills 
    FROM
        ( SELECT
            * 
        FROM
            `drug_exposure` d_exposure 
        WHERE
            (
                drug_concept_id IN (
                    SELECT
                        DISTINCT ca.descendant_id 
                    FROM
                        `cb_criteria_ancestor` ca 
                    JOIN
                        (
                            SELECT
                                DISTINCT c.concept_id       
                            FROM
                                `cb_criteria` c       
                            JOIN
                                (
                                    SELECT
                                        CAST(cr.id as string) AS id             
                                    FROM
                                        `cb_criteria` cr             
                                    WHERE
                                        concept_id IN (
                                            1123618, 1123627, 1123896, 1502826, 1502905, 1503297, 1513849, 1513876, 1516766, 1516976, 1517998, 1531601, 1544838, 1550023, 1567198, 1586346, 1586369, 1588986, 1590165, 1596977, 19013926, 19013951, 21600002, 21600003, 21600046, 21600713, 21600714, 21600722, 21600728, 21600735, 21600742, 21600744, 21600749, 21600765, 21600779, 21600783, 21600788, 21600857, 21601136, 21601167, 21601194, 21601238, 21601605, 21601606, 21602796, 21603215, 21603216, 21603551, 21603932, 21603933, 21604253, 21604254, 21604303, 21605008, 21605009, 21605125, 21605126, 21605144, 2718458, 2718459, 2721331, 2721332, 2721333, 2721334, 35602717, 40217598, 40218573, 46221581
                                        )             
                                        AND full_text LIKE '%_rank1]%'       
                                ) a 
                                    ON (
                                        c.path LIKE CONCAT('%.',
                                    a.id,
                                    '.%') 
                                    OR c.path LIKE CONCAT('%.',
                                    a.id) 
                                    OR c.path LIKE CONCAT(a.id,
                                    '.%') 
                                    OR c.path = a.id) 
                                WHERE
                                    is_standard = 1 
                                    AND is_selectable = 1
                                ) b 
                                    ON (
                                        ca.ancestor_id = b.concept_id
                                    )
                            )
                        )  
                        AND (
                            d_exposure.PERSON_ID IN (
                                SELECT
                                    distinct person_id  
                            FROM
                                `cb_search_person` cb_search_person  
                            WHERE
                                cb_search_person.person_id IN (
                                    SELECT
                                        person_id 
                                    FROM
                                        `cb_search_person` p 
                                    WHERE
                                        age_at_consent BETWEEN 18 AND 120 
                                ) 
                                AND cb_search_person.person_id IN (
                                    SELECT
                                        person_id 
                                    FROM
                                        `person` p 
                                    WHERE
                                        gender_concept_id IN (45878463, 45880669, 2000000002) 
                                ) 
                                AND cb_search_person.person_id IN (
                                    SELECT
                                        person_id 
                                    FROM
                                        `person` p 
                                    WHERE
                                        race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                                    UNION
                                    DISTINCT SELECT
                                        person_id 
                                    FROM
                                        `person` p 
                                    WHERE
                                        ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                                ) 
                                AND cb_search_person.person_id IN (
                                    SELECT
                                        criteria.person_id 
                                    FROM
                                        (SELECT
                                            DISTINCT person_id,
                                            entry_date,
                                            concept_id 
                                        FROM
                                            `cb_search_all_events` 
                                        WHERE
                                            (
                                                concept_id IN (1585940) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585945) 
                                                OR  concept_id IN (1585940) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585946) 
                                                OR  concept_id IN (1585940) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (2000000006) 
                                                OR  concept_id IN (1585940) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (2000000007)
                                            )) criteria 
                                    UNION
                                    DISTINCT SELECT
                                        criteria.person_id 
                                    FROM
                                        (SELECT
                                            DISTINCT person_id,
                                            entry_date,
                                            concept_id 
                                        FROM
                                            `cb_search_all_events` 
                                        WHERE
                                            (
                                                concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585376) 
                                                OR  concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585377) 
                                                OR  concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585378) 
                                                OR  concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585379) 
                                                OR  concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585380) 
                                                OR  concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585381) 
                                                OR  concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585382) 
                                                OR  concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585383) 
                                                OR  concept_id IN (1585375) 
                                                AND is_standard = 0  
                                                AND  value_source_concept_id IN (1585384)
                                            )) criteria ) 
                                )
                            )
                    ) d_exposure 
                LEFT JOIN
                    `concept` d_standard_concept 
                        ON d_exposure.drug_concept_id = d_standard_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
drug_81533400_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "drug_81533400",
  "drug_81533400_*.csv")
message(str_glue('The data will be written to {drug_81533400_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_81533400_drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_81533400_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {drug_81533400_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_81533400_drug_df <- read_bq_export_from_workspace_bucket(drug_81533400_path)

dim(dataset_81533400_drug_df)

head(dataset_81533400_drug_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES_2" for domain "person" and was generated for All of Us Registered Tier Dataset v7
dataset_81533400_person_sql <- paste("
    SELECT
        person.person_id 
    FROM
        `person` person   
    WHERE
        person.PERSON_ID IN (
            SELECT
                distinct person_id  
            FROM
                `cb_search_person` cb_search_person  
            WHERE
                cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `cb_search_person` p 
                    WHERE
                        age_at_consent BETWEEN 18 AND 120 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        gender_concept_id IN (45878463, 45880669, 2000000002) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                    UNION
                    DISTINCT SELECT
                        person_id 
                    FROM
                        `person` p 
                    WHERE
                        ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                ) 
                AND cb_search_person.person_id IN (
                    SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585945) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585946) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000006) 
                                OR  concept_id IN (1585940) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (2000000007)
                            )) criteria 
                    UNION
                    DISTINCT SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id,
                            entry_date,
                            concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (
                                concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585376) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585377) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585378) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585379) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585380) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585381) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585382) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585383) 
                                OR  concept_id IN (1585375) 
                                AND is_standard = 0  
                                AND  value_source_concept_id IN (1585384)
                            )) criteria ) 
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_81533400_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_81533400",
  "person_81533400_*.csv")
message(str_glue('The data will be written to {person_81533400_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_81533400_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_81533400_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_81533400_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_81533400_person_df <- read_bq_export_from_workspace_bucket(person_81533400_path)

dim(dataset_81533400_person_df)

head(dataset_81533400_person_df, 5)

library(tidyverse)
library(bigrquery)

# This query represents dataset "AoU_RExSES_2" for domain "survey" and was generated for All of Us Registered Tier Dataset v7
dataset_81533400_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.question,
        answer.answer_concept_id,
        answer.answer  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (
                1384532, 1384543, 1384566, 1740588, 1740613, 1740668, 43528678, 43528816, 43528819, 43528820, 43528869, 43530331, 43530334, 43530335, 43530384, 43530488, 43530491, 43530492, 43530542, 836799, 836800, 836848
            )
        )  
        AND (
            answer.PERSON_ID IN (
                SELECT
                    distinct person_id  
                FROM
                    `cb_search_person` cb_search_person  
                WHERE
                    cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `cb_search_person` p 
                        WHERE
                            age_at_consent BETWEEN 18 AND 120 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            gender_concept_id IN (45878463, 45880669, 2000000002) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            race_concept_id IN (8527, 8516, 8515, 2000000008, 45882607, 2000000001) 
                        UNION
                        DISTINCT SELECT
                            person_id 
                        FROM
                            `person` p 
                        WHERE
                            ethnicity_concept_id IN (38003564, 38003563, 1586148) 
                    ) 
                    AND cb_search_person.person_id IN (
                        SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585945) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585946) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000006) 
                                    OR  concept_id IN (1585940) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (2000000007)
                                )) criteria 
                        UNION
                        DISTINCT SELECT
                            criteria.person_id 
                        FROM
                            (SELECT
                                DISTINCT person_id,
                                entry_date,
                                concept_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                (
                                    concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585376) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585377) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585378) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585379) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585380) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585381) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585382) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585383) 
                                    OR  concept_id IN (1585375) 
                                    AND is_standard = 0  
                                    AND  value_source_concept_id IN (1585384)
                                )) criteria ) 
                    )
                )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_81533400_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_81533400",
  "survey_81533400_*.csv")
message(str_glue('The data will be written to {survey_81533400_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_81533400_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_81533400_path,
  destination_format = "CSV")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_81533400_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_81533400_survey_df <- read_bq_export_from_workspace_bucket(survey_81533400_path)

dim(dataset_81533400_survey_df)

head(dataset_81533400_survey_df, 5)

#clean condition data set
aou_cond <- dataset_81533400_condition_df
aou_cond$in_condition_df <- 1
dim(aou_cond)
head(aou_cond)
length(unique(aou_cond$person_id))

aou_cond <- aou_cond[,c('person_id','standard_concept_name','condition_start_datetime','in_condition_df')]
aou_cond$cond_onset_date <- as.Date(aou_cond$condition_start_datetime,format='%Y-%m-%d %H:%M:%S')
aou_cond <- aou_cond %>% group_by(person_id, standard_concept_name) %>% mutate(cond_onset_earliest_date=min(cond_onset_date))
aou_cond$cond_onset_earliest_date <- as.Date(aou_cond$cond_onset_earliest_date,format='%Y-%m-%d')
head(aou_cond)

#aou_cond$t2d_onset_date <- ifelse(grepl('diabetes mellitus|type 2',aou_cond$cond_onset_earliest_data, ignore.case=T),NA)

#table(aou_cond$standard_concept_name)
aou_cond$t1d <- ifelse(grepl('type 1|latent autoimmune',aou_cond$standard_concept_name, ignore.case=T),1,0)
aou_cond$ob <- ifelse(grepl('obes',aou_cond$standard_concept_name, ignore.case=T),1,0)
aou_cond$predm <- ifelse(grepl('Prediabetes|pre-diabetes|ifg|impaired fasting glucose|igt|impaired glucose tol',aou_cond$standard_concept_name, ignore.case=T) & aou_cond$t1d==0,1,0)
aou_cond$t2d <- ifelse(grepl('diabetes mellitus|type 2',aou_cond$standard_concept_name, ignore.case=T) & aou_cond$t1d==0,1,0)
addmargins(table(aou_cond$t1d, aou_cond$t2d, exclude=NULL))
addmargins(table(aou_cond$predm, aou_cond$t2d, exclude=NULL))
addmargins(table(aou_cond$ob, aou_cond$t2d, exclude=NULL))

aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(sum_t1d = sum(t1d))
aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(sum_ob = sum(ob))
aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(sum_predm = sum(predm))
aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(sum_t2d = sum(t2d))
aou_cond[1:10,]

#aou_con %>% group_by(person_id) %>% mutate(t2d_onset_date=if(sum_t2d >0 cond_onset_earliest_date else 'NA')
#aou_cond$t2d_onset_date <- ifelse(aou_cond_short$sum_t2d >0, grepl('diabetes mellitus|type 2',aou_cond$cond_onset_earliest_data, ignore.case=T),NA)
aou_cond$t2d_onset_date <- ifelse(aou_cond$t2d==1, as.Date(aou_cond$cond_onset_earliest_date,format='%Y-%m-%d'), NA)
aou_cond <- aou_cond %>% group_by(person_id) %>% mutate(t2d_onset_date = replace_na(t2d_onset_date, min(t2d_onset_date, na.rm=T)))
#fill(aou_cond, t2d_onset_date)
aou_cond <- aou_cond %>% group_by(person_id) %>% fill(t2d_onset_date, .direction='downup')
aou_cond$t2d_onset_date <- as.Date(aou_cond$t2d_onset_date,format='%Y-%m-%d')
aou_cond[1:10,c(1,2,6:15)]
#aou_cond %>% group_by(person_id) %>% mutate(t2d_onset_date=if(!is.na(t2d_onset_date) t2d_onset_date else NA)

aou_cond$t2d_onset_date <- as.Date(aou_cond$t2d_onset_date,format='%Y-%m-%d')
aou_cond[1:10,c(1,2,6:15)]
class(aou_cond$t2d_onset_date)

dim(aou_cond)
aou_cond_short <- aou_cond[!duplicated(aou_cond$person_id),c('person_id','in_condition_df','t1d','ob','predm','t2d',
                                                             'sum_t1d','sum_ob','sum_predm','sum_t2d','t2d_onset_date')]
dim(aou_cond_short)
head(aou_cond_short)

aou_cond_short$t1d <- ifelse(aou_cond_short$sum_t1d >0,1,0)
aou_cond_short$ob <- ifelse(aou_cond_short$sum_ob >0,1,0)
aou_cond_short$predm <- ifelse(aou_cond_short$sum_predm >0,1,0)
aou_cond_short$t2d <- ifelse(aou_cond_short$sum_t2d >0 & aou_cond_short$t1d==0,1,0)
addmargins(table(aou_cond_short$t1d, aou_cond_short$t2d, exclude=NULL))
addmargins(table(aou_cond_short$predm, aou_cond_short$t2d, exclude=NULL))
addmargins(table(aou_cond_short$ob, aou_cond_short$t2d, exclude=NULL))

aou_cond2_final <- aou_cond_short[,c('person_id','in_condition_df','t1d','ob','predm','t2d','t2d_onset_date')]
dim(aou_cond2_final)
head(aou_cond2_final)

#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
#save aou_cond2_final
write.csv(aou_cond2_final, file='AoU_RExSES_cond2_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_cond2_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_survey_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_survey_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)

#clean medication data set
aou_med <- dataset_81533400_drug_df
aou_med$in_med_df <- 1
dim(aou_med)
head(aou_med)
length(unique(aou_med$person_id))

#table(aou_med$standard_concept_name)
aou_med$insulin <- ifelse(grepl('insulin',aou_med$standard_concept_name, ignore.case=T),1,0)
aou_med$met <- ifelse(grepl('metformin',aou_med$standard_concept_name, ignore.case=T),1,0)
aou_med$su <- ifelse(grepl('glipizide|glyburide|glimepiride',aou_med$standard_concept_name, ignore.case=T),1,0)
aou_med$dpp4 <- ifelse(grepl('gliptin',aou_med$standard_concept_name, ignore.case=T),1,0)
aou_med$glp <- ifelse(grepl('aglutide|iglutide|exenatide',aou_med$standard_concept_name, ignore.case=T),1,0)
aou_med$sglt2 <- ifelse(grepl('gliflozin',aou_med$standard_concept_name, ignore.case=T),1,0)
aou_med$tzd <- ifelse(grepl('glitazone',aou_med$standard_concept_name, ignore.case=T),1,0)
aou_med$meglit <- ifelse(grepl('repaglinide|nateglinide',aou_med$standard_concept_name, ignore.case=T),1,0)
addmargins(table(aou_med$met, aou_med$insulin, exclude=NULL))
addmargins(table(aou_med$met, aou_med$su, exclude=NULL))
addmargins(table(aou_med$met, aou_med$glp, exclude=NULL))


aou_med <- aou_med %>% group_by(person_id) %>% mutate(sum_insulin = sum(insulin))
aou_med <- aou_med %>% group_by(person_id) %>% mutate(sum_met = sum(met))
aou_med <- aou_med %>% group_by(person_id) %>% mutate(sum_su = sum(su))
aou_med <- aou_med %>% group_by(person_id) %>% mutate(sum_dpp4 = sum(dpp4))
aou_med <- aou_med %>% group_by(person_id) %>% mutate(sum_glp = sum(glp))
aou_med <- aou_med %>% group_by(person_id) %>% mutate(sum_sglt2 = sum(sglt2))
aou_med <- aou_med %>% group_by(person_id) %>% mutate(sum_tzd = sum(tzd))
aou_med <- aou_med %>% group_by(person_id) %>% mutate(sum_meglit = sum(meglit))
aou_med$oad_ever <- ifelse(aou_med$sum_met>0 | aou_med$sum_su>0 | aou_med$sum_dpp4>0 | aou_med$sum_glp>0 | 
                             aou_med$sum_sglt2>0 | aou_med$sum_tzd>0 | aou_med$sum_meglit>0, 1, 0)
summary(aou_med$oad_ever)
aou_med$insulin_ever <- ifelse(aou_med$sum_insulin>0, 1, 0)
summary(aou_med$insulin_ever)
aou_med[1:10,]

addmargins(table(aou_med$oad_ever, aou_med$met, exclude=NULL))
addmargins(table(aou_med$oad_ever, aou_med$su, exclude=NULL))
addmargins(table(aou_med$oad_ever, aou_med$dpp4, exclude=NULL))
addmargins(table(aou_med$oad_ever, aou_med$glp, exclude=NULL))
addmargins(table(aou_med$oad_ever, aou_med$sglt2, exclude=NULL))
addmargins(table(aou_med$oad_ever, aou_med$tzd, exclude=NULL))
addmargins(table(aou_med$oad_ever, aou_med$meglit, exclude=NULL))


dim(aou_med)
aou_med_short <- aou_med[!duplicated(aou_med$person_id),c('person_id','in_med_df','sum_insulin','sum_met','sum_su','sum_dpp4',
                                                          'sum_glp','sum_sglt2','sum_tzd','sum_meglit','oad_ever','insulin_ever')]
dim(aou_med_short)
head(aou_med_short)
aou_med_final <- aou_med_short#[,c('person_id','t1d','ob','predm','t2d','t2d_onset_date')]


#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
#save aou_med_final
write.csv(aou_med_final, file='AoU_RExSES_med_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_med_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_survey_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_survey_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)

#clean person data set
aou_person <- dataset_81533400_person_df
aou_person$in_person_df <- 1
dim(aou_person)
head(aou_person)
length(unique(aou_person$person_id))

#clean survey data set
aou_survey <- dataset_81533400_survey_df
aou_survey$in_surv_df <- 1
dim(aou_survey)
head(aou_survey)
length(unique(aou_survey$person_id))
table(aou_survey$question)


dim(aou_survey)
#aou_cond$t1d <- ifelse(grepl('type 1|latent autoimmune',aou_cond$standard_concept_name, ignore.case=T),1,0)
aou_survey$self <- ifelse(grepl(' self',aou_survey$answer, ignore.case=T),1,0)
table(aou_survey$self, exclude=NULL)
aou_survey$t2d_selfrep <- ifelse(grepl('Including yourself, who in your family has had type 2 diabetes?',aou_survey$question, ignore.case=T) 
                                 & aou_survey$self==1,1,0)
aou_survey$t1d_selfrep <- ifelse(grepl('Including yourself, who in your family has had type 1 diabetes?',aou_survey$question, ignore.case=T) 
                                 & aou_survey$self==1,1,0)
aou_survey$otherdm_selfrep <- ifelse(grepl('Including yourself, who in your family has had other/unknown diabetes?',aou_survey$question, ignore.case=T) 
                                     & aou_survey$self==1,1,0)
aou_survey$ob_selfrep <- ifelse(grepl('Including yourself, who in your family has had obesity?',aou_survey$question, ignore.case=T) 
                                & aou_survey$self==1,1,0)
aou_survey$predm_selfrep <- ifelse(grepl('Including yourself, who in your family has had prediabetes?',aou_survey$question, ignore.case=T) 
                                   & aou_survey$self==1,1,0)
table(aou_survey$t2d_selfrep, exclude=NULL)
table(aou_survey$t1d_selfrep, exclude=NULL)
table(aou_survey$otherdm_selfrep, exclude=NULL)
table(aou_survey$ob_selfrep, exclude=NULL)

head(aou_survey)

aou_survey$t1d_age_selfrep <- ifelse(grepl('About how old were you when you were first told you had type 1 diabetes?',aou_survey$question, ignore.case=T),aou_survey$answer,NA)
aou_survey$t2d_age_selfrep <- ifelse(grepl('About how old were you when you were first told you had type 2 diabetes?',aou_survey$question, ignore.case=T),aou_survey$answer,NA)
aou_survey$otherdm_age_selfrep <- ifelse(grepl('About how old were you when you were first told you had other/unknown diabetes?',aou_survey$question, ignore.case=T),aou_survey$answer,NA)
aou_survey$ob_age_selfrep <- ifelse(grepl('About how old were you when you were first told you had obesity?',aou_survey$question, ignore.case=T),aou_survey$answer,NA)
aou_survey$predm_age_selfrep <- ifelse(grepl('About how old were you when you were first told you had prediabetes?',aou_survey$question, ignore.case=T),aou_survey$answer,NA)
table(aou_survey$t1d_age_selfrep)
table(aou_survey$t2d_age_selfrep)
table(aou_survey$otherdm_age_selfrep)
table(aou_survey$ob_age_selfrep)
table(aou_survey$predm_age_selfrep)


aou_survey <- aou_survey %>% group_by(person_id) %>% mutate(sum_t2d_selfrep = sum(t2d_selfrep))
aou_survey <- aou_survey %>% group_by(person_id) %>% mutate(sum_t1d_selfrep = sum(t1d_selfrep))
aou_survey <- aou_survey %>% group_by(person_id) %>% mutate(sum_otherdm_selfrep = sum(otherdm_selfrep))
aou_survey <- aou_survey %>% group_by(person_id) %>% mutate(sum_ob_selfrep = sum(ob_selfrep))
aou_survey <- aou_survey %>% group_by(person_id) %>% mutate(sum_predm_selfrep = sum(predm_selfrep))

aou_survey$t1d_selfrep <- ifelse(aou_survey$sum_t1d_selfrep >0,1,0)
aou_survey$ob_selfrep <- ifelse(aou_survey$sum_ob_selfrep >0,1,0)
aou_survey$otherdm_selfrep <- ifelse(aou_survey$sum_otherdm_selfrep >0,1,0)
aou_survey$t2d_selfrep <- ifelse(aou_survey$sum_t2d_selfrep >0,1,0)
aou_survey$predm_selfrep <- ifelse(aou_survey$sum_predm_selfrep >0,1,0)
addmargins(table(aou_survey$t1d_selfrep, aou_survey$t2d_selfrep, exclude=NULL))
addmargins(table(aou_survey$otherdm_selfrep, aou_survey$t2d_selfrep, exclude=NULL))
addmargins(table(aou_survey$ob_selfrep, aou_survey$t2d_selfrep, exclude=NULL))
addmargins(table(aou_survey$predm_selfrep, aou_survey$t2d_selfrep, exclude=NULL))

aou_survey <- aou_survey %>% group_by(person_id) %>% fill(t1d_age_selfrep, .direction='downup')
aou_survey <- aou_survey %>% group_by(person_id) %>% fill(t2d_age_selfrep, .direction='downup')
aou_survey <- aou_survey %>% group_by(person_id) %>% fill(otherdm_age_selfrep, .direction='downup')
aou_survey <- aou_survey %>% group_by(person_id) %>% fill(ob_age_selfrep, .direction='downup')
aou_survey <- aou_survey %>% group_by(person_id) %>% fill(predm_age_selfrep, .direction='downup')

dim(aou_survey)
aou_survey_short <- aou_survey[!duplicated(aou_survey$person_id),c('person_id','in_surv_df','t1d_selfrep','ob_selfrep',
                                                                   'otherdm_selfrep','t2d_selfrep','predm_selfrep',
                                                                   't1d_age_selfrep','t2d_age_selfrep','otherdm_age_selfrep',
                                                                   'ob_age_selfrep','predm_age_selfrep')]
dim(aou_survey_short)
head(aou_survey_short)


#HOW TO SAVE FILES - first to csv on memory (assume this will be lost), then to bucket
#save aou_cond_final
write.csv(aou_survey_short, file='AoU_RExSES_surv2_final.csv', row.names=F)

destination_filename <- 'AoU_RExSES_surv2_final.csv'

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
#system(paste0("gsutil cp ./AoU_RExSES_survey_final.csv", " ", AoU_RExSES_filepath,'/AoU_RExSES_survey_final.csv'), intern=T)

# Check if file is in the bucket
#system(paste0("gsutil ls ", AoU_RExSES_filepath), intern=T)

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_RExSES/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/AoU_RExSES/*.csv"), intern=T)

#gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/bq_exports/sara@researchallofus.org/20240429/condition_81533400/condition_81533400_*.csv
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), condition_type_concept_name = col_character(), source_concept_code = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_81533400_condition_df <- read_bq_export_from_workspace_bucket(condition_81533400_path)

dim(dataset_81533400_condition_df)

head(dataset_81533400_condition_df, 5)


#combine data sets and save to bucket
aou_cond2_final <- read.csv(file='gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/AoU_RExSES/AoU_RExSES_cond2_final.csv')
perscond <- merge(aou_person,aou_cond2_final, by='SEQN', all.x=T, all.y=T, sort=T)
perscondmed <- merge(perscond,aou_med_final, by='SEQN', all.x=T, all.y=T, sort=T)
aou_clean2_final <- merge(d,aou_survey_short, by='SEQN', all.x=T, all.y=T, sort=T)

dim(aou_clean2_final)
head(aou_clean2_final)

#####END2!#####