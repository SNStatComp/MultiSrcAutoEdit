
# Load libraries ----
library(stringr)
library(dplyr)
library(glue)
library(validatetools)
library(foreach)
library(doParallel)
library(purrr)
library(logger)
library(tidyr)
library(deductive)
library(errorlocate)
library(lintools)
library(simputation)
library(rspa)


PID <- tolower(substring(Sys.getenv("RSTUDIO_USER_IDENTITY")[[1]], 1, 4))

InputFolder <- "//Cbsp.nl/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Input"
OutputFolder <- "//Cbsp.nl/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Output"

if (.Platform$OS.type == "unix") {
  InputFolder <- glue::glue("/home/{PID}@cbsp.nl/shares/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Input")
  OutputFolder <- glue::glue("/home/{PID}@cbsp.nl/shares/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Output")
}

# Source files ----
InputFolder <- file.path(InputFolder, glue::glue(PID))
OutputFolder <- file.path(OutputFolder, glue::glue(PID))


# load constants ----
source(stringr::str_glue('./input_files/constants.R'))


########
# further settings can be adjusted here ----

Stap3Doen <- TRUE        # TRUE if PS should also be edited internally, FALSE otherwise
use_calculator <- FALSE   # TRUE if the code is to be run on calculator environment, FALSE otherwise
use_eo <- TRUE           # TRUE if special edit operations should be used, FALSE otherwise
use_linear_rules <- TRUE # TRUE if linearized PS edit rules should be used, FALSE otherwise
beidselection <- FALSE   # TRUE if error localization should be run only for records in BEIDSELECTION, FALSE otherwise
save_rdata <- FALSE      # TRUE if detailed output should be stored as an rdata file, FALSE otherwise
# (choose save_rdata = TRUE only if the number of records is small, because rdata file can become very large)

prefix_input <- PREFIX
prefix_output <- PREFIX

########


FILE_NAME_INPUT_DATA <- file.path(OutputFolder, stringr::str_glue("{prefix_input}data_{YEAR}_sel.rds"))

if ('neighbour' %in% set_weights_dynamic) {
  # dynamic weights
  FILE_NAME_WEIGHTS <- file.path(OutputFolder, stringr::str_glue("{prefix_input}weights_interstat_dynamic_{YEAR}.csv"))
} else if ('quality' %in% set_weights_dynamic) {
  # dynamic weights based only on input quality
  FILE_NAME_WEIGHTS <- file.path(OutputFolder, stringr::str_glue("{prefix_input}weights_interstat_invulkwaliteit_{YEAR}.csv"))
  FILE_NAME_BEIDSELECTION <- file.path(OutputFolder, stringr::str_glue("{prefix_input}beids_invulkwaliteit_crit_{YEAR}.csv"))
} else {
  # initial weights with modifications (NACE, size class, etc.)
  FILE_NAME_WEIGHTS <- file.path(OutputFolder, stringr::str_glue("{prefix_input}weights_interstat_ns_{YEAR}.csv"))
}

if (use_interstat_rules_soft) {
  FILE_NAME_WEIGHTS_SOFT <- file.path(OutputFolder, stringr::str_glue("{prefix_input}weights_interstat_soft_rules_{YEAR}.csv"))
}


# Load special edit operations for error_locate_eo
source(file.path("input_files",
                 stringr::str_glue("speciale_acties_{YEAR}.R")))
speciale_acties <- SPECIALE_ACTIES
weight_eo <- WEIGHT_EO_SPECIALE_ACTIES
vars_speciale_acties <- VARS_EDIT_OPERATIONS


###

# Logging ----
logger <- logger::layout_glue_generator(format = "{time} {level}: {fn} -> {msg}")
logger::log_layout(logger)

project_dir <- getwd()
output_log <- file.path(OutputFolder, stringr::str_glue("{prefix_output}log_output.txt"))
logger::log_appender(appender_file(output_log))
logger::log_layout(logger)
