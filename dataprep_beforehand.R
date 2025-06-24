
### Script to compute unit-specific reliability weights and input for imputation ###

rm(list = ls())
gc()

# Load libraries ----

library(validate)
library(stringr)
library(dplyr)
library(glue)
library(validatetools)
library(purrr)
library(logger)
library(tidyr)
library(accumulate)
library(foreach)
library(doParallel)
library(dcmodify)
#library(lumberjack)

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

FILE_NAME_INPUT_DATA <- file.path(InputFolder, stringr::str_glue("data_{YEAR}_sel.rds"))
FILE_NAME_INPUT_DATA_Tm1 <- file.path(InputFolder, stringr::str_glue("data_{YEAR-1}_sel.rds"))

prefix_output <- PREFIX


###
# Functions

read_file <- function(file_name) {
  if (grepl('[.]rds$', file_name)) {
    input_data <- readRDS(file_name)
  } else if (grepl('[.]csv$', file_name)) {
    input_data <- read.csv2(file_name, stringsAsFactors = FALSE)
  } else {
    stop('Unknown file format in read_file!')
  }
  return(input_data)
}


###

# Read data
data <- read_file(file_name = FILE_NAME_INPUT_DATA)
data_Tm1 <- read_file(file_name = FILE_NAME_INPUT_DATA_Tm1) %>%
# tijdelijke oplossing - deze variabelen moeten nog worden toegevoegd voor 2021
  dplyr::mutate(WIA.Investeringen_materiele_vaste_activa_INIVA_WIA = 0,
                WIA.Investeringen_immateriele_vaste_activa_INIVA_WIA = 0)


## Read PS rules (original format)
# rules used during automatic editing
rules_PS_auto_bron <- read_file(file_name = file.path("edited_input",
                                                      stringr::str_glue("edited_rules_ps_{YEAR}.csv")))
# other rules (including soft edit rules)
rules_PS_other_bron <- read_file(file_name = file.path("edited_input",
                                                      stringr::str_glue("edited_rules_ps_{YEAR}_soft.csv")))

# Read rules for common variables in PS
rules_PS_conf_bron <- read_file(file_name = file.path("edited_input",
                                                      stringr::str_glue("rules_conf_PS_{YEAR}.csv"))) %>%
  dplyr::mutate(VL_ID = 0) %>%
  dplyr::select(name, rule, VL_ID)

# Read rules for common variables across sources
rules_interstat_bron <- read_file(file_name = file.path("edited_input",
                                                        stringr::str_glue("interstat_rules_{YEAR}.csv")))

# Read initial weights
weights_PS_bron <- read_file(file_name = "input_files/weights_PS.csv") %>%
  replace(is.na(.), 1)

weights_interstat_bron <- read_file(file_name = file.path("input_files",
                                                          stringr::str_glue("weights_interstat_{YEAR}.csv")))

# Read overview of PS variables per questionnaire
variables_PS_bron <- read_file(file_name = file.path(InputFolder,
                                                     stringr::str_glue('PS_vragenlijstvariabelen_{YEAR}.csv')))


###

# Make list of edit rules for each VL_ID

vl_ids <- sort(unique(data$PS.VragenlijstID))

rules_PS_auto_list <- lapply(vl_ids, function(vl) {
  rules_PS_auto_bron <- rules_PS_auto_bron %>%
    dplyr::filter(VL_ID == vl | VL_ID == 9999) %>%
    dplyr::select(name, rule, VL_ID) %>%
    dplyr::mutate(rule = stringr::str_replace_all(rule, "([A-Z]+[0-9]{6})", "PS.\\1"))
    # PREFIX VOOR PS.VARIABELEN IN REGELS INDIEN AL NODIG

  rules_PS_auto <- validate::validator(.data = rules_PS_auto_bron)
  return(rules_PS_auto)
})
names(rules_PS_auto_list) <- as.character(vl_ids)

rules_PS_other_list <- lapply(vl_ids, function(vl) {
  rules_PS_other_bron <- rules_PS_other_bron %>%
    dplyr::filter(VL_ID == vl | VL_ID == 9999) %>%
    dplyr::select(name, rule, VL_ID) %>%
    dplyr::mutate(rule = stringr::str_replace_all(rule, "([A-Z]+[0-9]{6})", "PS.\\1"))
  # PREFIX VOOR PS.VARIABELEN IN REGELS INDIEN AL NODIG

  rules_PS_other <- validate::validator(.data = rules_PS_other_bron)
  return(rules_PS_other)
})
names(rules_PS_other_list) <- as.character(vl_ids)

rules_PS_conf <- validate::validator(.data = rules_PS_conf_bron)

rules_interstat <- validate::validator(.data = rules_interstat_bron)

# names of all common variables:
all_vars <- validate::variables(rules_interstat)
common_vars <- all_vars[grep('^Echt[.]', all_vars)]
common_vars <- gsub(pattern = '^Echt[.]', replacement = '', common_vars)


###

if (use_deductive_correction) {
  # Apply correction rules

  CC_rules <- modifier(.file = file.path("edited_input",
                                         stringr::str_glue("CC_interstat_rules_{YEAR}.txt")))
  data <- modify(data, CC_rules)
}



###

# Confront PS data with rules (separately for each VL_ID)

cf_PS_auto_list <- lapply(vl_ids, function(vl) {
  cf <- validate::confront(subset(data, PS.VragenlijstID == vl),
                           rules_PS_auto_list[[as.character(vl)]])
  confrontatie_ruw <- cbind(subset(data, PS.VragenlijstID == vl) %>% dplyr::select(BE_ID),
                            validate::values(cf))
  return(confrontatie_ruw)
})
names(cf_PS_auto_list) <- as.character(vl_ids)

cf_PS_other_list <- lapply(vl_ids, function(vl) {
  cf <- validate::confront(subset(data, PS.VragenlijstID == vl),
                           rules_PS_other_list[[as.character(vl)]])
  confrontatie_ruw <- cbind(subset(data, PS.VragenlijstID == vl) %>% dplyr::select(BE_ID),
                            validate::values(cf))
  return(confrontatie_ruw)
})
names(cf_PS_other_list) <- as.character(vl_ids)


## Count number of violated edit rules and total number of edit rules per record
## (based on questionnaire ID)

PS_edit_violations_auto <- do.call(rbind,
                                   lapply(cf_PS_auto_list, function(L) {
                                     L %>%
                                       dplyr::rowwise() %>%
                                       dplyr::mutate(PS.number_edits_auto = length(dplyr::c_across(-BE_ID)),
                                                     PS.number_edits_auto_failed = sum(!dplyr::c_across(-BE_ID), na.rm = TRUE)) %>%
                                       dplyr::ungroup() %>%
                                       dplyr::select(c(BE_ID, PS.number_edits_auto, PS.number_edits_auto_failed))
                                   })
)

PS_edit_violations_other <- do.call(rbind,
                                    lapply(cf_PS_other_list, function(L) {
                                      L %>%
                                        dplyr::rowwise() %>%
                                        dplyr::mutate(PS.number_edits_other = length(dplyr::c_across(-BE_ID)),
                                                      PS.number_edits_other_failed = sum(!dplyr::c_across(-BE_ID), na.rm = TRUE)) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::select(c(BE_ID, PS.number_edits_other, PS.number_edits_other_failed))
                                      })
)

# join counts back to data
data <- data %>%
  dplyr::left_join(PS_edit_violations_auto,
                   by = 'BE_ID') %>%
  dplyr::left_join(PS_edit_violations_other,
                   by = 'BE_ID')

# data %>%
#   mutate(PS.x = PS.number_edits_auto_failed / PS.number_edits_auto) %>%
#   group_by(GkSbsGecoordineerd1D) %>%
#   summarise(min = min(PS.x),
#             q025 = quantile(PS.x, 0.25),
#             q050 = quantile(PS.x, 0.50),
#             q075 = quantile(PS.x, 0.75),
#             q090 = quantile(PS.x, 0.90),
#             q095 = quantile(PS.x, 0.95),
#             max = max(PS.x),
#             mean = mean(PS.x))
#
# data %>%
#   mutate(PS.x = PS.number_edits_other_failed / PS.number_edits_other) %>%
#   group_by(GkSbsGecoordineerd1D) %>%
#   summarise(min = min(PS.x),
#             q025 = quantile(PS.x, 0.25),
#             q050 = quantile(PS.x, 0.50),
#             q075 = quantile(PS.x, 0.75),
#             q090 = quantile(PS.x, 0.90),
#             q095 = quantile(PS.x, 0.95),
#             max = max(PS.x),
#             mean = mean(PS.x))


## Count number of variables and number of empty PS variables per record
## (based on questionnaire ID)

PS_empty <- NULL

for (vl in vl_ids) {
  vars <- variables_PS_bron %>%
    dplyr::filter(VragenlijstID == vl) %>%
    dplyr::pull(VariabeleNaam)

  tmp <- data %>%
    dplyr::filter(PS.VragenlijstID == vl) %>%
    dplyr::select(c(BE_ID, dplyr::any_of(stringr::str_glue('PS.{vars}')))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(PS.number_vars = length(dplyr::c_across(-BE_ID)),
                  PS.number_vars_empty = sum(is.na(dplyr::c_across(-BE_ID)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(c(BE_ID, PS.number_vars, PS.number_vars_empty))

  PS_empty <- rbind(PS_empty, tmp)
}

# join counts back to data
data <- data %>%
  dplyr::left_join(PS_empty,
                   by = 'BE_ID')

# data %>%
#   mutate(PS.x = PS.number_vars_empty / PS.number_vars) %>%
#   group_by(GkSbsGecoordineerd1D) %>%
#   summarise(min = min(PS.x),
#             q025 = quantile(PS.x, 0.25),
#             q050 = quantile(PS.x, 0.50),
#             q075 = quantile(PS.x, 0.75),
#             q090 = quantile(PS.x, 0.90),
#             q095 = quantile(PS.x, 0.95),
#             max = max(PS.x),
#             mean = mean(PS.x))



###

# Prepare initial weights

weights_PS <- setNames(weights_PS_bron$weight, paste0("PS.", weights_PS_bron$variable))

weights_interstat_bron$weight <- weights_interstat_bron$weight * 1
weights_interstat <- setNames(weights_interstat_bron$weight, weights_interstat_bron$variable)


# Check if initial weights are available for all common variables that occur in rules
vars_in_rules_interstat <- unique(validate::variables(rules_interstat))
setdiff(vars_in_rules_interstat, names(weights_interstat)) # should have length 0
setdiff(names(weights_interstat), vars_in_rules_interstat) # should have length 0


###

# Make data.frame of weights per record (taking initial weights as starting point)

data_weights_init <- matrix(weights_interstat,
                            nrow = nrow(data), ncol = length(weights_interstat),
                            byrow = TRUE)
colnames(data_weights_init) <- names(weights_interstat)


# names of sources:
bronvariabelen <- data %>%
  dplyr::select(dplyr::ends_with('exist')) %>%
  colnames()
names_sources <- stringr::str_replace(bronvariabelen,
                                      pattern = '.exist',
                                      replacement = '')


###############
## Prepare adjusted weights based on NACE and size class

data_weights <- cbind(
  data %>% dplyr::select(BE_ID, SbiGecoordineerd2D, GkSbsGecoordineerd1D, GkSbsGecoordineerdSML,
                         PS.Netto_Omzet_minus_accijnzen.waarde = PS.Netto_Omzet_minus_accijnzen,
                         DRT.Netto_Omzet_minus_accijnzen.waarde = DRT.Netto_Omzet_minus_accijnzen,
                         WIA.Netto_Omzet_minus_accijnzen.waarde = WIA.Netto_Omzet_minus_accijnzen,
                         PS.Lonen.waarde = PS.Lonen,
                         SWL.Lonen.waarde = SWL.Lonen,
                         WIA.Lonen.waarde = WIA.Lonen),
  data_weights_init)

Weight_rules <- modifier(.file = file.path("edited_input",
                                       stringr::str_glue("Weight_rules_{YEAR}.txt")))
data_weights_ns <- modify(data_weights, Weight_rules)


###############
## Prepare adjusted weights based on input data quality
data_weights <- data_weights_ns %>%
  dplyr::select(-c(SbiGecoordineerd2D, GkSbsGecoordineerd1D, GkSbsGecoordineerdSML,
                   PS.Netto_Omzet_minus_accijnzen.waarde,
                   DRT.Netto_Omzet_minus_accijnzen.waarde,
                   WIA.Netto_Omzet_minus_accijnzen.waarde,
                   PS.Lonen.waarde,
                   SWL.Lonen.waarde,
                   WIA.Lonen.waarde))

if ('quality' %in% set_weights_dynamic) {
  FILE_NAME_CRITERIA <- file.path('input_files',
                                  stringr::str_glue('inputquality_criteria_{YEAR}.csv'))
  FILE_NAME_TOEPASBAARHEID <- file.path('input_files',
                                        stringr::str_glue('inputquality_relevance_{YEAR}.csv'))

  source('dataprep_beforehand_dataquality.R')
}


###############
## Determine which records can be used as reference data for
## stratum means and/or dynamic reliability weights of common variables
## because they do not violate any consistency rules between common variables
source('dataprep_beforehand_find_referencedata.R')


###############
if (use_interstat_rules_soft) {
  ## Determine reliability weights for soft interstat edit rules
  source('dataprep_beforehand_weights_soft_rules.R')
}


###############
## Compute stratum means of common variables for imputation in step 2
source('dataprep_beforehand_stratum_values.R')


###############
## Determine dynamic reliability weights based on reference data
if ('neighbour' %in% set_weights_dynamic) {
  source('dataprep_beforehand_dynamic_weights.R')
}


###############
## Collect and compute input ((T-1) values and stratum means) for imputation of PS variables in step 3
source('dataprep_beforehand_imputation_PS.R')



## Write reliability weights to files

write.csv2(data_weights_init,
           file = file.path(OutputFolder, stringr::str_glue("{prefix_output}weights_interstat_init_{YEAR}.csv")),
           row.names = FALSE)

write.csv2(data_weights_ns,
           file = file.path(OutputFolder, stringr::str_glue("{prefix_output}weights_interstat_ns_{YEAR}.csv")),
           row.names = FALSE)

if ('quality' %in% set_weights_dynamic) {
  write.csv2(data_weights,
             file = file.path(OutputFolder, stringr::str_glue("{prefix_output}weights_interstat_invulkwaliteit_{YEAR}.csv")),
             row.names = FALSE)
  write.csv2(beids_crit_subset,
             file = file.path(OutputFolder, stringr::str_glue("{prefix_output}beids_invulkwaliteit_crit_{YEAR}.csv")),
             row.names = FALSE)
}

if ('neighbour' %in% set_weights_dynamic) {
  write.csv2(data_weights_dyn,
             file = file.path(OutputFolder, stringr::str_glue("{prefix_output}weights_interstat_dynamic_{YEAR}.csv")),
             row.names = FALSE)
}

if (use_interstat_rules_soft) {
  write.csv2(data_weights_soft,
             file = file.path(OutputFolder, stringr::str_glue("{prefix_output}weights_interstat_soft_rules_{YEAR}.csv")),
             row.names = FALSE)
}


## Save current version of data
saveRDS(data, file.path(OutputFolder,
                        stringr::str_glue("{prefix_output}data_{YEAR}_sel.rds")))
