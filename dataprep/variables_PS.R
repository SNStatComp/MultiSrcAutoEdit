
rm(list = ls())
gc()

library(stringr)
library(dplyr)
library(glue)

# load constants ----
source(stringr::str_glue('./input_files/constants.R'))

PID <- Sys.getenv("USERNAME")
InputFolder <- "//Cbsp.nl/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Input"
InputFolder <- file.path(InputFolder, glue::glue(PID))

DataFolder <- "//Cbsp.nl/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Data"

input_csv <- file.path(DataFolder, 'tbl_VragenlijstVariabele.csv')
output_csv <- file.path(InputFolder, stringr::str_glue('PS_vragenlijstvariabelen_{YEAR}.csv'))


########

## Read overview of PS variables per questionnaire (multiple years)
input <- read.csv2(file = input_csv)


## Select relevant information
output <- input %>%
  dplyr::filter(VerslagJaar == YEAR) %>%
  dplyr::select(c(VragenlijstID, VariabeleNaam))


## Write results to csv
write.csv2(output, file = output_csv, row.names = FALSE)

