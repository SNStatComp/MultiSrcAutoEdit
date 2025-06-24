
rm(list = ls())
gc()

library(knitr)
library(rmarkdown)
library(stringr)


# set working directory to location of current source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# list of variants for which the evaluation script has to be run
variants <- list(
  c(prefix_input = 'sc1_',
    prefix_output = 'sc1_',
    file_constants = file.path('..', 'scenarios', 'sc1_constants.R')),
  c(prefix_input = 'sc2_',
    prefix_output = 'sc2_',
    file_constants = file.path('..', 'scenarios', 'sc2_constants.R')),
  c(prefix_input = 'sc3_',
    prefix_output = 'sc3_',
    file_constants = file.path('..', 'scenarios', 'sc3_constants.R')),
  c(prefix_input = 'sc4_',
    prefix_output = 'sc4_',
    file_constants = file.path('..', 'scenarios', 'sc4_constants.R')),
  c(prefix_input = 'sc5_',
    prefix_output = 'sc5_',
    file_constants = file.path('..', 'scenarios', 'sc5_constants.R'))
)

for (v in 1:length(variants)) {
  prefix_input <- variants[[v]]['prefix_input']
  prefix_output <- variants[[v]]['prefix_output']

  if ('file_constants' %in% names(variants[[v]])) {
    file_constants <- variants[[v]]['file_constants']
  } else { # default
    file_constants <- file.path('..', 'input_files', 'constants.R')
  }

  rmarkdown::render(
    input = 'evaluation.Rmd',
    output_file = stringr::str_glue('{prefix_output}evaluation.html'),
    output_format = 'html_document',
    intermediates_dir = 'C:/temp'
  )
}

