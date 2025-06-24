
rule_matrix <- load_input(file_name = file.path("input_files", stringr::str_glue("rule_matrix_{YEAR}.csv")))

data <- prepare_data(
  file_name = FILE_NAME_INPUT_DATA,
  use_eo = use_eo,
  conf_vars = rule_matrix$Naam
)

# read file with interstat weights per record
weights_interstat <- load_input(file_name = FILE_NAME_WEIGHTS)

if (use_interstat_rules_soft) { # add weights for soft edit rules
  weights_interstat_soft <- load_input(file_name = FILE_NAME_WEIGHTS_SOFT)
  weights_interstat <- cbind(weights_interstat,
                             weights_interstat_soft)
}

if (beidselection) {
  # only keep BE_IDs that are included in FILE_NAME_BEIDSELECTION
  beidsel <- load_input(file_name = FILE_NAME_BEIDSELECTION)

  data <- data %>%
    dplyr::inner_join(beidsel %>% dplyr::select(BE_ID),
                      by = 'BE_ID')

  weights_interstat <- weights_interstat %>%
    dplyr::inner_join(beidsel %>% dplyr::select(BE_ID),
                      by = 'BE_ID')
}

