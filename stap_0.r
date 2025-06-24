# ****************************************************************************
# STAP 0: Voorbereiden data
# ****************************************************************************
selected_be_id <- data_row$BE_ID
selected_vl_id <- data_row$PS.VragenlijstID

logger::log_info(stringr::str_glue("Record BE_ID = {selected_be_id}"))

results <- list()
results$BE_ID <- selected_be_id

# PS regels
if (use_linear_rules == TRUE) {
  logger::log_info("PS")

  # basis lineaire regels PS
  rules_PS <- load_input(file_name = file.path('edited_input', stringr::str_glue('edited_rules_ps_{YEAR}_linearized.csv')))

  # PREFIX VOOR PS.VARIABELEN IN REGELS en alleen regels met V selecteren
  # Regels met A (analisten) en R (rechtsvorm) worden hier nog niet meegenomen
  rules_PS <- rules_PS %>%
    dplyr::filter(VL_ID == selected_vl_id | VL_ID == 9999) %>%
    dplyr::select(name, rule, VL_ID) %>%
    dplyr::mutate(rule = stringr::str_replace_all(rule, "([A-Z]+[0-9]{6})", "PS.\\1")) %>%
    dplyr::filter(stringr::str_detect(name, "^V"))

} else {
  rules_PS <- load_input(file_name = file.path('edited_input', stringr::str_glue('edited_rules_ps_{YEAR}.csv')))

  # PREFIX VOOR PS.VARIABELEN IN REGELS INDIEN AL NODIG
  rules_PS <- rules_PS %>%
    dplyr::filter(VL_ID == selected_vl_id | VL_ID == 9999) %>%
    dplyr::select(name, rule, VL_ID) %>%
    dplyr::mutate(rule = str_replace_all(rule, "([A-Z]*[0-9]{6})", "PS.\\1"))

}

# Regels edit operations PS
if (use_eo == TRUE) {
  rules_eo <- load_input(file_name = file.path("input_files", stringr::str_glue("eo_rules_echt_{YEAR}.csv"))) %>%
    dplyr::mutate(VL_ID = 0) %>%
    dplyr::select(name, rule, VL_ID)

  rules_eo$rule <- gsub(">= -M", paste0(">= -", M), rules_eo$rule)
} else {
  rules_eo <- NULL
}

# Regels confrontatievariabelen PS
rules_conf <- load_input(file_name = file.path("edited_input", stringr::str_glue("rules_conf_PS_{YEAR}.csv"))) %>%
  dplyr::mutate(VL_ID = 0) %>%
  dplyr::filter(sapply(condition, function(s) eval(parse(text = s), envir = data_row))) %>%
  dplyr::select(name, rule, VL_ID)

# Samenvoegen PS-regels
rules_PS_all <- rules_PS %>%
  dplyr::bind_rows(rules_eo) %>%
  dplyr::bind_rows(rules_conf)

rules <- validate::validator(.data = rules_PS_all)

# ****************************************************************************
# TUSSENSTAP: Opslaan confrontatie regels ruwe data
# ****************************************************************************
rules_cf <- as.data.frame(rules) %>%
  dplyr::select(name, rule) %>%
  unique()
rules_cf <- validate::validator(.data = rules_cf)
cf <- validate::confront(data_row, rules_cf)
confrontatie_ruw <- cbind(select(data_row, BE_ID), values(cf))
results$confrontatie_ruw <- confrontatie_ruw
# ****************************************************************************


# Regels confrontatie van statistieken
# uit elkaar halen -> deel 1 genereren, deel 2 extra_relaties

rules_interstat_bron <- load_input(file_name = file.path("edited_input", stringr::str_glue("interstat_rules_{YEAR}.csv")))

if (use_interstat_rules_soft == TRUE) {
  rules_interstat_soft <- load_input(file_name = file.path("edited_input", stringr::str_glue("interstat_rules_soft_{YEAR}.csv")))

  softvars_all <- rules_interstat_soft %>%
    dplyr::pull(softvar) %>%
    unique()

  rules_interstat_soft <- rules_interstat_soft %>%
    dplyr::filter(industry %in% data_row[ , c('SbiGecoordineerd2D', 'SbiGecoordineerd3D', 'SbiGecoordineerd4D')])

  gk_available <- unique(as.integer(rules_interstat_soft$gk1))
  gk_select <- gk_available[which.min(abs(gk_available - as.integer(data_row$GkSbsGecoordineerd1D)))]
  rules_interstat_soft <- rules_interstat_soft %>%
    dplyr::filter(gk1 == gk_select)

  softvars <- unique(rules_interstat_soft$softvar)
  if (length(softvars) > 0) { # add auxiliary variables for soft edit rules
    data_row[, softvars] <- 0
    rules_interstat_bron <- rbind(
      rules_interstat_bron,
      rules_interstat_soft %>% dplyr::select(c(name, rule))
    )
  }
} else {
  softvars_all <- NULL
  softvars <- NULL
}

rules_interstat <- validate::validator(.data = rules_interstat_bron)



# Gewichten klaarzetten voor stap 1 en 2
weights_interstat_i <- data.matrix(
  weights_interstat_row[ , !(names(weights_interstat_row) %in% c('BE_ID', setdiff(softvars_all, softvars)))]
  )[ , , drop = TRUE] # drop weights for irrelevant soft edit rules

if (any(is.na(weights_interstat_i))) {
  logger::log_error(glue::glue("Ontbrekende gewichten voor interstat edit rules bij BE_ID {selected_be_id} vervangen door 1"))
  weights_interstat_i[is.na(weights_interstat_i)] <- 1
}


# Gewichten laden voor stap 3
weights_PS <- load_input(file_name = file.path("input_files", "weights_PS.csv")) %>%
  replace(is.na(.), 1)

weights_PS <- stats::setNames(weights_PS$weight, paste0("PS.", weights_PS$variable))

if(use_eo == TRUE) {
  vars_eo <- validate::validator(.data = rules_eo) %>%
    validate::variables()
  weights_eo <- stats::setNames(rep(Inf, length(vars_eo)), vars_eo)
} else {
  vars_eo <- NULL
  weights_eo <- NULL
}

vars_conf <- paste0("PS.", rules_conf$name)
weights_conf <- stats::setNames(rep(0.01, length(vars_conf)), vars_conf)

weights_PS_all <- c(weights_PS, weights_eo, weights_conf)

# Data voorbereiden
vars_in_rules <- validate::variables(rules)
if (use_eo == TRUE) {
  # make sure all variables that occur in special edit operations are included during error localization
  vars_in_rules <- unique(c(vars_in_rules,
                            do.call(c, sapply(SPECIALE_ACTIES, function(l) l$varTarget))))
}

missing_weights <- dplyr::setdiff(vars_in_rules, names(weights_PS_all)) #rwls: setdiff could be base as well...
missing_weights <- stats::setNames(rep(1, length(missing_weights)), missing_weights)
weights_PS_all <- c(weights_PS_all, missing_weights)
weights_in_PS_not_in_vars <- dplyr::setdiff(names(weights_PS_all), vars_in_rules)
weights_PS_all <- weights_PS_all[!names(weights_PS_all) %in% weights_in_PS_not_in_vars]

vars_in_rules_interstat <- unique(validate::variables(rules_interstat))


if (any(sapply(data_row[unique(c(vars_in_rules, vars_in_rules_interstat))], function(v) {
  any(is.numeric(v) & abs(v) > 1e7, na.rm = TRUE)
}))) {
  logger::log_info("Variable too big")
  data_row <- data_row %>%
    dplyr::mutate(dplyr::across(all_of(unique(c(vars_in_rules, vars_in_rules_interstat))), ~ .x / 1000))

  results$scaling <- TRUE
}
