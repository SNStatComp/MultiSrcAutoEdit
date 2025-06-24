# ****************************************************************************
# STAP 1: Echte waarden afleiden confrontatievariabelen d.m.v. foutlokalisatie
# ****************************************************************************
logger::log_info(glue::glue("Stap 1 ({selected_be_id}): Echte waarden afleiden confrontatievariabelen d.m.v. foutlokalisatie"))

data_interstat_i <- data_row[vars_in_rules_interstat]

rules_interstat_i <- rules_interstat_bron

# Verwijderen van alle regels die betrekking hebben op een bron die niet voorkomt in deze record
# Bij match_restrictions levert een error als er een regel is waarbij de variabele in de regel niet bestaat in de dataset
if (is.na(data_row["PS.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("PS[.]", rule))
}
if (is.na(data_row["PCM.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("PCM[.]", rule))
}
if (is.na(data_row["SFGO.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("SFGO[.]", rule))
}
if (is.na(data_row["DRT.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("DRT[.]", rule))
}
if (is.na(data_row["KICR.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("KICR[.]", rule))
}
if (is.na(data_row["SWL.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("SWL[.]", rule))
}
if (is.na(data_row["IH.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("IH[.]", rule))
}
if (is.na(data_row["WIA.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("WIA[.]", rule))
}
if (is.na(data_row["INIVA.exist"])) {
  rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("INIVA[.]", rule))
}

rules_interstat_i_obj <- validate::validator(.data = rules_interstat_i)


results_interstat <- tryCatch(
  {
    set.seed(42)
    le_interstat <- errorlocate::locate_errors(
      data = data_interstat_i,
      x = rules_interstat_i_obj,
      weight = weights_interstat_i,
      timeout = TIMEOUT
    )

    re_interstat <- errorlocate::replace_errors(
      data = data_interstat_i,
      x = le_interstat
    )

    list(le_interstat = le_interstat, re_interstat = re_interstat)
  },
  error = function(e) {
    logger::log_error(glue::glue("Fout in stap 1 bij BE_ID {selected_be_id}"))
    logger::log_error(toString(e))
    return(list(le_interstat = NULL, re_interstat = NULL))
  }
)

if (is.null(results_interstat$re_interstat)) {
  stap1_gelukt <- FALSE
  stap2_gelukt <- FALSE
} else {
  stap1_gelukt <- TRUE
  results$le_interstat <- results_interstat$le_interstat
  results$re_interstat <- results_interstat$re_interstat
}
