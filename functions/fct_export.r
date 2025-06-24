# Functies export data ----

export_output <- function(Stap3Doen = TRUE, results_errorlocate,
                          results_overhevelen, results_optelling_nul, results_ps,
                          results_ded, results_imp, results_mr, results_final,
                          data_org, prefix_output, save_rdata, conf_vars) {
  logger::log_info("Start export_output")
  project_dir <- getwd()
  output_dir <- OutputFolder

  if (save_rdata == TRUE) {
    logger::log_info("Opslaan errorlocate rdata")
    save(list = c('results_errorlocate', 'results_overhevelen',
                  'results_optelling_nul', 'results_ps',
                  'results_ded', 'results_imp', 'results_final'),
         file = file.path(output_dir, glue::glue("{prefix_output}results_{YEAR}.rdata")))
  }

  export_status_duration_weight(Stap3Doen = Stap3Doen, results_errorlocate, output_dir, prefix_output)

  export_errors_interstat(results_errorlocate, output_dir, prefix_output)
  if (Stap3Doen == TRUE) {
    export_errors_ps(results_errorlocate, output_dir, prefix_output)
  }
  variables_org <- names(data_org)
  export_results_interstat(results_errorlocate, variables_org, output_dir, prefix_output)
  export_extra_regels(results_errorlocate, output_dir, prefix_output)

  if (Stap3Doen == TRUE) {
    export_results(results_ps, output_dir, prefix_output, name = 'results_ps')
    export_results(results_ded, output_dir, prefix_output, name = 'results_ded')
    export_results(results_imp, output_dir, prefix_output, name = 'results_imp')
    export_results(results_mr, output_dir, prefix_output, name = 'results_mr')
    export_results(results_final, output_dir, prefix_output, name = 'results_final')
    export_scores(results_errorlocate, results_final, data_org, output_dir, prefix_output, conf_vars)
    export_results(results_overhevelen, output_dir, prefix_output, name = 'overhevelen')
    export_results(results_optelling_nul, output_dir, prefix_output, name = 'optelling_nul')
    export_confrontatie_ruw(results_errorlocate, output_dir, prefix_output)
  }

  export_randen(results_errorlocate, output_dir, prefix_output)
  export_imputaties_echt(results_errorlocate, output_dir, prefix_output)

  logger::log_info("Eind export_output")
}


export_status_duration_weight <- function(Stap3Doen = TRUE, results_errorlocate, output_dir, prefix_output) {
  logger::log_info("Start export_status_duration_weight")
  beids <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "BE_ID")))

  le_interstat <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "le_interstat")))
  le_interstat_non_empty <- unlist(lapply(le_interstat, function(z) {
    (length(z) > 0)
  }))

  le_interstat_beid <- do.call("rbind", lapply(beids[le_interstat_non_empty], function(x) x[["BE_ID"]]))
  le_interstat_status <- do.call("rbind", lapply(le_interstat[le_interstat_non_empty], function(x) x[["le_interstat"]][["status"]]))
  le_interstat_duration <- do.call("rbind", lapply(le_interstat[le_interstat_non_empty], function(x) x[["le_interstat"]][["duration"]]))
  le_interstat_weight <- do.call("rbind", lapply(le_interstat[le_interstat_non_empty], function(x) x[["le_interstat"]][["weight"]]))

  df_le_interstat <- data.frame(
    BE_ID = le_interstat_beid,
    status_interstat = le_interstat_status,
    duration_interstat = le_interstat_duration,
    weight_interstat = le_interstat_weight
  )
  if (Stap3Doen == TRUE) {
    le_ps <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "le_ps")))
    le_ps_non_empty <- unlist(lapply(le_ps, function(z) {
      (length(z) > 0)
    }))

    le_ps_beid <- do.call("rbind", lapply(beids[le_ps_non_empty], function(x) x[["BE_ID"]]))
    le_ps_status <- do.call("rbind", lapply(le_ps[le_ps_non_empty], function(x) x[["le_ps"]][["status"]]))
    le_ps_duration <- do.call("rbind", lapply(le_ps[le_ps_non_empty], function(x) x[["le_ps"]][["duration"]]))
    le_ps_weight <- do.call("rbind", lapply(le_ps[le_ps_non_empty], function(x) x[["le_ps"]][["weight"]]))
    le_ps_errors_speciale_acties <- do.call("bind_rows", lapply(le_ps[le_ps_non_empty], function(x) as.data.frame(x[["le_ps"]][["errors"]]))) %>%
      as.data.frame() %>%
      dplyr::select(dplyr::all_of(names(speciale_acties)))

    status_final <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "status_final")))
    status_final_ps <- do.call("rbind", lapply(status_final[le_ps_non_empty], function(x) x[["status_final"]]))

    df_le_ps <- data.frame(
      BE_ID = le_ps_beid,
      status_ps = le_ps_status,
      duration_ps = le_ps_duration,
      weight_ps = le_ps_weight,
      status_final_ps = status_final_ps
    ) %>%
      cbind(le_ps_errors_speciale_acties)

    df <- data.frame(BE_ID = unname(unlist(beids))) %>%
      dplyr::left_join(df_le_interstat, by = 'BE_ID') %>%
      dplyr::left_join(df_le_ps, by = 'BE_ID')
  } else {
    df <- data.frame(BE_ID = unname(unlist(beids))) %>%
      dplyr::left_join(df_le_interstat)
  }
  write.csv2(df, stringr::str_glue("{output_dir}/{prefix_output}status_duration_{YEAR}.csv"), row.names = FALSE)
}


export_errors_interstat <- function(results_errorlocate, output_dir, prefix_output) {
  logger::log_info("Start export_errors_interstat")
  beids <- unname(unlist(purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "BE_ID")))))

  le_interstat <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "le_interstat")))
  le_interstat_non_empty <- unlist(lapply(le_interstat, function(z) {
    (length(z) > 0)
  }))

  beids_non_empty <- beids[le_interstat_non_empty]

  df_errors <- data.frame(do.call("bind_rows", lapply(le_interstat[le_interstat_non_empty], function(x) as.data.frame(values(x[["le_interstat"]]))))) %>%
    dplyr::mutate(BE_ID = beids_non_empty, .before = 1)

  write.csv2(df_errors, stringr::str_glue("{output_dir}/{prefix_output}errors_interstat_{YEAR}.csv"), row.names = FALSE)
}


export_errors_ps <- function(results_errorlocate, output_dir, prefix_output) {
  logger::log_info("Start export_errors_ps")
  beids <- unname(unlist(purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "BE_ID")))))

  le_ps <- map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "le_ps")))
  le_ps_non_empty <- unlist(lapply(le_ps, function(z) {
    (length(z) > 0)
  }))

  beids_non_empty <- beids[le_ps_non_empty]
  df_errors <- data.frame(do.call("bind_rows", lapply(le_ps[le_ps_non_empty], function(x) as.data.frame(values(x[["le_ps"]]))))) %>%
    dplyr::mutate(BE_ID = beids_non_empty, .before = 1)

  write.csv2(df_errors, stringr::str_glue("{output_dir}/{prefix_output}errors_ps_{YEAR}.csv"), row.names = FALSE)
}


export_results_interstat <- function(results_errorlocate, variables_org, output_dir, prefix_output) {
  logger::log_info("Start export_results_interstat")
  re_interstat <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "re_interstat")))
  re_interstat_non_empty <- unlist(lapply(re_interstat, function(z) {
    (length(z) > 0)
  }))

  ordered_vars <- dplyr::intersect(variables_org, names(re_interstat))

  results_interstat <- do.call("bind_rows", lapply(re_interstat[re_interstat_non_empty], function(x) x[["re_interstat"]]))

  write.csv2(results_interstat, stringr::str_glue("{output_dir}/{prefix_output}results_interstat_{YEAR}.csv"), row.names = FALSE)
}


export_results <- function(results, output_dir, prefix_output, name) {
  logger::log_info(stringr::str_glue("Start export_results - {name}"))
  write.csv2(results, str_glue("{output_dir}/{prefix_output}{name}_{YEAR}.csv"), row.names = FALSE)
}

export_extra_regels <- function(results_errorlocate, output_dir, prefix_output) {
  logger::log_info("Start export_extra_regels")
  extra_regels <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "extra_regels")))
  extra_regels_non_empty <- unlist(lapply(extra_regels, function(z) {
    (length(z) > 0)
  }))

  results_extra_regels <- do.call("bind_rows", lapply(extra_regels[extra_regels_non_empty], function(x) x[["extra_regels"]]))

  write.csv2(results_extra_regels, stringr::str_glue("{output_dir}/{prefix_output}extra_regels_{YEAR}.csv"), row.names = FALSE)
}


export_randen <- function(results_errorlocate, output_dir, prefix_output) {
  logger::log_info("Start export_randen")
  randen <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "randen")))
  results_randen <- do.call("bind_rows", lapply(randen, function(x) x[["randen"]])) %>%
    dplyr::relocate(BE_ID, .before = variable)

  write.csv2(results_randen, stringr::str_glue("{output_dir}/{prefix_output}randen_interstat_{YEAR}.csv"), row.names = FALSE)
}

export_imputaties_echt <- function(results_errorlocate, output_dir, prefix_output) {
  logger::log_info("Start export_imputaties_echt")
  echt_imp <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "echt_imp")))
  results_echt_imp <- do.call("bind_rows", lapply(echt_imp, function(x) x[["echt_imp"]])) %>%
    dplyr::relocate(BE_ID, .before = variable)

  write.csv2(results_echt_imp, stringr::str_glue("{output_dir}/{prefix_output}imputaties_interstat_{YEAR}.csv"), row.names = FALSE)
}


export_scores <- function(results_errorlocate, results_final, data_org, output_dir, prefix_output, conf_vars) {
  logger::log_info("Start export_scores")

  #Prepare weights DF
  gewichten <- stats::setNames(rep(1, length(conf_vars)), conf_vars) %>%
    t() %>%
    as.data.frame()

  #Data combineren die nodig is voor scores (.Echt en PS interstat variabelen)
  waarden_echt <- results_final %>%
    dplyr::select(BE_ID, dplyr::starts_with("Echt."))

  interstat_PS <- data_org %>%
    dplyr::select(BE_ID, dplyr::all_of(paste0("PS.", conf_vars)))

  data_scores <- dplyr::left_join(waarden_echt, interstat_PS, by = "BE_ID")

  #Berekenen lokale scores
  scores <- data_scores %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(paste0("Echt.", conf_vars)), ~ abs(get(gsub("Echt.", "PS.", cur_column())) - .x) /
                                  (abs(.x) + 1), .names = "Score.{.col}")) %>%
    dplyr::select(dplyr::starts_with("score.")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(.x > 2, 2, .x)))

  #Naam aanpassen
  names(scores) <- gsub("Echt.", "", names(scores))

  #Globale scores berekenen
  #Alleen gewichten overhouden waar er ook lokale scores bestaan.
  gewichten <- gewichten %>%
    dplyr::slice(rep(1:n(), each = nrow(scores)))

  gewichten <- gewichten * (scores > -Inf)

  scores$Globale_Score <- rowSums(scores * gewichten, na.rm = TRUE) / rowSums(gewichten, na.rm = TRUE)

  scores <- scores %>%
    dplyr::select(Globale_Score, dplyr::everything())

  scores_output <- data_scores %>%
    dplyr::select(BE_ID) %>%
    dplyr::bind_cols(scores)

  write.csv2(scores_output, stringr::str_glue("{output_dir}/{prefix_output}scores_{YEAR}.csv"), row.names = FALSE)
}


export_confrontatie_ruw <- function(results_errorlocate, output_dir, prefix_output) {
  logger::log_info("Start export_results_confrontatie_ruw")
  confrontatie_ruw <- purrr::map(results_errorlocate, ~ keep(.x, .p = stringr::str_detect(names(.x), "confrontatie_ruw")))
  confrontatie_ruw_non_empty <- unlist(lapply(confrontatie_ruw, function(z) {
    (length(z) > 0)
  }))

  confrontatie_ruw <- do.call("bind_rows", lapply(confrontatie_ruw[confrontatie_ruw_non_empty], function(x) x[["confrontatie_ruw"]]))

  write.csv2(confrontatie_ruw, stringr::str_glue("{output_dir}/{prefix_output}confrontatie_ruw_{YEAR}.csv"), row.names = FALSE)
}
