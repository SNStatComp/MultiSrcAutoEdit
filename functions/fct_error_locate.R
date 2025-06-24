start_errorlocate <- function(Stap3Doen = TRUE,
                              data, weights_interstat,
                              n_start = 1, n_end = NULL,
                              rule_matrix, prefix_output = "",
                              save_rdata = FALSE,
                              use_eo = TRUE, use_linear_rules = TRUE,
                              use_interstat_rules_soft = TRUE,
                              use_calculator = FALSE){

  if (is.null(n_end) || (n_end > nrow(data))) {
    n_end <- nrow(data)
  }

  logger::log_info(glue::glue("Start errorlocate rij {n_start}:{n_end}"))

  start_time <- proc.time()

  if (use_calculator) {
    cores <- max(1, floor(parallel::detectCores() * 0.4))
  } else {
    cores <- max(1, parallel::detectCores() - 1)
  }
  logger::log_info(glue::glue("Aantal cores {cores}"))

  # Divide the work into tranches of at most N_MAX records
  # and make a new parallel cluster for each tranche
  # (to avoid memory overload)
  A <- 1 + (n_end - n_start) %/% N_MAX
  aggregated_result <- vector(mode = 'list', length = n_end - n_start + 1)
  results_overhevelen <- NULL
  results_optelling_nul <- NULL
  results_ps <- NULL
  results_ded <- NULL
  results_imp <- NULL
  results_mr <- NULL
  results_final <- NULL

  for (a in 1:A) {

    cl <- parallel::makeCluster(cores[1], outfile = output_log)
    doParallel::registerDoParallel(cl)

    n_start_a <- n_start + (a-1)*N_MAX
    n_end_a <- min(n_end, n_start + a*N_MAX - 1)

    aggregated_result_a <- foreach::foreach(i = n_start_a:n_end_a,
                                            .combine = c,
                                            .inorder = TRUE,
                                            .verbose = TRUE,
                                            .packages = c('deductive', 'errorlocate',
                                                          'validatetools', 'lintools',
                                                          'simputation', 'rspa',
                                                          'stringr', 'dplyr', 'glue',
                                                          'foreach', 'purrr', 'logger', 'tidyr'),
                                            .export = setdiff(ls(.GlobalEnv), 'Stap3Doen')) %dopar% {

                                              # Inladen error_locate_eo functies
                                              source("functions/editoperations_auxiliary_functions.R")

                                              logger::log_info(paste("error locate version:", packageVersion("errorlocate")))
                                              logger::log_info(stringr::str_glue("start process record {i}/{nrow(data)})"))
                                              temp_result <- list(process_row_errorlocate(Stap3Doen = Stap3Doen,
                                                                                          data_row = data[i, ],
                                                                                          weights_interstat_row = weights_interstat[i, ],
                                                                                          use_eo = use_eo,
                                                                                          use_linear_rules = use_linear_rules,
                                                                                          use_interstat_rules_soft = use_interstat_rules_soft)) #calling a function
                                              logger::log_info(stringr::str_glue("end process record {i}/{nrow(data)})"))
                                              temp_result #Equivalent to finalMatrix = rbind(finalMatrix, tempMatrix)
                                            }

    if (Stap3Doen == TRUE) {
      # to save memory usage, already bind together large vectors of results into a data.frame across BE_IDs
      # instead of a list per BE_ID

      overhevelen_a <- purrr::map(aggregated_result_a, ~ keep(.x, .p = stringr::str_detect(names(.x), "overhevelen")))
      overhevelen_a_non_empty <- unlist(lapply(overhevelen_a, function(z) (length(z) > 0) ))
      results_overhevelen_a <- do.call("bind_rows", lapply(overhevelen_a[overhevelen_a_non_empty], function(x) x[["overhevelen"]]))

      optelling_nul_a <- purrr::map(aggregated_result_a, ~ keep(.x, .p = stringr::str_detect(names(.x), "optelling_nul")))
      optelling_nul_a_non_empty <- unlist(lapply(optelling_nul_a, function(z) (length(z) > 0) ))
      results_optelling_nul_a <- do.call("bind_rows", lapply(optelling_nul_a[optelling_nul_a_non_empty], function(x) x[["optelling_nul"]]))

      re_ps_a <- purrr::map(aggregated_result_a, ~ keep(.x, .p = stringr::str_detect(names(.x), "re_ps")))
      re_ps_a_non_empty <- unlist(lapply(re_ps_a, function(z) (length(z) > 0) ))
      results_ps_a <- do.call("bind_rows", lapply(re_ps_a[re_ps_a_non_empty], function(x) x[["re_ps"]]))

      re_ded_a <- purrr::map(aggregated_result_a, ~ keep(.x, .p = stringr::str_detect(names(.x), "re_ded")))
      re_ded_a_non_empty <- unlist(lapply(re_ded_a, function(z) (length(z) > 0) ))
      results_ded_a <- do.call("bind_rows", lapply(re_ded_a[re_ded_a_non_empty], function(x) x[["re_ded"]]))

      re_imp_a <- purrr::map(aggregated_result_a, ~ keep(.x, .p = stringr::str_detect(names(.x), "re_imp")))
      re_imp_a_non_empty <- unlist(lapply(re_imp_a, function(z) (length(z) > 0) ))
      results_imp_a <- do.call("bind_rows", lapply(re_imp_a[re_imp_a_non_empty], function(x) x[["re_imp"]]))

      re_mr_a <- purrr::map(aggregated_result_a, ~ keep(.x, .p = stringr::str_detect(names(.x), "re_mr")))
      re_mr_a_non_empty <- unlist(lapply(re_mr_a, function(z) (length(z) > 0) ))
      results_mr_a <- do.call("bind_rows", lapply(re_mr_a[re_mr_a_non_empty], function(x) x[["re_mr"]]))

      re_final_a <- purrr::map(aggregated_result_a, ~ keep(.x, .p = stringr::str_detect(names(.x), "re_final")))
      re_final_a_non_empty <- unlist(lapply(re_final_a, function(z) (length(z) > 0) ))
      results_final_a <- do.call("bind_rows", lapply(re_final_a[re_final_a_non_empty], function(x) x[["re_final"]]))

      results_overhevelen <- dplyr::bind_rows(results_overhevelen, results_overhevelen_a)
      results_optelling_nul <- dplyr::bind_rows(results_optelling_nul, results_optelling_nul_a)
      results_ps <- dplyr::bind_rows(results_ps, results_ps_a, )
      results_ded <- dplyr::bind_rows(results_ded, results_ded_a)
      results_imp <- dplyr::bind_rows(results_imp, results_imp_a)
      results_mr <- dplyr::bind_rows(results_mr, results_mr_a)
      results_final <- dplyr::bind_rows(results_final, results_final_a)

      aggregated_result_a <- purrr::map(aggregated_result_a,
                                        ~ discard(.x, .p = (names(.x) %in% c("overhevelen", "optelling_nul", "re_ps",
                                                                             "re_ded", "re_imp", "re_mr", "re_final"))))

      rm(overhevelen_a, overhevelen_a_non_empty, results_overhevelen_a,
         optelling_nul_a, optelling_nul_a_non_empty, results_optelling_nul_a,
         re_ps_a, re_ps_a_non_empty, results_ps_a,
         re_ded_a, re_ded_a_non_empty, results_ded_a,
         re_imp_a, re_imp_a_non_empty, results_imp_a,
         re_mr_a, re_mr_a_non_empty, results_mr_a,
         re_final_a, re_final_a_non_empty, results_final_a)
    }

    aggregated_result[n_start_a:n_end_a] <- aggregated_result_a
    rm(aggregated_result_a)
    gc()

    parallel::stopCluster(cl)

  }

  loop_duration <- proc.time() - start_time
  print(loop_duration)

  return(list(
    results_errorlocate = aggregated_result,
    results_overhevelen = results_overhevelen,
    results_optelling_nul = results_optelling_nul,
    results_ps = results_ps,
    results_ded = results_ded,
    results_imp = results_imp,
    results_mr = results_mr,
    results_final = results_final
  ))

  # export_output(
  #   Stap3Doen = Stap3Doen,
  #   results_errorlocate = aggregated_result,
  #   results_overhevelen = results_overhevelen,
  #   results_optelling_nul = results_optelling_nul,
  #   results_ps = results_ps,
  #   results_ded = results_ded,
  #   results_imp = results_imp,
  #   results_mr = results_mr,
  #   results_final = results_final,
  #   data_org = data,
  #   prefix_output = prefix_output,
  #   save_rdata = save_rdata,
  #   conf_vars = rule_matrix$Naam[!is.na(rule_matrix$PS) & rule_matrix$PS != ''])
 }
