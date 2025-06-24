logger::log_info(glue::glue("Stap3 uitvoeren? ({Stap3Doen})"))

if (Stap3Doen == TRUE) {
  logger::log_info(glue::glue("Stap3 ({selected_be_id}): Foutlokalisatie PS"))

  # ****************************************************************************
  # STAP 3a: onderliggende waarden optelling op nul zetten
  #
  # Tijdelijke oplossing: wachten op fix package deductive
  #
  # ****************************************************************************
  tryCatch({
    set.seed(42)
    na_impute_lr <- impute_lr(data_row, rules_i, methods = c("piv", "zeros", "implied"))
    vervang_na_door_nul <- is.na(data_row) & na_impute_lr == 0
    data_row[vervang_na_door_nul] <- 0
    logger::log_info(glue::glue("Onderliggende waarden optelling op nul zetten {paste(colnames(data_row[, which(vervang_na_door_nul)]), collapse = ', ')}"))
  }, error = function(e) {
    print(e)
  })

  results$optelling_nul <- data_row
  if (length(softvars) > 0) {
    results$optelling_nul <- results$optelling_nul %>%
      dplyr::select(-dplyr::all_of(softvars))
  }


  # ****************************************************************************
  # STAP 3b: overhevelen buitenland -> binnenland
  #
  # toevoegen regel:
  # INDIEN: Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten != NA EN  (PS.OMZETPS211150 + PS.OMZETPS211140) is (binnen 5%) gelijk aan Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten EN PS.OMZET211140 != 0
  # DAN:    PS.OMZETPS211140 volledig overhevelen naar PS.OMZETPS211150 EN PS.INKOPEN120200 volledig overhevelen naar PS.INKOPEN120100
  #
  # ****************************************************************************

  Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten_missing <- TRUE
  if(exists('waarden_echt')) {
    if("Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten" %in% names(waarden_echt)) {
      Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten_missing <- is.na(waarden_echt$Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten)
    }
  }

  logger::log_info(paste("waarde_prod_in_nl_missing:", Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten_missing))

  data_row <- data_row %>%
    dplyr::mutate(overhevelen_prod_buitenland = 0)

  if(!Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten_missing) {
    Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten <- waarden_echt$Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten

    # som gefactureerde omzet van in binnen- en buitenland zelfvervaardigde producten
    som_gefactureerde_omzet <- sum(data_row$PS.OMZETPS211150 + data_row$PS.OMZETPS211140, na.rm = TRUE)
    cond_overhevelen <- 'PS.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten' %in% randen_PS$variabele &
      som_gefactureerde_omzet >= Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten * 0.95 &
      som_gefactureerde_omzet <= Echt.Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten * 1.05 &
      !is.na(data_row$PS.OMZETPS211140) &
      data_row$PS.OMZETPS211140 != 0

    logger::log_info(paste("Overhevelen = ", cond_overhevelen))
    if (cond_overhevelen) {
      data_row$overhevelen_prod_buitenland <- 1
    }

    som_inkopen <- sum(data_row$PS.INKOPEN120100, data_row$PS.INKOPEN120200, na.rm = TRUE)

    if (data_row$overhevelen_prod_buitenland == 1) {
      data_row <- data_row %>%
        dplyr::mutate(PS.OMZETPS211150 = som_gefactureerde_omzet) %>%
        dplyr::mutate(PS.INKOPEN120100 = som_inkopen) %>%
        dplyr::mutate(PS.OMZETPS211140 = 0) %>%
        dplyr::mutate(PS.INKOPEN120200 = 0)
    }
  }

  results$overhevelen <- data_row
  if (length(softvars) > 0) {
    results$overhevelen <- results$overhevelen %>%
      dplyr::select(-dplyr::all_of(softvars))
  }


  # ****************************************************************************
  # STAP 3c: Foutlokalisatie PS
  # ****************************************************************************
  logger::log_info(glue::glue("Stap 3 ({selected_be_id}): Foutlokalisatie PS"))
  data_i <- data_row[vars_in_rules]

  # variabelen die in Stap 1 al op fout zijn gezet: NA's overnemen
  fout_in_stap1 <- colnames(le_interstat$errors)[!is.na(le_interstat$errors) & le_interstat$errors == TRUE]
  data_i[, names(data_i) %in% fout_in_stap1] <- NA_real_

  results_ps <- tryCatch(
    {
      set.seed(42)
      if(use_eo == TRUE) {
        logger::log_info(glue::glue("use_eo = {use_eo}"))

        solution_PS <- FALSE
        attempt <- 0
        while (!solution_PS & attempt < 3) {
          attempt <- attempt + 1
          le_PS <- locate_errors_eo(
            data = data_i,
            x = rules_i,
            operations = speciale_acties,
            weight = weights_PS_all,
            weight_eo = weight_eo,
            timeout = TIMEOUT
          )
          solution_PS <- le_PS$status %in% c(0,1,3,4,9,12)
        }

        if (solution_PS) {
          re_PS <- replace_errors_eo(
            data = data_i,
            x = le_PS,
            operations = speciale_acties
          )
        } else {
          re_PS <- NULL
        }

      } else {
        logger::log_info(glue::glue("use_eo = {use_eo}"))

        solution_PS <- FALSE
        attempt <- 0
        while (!solution_PS & attempt < 3) {
          attempt <- attempt + 1
          le_PS <- locate_errors(
            data = data_i,
            x = rules_i,
            weight = weights_PS_all,
            timeout = TIMEOUT
          )
          solution_PS <- le_PS$status %in% c(0,1,3,4,9,12)
        }

        if (solution_PS) {
          re_PS <- replace_errors(
            data = data_i,
            x = le_PS
          )
        } else {
          re_PS <- NULL
        }
      }

      list(le_ps = le_PS, re_ps = re_PS)
    },
    error = function(e) {
      logger::log_error(glue::glue("Fout in stap 3 (foutlokalisatie PS) bij BE_ID {selected_be_id}"))
      logger::log_error(toString(e))
      return(list(le_ps = NULL, re_ps = NULL))
    }
  )

  if (is.null(results_ps$re_ps)) {
    stap3_el_gelukt <- FALSE
  } else {
    stap3_el_gelukt <- TRUE
    results$le_ps <- results_ps$le_ps
    re_ps <- results_ps$re_ps
    results$re_ps <- re_ps %>%
      dplyr::mutate(BE_ID = selected_be_id) %>%
      dplyr::relocate(BE_ID)
  }


  # ****************************************************************************
  # STAP 3d: Deductieve imputatie PS
  # ****************************************************************************

  if (stap3_el_gelukt) {
    logger::log_info(glue::glue("Stap 3 ({selected_be_id}): Deductieve imputatie PS"))

    # check that all rules are linear and remove any that are not
    rules_i_lin <- rules_i[validatetools::is_linear(rules_i)]

    # apply deductive imputation
    results_ded <- tryCatch(
      {
        set.seed(42)

        # piv, zeros and implied cannot be run simultaneously, so do this in two steps
        result_intermediate <- impute_lr_eo(re_ps, rules_i_lin, methods = c("piv", "zeros"))

        result_intermediate <- result_intermediate %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) {
            if (!is.na(x) & abs(x) < 1e-6) round(x, digits = 6) else x }))

        result <- impute_lr_eo(result_intermediate, rules_i_lin, methods = "implied")

        result <- result %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) {
            if (!is.na(x) & abs(x) < 1e-6) round(x, digits = 6) else x }))

        result
      }, error = function(e) {
        logger::log_error(glue::glue("Fout in stap 3 (deductieve imputatie PS) bij BE_ID {selected_be_id}"))
        logger::log_error(toString(e))
        return(NULL)
      }
    )

    if (is.null(results_ded)) {
      stap3_ded_gelukt <- FALSE
    } else {
      stap3_ded_gelukt <- TRUE
      results$re_ded <- results_ded %>%
        dplyr::mutate(BE_ID = selected_be_id) %>%
        dplyr::relocate(BE_ID)
    }
  } else {
    stap3_ded_gelukt <- FALSE
  }


  # ****************************************************************************
  # STAP 3e: Imputation steps PS
  # ****************************************************************************

  if (stap3_ded_gelukt) {
    data_curr <- results_ded
  } else if (stap3_el_gelukt) {
    data_curr <- re_ps
  }

  if (stap3_ded_gelukt | stap3_el_gelukt) {
    logger::log_info(glue::glue("Stap 3 ({selected_be_id}): Verdere imputatiestappen PS"))

    vars_to_impute <- intersect(
      data_curr %>%
        dplyr::select(dplyr::starts_with('PS.')) %>%
        colnames(),
      vars_in_rules)


    # join columns containing auxiliary information for imputation methods
    data_curr <- cbind(
      data_curr,
      data_row %>%
        dplyr::select(dplyr::starts_with('PS.') &
                        dplyr::ends_with(c('.Tm1', '_ratio_omzetPS', '_ratio_wpPS', '_gem')))
    )


    # === 1. (T-1) imputation =====================================================

    for (col in vars_to_impute) {
      form <- as.formula(stringr::str_glue('{col} ~ {col}.Tm1'))
      data_curr <- data_curr %>%
        simputation::impute_proxy(formula = form)
    }

    # === 2. ratio imputation with turnover =======================================
    # (only for variables with _ratio_omzetPS present in data)

    vars_to_impute_ratio_omzetPS <- intersect(
      vars_to_impute,
      data_curr %>%
        dplyr::select(dplyr::ends_with('_ratio_omzetPS')) %>%
        dplyr::rename_with(.cols = dplyr::everything(),
                           .fn = ~stringr::str_replace(.x,
                                                       pattern = '_ratio_omzetPS',
                                                       replacement = '')) %>%
        colnames()
    )

    for (col in vars_to_impute_ratio_omzetPS) {
      form <- as.formula(stringr::str_glue('{col} ~ {col}_ratio_omzetPS * {naam_omzetvar_PS}'))
      data_curr <- data_curr %>%
        simputation::impute_proxy(formula = form)
    }


    # === 3. ratio imputation with number of working persons ======================
    # (only for variables with _ratio_wpPS present in data)

    vars_to_impute_ratio_wpPS <- intersect(
      vars_to_impute,
      data_curr %>%
        dplyr::select(dplyr::ends_with('_ratio_wpPS')) %>%
        dplyr::rename_with(.cols = dplyr::everything(),
                           .fn = ~stringr::str_replace(.x,
                                                       pattern = '_ratio_wpPS',
                                                       replacement = '')) %>%
        colnames()
    )

    for (col in vars_to_impute_ratio_wpPS) {
      form <- as.formula(stringr::str_glue('{col} ~ {col}_ratio_wpPS * {naam_wpvar_PS}'))
      data_curr <- data_curr %>%
        simputation::impute_proxy(formula = form)
    }


    # === 4. mean imputation ======================================================

    for (col in vars_to_impute) {
      form <- as.formula(stringr::str_glue('{col} ~ {col}_gem'))
      data_curr <- data_curr %>%
        simputation::impute_proxy(formula = form)
    }


    # === 5. zero imputation (last resort) ========================================

    for (col in vars_to_impute) {
      form <- as.formula(stringr::str_glue('{col} ~ 0'))
      data_curr <- data_curr %>%
        simputation::impute_proxy(formula = form)
    }


    # check that all missing values of relevant variables have been imputed
    if (any(is.na(data_curr[ , vars_to_impute]))) {
      stap3_imp_gelukt <- FALSE
    } else {
      stap3_imp_gelukt <- TRUE
      re_imp <- data_curr[ , colnames(re_ps), drop = FALSE]
      results$re_imp <- re_imp %>%
        dplyr::mutate(BE_ID = selected_be_id) %>%
        dplyr::relocate(BE_ID)
    }
  } else {
    stap3_imp_gelukt <- FALSE
  }


  # ****************************************************************************
  # STAP 3f: match_restrictions PS
  # ****************************************************************************

  if (stap3_imp_gelukt) {
    logger::log_info(glue::glue("Stap 3 ({selected_be_id}): match_restrictions PS"))

    voptions(rules_i_lin, lin.eq.eps = LIN.EQ.EPS, lin.ineq.eps = LIN.INEQ.EPS)
    vars_in_rules_mr <- variables(rules_i_lin)

    # Check that all rules can be handled by match_restrictions
    test_rules_mr <- tryCatch(
      expr = {
        errorlocate:::to_miprules(rules_i_lin)
      },
      error = function(e){
        logger::log_error(glue::glue("Fout in stap 3 (match_restrictions PS) bij BE_ID {selected_be_id}"))
        logger::log_error(toString(e))
        return(NULL)
      },
      warning = function(e){ # also stop in case of a warning, for this should not happen
        logger::log_error(glue::glue("Fout in stap 3 (match_restrictions PS) bij BE_ID {selected_be_id}"))
        logger::log_error(toString(e))
        return(NULL)
      }
    )

    if (is.null(test_rules_mr)) {
      stap3_mr_gelukt <- FALSE
    } else {

      # temporarily remove all variables that do not occur in rules_i_lin
      # and replace any remaining missing values by zero
      # (e.g., in auxiliary variables related to edit operations)
      data_rec <- re_imp %>%
        dplyr::select(-dplyr::all_of(setdiff(vars_in_rules, vars_in_rules_mr))) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                    ~tidyr::replace_na(.x, replace = 0)))

      # round to avoid problems with very small negative values (e.g., -1.24e-12)
      data_rec <- data_rec %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) {
          if (abs(x) < 1e-4) round(x, digits = 4) else x }))

      # Tag missing deductive:
      # logical matrix where TRUE indicates that a value may be adjusted
      # Only the variables that were NA after errorlocate (step 3c) may be adjusted.
      adjust_mr <- re_ps[ , names(data_rec)] %>%
        dplyr::select(-c(dplyr::starts_with('aux_'), dplyr::any_of('addEdits'))) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~is.na(.x)))

      # set weight matrix for match_restrictions
      data_rec_weights <- data_rec %>%
        dplyr::select(-c(dplyr::starts_with('aux_'), dplyr::any_of('addEdits')))

      data_rec_weights <- 1/(1 + as.matrix(abs(data_rec_weights)))
      # set smallest possible weight, for numerical reasons
      data_rec_weights <- data_rec_weights + (1e-04 - min(data_rec_weights))

      # run match_restrictions
      data_mr <- tryCatch(
        expr = {
          data_mr <- data_rec %>%
            match_restrictions_eo(rules_i_lin,
                                  weight = data_rec_weights,
                                  maxiter = MAX_IT_MR,
                                  eps = min(LIN.EQ.EPS, LIN.INEQ.EPS)/10,
                                  adjust = adjust_mr)

          data_mr <- data_mr %>%
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) {
              if (x < 0 & abs(x) < 1e-4) round(x, digits = 4) else x }))

          # Check success
          cf_mr <- confront(data_mr, rules_i_lin)
          summ <- summary(cf_mr)

          data_mr <- data_mr %>%
            dplyr::mutate(PS.nr_fails_mr = sum(summ$fails))

          if (!identical(names(re_imp), vars_in_rules_mr)) {
            data_mr <- data_mr %>%
              cbind(re_imp %>%
                      dplyr::select(-dplyr::any_of(vars_in_rules_mr)))
          }

          data_mr
        },
        error = function(e) {
          logger::log_error(glue::glue("Fout in stap 3 (match_restrictions PS) bij BE_ID {selected_be_id}"))
          logger::log_error(toString(e))
          return(NULL)
        }
      )

      if (is.null(data_mr)) {
        stap3_mr_gelukt <- FALSE
      } else {
        stap3_mr_gelukt <- TRUE
        results$re_mr <- data_mr %>%
          dplyr::mutate(BE_ID = selected_be_id) %>%
          dplyr::relocate(BE_ID)
      }
    }
  } else {
    stap3_mr_gelukt <- FALSE
  }

  if (stap3_mr_gelukt) {
    re_final <- data_mr
    results$status_final <- 'final'
  } else if (stap3_imp_gelukt) {
    re_final <- re_imp
    results$status_final <- 'mr'
  } else if (stap3_ded_gelukt) {
    re_final <- re_ded
    results$status_final <- 'imp'
  } else if (stap3_el_gelukt) {
    re_final <- re_ps
    results$status_final <- 'ded'
  } else {
    re_final <- NULL
    results$status_final <- 'el'
  }

  if (!is.null(re_final)) {
    #  merge back
    # 1. originele waarden voor variabelen die niet in regels PS en interstat zitten
    # 2. waarden van stap 1 voor variabelen die in interstat regels zitten
    # 3. waarden variabelen echt
    vars_in_rules_and_vars_in_interstat <- unique(c(names(re_interstat), vars_in_rules))
    intersect_vars_in_rules_and_vars_in_interstat <- dplyr::intersect(names(re_interstat), vars_in_rules)
    vars_in_interstat_not_in_vars_in_rules <- dplyr::setdiff(names(re_interstat), intersect_vars_in_rules_and_vars_in_interstat)

    re_final <- re_final %>%
      cbind(dplyr::select(data_row, -dplyr::all_of(vars_in_rules_and_vars_in_interstat))) %>%
      cbind(dplyr::select(re_interstat, dplyr::all_of(vars_in_interstat_not_in_vars_in_rules))) %>%
      dplyr::select(-(dplyr::starts_with('PS.') &
                        dplyr::ends_with(c('.Tm1', '_ratio_omzetPS', '_ratio_wpPS', '_gem')))) %>%
      dplyr::select(-(dplyr::starts_with('Echt.') &
                        dplyr::ends_with(c('_ratio_omzet', '_ratio_wp', '_gem')))) %>%
      dplyr::mutate(PS.error_locate.status = ifelse(stap3_el_gelukt, results$le_ps$status, NA),
                    PS.error_locate.duration = ifelse(stap3_el_gelukt, results$le_ps$duration, NA),
                    PS.error_locate.weight = ifelse(stap3_el_gelukt, results$le_ps$weight, NA))

    if (length(softvars) > 0) {
      re_final <- re_final %>%
        dplyr::select(-dplyr::all_of(softvars))
    }

    # reorder columns
    re_final <- re_final %>%
      dplyr::relocate(dplyr::any_of(names(data_row)))

    results$re_final <- re_final
  } else {
    results$re_final <- NULL
  }

} else {
  logger::log_info(glue::glue("Stap3: geskipt"))
}
