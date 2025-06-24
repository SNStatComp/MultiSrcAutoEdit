# ****************************************************************************
# STAP 2: Extra restricties afleiden t.b.v. foutlokalisatie PS
# ****************************************************************************

# STAP 2a: Bepaal imputatiewaarden voor echte consistentievariabelen
logger::log_info(glue::glue("Stap 2a ({selected_be_id}): Bepaal imputatiewaarden voor echte consistentievariabelen"))

# if there are soft edit rules, replace any missing values on auxiliary variables by ones
# (these correspond to soft edit rules that do not have to be satisfied for this record)
if (length(softvars) > 0) {
  re_interstat <- re_interstat %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(softvars),
                                ~tidyr::replace_na(.x, replace = 1)))

  rules_interstat_i_obj <- validatetools::substitute_values(rules_interstat_i_obj,
                                                             as.list(re_interstat[, softvars]),
                                                             .add_constraints = FALSE)
}

# variabelen die niet als fout zijn aangewezen invullen in consistentieregels
invullen <- names(re_interstat)[!is.na(re_interstat)]
rules_interstat_i_post <- validatetools::substitute_values(rules_interstat_i_obj,
                                                           as.list(re_interstat[, invullen]),
                                                           .add_constraints = FALSE)


## Boven- en ondergrenzen op echte waarden consistentievariabelen afleiden
# (om rekentijd te beperken: beschouw elke variabele apart)

te_verwerken_echte_variabelen <- variables(rules_interstat_i_post)
te_verwerken_echte_variabelen <- te_verwerken_echte_variabelen[grep('^Echt.', te_verwerken_echte_variabelen)]

randen <- NULL
rules_interstat_i_post_txt <- as.data.frame(rules_interstat_i_post)$rule
for (v in te_verwerken_echte_variabelen) {
  temp_rules <- rules_interstat_i_post[grep(paste0(v, '[^_]'), rules_interstat_i_post_txt)]
  temp <- validatetools::detect_boundary_num(temp_rules)
  randen <- rbind(randen,
                  temp %>% dplyr::filter(variable == v))
}

echt_imp <- randen %>%
  dplyr::filter(grepl('Echt.', variable)) %>%
  dplyr::mutate(imp = NA_real_,
                type_imp = NA_character_)

rules_interstat_i_txt <- as.data.frame(rules_interstat_i_obj)$rule


## Stap 2a.1: Bepaal imputatiewaarden voor (echte) consistentievariabelen
## op basis van de toegelaten intervallen (voor zover mogelijk).
# - Kies indien mogelijk de waargenomen waarde uit de overgebleven bron met het hoogste gewicht
# - Kies anders het middelpunt van het toegelaten interval
# Verwerk in deze stap alleen grenzen voor echte waarden
# waarbij het toegelaten interval niet oneindig groot is
# (N.B. 1e7 komt binnen het errorlocate-package neer op hetzelfde als 'oneindig')

te_verwerken_rijen <- with(echt_imp,
                           which(is.finite(lowerbound) & abs(lowerbound) <= 1e7 - 1 &
                                   is.finite(upperbound) & abs(upperbound) <= 1e7 - 1))

for (k in te_verwerken_rijen) {
  v <- echt_imp$variable[k]

  lb <- echt_imp$lowerbound[echt_imp$variable == v]
  ub <- echt_imp$upperbound[echt_imp$variable == v]

  # Bepaal welke regels gaan over deze consistentievariabele
  welke_v <- grep(paste0(v, '[^_]'), rules_interstat_i_txt)
  rules_v <- rules_interstat_i_obj[welke_v]
  vars_v <- validate::variables(rules_v)

  # Bepaal de (niet op NA gezette) waargenomen waarden bij deze consistentievariabele
  # die binnen het toegelaten interval liggen
  # (N.B. In re_interstat zijn op dit moment de 'echte' consistentievariabelen nog NA,
  # dus ook als zulke variabelen voorkomen in vars_v worden ze hier niet gebruikt.)
  values_available_v <- re_interstat %>%
    dplyr::select(all_of(vars_v)) %>%
    tidyr::pivot_longer(cols = all_of(vars_v)) %>%
    dplyr::mutate(weight_v = weights_interstat_i[name]) %>%
    dplyr::filter(!is.na(value), value >= lb, value <= ub)

  if (nrow(values_available_v) >= 1) {
    # imputeer de consistentievariabele met de waargenomen waarde met het hoogste gewicht
    echt_imp$imp[k] <- values_available_v$value[which.max(values_available_v$weight_v)]
    echt_imp$type_imp[k] <- 'waargenomen'
  } else {
    # imputeer het middelpunt van het toegelaten interval
    echt_imp$imp[k] <- mean(c(lb,ub))
    echt_imp$type_imp[k] <- 'middelpunt'
  }
  if (abs(echt_imp$imp[k]) < 0.001) echt_imp$imp[k] <- round(echt_imp$imp[k], digits = 0)

}


## Stap 2a.2: Bepaal imputatiewaarden voor de overige (echte) consistentievariabelen
## op basis van stratumgemiddelden.
# - Gebruik indien mogelijk een ratio-imputatie o.b.v. eerder geïmputeerde totale omzet of werkzame personen.
# - Imputeer anders het stratumgemiddelde.

imp_omzet <- echt_imp$imp[echt_imp$variable == paste0('Echt.', naam_omzetvar)]
imp_wp <- echt_imp$imp[echt_imp$variable == paste0('Echt.', naam_wpvar)]

if (!is.na(imp_omzet)) {
  echt_imp <- echt_imp %>%
    dplyr::mutate(imp = if_else(is.na(imp),
                                unlist(data_row[paste0(variable, '_ratio_omzet')] * imp_omzet),
                                imp),
                  type_imp = if_else(is.na(type_imp), 'stratum_omzet', type_imp))
} else if (!is.na(imp_wp)) {
  echt_imp <- echt_imp %>%
    dplyr::mutate(imp = if_else(is.na(imp),
                                unlist(data_row[paste0(variable, '_ratio_wp')] * imp_wp),
                                imp),
                  type_imp = if_else(is.na(type_imp), 'stratum_wp', type_imp))
} else {
  echt_imp <- echt_imp %>%
    dplyr::mutate(imp = if_else(is.na(imp),
                                unlist(data_row[paste0(variable, '_gem')]),
                                imp),
                  type_imp = if_else(is.na(type_imp), 'stratum_gem', type_imp))
}



## Stap 2a.3: Controleer of de huidige imputaties voldoen aan alle interstat-regels.
## Zo ja, dan is stap 2a klaar.
## Anders: gebruik match_restrictions om de huidige imputaties aan te passen.

# Vul de huidige imputaties in als voorlopige waarden in het record met data
waarden_echt_voorlopig <- echt_imp %>%
  dplyr::select(variable, imp) %>%
  dplyr::distinct() %>%
  tidyr::spread(variable, imp)

re_interstat_voorlopig <- re_interstat %>%
  dplyr::select(-tidyr::all_of(names(waarden_echt_voorlopig))) %>%
  {if(ncol(waarden_echt_voorlopig) > 0) cbind(., waarden_echt_voorlopig) else .}

if (length(softvars) > 0) {
  re_interstat_voorlopig <- re_interstat_voorlopig %>%
    dplyr::select(-dplyr::all_of(softvars))
}

cf_interstat_voorlopig <- validate::confront(re_interstat_voorlopig, rules_interstat_i_obj)

imputaties_klaar <- all(validate::values(cf_interstat_voorlopig), na.rm = TRUE)


if (imputaties_klaar) { # imputaties zijn al goed
  stap2a_gelukt <- TRUE

  echt_imp <- echt_imp %>%
    dplyr::mutate(imp_def = imp,
                  type_imp_def = type_imp)

  waarden_echt <- waarden_echt_voorlopig
  re_interstat <- re_interstat_voorlopig

  results$echt_imp <- echt_imp %>%
    dplyr::mutate(BE_ID = selected_be_id)

} else { # pas imputaties aan via match_restrictions_lp

  # Gewichten voor match_restrictions_lp:
  # - niet-geïmputeerde waargenomen variabelen mogen niet worden aangepast (gewicht = Inf)
  # - maak twee versies met gewichten voor de 'echte' consistentievariabelen:
  #   1) probeer eerst match_restrictions_lp uit te voeren waarbij de imputaties
  #     gebaseerd op waargenomen waarden niet mogen worden aangepast (gewicht = Inf)
  #     en de andere imputaties wel (gewicht = 1)
  #   2) probeer anders een versie waarbij alle imputaties mogen worden aangepast (gewicht = 1)
  weights_lp2 <- weights_interstat_i[colnames(re_interstat_voorlopig)]
  weights_lp2[] <- 1
  weights_lp2[!is.na(re_interstat_voorlopig) & !grepl('^Echt[.]', names(weights_lp2))] <- Inf

  weights_lp1 <- weights_lp2
  weights_lp1[echt_imp$variable[echt_imp$type_imp == 'waargenomen']] <- Inf

  # Lege waarden (in andere variabelen dan de 'echte' consistentievariabelen)
  # worden hier tijdelijk vervangen door de imputaties van de bijbehorende 'echte' consistentievariabelen
  # Deze waarden mogen indien nodig 'onbeperkt' worden aangepast door match_restrictions_lp
  # (geef ze een zeer laag gewicht)
  weights_lp1[is.na(re_interstat_voorlopig)] <- 1e-04
  weights_lp2[is.na(re_interstat_voorlopig)] <- 1e-04
  vv <- apply(re_interstat_voorlopig, 2, function(m) any(is.na(m)))
  vv <- names(vv)[vv == TRUE]
  re_interstat_voorlopig[1, vv] <-
    sapply(vv, function(v) {
      imp <- echt_imp$imp[echt_imp$variable == paste0("Echt.", gsub("[A-Z]*\\.", "", v))]
      if (length(imp) == 1) return (imp) else return (0)
    })

  rules_interstat_i_lp <- rules_interstat_i_obj
  validate::voptions(rules_interstat_i_lp, lin.eq.eps=0.01,lin.ineq.eps=0.01)

  # match_restrictions_lp waarbij de imputaties
  # gebaseerd op waargenomen waarden niet mogen worden aangepast
  result_lp <- tryCatch(
    expr = {
      re_interstat_voorlopig %>%
        unlist() %>%
        match_restrictions_lp(rules = rules_interstat_i_lp,
                              weights = weights_lp1)
    },
    error = function(e){
      "ERROR"
    }
  )

  if (identical(result_lp, "ERROR") || (length(result_lp) == 3 & !result_lp$solution)) {
    lp_klaar <- FALSE
  } else {
    ontbrekend <- setdiff(colnames(re_interstat_voorlopig), names(result_lp$lp_values))
    if (length(ontbrekend) > 0) {
      # kolommen die door match_restrictions_lp zijn weggelaten weer toevoegen
      result_lp$lp_values <- c(result_lp$lp_values,
                               unlist(re_interstat_voorlopig[1, ontbrekend]))
    }
    data_result <- as.data.frame(t(result_lp$lp_values[colnames(re_interstat_voorlopig)]))
    cf_lp <- validate::confront(data_result, rules_interstat_i_lp)
    lp_klaar <- all(validate::values(cf_lp), na.rm = TRUE) & !any(is.na(data_result)) &
      (max(abs(data_result), na.rm = TRUE) >= 1) & (max(abs(data_result), na.rm = TRUE) <= 1e10)
    # laatste toevoeging omdat er soms een schijnoplossing wordt gevonden waarbij alle aangepaste waarden ongeveer 0 of extreem hoog zijn
  }

  if (!lp_klaar) {
    # match_restrictions_lp waarbij de imputaties
    # gebaseerd op waargenomen waarden ook mogen worden aangepast
    result_lp <- tryCatch(
      expr = {
        re_interstat_voorlopig %>%
          unlist() %>%
          match_restrictions_lp(rules = rules_interstat_i_lp,
                                weights = weights_lp2)
      },
      error = function(e){
        "ERROR"
      }
    )

    if (identical(result_lp, "ERROR") || (length(result_lp) == 3 & !result_lp$solution)) {
      lp_klaar <- FALSE
    } else {
      ontbrekend <- setdiff(colnames(re_interstat_voorlopig), names(result_lp$lp_values))
      if (length(ontbrekend) > 0) {
        # kolommen die door match_restrictions_lp zijn weggelaten weer toevoegen
        result_lp$lp_values <- c(result_lp$lp_values,
                                 unlist(re_interstat_voorlopig[1, ontbrekend]))
      }
      data_result <- as.data.frame(t(result_lp$lp_values[colnames(re_interstat_voorlopig)]))
      #cf_lp <- validate::confront(data_result, rules_interstat_i_lp)
      #lp_klaar <- all(validate::values(cf_lp), na.rm = TRUE) & !any(is.na(data_result))
      lp_klaar <- !any(is.na(data_result)) & (max(abs(data_result), na.rm = TRUE) >= 1) & (max(abs(data_result), na.rm = TRUE) <= 1e10)
      # laatste toevoeging omdat er soms een schijnoplossing wordt gevonden waarbij alle aangepaste waarden ongeveer 0 of extreem hoog zijn
    }
  }

  if (!lp_klaar) {
    # doe een laatste poging: vervang Inf-gewichten door heel hoge gewichten
    weights_lp2[is.infinite(weights_lp2)] <- 1e7

    result_lp <- tryCatch(
      expr = {
        re_interstat_voorlopig %>%
          unlist() %>%
          match_restrictions_lp(rules = rules_interstat_i_lp,
                                weights = weights_lp2)
      },
      error = function(e){
        "ERROR"
      }
    )

    if (identical(result_lp, "ERROR") || (length(result_lp) == 3 & !result_lp$solution)) {
      lp_klaar <- FALSE
    } else {
      ontbrekend <- setdiff(colnames(re_interstat_voorlopig), names(result_lp$lp_values))
      if (length(ontbrekend) > 0) {
        # kolommen die door match_restrictions_lp zijn weggelaten weer toevoegen
        result_lp$lp_values <- c(result_lp$lp_values,
                                 unlist(re_interstat_voorlopig[1, ontbrekend]))
      }
      data_result <- as.data.frame(t(result_lp$lp_values[colnames(re_interstat_voorlopig)]))
      #cf_lp <- validate::confront(data_result, rules_interstat_i_lp)
      #lp_klaar <- all(validate::values(cf_lp), na.rm = TRUE) & !any(is.na(data_result))
      lp_klaar <- !any(is.na(data_result)) & (max(abs(data_result), na.rm = TRUE) >= 1) & (max(abs(data_result), na.rm = TRUE) <= 1e10)
      # laatste toevoeging omdat er soms een schijnoplossing wordt gevonden waarbij alle aangepaste waarden ongeveer 0 of extreem hoog zijn
    }
  }

  if (lp_klaar) { # gelukt
    stap2a_gelukt <- TRUE

    echt_imp <- echt_imp %>%
      dplyr::left_join(data_result %>% tidyr::pivot_longer(cols = dplyr::everything(),
                                                           names_to = 'variable',
                                                           values_to = 'imp_def'),
                       by = 'variable') %>%
      dplyr::mutate(type_imp_def = if_else(abs(imp_def - imp) < 1e-6, type_imp, 'aangepast'))

    waarden_echt <- echt_imp %>%
      dplyr::select(variable, imp_def) %>%
      dplyr::distinct() %>%
      tidyr::spread(variable, imp_def)

    re_interstat <- re_interstat %>%
      dplyr::select(-tidyr::all_of(names(waarden_echt))) %>%
      {if(ncol(waarden_echt) > 0) cbind(., waarden_echt) else .}

    results$echt_imp <- echt_imp %>%
      dplyr::mutate(BE_ID = selected_be_id)

  } else { # niet gelukt
    stap2a_gelukt <- FALSE
    logger::log_error(glue::glue("Fout in stap 2 bij BE_ID {selected_be_id}"))
  }

}

results$re_interstat <- re_interstat %>%
  dplyr::mutate(BE_ID = selected_be_id) %>%
  dplyr::relocate(BE_ID)


if (stap2a_gelukt) {

  ## STAP 2b: Bepaal onder- en bovengrenzen voor waargenomen variabelen in PS,
  ## gegeven de imputaties uit stap 2a
  logger::log_info(glue::glue("Stap 2b ({selected_be_id}): Bepaal onder- en bovengrenzen voor waargenomen variabelen in PS"))

  rules_interstat_i_post <- rules_interstat_i_obj

  # Bepaal welke variabelen oorspronkelijk waren waargenomen:
  interstat_i_zonder_na <- data_interstat_i %>%
    dplyr::select_if(~ !any(is.na(.)))

  # Neem alleen geïmputeerde 'echte' consistentiewaarden over bij variabelen
  # die in ten minste 1 bron waren waargenomen

  # names(waarden_echt) bevat de namen van alle 'echte' consistentievariabelen die geïmputeerd zijn

  imputatie_gebruiken <- NULL

  for (var in names(waarden_echt)) {
    pattern <- paste0("[A-Z]*\\.", gsub("Echt\\.", "", var), "$")
    count <- sum(stringr::str_detect(names(interstat_i_zonder_na), pattern), na.rm = T)

    if(count >= 1) {
      imputatie_gebruiken <- c(imputatie_gebruiken, var)
    }
  }

  if (length(imputatie_gebruiken) > 0) {
    ## imputaties echte consistentievariabelen invullen in consistentieregels
    invullen_echt <- echt_imp %>%
      dplyr::filter(!is.na(imp_def), variable %in% imputatie_gebruiken) %>%
      dplyr::select(variable, imp_def) %>%
      tidyr::spread(variable, imp_def)

    validate::voptions(rules_interstat_i_post, lin.eq.eps=0.001,lin.ineq.eps=0.001)

    rules_interstat_i_post <- validatetools::substitute_values(rules_interstat_i_post,
                                                               as.list(invullen_echt),
                                                               .add_constraints = FALSE)
  }

  ## boven- en ondergrenzen op waargenomen waarden consistentievariabelen afleiden
  ## per statistiek -- hier alleen voor PS
  # (om rekentijd te beperken: beschouw elke variabele apart)

  te_verwerken_variabelen <- variables(rules_interstat_i_post)
  te_verwerken_variabelen <- te_verwerken_variabelen[grep('^PS.', te_verwerken_variabelen)]

  randen_PS <- NULL
  rules_interstat_i_post_txt <- as.data.frame(rules_interstat_i_post)$rule
  for (v in te_verwerken_variabelen) {
    temp_rules <- rules_interstat_i_post[grep(paste0(v, '[^_]'), rules_interstat_i_post_txt)]
    temp <- validatetools::detect_boundary_num(temp_rules)
    randen_PS <- rbind(randen_PS,
                       temp %>% dplyr::filter(variable == v))
  }

  randen_PS <- randen_PS %>%
    dplyr::filter(grepl('PS.', variable),
                  (is.finite(lowerbound) & abs(lowerbound) <= 1e7 - 1) |
                    (is.finite(upperbound) & abs(upperbound) <= 1e7 - 1))

  ## extra regels afleiden voor foutlokalisatie PS
  # deze horen bij alle PS-consistentievariabelen waarvoor een onder- en/of bovengrens zijn afgeleid

  # (hier evt. nog grenswaarden afronden op een vast aantal decimalen??)

  extra_regels_a <- randen_PS %>%
    dplyr::filter(is.finite(lowerbound), abs(lowerbound) <= 1e7 - 1) %>%
    dplyr::mutate(name = paste0(variable, '_a'),
                  rule = paste0(variable, ' >= ', lowerbound)) %>%
    dplyr::select(c(name, rule))

  extra_regels_b <- randen_PS %>%
    dplyr::filter(is.finite(upperbound), abs(upperbound) <= 1e7 - 1) %>%
    dplyr::mutate(name = paste0(variable, '_b'),
                  rule = paste0(variable, ' <= ', upperbound)) %>%
    dplyr::select(c(name, rule))


  #Extra regels om te voorkomen dat voorraad mutatie te veel wordt aangepast
  # TO DO: dit buiten de code trekken en regels als inputbestand meegeven
  grens <- min(echt_imp$imp_def[echt_imp$variable == paste0("Echt.", naam_omzetvar)] * 0.1, 1000, na.rm = T)
  correctie_grenzen <- data.frame("variable" = c("PS.VOORRAD100900",
                                                 "PS.OPBRENG100000"),
                                  "lowerbound" = c(sum(c(data_row$PS.VOORRAD100900, -grens), na.rm = T),
                                                   sum(c(data_row$PS.OPBRENG100000, -grens), na.rm = T)),
                                  "upperbound" = c(sum(c(data_row$PS.VOORRAD100900, grens), na.rm = T),
                                                   sum(c(data_row$PS.OPBRENG100000, grens), na.rm = T))
  )

  correctie_grenzen <- correctie_grenzen %>% dplyr::filter(variable %in% vars_in_rules)

  extra_regels_c <- correctie_grenzen %>%
    dplyr::mutate(name = paste0(variable, '_c'),
                  rule = paste0(variable, ' >= ', lowerbound)) %>%
    dplyr::select(c(name, rule))

  extra_regels_d <- correctie_grenzen %>%
    dplyr::mutate(name = paste0(variable, '_d'),
                  rule = paste0(variable, ' <= ', upperbound)) %>%
    dplyr::select(c(name, rule))


  extra_regels <- extra_regels_a %>%
    dplyr::bind_rows(extra_regels_b) %>%
    dplyr::bind_rows(extra_regels_c) %>%
    dplyr::bind_rows(extra_regels_d)

  row.names(extra_regels) <- NULL

  results$extra_regels <- extra_regels

  if(nrow(results$extra_regels) > 0) {
    results$extra_regels$BE_ID <- selected_be_id
  }

  results$randen <- randen %>%
    dplyr::mutate(BE_ID = selected_be_id)
  row.names(results$randen) <- NULL

  if(nrow(results$extra_regels) > 0) {
    rules_extra <- validate::validator(.data = extra_regels)
    rules_i <- rules + rules_extra
  } else {
    rules_i <- rules
  }

  stap2_gelukt <- TRUE

} else {
  stap2_gelukt <- FALSE
}

