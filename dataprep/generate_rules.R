library(dplyr)
library(stringr)
library(tidyr)
library(readxl)

rm(list = ls())
gc()


# load constants ----
source(stringr::str_glue('./input_files/constants.R'))


# helper functions ----

create_interstat_rules <- function(rule_matrix) {
  rule_matrix <- rule_matrix %>%
    dplyr::rename(basename = Naam)

  echt_matrix <- rule_matrix %>%
    dplyr::select(basename) %>%
    dplyr::mutate(src_echt = 'Echt')

  rule_combinations <- get_rule_combinations(rule_matrix, echt_matrix)

  rule_combinations_lower_bound <-
    create_bounded_rules(rule_combinations, " >= ", LOWER_BOUND_REL, "_a") %>%
    dplyr::mutate(lb_abs = if_else(basename %in% count_vars, LOWER_BOUND_ABS_COUNT, LOWER_BOUND_ABS),
                  ub_abs = if_else(basename %in% count_vars, UPPER_BOUND_ABS_COUNT, UPPER_BOUND_ABS))

  rule_combinations_upper_bound <-
    create_bounded_rules(rule_combinations, " <= ", UPPER_BOUND_REL, "_b") %>%
    dplyr::mutate(lb_abs = if_else(basename %in% count_vars, LOWER_BOUND_ABS_COUNT, LOWER_BOUND_ABS),
                  ub_abs = if_else(basename %in% count_vars, UPPER_BOUND_ABS_COUNT, UPPER_BOUND_ABS))

  rule_combinations_lower_bound <-
    make_rules_conditional(rule_combinations_lower_bound, type = 'lower',
                           neg_vars, " >= ", " <= ", method_bound_abs) %>%
    dplyr::select(name, rule)

  rule_combinations_upper_bound <-
    make_rules_conditional(rule_combinations_upper_bound, type = 'upper',
                           neg_vars, " <= ", " >= ", method_bound_abs) %>%
    dplyr::select(name, rule)

  # non-negativity rules
  nneg_rules_echt <- echt_matrix %>%
    dplyr::filter(!(basename %in% neg_vars)) %>%
    dplyr::mutate(name = paste0(basename, '_nneg'),
                  rule = paste0(src_echt, '.', basename, ' >= 0')) %>%
    dplyr::select(-c(basename, src_echt))

  nneg_rules_src <- rule_combinations %>%
    dplyr::filter(!(basename %in% neg_vars)) %>%
    dplyr::mutate(name = paste0(basename, '_', src, '_nneg'),
                  rule = paste0(src, '.', basename, ' >= 0')) %>%
    dplyr::select(c(name, rule))

  interstat_rules <-
    rbind(
      rule_combinations_lower_bound,
      rule_combinations_upper_bound,
      nneg_rules_echt,
      nneg_rules_src
    )
}


get_rule_combinations <- function(rule_matrix, echt_matrix) {
  flattened_matrix <-
    tidyr::gather(rule_matrix, src, equation, -basename, factor_key = TRUE) %>%
    tidyr::drop_na(equation)

  rule_combinations <-
    flattened_matrix %>%
    dplyr::inner_join(echt_matrix, by = "basename") %>%
    dplyr::mutate(name = paste0(basename, "_", src))
}


create_bounded_rules <-
  function(rule_combinations,
           operator,
           multiplier,
           name_suffix) {
    prefix <- paste0(multiplier, " * ")
    bounded_rules <- rule_combinations %>%
      dplyr::rowwise() %>%
      dplyr::mutate(equation.x = paste0(src, '.', basename)) %>%
      dplyr::mutate(equation.y = paste0(src_echt, '.', basename)) %>%
      dplyr::mutate(name = paste0(basename, name_suffix)) %>%
      dplyr::mutate(rule = paste0(equation.x, operator, prefix, equation.y))
  }

make_rules_conditional <-
  function(rule_combinations,
           type = c('lower','upper'),
           neg_vars,
           operator_orig,
           operator_neg,
           method_bound_abs = 'exact') {

    if (method_bound_abs == 'exact') {
      make_rules_conditional_exact(rule_combinations,
                                   type,
                                   neg_vars,
                                   operator_orig,
                                   operator_neg)
    } else if (method_bound_abs == 'approx') {
      make_rules_conditional_approx(rule_combinations,
                                    type,
                                    neg_vars,
                                    operator_orig,
                                    operator_neg)
    }

  }


make_rules_conditional_exact <-
  function(rule_combinations,
           type = c('lower','upper'),
           neg_vars,
           operator_orig,
           operator_neg) {

    rule_combinations_pos <- rule_combinations %>%
      dplyr::filter(!(basename %in% neg_vars))

    if (type == 'lower') {
      rule_combinations_pos <- rule_combinations_pos %>%
        dplyr::mutate(rule = paste0('if (', equation.x, ' < ', equation.y, ' - ', lb_abs, ') ', rule),
                      name = paste0(name, 'p'))
    } else if (type == 'upper') {
      rule_combinations_pos <- rule_combinations_pos %>%
        dplyr::mutate(rule = paste0('if (', equation.x, ' > ', equation.y, ' + ', ub_abs, ') ', rule),
                      name = paste0(name, 'p'))
    }

    rule_combinations_neg <- rule_combinations %>%
      dplyr::filter(basename %in% neg_vars)

    if (type == 'lower') {
      rule_combinations_neg_p <- rule_combinations_neg %>%
        dplyr::mutate(rule = paste0('if (', equation.y, ' >= 0 & ', equation.x, ' < ', equation.y, ' - ', lb_abs, ') ', rule),
                      name = paste0(name, 'p'))
      rule_combinations_neg_n <- rule_combinations_neg %>%
        dplyr::mutate(rule = gsub(pattern = operator_orig, replacement = operator_neg, rule)) %>%
        dplyr::mutate(rule = paste0('if (', equation.y, ' < 0 & ', equation.x, ' > ', equation.y, ' + ', ub_abs, ') ', rule),
                      name = paste0(name, 'n'))
    } else if (type == 'upper') {
      rule_combinations_neg_p <- rule_combinations_neg %>%
        dplyr::mutate(rule = paste0('if (', equation.y, ' >= 0 & ', equation.x, ' > ', equation.y, ' + ', ub_abs, ') ', rule),
                      name = paste0(name, 'p'))
      rule_combinations_neg_n <- rule_combinations_neg %>%
        dplyr::mutate(rule = gsub(pattern = operator_orig, replacement = operator_neg, rule)) %>%
        dplyr::mutate(rule = paste0('if (', equation.y, ' < 0 & ', equation.x, ' < ', equation.y, ' - ', lb_abs, ') ', rule),
                      name = paste0(name, 'n'))
    }

    return(rbind(rule_combinations_pos,
                 rule_combinations_neg_p,
                 rule_combinations_neg_n))
  }


make_rules_conditional_approx <-
  function(rule_combinations,
           type = c('lower','upper'),
           neg_vars,
           operator_orig,
           operator_neg) {

    rule_combinations_pos <- rule_combinations %>%
      dplyr::filter(!(basename %in% neg_vars))

    if (type == 'lower') {
      rule_combinations_pos <- rule_combinations_pos %>%
        dplyr::mutate(rule = paste0(rule, ' - ', lb_abs),
                      name = paste0(name, 'p'))
    } else if (type == 'upper') {
      rule_combinations_pos <- rule_combinations_pos %>%
        dplyr::mutate(rule = paste0(rule, ' + ', ub_abs),
                      name = paste0(name, 'p'))
    }

    rule_combinations_neg <- rule_combinations %>%
      dplyr::filter(basename %in% neg_vars)

    if (type == 'lower') {
      rule_combinations_neg_p <- rule_combinations_neg %>%
        dplyr::mutate(rule = paste0('if (', equation.y, ' >= 0) ', rule, ' - ', lb_abs),
                      name = paste0(name, 'p'))
      rule_combinations_neg_n <- rule_combinations_neg %>%
        dplyr::mutate(rule = gsub(pattern = operator_orig, replacement = operator_neg, rule)) %>%
        dplyr::mutate(rule = paste0('if (', equation.y, ' < 0) ', rule, ' + ', ub_abs),
                      name = paste0(name, 'n'))
    } else if (type == 'upper') {
      rule_combinations_neg_p <- rule_combinations_neg %>%
        dplyr::mutate(rule = paste0('if (', equation.y, ' >= 0) ', rule, ' + ', ub_abs),
                      name = paste0(name, 'p'))
      rule_combinations_neg_n <- rule_combinations_neg %>%
        dplyr::mutate(rule = gsub(pattern = operator_orig, replacement = operator_neg, rule)) %>%
        dplyr::mutate(rule = paste0('if (', equation.y, ' < 0) ', rule, ' - ', lb_abs),
                      name = paste0(name, 'n'))
    }

    return(rbind(rule_combinations_pos,
                 rule_combinations_neg_p,
                 rule_combinations_neg_n))
  }


derive_stat_rules <- function(rule_matrix, statnaam) {
  sel_rule_matrix <- rule_matrix %>%
    dplyr::select(name = Naam, rule = {statnaam}) %>%
    dplyr::filter(!is.na(rule))

  conds <- sapply(sel_rule_matrix$rule, modify_conditional_rules, statnaam = statnaam)
  which_conds <- which(sapply(conds, length) == 2)

  if (length(which_conds) == 0) {

    sel_rules <- sel_rule_matrix %>%
      dplyr::mutate(rule = paste0(statnaam, '.', name, ' == ', rule),
                    condition = TRUE)

  } else {

    # split conditional rules into separate rules
    rules_extra <- sel_rule_matrix[which_conds, ]
    rules_extra <- replicate(n = 2, rules_extra, simplify = FALSE)
    rules_extra <- do.call(rbind, rules_extra) %>%
      mutate(rule = do.call(c, lapply(conds[which_conds], function(l) l[[2]])),
             condition = do.call(c,
                                 lapply(conds[which_conds], function(l) c(l[[1]], paste0('!', l[[1]])))))

    sel_rules <- rbind(sel_rule_matrix[-which_conds, ] %>%
                         dplyr::mutate(rule = paste0(statnaam, '.', name, ' == ', rule),
                                       condition = TRUE),
                       rules_extra %>%
                         dplyr::mutate(rule = paste0(statnaam, '.', name, ' == ', rule))
    )
  }

  return(sel_rules)
}


modify_conditional_rules <- function(rule, statnaam) {

  cond <- stringr::str_extract(rule, 'ifelse[(][[:print:]]+[)]')

  if(!is.na(cond)) {
    # extract clauses from conditional part of rule
    cond_true <- stringr::str_extract(cond, paste0('[,] ', statnaam, '[[:print:]]+?[,]'))
    cond_rest <- gsub(pattern = paste0('[,] ', statnaam, '[[:print:]]+?[,]'), replacement = 'XXX', cond)
    cond_false <- stringr::str_extract(cond_rest, 'XXX [[:print:]]+[)]')
    cond_rest <- gsub(pattern = 'XXX [[:print:]]+[)]', replacement = 'XXX', cond_rest)
    cond_test <- stringr::str_extract(cond_rest, 'ifelse[(][[:print:]]+XXX')

    cond_test <- gsub(pattern = '^ifelse[(]', replacement = '', cond_test)
    cond_test <- gsub(pattern = 'XXX', replacement = '', cond_test)
    cond_true <- gsub(pattern = '[,]', replacement = '', cond_true)
    cond_false <- gsub(pattern = '[)]$', replacement = '', cond_false)
    cond_false <- gsub(pattern = 'XXX', replacement = '', cond_false)

    # create two versions of rule: when cond_test == TRUE and when cond_test == FALSE
    rule_begin <- gsub(pattern = 'ifelse[(][[:print:]]+[)]', replacement = '', rule)
    rule_true <- paste(rule_begin, cond_true)
    rule_false <- paste(rule_begin, cond_false)

    return(list(cond_test, c(rule_true, rule_false)))
  } else {
    return(rule)
  }
}


# create interstat rules ----

rule_matrix <- read.csv2(
  stringr::str_glue('./input_files/rule_matrix_{YEAR}.csv'),
  stringsAsFactors = FALSE,
  na.strings = c("NA", "")
)

rules <- create_interstat_rules(rule_matrix)

# read additional interstat rules from file
extra_rules <- read.csv2(
  stringr::str_glue('./input_files/additional_interstat_rules_{YEAR}.csv'),
  stringsAsFactors = FALSE
)

rules <- rbind(rules,
               extra_rules)

write.csv2(rules,
           stringr::str_glue('./edited_input/interstat_rules_{YEAR}.csv'),
           row.names = FALSE)



# read and select soft interstat rules based on prediction intervals ----
pred_rules_source <- rbind(
  readxl::read_xlsx(
    path = './input_files/PS_rules_summary_poly2.xlsx'
  ),
  readxl::read_xlsx(
    path = './input_files/INIVA_rules_summary_poly2.xlsx'
  )
)

pred_rules <- pred_rules_source %>%
  dplyr::filter(r_squared >= pred_rule_minR2) %>%
  dplyr::mutate(industry = dplyr::case_when(
    industry == "Bakkers" ~ "107",
    industry == "Bouw" ~ "412",
    industry == "Dienstverlening" ~ "620",
    industry == "Expediteurs" ~ "5229",
    industry == "GH" ~ "46",
    industry == "Goederen" ~ "494",
    industry == "Restaurants" ~ "561",
    .default = NA
  )) %>%
  dplyr::select(c(industry, gk1, rule_1, rule_2, target_variable, input_variable))

pred_rules <- pred_rules %>%
  dplyr::mutate(softvar = stringr::str_glue('S{1:nrow(pred_rules)}'),
                dplyr::across(dplyr::starts_with('rule_'),
                              ~stringr::str_replace_all(string = .x,
                                                        pattern = input_variable,
                                                        replacement = stringr::str_glue('Echt.{input_variable}'))),
                dplyr::across(dplyr::starts_with('rule_'),
                              ~stringr::str_replace_all(string = .x,
                                                        pattern = stringr::str_glue('^{target_variable}'),
                                                        replacement = stringr::str_glue('Echt.{target_variable}')))) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('rule'),
                      values_to = 'rule', names_to = 'nr') %>%
  dplyr::mutate(rule = stringr::str_glue('if ({softvar} == 0) {rule}')) %>%
  dplyr::mutate(name =
                  stringr::str_glue('{softvar}{stringr::str_replace(string = nr,
                                    pattern = "rule", replacement = "")}')) %>%
  dplyr::select(-c(nr, target_variable, input_variable))

write.csv2(pred_rules,
           stringr::str_glue('./edited_input/interstat_rules_soft_{YEAR}.csv'),
           row.names = FALSE)




# create confrontation rules for specific statistics (currently only PS)

rules_PS <- derive_stat_rules(rule_matrix, statnaam = 'PS')

write.csv2(rules_PS,
           stringr::str_glue('./edited_input/rules_conf_PS_{YEAR}.csv'),
           row.names = FALSE)


