
### Note: this script should be called from dataprep_beforehand.R ###

print(stringr::str_glue('Compute stratum means of common variables for imputation...'))

### Compute stratum means of common variables for imputation in step 2

# Determine subset of suitable reference records (combine years T and T-1)
data_sub <- rbind(
  data %>%
    dplyr::filter(useAsReference) %>%
    dplyr::select(c(dplyr::any_of(all_vars),
                    dplyr::all_of(bronvariabelen),
                    dplyr::starts_with('SbiGecoordineerd'),
                    dplyr::starts_with('GkSbsGecoordineerd'),
                    dplyr::ends_with('Insluitgewicht', ignore.case = FALSE),
                    een)) %>%
    dplyr::mutate(Jaar = YEAR),
  data_Tm1 %>%
    dplyr::filter(useAsReference) %>%
    dplyr::select(c(dplyr::any_of(all_vars),
                    dplyr::all_of(bronvariabelen),
                    dplyr::starts_with('SbiGecoordineerd'),
                    dplyr::starts_with('GkSbsGecoordineerd'),
                    dplyr::ends_with('Insluitgewicht', ignore.case = FALSE),
                    een)) %>%
    dplyr::mutate(Jaar = YEAR - 1)
)

# For the purpose of computing means:
# replace item nonresponse by 0 (but not unit nonresponse)
for (s in names_sources) {
  data_sub <- data_sub %>%
    dplyr::mutate(dplyr::across(dplyr::matches(paste0('^', s, '[.]', common_vars, '$')),
                                function(x) replace_na(x, replace = 0) * !!rlang::sym(paste0(s, '.exist'))))

}

# For each unit, compute mean of available values for each common variable
for (v in common_vars) {
  temp <- data_sub %>% dplyr::select(dplyr::ends_with(v))

  data_sub <- data_sub %>%
    dplyr::mutate(a = rowMeans(temp, na.rm = TRUE)) %>%
    dplyr::rename_with(.cols = a, .fn = function(x) paste0('Echt.', v))
}


## Compute mean by stratum
# Do this separately for each block of variables
# Use accumulate package to collapse empty strata

res <- NULL

for (b in names(variable_blocks)) {

  bron <- stringr::str_replace(string = b, pattern = '_[:alpha:]*$', replacement = '')
  useWIAinBlock <- stringr::str_detect(string = b, pattern = '_WIA$')

  if (bron == 'PS') { # use PS weights
    data_temp <- data_sub %>%
      dplyr::mutate(gewicht = PS.Insluitgewicht)
  } else { # for other surveys: no weights currently available in data file
    data_temp <- data_sub %>%
      dplyr::mutate(gewicht = 1)
  }

  if (useWIAinBlock) { # correct weights for presence/absence WIA
    data_temp <- data_temp %>%
      dplyr::mutate(gewicht = as.numeric(!is.na(WIA.exist)) +
                      gewicht * is.na(WIA.exist) * !is.na(get(stringr::str_glue('{bron}.exist'))))
  }

  ## compute, as input for imputation methods:
  #  1) stratum ratios w.r.t. naam_omzetvar
  #  2) stratum ratios w.r.t. naam_wpvar
  #  3) stratum means

  sel1 <- unique(c(stringr::str_glue('Echt.{variable_blocks[[b]]}'),
                   stringr::str_glue('Echt.{naam_omzetvar}')))
  res_temp1 <- accumulate::accumulate(
    data = data_temp %>%
      dplyr::select(c(dplyr::all_of(sel1),
                      SbiGecoordineerd3D, SbiGecoordineerd2D,
                      GkSbsGecoordineerd1D, GkSbsGecoordineerdSML,
                      een, Jaar, gewicht)) %>%
      dplyr::mutate(gewicht = if_else(dplyr::if_any(dplyr::all_of(sel1), is.na), NA, gewicht),
                    dplyr::across(dplyr::all_of(sel1),
                                  function(x) x * gewicht)) %>%
      dplyr::arrange(-Jaar, SbiGecoordineerd3D, GkSbsGecoordineerd1D),
    collapse = COLLAPSE_IMPUTATION,
    test = min_complete(n = MIN_STRATUM_MEAN),
    fun = sum, na.rm = TRUE
  ) %>%
    dplyr::rename_with(.cols = -c(Jaar, level, SbiGecoordineerd3D, GkSbsGecoordineerd1D),
                       .fn = function(x) stringr::str_glue('{x}_total')) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(stringr::str_glue('Echt.{variable_blocks[[b]]}_total')),
                                function(x) x / .data[[stringr::str_glue('Echt.{naam_omzetvar}_total')]])) %>%
    dplyr::rename_with(.cols = dplyr::all_of(stringr::str_glue('Echt.{variable_blocks[[b]]}_total')),
                       .fn = function(x) stringr::str_replace(string = x,
                                                              pattern = '_total$',
                                                              replacement = '_ratio_omzet'))

  sel2 <- unique(c(stringr::str_glue('Echt.{variable_blocks[[b]]}'),
                   stringr::str_glue('Echt.{naam_wpvar}')))
  res_temp2 <- accumulate::accumulate(
    data = data_temp %>%
      dplyr::select(c(dplyr::all_of(sel2),
                      SbiGecoordineerd3D, SbiGecoordineerd2D,
                      GkSbsGecoordineerd1D, GkSbsGecoordineerdSML,
                      een, Jaar, gewicht)) %>%
      dplyr::mutate(gewicht = if_else(dplyr::if_any(dplyr::all_of(sel2), is.na), NA, gewicht),
                    dplyr::across(dplyr::all_of(sel2),
                                  function(x) x * gewicht)) %>%
      dplyr::arrange(-Jaar, SbiGecoordineerd3D, GkSbsGecoordineerd1D),
    collapse = COLLAPSE_IMPUTATION,
    test = min_complete(n = MIN_STRATUM_MEAN),
    fun = sum, na.rm = TRUE
  ) %>%
    dplyr::rename_with(.cols = -c(Jaar, level, SbiGecoordineerd3D, GkSbsGecoordineerd1D),
                       .fn = function(x) stringr::str_glue('{x}_total')) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(stringr::str_glue('Echt.{variable_blocks[[b]]}_total')),
                                function(x) x / .data[[stringr::str_glue('Echt.{naam_wpvar}_total')]])) %>%
    dplyr::rename_with(.cols = dplyr::all_of(stringr::str_glue('Echt.{variable_blocks[[b]]}_total')),
                       .fn = function(x) stringr::str_replace(string = x,
                                                              pattern = '_total$',
                                                              replacement = '_ratio_wp'))

  sel3 <- stringr::str_glue('Echt.{variable_blocks[[b]]}')
  res_temp3 <- accumulate::accumulate(
    data = data_temp %>%
      dplyr::select(c(dplyr::all_of(sel3),
                      SbiGecoordineerd3D, SbiGecoordineerd2D,
                      GkSbsGecoordineerd1D, GkSbsGecoordineerdSML,
                      een, Jaar, gewicht)) %>%
      dplyr::mutate(gewicht = if_else(dplyr::if_any(dplyr::all_of(sel3), is.na), NA, gewicht),
                    dplyr::across(dplyr::all_of(sel3),
                                  function(x) x * gewicht)) %>%
      dplyr::arrange(-Jaar, SbiGecoordineerd3D, GkSbsGecoordineerd1D),
    collapse = COLLAPSE_IMPUTATION,
    test = min_complete(n = MIN_STRATUM_MEAN),
    fun = sum, na.rm = TRUE
  ) %>%
    dplyr::rename_with(.cols = -c(Jaar, level, SbiGecoordineerd3D, GkSbsGecoordineerd1D),
                       .fn = function(x) stringr::str_glue('{x}_total')) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(stringr::str_glue('Echt.{variable_blocks[[b]]}_total')),
                                function(x) x / .data[[stringr::str_glue('gewicht_total')]])) %>%
    dplyr::rename_with(.cols = dplyr::all_of(stringr::str_glue('Echt.{variable_blocks[[b]]}_total')),
                       .fn = function(x) stringr::str_replace(string = x,
                                                              pattern = '_total$',
                                                              replacement = '_gem'))

  res_temp <- res_temp1 %>%
    dplyr::select(c(Jaar, SbiGecoordineerd3D, GkSbsGecoordineerd1D,
                    dplyr::ends_with('_ratio_omzet'))) %>%
    dplyr::full_join(res_temp2 %>%
                       dplyr::select(c(Jaar, SbiGecoordineerd3D, GkSbsGecoordineerd1D,
                                       dplyr::ends_with('_ratio_wp'))),
                     by = c('Jaar', 'SbiGecoordineerd3D', 'GkSbsGecoordineerd1D')) %>%
    dplyr::full_join(res_temp3 %>%
                       dplyr::select(c(Jaar, SbiGecoordineerd3D, GkSbsGecoordineerd1D,
                                       dplyr::ends_with('_gem'))),
                     by = c('Jaar', 'SbiGecoordineerd3D', 'GkSbsGecoordineerd1D'))

  if (is.null(res)) {
    res <- res_temp
  } else {
    res <- res %>%
      dplyr::full_join(res_temp,
                       by = c('Jaar', 'SbiGecoordineerd3D', 'GkSbsGecoordineerd1D'))
  }

}


## Merge stratum means with original data (of current year)

data <- data %>%
  dplyr::select(-dplyr::starts_with('reldif')) %>%
  dplyr::left_join(res %>%
                     dplyr::filter(Jaar == YEAR) %>%
                     dplyr::select(-Jaar),
                   by = c('SbiGecoordineerd3D', 'GkSbsGecoordineerd1D'))


# check that data are still in the same order as when we derived the weights
stopifnot(identical(data$BE_ID, data_weights$BE_ID))

