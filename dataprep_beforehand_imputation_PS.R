
### Note: this script should be called from dataprep_beforehand.R ###

print(stringr::str_glue('Collect and compute input ((T-1) values and stratum means) for imputation of PS variables...'))


# Focus on PS variables
data_PS <- data %>%
  dplyr::select(c(BE_ID, OG_ID, een,
                  dplyr::starts_with('SbiGecoordineerd'),
                  dplyr::starts_with('GkSbsGecoordineerd'),
                  dplyr::all_of(dplyr::starts_with('PS.'))))

data_PS_Tm1 <- data_Tm1 %>%
  dplyr::select(c(BE_ID, OG_ID,
                  dplyr::all_of(dplyr::starts_with('PS.')))) %>%
  dplyr::filter(PS.IsGeimputeerd == 0) %>%
  dplyr::select(-dplyr::any_of(c('PS.InsluitGewicht', 'PS.Insluitgewicht',
                                 'PS.STATUS_CP', 'PS.STATUS_IG',
                                 'PS.VragenlijstID',
                                 'PS.WerkzamePersonen', 'PS.WP_origineel',
                                 'PS.IsGeimputeerd', 'PS.exist'))) %>%
  dplyr::rename_with(.cols = -c(BE_ID, OG_ID),
                     .fn = function(x) stringr::str_glue('{x}.Tm1')) %>%
  dplyr::mutate(dplyr::across(-c(BE_ID, OG_ID), ~tidyr::replace_na(.x, replace = 0)))


# Collect (T-1) values (where available)
data <- data %>%
  dplyr::left_join(data_PS_Tm1,
                   by = c('BE_ID', 'OG_ID'))


# Determine subset of suitable reference records for stratum means year T
cf_PS_auto_mat <- do.call(rbind,
                          lapply(cf_PS_auto_list, function(l) {
                            data.frame(BE_ID = l$BE_ID,
                                       n_violated = rowSums(!(l %>% dplyr::select(-BE_ID)),
                                                            na.rm = TRUE))
                          }))
row.names(cf_PS_auto_mat) <- NULL

data_PS_sub <- data_PS %>%
  dplyr::left_join(cf_PS_auto_mat,
                   by = 'BE_ID') %>%
  dplyr::filter(n_violated == 0) %>%
  dplyr::select(-c(n_violated, PS.exist)) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with('PS.'),
                              ~tidyr::replace_na(.x, replace = 0)))
# For the purpose of computing means:
# replace item nonresponse by 0


## Compute means by stratum
# Use accumulate package to collapse empty strata

## compute, as input for imputation methods:
#  1) stratum ratios w.r.t. naam_omzetvar_PS
#  2) stratum ratios w.r.t. naam_wpvar_PS
#  3) stratum means

res_PS_temp1 <- accumulate::accumulate(
  data = data_PS_sub %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with('PS.') &
                                  !dplyr::ends_with(c('VragenlijstID', 'Insluitgewicht')),
                                function(x) x * PS.Insluitgewicht)) %>%
    dplyr::select(-c(dplyr::contains('PERSON', ignore.case = FALSE),
                     dplyr::contains('SUBTOWP'),
                     SbiGecoordineerd, GkSbsGecoordineerd,
                     PS.Insluitgewicht, BE_ID, OG_ID)) %>%
    dplyr::arrange(PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D),
  collapse = COLLAPSE_IMPUTATION_PS,
  test = min_complete(n = MIN_STRATUM_MEAN),
  fun = sum, na.rm = TRUE
) %>%
  dplyr::rename_with(.cols = -c(level, PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D),
                     .fn = function(x) stringr::str_glue('{x}_total')) %>%
  dplyr::mutate(dplyr::across(dplyr::ends_with('_total'),
                              function(x) x / .data[[stringr::str_glue('{naam_omzetvar_PS}_total')]])) %>%
  dplyr::rename_with(.cols = dplyr::ends_with('_total'),
                     .fn = function(x) stringr::str_replace(string = x,
                                                            pattern = '_total$',
                                                            replacement = '_ratio_omzetPS'))

res_PS_temp2 <- accumulate::accumulate(
  data = data_PS_sub %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with('PS.') &
                                  !dplyr::ends_with(c('VragenlijstID', 'Insluitgewicht')),
                                function(x) x * PS.Insluitgewicht)) %>%
    dplyr::select(-c(dplyr::starts_with('PS.') &
                       !(dplyr::contains('PERSON', ignore.case = FALSE) |
                           dplyr::contains('SUBTOWP') |
                           dplyr::contains(naam_wpvar_PS) |
                           dplyr::contains('VragenlijstID')),
                     SbiGecoordineerd, GkSbsGecoordineerd,
                     PS.Insluitgewicht, BE_ID, OG_ID)) %>%
    dplyr::arrange(PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D),
  collapse = COLLAPSE_IMPUTATION_PS,
  test = min_complete(n = MIN_STRATUM_MEAN),
  fun = sum, na.rm = TRUE
) %>%
  dplyr::rename_with(.cols = -c(level, PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D),
                     .fn = function(x) stringr::str_glue('{x}_total')) %>%
  dplyr::mutate(dplyr::across(dplyr::ends_with('_total'),
                              function(x) x / .data[[stringr::str_glue('{naam_wpvar_PS}_total')]])) %>%
  dplyr::rename_with(.cols = dplyr::ends_with('_total'),
                     .fn = function(x) stringr::str_replace(string = x,
                                                            pattern = '_total$',
                                                            replacement = '_ratio_wpPS'))

res_PS_temp3 <- accumulate::accumulate(
  data = data_PS_sub %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with('PS.') &
                                  !dplyr::ends_with(c('VragenlijstID', 'Insluitgewicht')),
                                function(x) x * PS.Insluitgewicht)) %>%
    dplyr::select(-c(SbiGecoordineerd, GkSbsGecoordineerd, BE_ID, OG_ID)) %>%
    dplyr::arrange(PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D),
  collapse = COLLAPSE_IMPUTATION_PS,
  test = min_complete(n = MIN_STRATUM_MEAN),
  fun = sum, na.rm = TRUE
) %>%
  dplyr::rename_with(.cols = -c(level, PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D),
                     .fn = function(x) stringr::str_glue('{x}_total')) %>%
  dplyr::mutate(dplyr::across(dplyr::ends_with('_total'),
                              function(x) x / .data[['PS.Insluitgewicht_total']])) %>%
  dplyr::select(-PS.Insluitgewicht_total) %>%
  dplyr::rename_with(.cols = dplyr::ends_with('_total'),
                     .fn = function(x) stringr::str_replace(string = x,
                                                            pattern = '_total$',
                                                            replacement = '_gem'))

res_PS_temp <- res_PS_temp1 %>%
  dplyr::select(c(PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D,
                  dplyr::ends_with('_ratio_omzetPS'))) %>%
  dplyr::full_join(res_PS_temp2 %>%
                     dplyr::select(c(PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D,
                                     dplyr::ends_with('_ratio_wpPS'))),
                   by = c('PS.VragenlijstID', 'SbiGecoordineerd4D', 'GkSbsGecoordineerd1D')) %>%
  dplyr::full_join(res_PS_temp3 %>%
                     dplyr::select(c(PS.VragenlijstID, SbiGecoordineerd4D, GkSbsGecoordineerd1D,
                                     dplyr::ends_with('_gem'))),
                   by = c('PS.VragenlijstID', 'SbiGecoordineerd4D', 'GkSbsGecoordineerd1D'))



## Merge stratum means with original data (of current year)

data <- data %>%
  dplyr::left_join(res_PS_temp,
                   by = c('PS.VragenlijstID', 'SbiGecoordineerd4D', 'GkSbsGecoordineerd1D'))


# check that data are still in the same order as when we derived the weights
stopifnot(identical(data$BE_ID, data_weights$BE_ID))

