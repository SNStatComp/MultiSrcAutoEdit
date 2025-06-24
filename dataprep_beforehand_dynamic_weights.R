
### Note: this script should be called from dataprep_beforehand.R ###

print(stringr::str_glue('Compute dynamic reliability weights...'))

### auxiliary functions

find_nearest_neighbour <- function(dat, raw, ref,
                                   return.factor = TRUE) {
  # return.factor = FALSE: return relative contributions to distance function
  # return.factor = TRUE: return relative contributions divided by the mean contribution

  yraw <- dat %>%
    dplyr::filter(BE_ID == raw & Jaar == YEAR) %>%
    dplyr::select(dplyr::all_of(relevant_variables.current)) %>%
    data.matrix() %>%
    t()

  yref <- dat %>%
    dplyr::filter(BE_ID.Jaar %in% ref) %>%
    dplyr::select(dplyr::all_of(relevant_variables.current)) %>%
    data.matrix() %>%
    t()

  medref <- apply(yref, 1, median, na.rm = TRUE)
  sref <- apply(yref, 1, quantile, probs = S_QUANTILE_UPPER, na.rm = TRUE) -
    apply(yref, 1, quantile, probs = S_QUANTILE_LOWER, na.rm = TRUE)

  yraw0 <- yraw[sref != 0, , drop = FALSE]
  yref0 <- yref[sref != 0, , drop = FALSE]
  medref0 <- medref[sref != 0]
  sref0 <- sref[sref != 0]

  # rescaling
  xref <- sweep(yref0, MARGIN = 1, medref0, FUN = '-')
  xref <- sweep(xref, MARGIN = 1, sref0, FUN = '/')
  # same rescaling on yraw
  xraw <- sweep(yraw0, MARGIN = 1, medref0, FUN = '-')
  xraw <- sweep(xraw, MARGIN = 1, sref0, FUN = '/')

  # compute distances between xraw en xref
  dmat <- abs(c(xraw) - xref)
  dsum <- colSums(dmat, na.rm = TRUE)

  # find nearest neighbour
  l <- which.min(dsum)
  z0 <- dmat[ ,l]/dsum[l]

  z <- setNames(as.integer(yraw != medref), row.names(yraw))
  z[names(z0)] <- z0
  # for variables that were included in the distance function,
  # z contains the relative contribution of that variable to dsum[l];
  # for all other variables (with sref = 0),
  # z = 0 if yraw = medref and z = 1 otherwise

  if (return.factor == TRUE) {
    mn <- 1/length(z0)
    return(z/mn)
  } else {
    return(z)
  }
}

weight_function <- function(z, a = 1) {
  weight <- 10 - 9*z^(1/a)
  return(weight)
}

###

pattern.temp <- paste0('{',
                       stringr::str_flatten(bronvariabelen, collapse = '}{'),
                       '}')

inputdata_gewichten <- rbind(
  data %>%
    dplyr::select(c(BE_ID, OG_ID,
                    dplyr::any_of(all_vars),
                    dplyr::all_of(bronvariabelen),
                    dplyr::starts_with('SbiGecoordineerd'),
                    dplyr::starts_with('GkSbsGecoordineerd'),
                    useAsReference, een)) %>%
    dplyr::mutate(Jaar = YEAR),
  data_Tm1 %>%
    dplyr::select(c(BE_ID, OG_ID,
                    dplyr::any_of(all_vars),
                    dplyr::all_of(bronvariabelen),
                    dplyr::starts_with('SbiGecoordineerd'),
                    dplyr::starts_with('GkSbsGecoordineerd'),
                    useAsReference, een)) %>%
    dplyr::mutate(Jaar = YEAR - 1)
) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(bronvariabelen),
                              function(x) tidyr::replace_na(x, replace = 0)),
                pattern.exist = stringr::str_glue(pattern.temp),
                BE_ID.Jaar = stringr::str_glue('{BE_ID}.{Jaar}'))

patterns <- inputdata_gewichten %>%
  dplyr::group_by(pattern.exist) %>%
  dplyr::summarise(n = sum(Jaar == YEAR),
                   nref = sum(Jaar == YEAR & useAsReference)) %>%
  dplyr::filter(n - nref > 0) # patterns with no records requiring editing can be skipped

K <- nrow(patterns)

## process each pattern separately
# do this in parallel to save time
# (for now: assume that this code is run on ALP, not calculator)

cores <- max(1, parallel::detectCores() - 1)
logfile <- file.path(OutputFolder, glue::glue('{prefix_output}log_dynamic_weights.txt'))
cl <- parallel::makeCluster(cores[1], outfile = logfile)
doParallel::registerDoParallel(cl)

data_weights_new <- foreach::foreach(k = 1:K,
                                     .combine = rbind,
                                     .inorder = TRUE,
                                     .verbose = TRUE,
                                     .packages = c('stringr', 'dplyr', 'tidyr', 'accumulate'),
                                     .export = ls(.GlobalEnv)) %dopar% {

                                       print(stringr::str_glue('Start processing pattern {k} of {K}'))

                                       pattern.current <- patterns$pattern.exist[k]
                                       pattern.current.vec <- as.integer(stringr::str_split_1(pattern.current,
                                                                                              pattern = ''))

                                       bronvariabelen.current <- bronvariabelen[pattern.current.vec == 1]
                                       names_sources.current <- names_sources[pattern.current.vec == 1]
                                       relevant_variables.current <- all_vars[stringr::str_extract(all_vars, pattern = '^[[:alpha:]]*[.]') %in%
                                                                                paste0(names_sources.current, '.')]

                                       # suitable donors (for units with the current pattern):
                                       # all units which occur at least in the same subset of sources
                                       # and have useAsReference == TRUE
                                       inputdata_gewichten <- inputdata_gewichten %>%
                                         dplyr::mutate(donor_suitable = dplyr::if_all(dplyr::all_of(bronvariabelen.current),
                                                                                      .fns = ~ . == 1) & useAsReference)

                                       if (sum(inputdata_gewichten$donor_suitable, na.rm = TRUE) >= MIN_STRATUM_WEIGHTS) {

                                         find_donors <- accumulate::cumulate(
                                           data = inputdata_gewichten %>%
                                             dplyr::select(BE_ID, SbiGecoordineerd3D, SbiGecoordineerd2D,
                                                           GkSbsGecoordineerd1D, GkSbsGecoordineerdSML,
                                                           pattern.exist, donor_suitable, Jaar, een, BE_ID.Jaar) %>%
                                             dplyr::filter((pattern.exist == pattern.current) | donor_suitable),
                                           collapse = COLLAPSE_DYNAMIC_WEIGHTS,
                                           test = function(d) sum(d$donor_suitable) >= MIN_STRATUM_WEIGHTS,
                                           donors = BE_ID.Jaar[donor_suitable == TRUE])

                                         # only keep units for which a donor is needed
                                         relevant_donors <- find_donors %>%
                                           dplyr::filter(Jaar == YEAR) %>%
                                           dplyr::inner_join(inputdata_gewichten %>%
                                                               dplyr::filter(!donor_suitable & Jaar == YEAR) %>%
                                                               dplyr::select(BE_ID),
                                                             by = 'BE_ID')

                                         distances_nearest_neighbour <- sapply(1:nrow(relevant_donors),
                                                                               function(i) find_nearest_neighbour(
                                                                                 dat = inputdata_gewichten,
                                                                                 raw = relevant_donors$BE_ID[i],
                                                                                 ref = relevant_donors$donors[[i]],
                                                                                 return.factor = (weight_method == 'factor')
                                                                               ))

                                         data_weights_temp <- data_weights[match(relevant_donors$BE_ID,
                                                                                 data_weights$BE_ID), ]

                                         if (weight_method == 'weight_function') {

                                           weights_nearest_neighbour <- t(apply(distances_nearest_neighbour,
                                                                                2, weight_function, a = a_weight))

                                           data_weights_temp[ , relevant_variables.current] <-
                                             weights_nearest_neighbour[ , relevant_variables.current]

                                         } else if (weight_method == 'factor') {

                                           weights_factor_nearest_neighbour <- 1 + t(apply(distances_nearest_neighbour,
                                                                                           2, floor))

                                           data_weights_temp[ , relevant_variables.current] <-
                                             data_weights_temp[ , relevant_variables.current] /
                                             weights_factor_nearest_neighbour[ , relevant_variables.current]

                                         }

                                         data_weights_temp <- data_weights_temp %>%
                                           dplyr::mutate(dynamicWeights = TRUE)

                                       } else {
                                         print(stringr::str_glue('Not enough reference records available for pattern {pattern.current}'))
                                         print(stringr::str_glue('Weights are not altered'))

                                         relevant_donors <- inputdata_gewichten %>%
                                           dplyr::filter((pattern.exist == pattern.current) & !donor_suitable & Jaar == YEAR) %>%
                                           dplyr::select(BE_ID)

                                         data_weights_temp <- data_weights[match(relevant_donors$BE_ID,
                                                                                 data_weights$BE_ID), ]

                                         data_weights_temp <- data_weights_temp %>%
                                           dplyr::mutate(dynamicWeights = FALSE)

                                       }

                                       return(data_weights_temp)

} # end foreach loop

parallel::stopCluster(cl)


# combine all weights
data_weights_dyn <- data_weights
data_weights_dyn[match(data_weights_new$BE_ID, data_weights_dyn$BE_ID), ] <-
  data_weights_new[ , names(data_weights_dyn)]

data <- data %>%
  dplyr::left_join(inputdata_gewichten %>%
                     dplyr::filter(Jaar == YEAR) %>%
                     dplyr::select(c(BE_ID, pattern.exist)),
                   by = 'BE_ID') %>%
  dplyr::left_join(data_weights_new %>%
                     dplyr::select(c(BE_ID, dynamicWeights)),
                   by = 'BE_ID')


# check that data are still in the same order as when we derived the original weights
stopifnot(identical(data_weights_dyn$BE_ID, data_weights$BE_ID))
stopifnot(identical(data$BE_ID, data_weights$BE_ID))
