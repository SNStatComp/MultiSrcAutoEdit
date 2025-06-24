
### Note: this script should be called from dataprep_beforehand.R ###

print(stringr::str_glue('Determine reference units with only small differences between sources...'))

# Function to compute relative differences between sources
# min.abs: absolute differences smaller than this value do not count as differences
relDif <- function(a, min.abs = 100) {
  paren <- expand.grid(i = 1:length(a), j = 1:length(a))
  paren <- subset(paren, i != j)
  paren$reldif <- apply(paren, 1, function(m) {
    ifelse(a[m[2]] != 0,
           ifelse(abs(a[m[1]] - a[m[2]]) >= min.abs,
                  abs((a[m[1]] - a[m[2]])/a[m[2]]),
                  0),
           NA)
  })

  res <- suppressWarnings(max(paren$reldif, na.rm = TRUE))
  return(ifelse(res >= 0, res, NA))
}

## Compute relative differences for each common variable
for (v in common_vars) {

  # data of current year
  temp <- data %>%
    dplyr::select(dplyr::ends_with(v)) %>%
    dplyr::rename_with(.fn = function(x) gsub(x, pattern = paste0('[.]', v), replacement = ''))

  data <- data %>%
    dplyr::mutate(a = apply(temp, 1, relDif,
                            min.abs = ifelse(v %in% count_vars,
                                             2*min(LOWER_BOUND_ABS_COUNT, UPPER_BOUND_ABS_COUNT),
                                             2*min(LOWER_BOUND_ABS, UPPER_BOUND_ABS)))) %>%
    dplyr::rename_with(.cols = a, .fn = function(x) paste0('reldif_', v))

  # data of previous year
  temp <- data_Tm1 %>%
    dplyr::select(dplyr::ends_with(v)) %>%
    dplyr::rename_with(.fn = function(x) gsub(x, pattern = paste0('[.]', v), replacement = ''))

  data_Tm1 <- data_Tm1 %>%
    dplyr::mutate(a = apply(temp, 1, relDif,
                            min.abs = ifelse(v %in% count_vars,
                                             2*min(LOWER_BOUND_ABS_COUNT, UPPER_BOUND_ABS_COUNT),
                                             2*min(LOWER_BOUND_ABS, UPPER_BOUND_ABS)))) %>%
    dplyr::rename_with(.cols = a, .fn = function(x) paste0('reldif_', v))
}

## Compute maximal relative difference across all common variables

# data of current year
reldif <- data %>% dplyr::select(dplyr::contains('reldif'))

data <- data %>%
  dplyr::mutate(reldif_max = apply(reldif, 1, function(m) {
    a <- suppressWarnings(max(m, na.rm = TRUE))
    return(ifelse(a >= 0, a, NA))
  }))
print(summary(data$reldif_max))

data <- data %>%
  dplyr::mutate(useAsReference = is.na(reldif_max) | reldif_max <= 2*min(1 - LOWER_BOUND_REL, UPPER_BOUND_REL - 1))


# data of previous year
reldif_Tm1 <- data_Tm1 %>% dplyr::select(dplyr::contains('reldif'))

data_Tm1 <- data_Tm1 %>%
  dplyr::mutate(reldif_max = apply(reldif_Tm1, 1, function(m) {
    a <- suppressWarnings(max(m, na.rm = TRUE))
    return(ifelse(a >= 0, a, NA))
  }))
print(summary(data_Tm1$reldif_max))

data_Tm1 <- data_Tm1 %>%
  dplyr::mutate(useAsReference = is.na(reldif_max) | reldif_max <= 2*min(1 - LOWER_BOUND_REL, UPPER_BOUND_REL - 1))
