
### Note: this script should be called from dataprep_beforehand.R ###

print(stringr::str_glue('Determine weights for soft interstat edit rules...'))


# Load soft interstat edit rules
rules_interstat_soft_bron <- read.csv2(file = file.path("edited_input", stringr::str_glue("interstat_rules_soft_{YEAR}.csv")))
softvars <- unique(rules_interstat_soft_bron$softvar)

data_weights_soft <- matrix(NA_real_, nrow = nrow(data), ncol = length(softvars))
colnames(data_weights_soft) <- softvars

data_weights_soft <- cbind(
  data %>% dplyr::select(BE_ID),
  data.frame(data_weights_soft)
)


# For each soft interstat edit rule:
# - find which subset of BE_IDs this rule applies to
# - find which common variables are involved in the rule and check
#   whether the original values of these variables for the selected BE_IDs
#   disagree across sources (difference larger than acceptable margin)
# - for BE_IDs with yes: assign WEIGHT_SOFT_HIGH
#   (i.e., the soft rule almost becomes a hard rule)
# - for BE_IDs with no: assign WEIGHT_SOFT_LOW
#   (i.e., the soft rule is allowed to be violated easily)

for (s in softvars) {

  rules_interstat_soft_s <- rules_interstat_soft_bron %>%
    dplyr::filter(softvar == s)

  industry_s <- unique(rules_interstat_soft_s$industry)
  gk_available <- rules_interstat_soft_bron %>%
    dplyr::filter(industry == industry_s) %>%
    pull(gk1) %>%
    as.integer() %>%
    unique()
  gk_s <- unique(as.integer(rules_interstat_soft_s$gk1))

  sel_s <- which(
    (data$SbiGecoordineerd2D == industry_s |
       data$SbiGecoordineerd3D == industry_s |
       data$SbiGecoordineerd4D == industry_s) &
      (data$GkSbsGecoordineerd1D == gk_s |
         sapply(as.integer(data$GkSbsGecoordineerd1D), function(x) min(abs(gk_available - x)) == abs(gk_s - x)))
      )

  if (length(sel_s) > 0) {
    V_s <- validate::validator(
      .data = rules_interstat_soft_s %>%
        dplyr::select(c(name, rule))
    ) %>%
      validate::variables()

    V_s <- stringr::str_replace(string = setdiff(V_s, s),
                                pattern = '^Echt.', replacement = 'reldif_')

    weights_s <- sapply(sel_s, function(i) {
      suppressWarnings(max(reldif[i, V_s], na.rm = TRUE)) > 2*min(1 - LOWER_BOUND_REL, UPPER_BOUND_REL - 1)
    })

    data_weights_soft[sel_s, s] <- ifelse(weights_s,
                                          WEIGHT_SOFT_HIGH,
                                          WEIGHT_SOFT_LOW)
  }
}

