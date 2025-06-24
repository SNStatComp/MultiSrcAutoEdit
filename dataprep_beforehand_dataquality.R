
### Note: this script should be called from dataprep_beforehand.R ###

print(stringr::str_glue('Prepare adjusted weights based on input data quality...'))


### Load criteria to adjust weights based on input quality
input_criteria <- read.csv2(FILE_NAME_CRITERIA)
input_relevance <- read.csv2(FILE_NAME_TOEPASBAARHEID)

rules_inputquality <- validate::validator(
  .data = data.frame(name = input_criteria$name,
                     description = input_criteria$description,
                     rule = input_criteria$criterion)
)

### Apply criteria to data
cf_inputquality <- validate::confront(data, rules_inputquality)
summary(cf_inputquality)


#####

beids_crit <- data %>%
  dplyr::select(BE_ID, SbiGecoordineerd, GkSbsGecoordineerd)

beids_crit <- cbind(beids_crit,
                    as.data.frame(!values(cf_inputquality)))

# count number of records that satisfy each criterion
print(beids_crit %>% dplyr::summarise(dplyr::across(dplyr::all_of(input_criteria$name),
                                                    ~sum(.x, na.rm = TRUE))))

beids_crit_subset <- beids_crit %>%
  dplyr::filter(dplyr::if_any(dplyr::all_of(input_criteria$name)))

beidsel <- beids_crit_subset$BE_ID



## Adjust weights per record
# For each criterion:
# - get affected records from beids_crit
# - get relevant variables from input_relevance
# - get factor by which the weights should be lowered from input_criteria

for (v in input_criteria$name) {

  v_records <- which(beids_crit[ , v])
  v_vars <- input_relevance$variable[input_relevance[ , v] == 1]
  v_factor <- input_criteria$factor[input_criteria$name == v]

  if (length(v_records) >= 1 & length(v_vars) >= 1) {
    data_weights[v_records, v_vars] <- data_weights[v_records, v_vars] * v_factor
  }

}

