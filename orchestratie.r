
# clear memory
rm(list = ls())
gc()

source("./global.r", local = TRUE) # load global settings
source("./functions/fct_error_locate.r", local = TRUE) # load parallelised process_row_errorlocate function
source("./functions/fct_match_restrictions_lp.r", local = TRUE) # load auxiliary functions for adjusting imputed values in step 2
source("./functions/fct_dataprep.r", local = TRUE) # load dataprep functions
source("./functions/fct_export.r", local = TRUE) # load export functions


# Orchestration error locate function
process_row_errorlocate <- function(Stap3Doen = TRUE,
                                    data_row,
                                    weights_interstat_row,
                                    use_eo,
                                    use_linear_rules,
                                    use_interstat_rules_soft) {
  source("./stap_0.r", local = TRUE) #data laden / data prep
  source("./stap_1.r", local = TRUE)
  if (stap1_gelukt) source("./stap_2.r", local = TRUE)
  if (stap2_gelukt) source("./stap_3.r", local = TRUE)

  return(results)
}


# Load data and weights
source("./dataprep_error_locate.r", local = TRUE)


# Call to start_errorlocate
res <- start_errorlocate(Stap3Doen = Stap3Doen,
                         data = data,
                         weights_interstat = weights_interstat,
                         n_start = 1,
                         n_end = nrow(data),
                         rule_matrix = rule_matrix,
                         prefix_output = prefix_output,
                         save_rdata = save_rdata,
                         use_eo = use_eo,
                         use_linear_rules = use_linear_rules,
                         use_interstat_rules_soft = use_interstat_rules_soft,
                         use_calculator = use_calculator)

export_output(
  Stap3Doen = Stap3Doen,
  results_errorlocate = res$results_errorlocate,
  results_overhevelen = res$results_overhevelen,
  results_optelling_nul = res$results_optelling_nul,
  results_ps = res$results_ps,
  results_ded = res$results_ded,
  results_imp = res$results_imp,
  results_mr = res$results_mr,
  results_final = res$results_final,
  data_org = data,
  prefix_output = prefix_output,
  save_rdata = save_rdata,
  conf_vars = rule_matrix$Naam[!is.na(rule_matrix$PS) & rule_matrix$PS != '']
)
