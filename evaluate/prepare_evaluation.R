
# Load manually edited data from database as input for evaluation

rm(list = ls())
gc()

# Load libraries ----

library(glue)
library(stringr)
library(dplyr)
library(tidyr)
library(bit64)
library(validate)
library(errorlocate)

PID <- tolower(substring(Sys.getenv("RSTUDIO_USER_IDENTITY")[[1]], 1, 4))

InputFolder <- "//Cbsp.nl/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Input"
DataFolder <- "//Cbsp.nl/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Data"

if (.Platform$OS.type == "unix") {
  InputFolder <- stringr::str_glue("/home/{PID}@cbsp.nl/shares/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Input")
  DataFolder <- stringr::str_glue("/home/{PID}@cbsp.nl/shares/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Data")
}

# Source files ----
InputFolder <- file.path(InputFolder, glue::glue(PID))

# Load constants ----
source(file.path('.', 'input_files', 'constants.R'))


###
# auxiliary functions

db_verbinding_maken <- function(server, database) {

  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver    = "SQL Server",
    Server    = server,
    Database  = database
  )

  return(con)
}

db_verbinding_sluiten <- function(connectie) {
  DBI::dbDisconnect(connectie)
}

read_file <- function(file_name) {
  if (grepl('[.]rds$', file_name)) {
    input_data <- readRDS(file_name)
  } else if (grepl('[.]csv$', file_name)) {
    input_data <- read.csv2(file_name, stringsAsFactors = FALSE)
  } else {
    stop('Unknown file format in read_file!')
  }
  return(input_data)
}


###

con <- db_verbinding_maken(
  server = "SQL_EBS_Grant_AG_PRD\\i01,50001",
  database = "EBS_Grant_AG_PRD")

# data.frame met alle aangepaste waarden
df_aanpassingen <- dplyr::tbl(con, dbplyr::in_schema("dbo", "aanpassingen_analist")) %>%
  dplyr::collect()

data_INIVA <- dplyr::tbl(con, dbplyr::in_schema("pilot", "vw_INIVA_2022")) %>%
  dplyr::filter(aantalkeerOG == 1) %>%
  dplyr::select(dplyr::all_of(c("BE_ID", "OG_ID",
                                "Investeringen_materiele_vaste_activa_INIVA_WIA",
                                "Investeringen_immateriele_vaste_activa_INIVA_WIA",
                                "Investeringen_materiele_vaste_activa_INIVA_SFGO",
                                "Investeringen_immateriele_vaste_activa_INIVA_SFGO",
                                "Investeringen_op_lease_INIVA_SFGO"))) %>%
  dplyr::collect()

data_SFGO <- dplyr::tbl(con, dbplyr::in_schema("pilot", "vw_SFGO_2022")) %>%
  dplyr::filter(aantalkeerOG == 1) %>%
  dplyr::select(dplyr::all_of(c("OG_ID",
                                "EBITDA",
                                "Investeringen_materiele_vaste_activa_INIVA_SFGO",
                                "Investeringen_immateriele_vaste_activa_INIVA_SFGO",
                                "Investeringen_op_lease_INIVA_SFGO"))) %>%
  dplyr::collect()

data_WIA <- dplyr::tbl(con, dbplyr::in_schema("pilot", "vw_WIA_2022")) %>%
  dplyr::filter(aantalkeerOG == 1) %>%
  dplyr::select(dplyr::all_of(c("BE_ID", "OG_ID", "Inkoopwaarde_Omzet", "Marge_omzet",
                                "Investeringen_materiele_vaste_activa_INIVA_WIA",
                                "Investeringen_immateriele_vaste_activa_INIVA_WIA"))) %>%
  dplyr::collect() %>%
  dplyr::mutate(dplyr::across(where(bit64::is.integer64), ~bit64::as.integer.integer64(.x))) %>%
  dplyr::mutate(dplyr::across(c(BE_ID, OG_ID), as.integer))

data_PS <- dplyr::tbl(con, dbplyr::in_schema("pilot", "vw_m_PS_2022")) %>%
  dplyr::filter(aantalkeerOG == 1) %>%
  dplyr::select(dplyr::all_of(c("BE_ID", "OG_ID",
                                "SbiGecoordineerd", "Inkoopwaarde_Omzet",
                                "Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten"))) %>%
  dplyr::collect() %>%
  dplyr::mutate(dplyr::across(c(BE_ID, OG_ID), as.integer))

db_verbinding_sluiten(con)



### Recreate manually edited data

kader <- read_file(file.path(DataFolder, 'steekproef_kader.rds'))
stp_eenheden <- read_file(file.path(DataFolder, 'steekproef_eenheden.csv'))

stp_data <- kader %>%
  inner_join(stp_eenheden, by = c('BE_ID', 'OG_ID')) %>%
  select(-c(starts_with('reldif_'), N, n, aselect))
data.table::setDF(stp_data)

# add extra/corrected variables that were added to the database after the sample was drawn
stp_data <- stp_data %>%
  select(-EBITDA.SFGO) %>%
  left_join(data_INIVA %>%
              rename_with(function(x) paste0(x, '.INIVA'), starts_with('Investeringen')),
            by = c('BE_ID', 'OG_ID')) %>%
  left_join(data_SFGO %>%
              rename_with(function(x) paste0(x, '.SFGO'), c(starts_with('Investeringen'), EBITDA)),
            by = c('OG_ID')) %>%
  left_join(data_WIA %>%
              rename_with(function(x) paste0(x, '.WIA'), starts_with('Investeringen')),
            by = c('BE_ID', 'OG_ID')) %>%
  mutate(Inkoopwaarde_Omzet.WIA = if_else(!is.na(Inkoopwaarde_Omzet.WIA) | (!is.na(Inkoopwaarde_Omzet) & Inkoopwaarde_Omzet > 0),
                                          Inkoopwaarde_Omzet,
                                          Inkoopwaarde_Omzet.WIA),
         Marge_omzet.WIA = if_else(!is.na(Marge_omzet) & Marge_omzet > 0,
                                   Marge_omzet,
                                   Marge_omzet.WIA),
         Inkoopwaarde_Omzet = NULL,
         Marge_omzet = NULL) %>%
  select(-Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten.PS) %>%
  left_join(data_PS %>%
              rename_with(function(x) paste0(x, '.PS'), starts_with('Omzet')),
            by = c('BE_ID', 'OG_ID')) %>%
  mutate(Inkoopwaarde_Omzet.PS = if_else(!is.na(Inkoopwaarde_Omzet.PS) | (!is.na(Inkoopwaarde_Omzet) & Inkoopwaarde_Omzet > 0),
                                         Inkoopwaarde_Omzet,
                                         Inkoopwaarde_Omzet.PS),
         Inkoopwaarde_Omzet = NULL)


stp_data <- stp_data %>%
  mutate(SbiGecoordineerd3D = substr(SbiGecoordineerd, 1, 3)) %>%
  mutate(SbiStratum = if_else(SbiStratum == '461/3/5',
                              if_else(SbiGecoordineerd3D == '461', '461', '463/5'),
                              SbiStratum))

stp_data_aangepast <- stp_data

# order adjustments chronologically, so the final adjustment is used here
df_aanpassingen <- df_aanpassingen %>%
  arrange(BE_ID, OG_ID, bron, variabele, timestamp)

# Note: Remarks by the analysts without setting a new value are stored
# in the database as missing values for the adjusted value.
# We do not want to use these missing values as adjustments.
# On 18 September 2024, the dashboard was changed so it was no longer possible
# to leave a remark without setting a new value.
datum_dashboard_aangepast <- as.Date('20240918', format = '%Y%m%d')

tel_aanpassingen <- 0
be_aangepast <- NULL
for (i in 1:nrow(df_aanpassingen)) {
  be <- df_aanpassingen$BE_ID[i]
  og <- df_aanpassingen$OG_ID[i]
  variabele <- paste0(df_aanpassingen$variabele[i], '.', df_aanpassingen$bron[i])
  nieuwe_waarde <- df_aanpassingen$nieuwe_waarde[i]
  timestamp <- as.Date.POSIXct(df_aanpassingen$timestamp[i])

  if (variabele %in% names(stp_data_aangepast) & (!is.na(nieuwe_waarde) | (timestamp >= datum_dashboard_aangepast))) {
    tel_aanpassingen <- tel_aanpassingen + 1
    be_aangepast <- c(be_aangepast, be)
    r <- which(stp_data_aangepast$BE_ID == be & stp_data_aangepast$OG_ID == og)
    stp_data_aangepast[r, variabele] <- nieuwe_waarde
  }
}
be_aangepast <- unique(be_aangepast)



## Prepare output

results_analysts <- stp_data_aangepast %>%
  dplyr::select(c(BE_ID, OG_ID,
                  dplyr::starts_with('in'), dplyr::starts_with('OGin'),
                  dplyr::ends_with(c('.PS', '.WIA', '.DRT', '.SWL',
                                     '.SFGO', '.INIVA', '.IH',
                                     '.KICR', '.PCM')))) %>%
  dplyr::rename_with(.fn = ~stringr::str_replace_all(string = .x,
                                                     pattern = '([[:print:]]*)[.]([[:print:]]*$)',
                                                     replacement = '\\2.\\1'))
# column names renamed with name of source at the beginning rather than the end


###### Check whether the manually edited data satisfy all interstat edit rules

# read edit rules (and remove rules related to variables that were not included in the dashboard)
rules_interstat_bron <- read.csv2(file = file.path("edited_input", stringr::str_glue("interstat_rules_{YEAR}.csv"))) %>%
  dplyr::filter(!grepl('Sociale_lasten|Pensioenlasten|Overige_personeelskosten|Uitbesteed_werk|_excl_uitbesteed', rule))
rules_interstat <- validate::validator(.data = rules_interstat_bron)
vars_in_rules_interstat <- unique(validate::variables(rules_interstat))

extravars <- setdiff(vars_in_rules_interstat, names(results_analysts))

result_interstat <- vector(mode = 'list', length = nrow(results_analysts))

for (i in 1:nrow(results_analysts)) {
  print(i)

  # add 'Echt.' variables
  data_row <- cbind(
    results_analysts[i, , drop = FALSE],
    data.frame(t(stats::setNames(rep(NA_real_, length(extravars)), extravars)))
  )

  data_interstat_i <- data_row[vars_in_rules_interstat]

  rules_interstat_i <- rules_interstat_bron

  # Remove all rules related to sources in which record i does not occur
  # Set missing values equal to zero for all sources in which record i does occur

  if (is.na(data_row["inPS"]) | data_row["inPS"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("PS[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('PS.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }
  if (is.na(data_row["inPCM"]) | data_row["inPCM"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("PCM[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('PCM.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }
  if (is.na(data_row["OGinSFGO"]) | data_row["OGinSFGO"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("SFGO[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('SFGO.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }
  if (is.na(data_row["inDRT"]) | data_row["inDRT"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("DRT[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('DRT.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }
  if (is.na(data_row["inKICR"]) | data_row["inKICR"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("KICR[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('KICR.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }
  if (is.na(data_row["inSWL"]) | data_row["inSWL"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("SWL[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('SWL.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }
  if (is.na(data_row["inIH"]) | data_row["inIH"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("IH[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('IH.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }
  if (is.na(data_row["inWIA"]) | data_row["inWIA"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("WIA[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('WIA.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }
  if (is.na(data_row["inINIVA"]) | data_row["inINIVA"] == FALSE) {
    rules_interstat_i <- rules_interstat_i %>% dplyr::filter(!grepl("INIVA[.]", rule))
  } else {
    data_interstat_i <- data_interstat_i %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('INIVA.'),
                                  ~tidyr::replace_na(.x, replace = 0)))
  }

  rules_interstat_i_obj <- validate::validator(.data = rules_interstat_i)
  vars_in_rules_interstat_i_obj <- unique(validate::variables(rules_interstat_i_obj))
  data_interstat_i_obj <- data_interstat_i[vars_in_rules_interstat_i_obj]

  result_interstat[[i]] <- tryCatch(
    {
      set.seed(42)
      le_interstat <- errorlocate::locate_errors(
        data = data_interstat_i_obj,
        x = rules_interstat_i_obj,
        timeout = TIMEOUT
      )
      le_interstat
    },
    error = function(e) {
      return(NULL)
    }
  )

}

results_analysts$el_interstat_status <- sapply(result_interstat, function(l) l$status)
results_analysts$el_interstat_nerror <- sapply(result_interstat, function(l) sum(l$errors, na.rm = TRUE))



## Prepare output

results_analysts <- results_analysts %>%
  dplyr::select(-c(dplyr::starts_with('in', ignore.case = FALSE),
                   dplyr::starts_with('OGin')))


write.csv2(results_analysts,
           file.path(InputFolder, "results_analysts.csv"),
           row.names = FALSE)

