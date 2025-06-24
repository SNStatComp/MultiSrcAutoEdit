rm(list = ls())
gc()

library(stringr)
library(dplyr)
library(glue)
library(bit64)

PID <- tolower(substring(Sys.getenv("RSTUDIO_USER_IDENTITY")[[1]], 1, 4))
InputFolder <- "//Cbsp.nl/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Input"
if (.Platform$OS.type == "unix") {
  InputFolder <- glue::glue("/home/{PID}@cbsp.nl/shares/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Input")
}
InputFolder <- file.path(InputFolder, glue::glue(PID))

# load constants ----
source(stringr::str_glue('./input_files/constants.R'))

sbiSelectie <- c(
  "107",               # bakkers
  "412",               # bouw
  "461", "463", "465", # groothandel: handelsbemiddeling, GH in voedingsmiddelen, GH in elektronica
  "494",               # goederenvervoer over de weg
  "5229",              # expediteurs
  "561",               # restaurants
  "620"                # dienstverlening IT
)

gkSelectie <- 1:7

# variabelen die om een of andere reden een foutmelding geven
# bij het inlezen uit de database
# en die we hier verder toch niet gebruiken
variabelen_niet_inlezen <- c('opmerking', stringr::str_glue('D00{1:7}001'))


# auxiliary function ----
data_inlezen <- function(jaar, bronnen, common_vars) {

  if (exists("data_totaal")) rm(data_totaal, inherits = TRUE)

  # verbinding maken met de database
  source("dataprep/utils_db_verbinding.R")
  con <- db_verbinding_maken(
    server = "SQL_EBS_Grant_AG_PRD\\i01,50001",
    database = "EBS_Grant_AG_PRD")

  beschikbaar <- DBI::dbListTables(con)

  # van elke bron de data ophalen en deze joinen, daarna alleen de opgaven overhouden die in 2 of meer bronnen zitten
  for (i in bronnen) {
    bron_data_sql_view <- stringr::str_glue('vw_m_{i}_{jaar}')
    if (!(bron_data_sql_view %in% beschikbaar)) {
      bron_data_sql_view <- stringr::str_glue('{bron_data_sql_view}_gaaf')
    }

    bron_data <- dplyr::tbl(con, dbplyr::in_schema("pilot", bron_data_sql_view)) %>%
      dplyr::select(-dplyr::any_of(c("Periode", "SbiActueel", "SbiGecoordineerd", "GkSbs",
                                     "GkSbsGecoordineerd", "WPActueel",
                                     variabelen_niet_inlezen))) %>%
      dplyr::filter(aantalkeerOG == 1) %>%
      dplyr::collect() %>%
      dplyr::select(-aantalkeerOG) %>%
      dplyr::mutate(dplyr::across(where(bit64::is.integer64), ~bit64::as.integer.integer64(.x))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c('DEKKINGS_PCT_WIA', 'VULLINGS_PCT_WIA')), function(x) as.numeric(stringr::str_replace(x, ",", ".")))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c('BE_ID', 'OG_ID')), ~as.integer(.x))) %>%
      dplyr::select(dplyr::where(is.numeric)) %>%
      dplyr::relocate(dplyr::any_of(c('BE_ID', 'OG_ID'))) # move these columns to the front

    # exist-kolom afleiden: bij DRT en WIA gelden aanvullende eisen
    # als niet aan die eisen voldaan wordt, dan overlappende variabelen leegmaken
    if (i == "DRT") {
      bron_data <- bron_data %>%
        dplyr::mutate(exist = dplyr::if_else(!is.na(ResponsePercentage) & ResponsePercentage >= 0.95, 1, NA)) %>%
        dplyr::mutate(dplyr::across(any_of(common_vars), function(x) dplyr::if_else(is.na(exist), NA, x)))
    } else if (i == "WIA") {
      bron_data <- bron_data %>%
        dplyr::mutate(exist = dplyr::if_else(!is.na(DEKKINGS_PCT_WIA) & !is.na(VULLINGS_PCT_WIA) &
                                               DEKKINGS_PCT_WIA >= 95 & VULLINGS_PCT_WIA >= 95, 1, NA)) %>%
        dplyr::mutate(dplyr::across(any_of(common_vars), function(x) dplyr::if_else(is.na(exist), NA, x)))
    } else {
      bron_data <- bron_data %>%
        dplyr::mutate(exist = 1)
    }

    # prefixes maken
    colnames(bron_data) <- paste0(i, ".", colnames(bron_data))
    # SFGO heeft enkel OG_ID, geen BE_ID. Deze variabelen hebben geen prefix nodig want hier is de join op gebaseerd
    if (i == "SFGO") {
      names(bron_data)[1] <- c("OG_ID")
    } else {
      names(bron_data)[1:2] <- c("BE_ID", "OG_ID")
    }

    # lees ook insluitgewichten in voor de PS
    if (i == "PS") {
      bron_data_gewichten <- dplyr::tbl(con, dbplyr::in_schema("dbo", stringr::str_glue('tbl_Be_ID_Meta_JR{jaar}_1'))) %>%
        dplyr::select(c(BE_ID = BE_Id, PS.Insluitgewicht = InsluitGewicht)) %>%
        dplyr::collect()
      bron_data <- bron_data %>%
        dplyr::left_join(bron_data_gewichten, by = 'BE_ID')
    }

    if (exists("data_totaal")) {
      if (i == "SFGO"){
        data_totaal <- data_totaal %>%
          dplyr::full_join(bron_data, by = c("OG_ID" = "OG_ID"))
      } else {
        data_totaal <- data_totaal %>%
          dplyr::full_join(bron_data, by = c("BE_ID" = "BE_ID", "OG_ID" = "OG_ID"))
      }
    } else {
      data_totaal <- bron_data
    }

    print(paste0("Bron ",bron_data_sql_view, " ingeladen."))

  }

  # achtergrondkenmerken inladen en aankoppelen
  # (inclusief SbiGecoordineerd, GkSbsGecoordineerd en WPActueel)
  bron_data_sql_view <- paste0("src_EHO_EBN2_POC_ANA_dbo_EHO_jaarkader_", jaar)
  kenmerken <- dplyr::tbl(con, dbplyr::in_schema("pilot", bron_data_sql_view)) %>%
    dplyr::select(-c(HandelsNaam, JuridischeNaam, Jaar)) %>%
    dplyr::collect()

  data_totaal <- data_totaal %>%
    dplyr::left_join(kenmerken, by = c('BE_ID' = 'BeIdentificatie', 'OG_ID' = 'OgIdentificatie'))

  bron_data_sql_view <- paste0("vw_beid_cbspersoon_", jaar)
  cbspersonen <- dplyr::tbl(con, dbplyr::in_schema("pilot", bron_data_sql_view)) %>%
    dplyr::collect() %>%
    dplyr::group_by(BE_ID, OG_ID) %>%
    summarise(aantalCBSpersonen = n(), .groups = 'keep')

  data_totaal <- data_totaal %>%
    dplyr::left_join(cbspersonen, by = c('BE_ID', 'OG_ID'))

  # een paar extra versies van achtergrondkenmerken aanmaken
  data_totaal <- data_totaal %>%
    dplyr::mutate(SbiGecoordineerd2D = stringr::str_sub(SbiGecoordineerd, 1, 2),
                  SbiGecoordineerd3D = stringr::str_sub(SbiGecoordineerd, 1, 3),
                  SbiGecoordineerd4D = stringr::str_sub(SbiGecoordineerd, 1, 4),
                  GkSbsGecoordineerd1D = stringr::str_sub(GkSbsGecoordineerd, 1, 1),
                  GkSbsGecoordineerdSML =
                    dplyr::case_when(
                      (GkSbsGecoordineerd1D <= 2) ~ 'S',
                      (GkSbsGecoordineerd1D %in% c(3, 4, 5)) ~ 'M',
                      (GkSbsGecoordineerd1D >= 6) ~ 'L',
                      TRUE ~ NA),
                  een = 1)

  print(paste0("Achtergrondkenmerken ingeladen."))

  db_verbinding_sluiten(con)

  # selecteer alleen opgaven die in meer dan 1 bron voorkomen
  data_totaal <- data_totaal %>%
    dplyr::mutate(
      total_exists = select(., ends_with("exist")) %>% rowSums(na.rm = TRUE)
    ) %>%
    dplyr::filter(total_exists > 1)

  return(data_totaal)
}


# namen overlappende variabelen inlezen vanuit regelmatrix
rule_matrix <- read.csv2(
  sprintf("./input_files/rule_matrix_%d.csv", YEAR),
  stringsAsFactors = FALSE,
  na.strings = c("NA", "")
)
lijst_met_variabelen <- rule_matrix$Naam

# data inlezen vanuit database
lijst_met_bronnen <- c("DRT", "IH", "INIVA", "KICR", "PCM", "PS", "SFGO", "SWL", "WIA")

# functie aftrappen
data_totaal <- data_inlezen(jaar = YEAR, bronnen = lijst_met_bronnen,
                            common_vars = lijst_met_variabelen)


# dit bestand opslaan als een RDS-bestand
saveRDS(data_totaal, file.path(InputFolder, stringr::str_glue("data_{YEAR}.rds")))

# ook nadere selecties maken uit de data
data_sel <- data_totaal %>%
  dplyr::filter(!is.na(PS.exist), !is.na(SbiGecoordineerd)) %>%
  dplyr::filter(SbiGecoordineerd3D %in% sbiSelectie | SbiGecoordineerd4D %in% sbiSelectie) %>%
  dplyr::filter(GkSbsGecoordineerd1D %in% gkSelectie)

# dit bestand opslaan als een RDS-bestand
saveRDS(data_sel, file.path(InputFolder, stringr::str_glue("data_{YEAR}_sel.rds")))


## herhalen voor jaar t-1 (referentiedata)

data_totaal <- data_inlezen(jaar = YEAR - 1, bronnen = lijst_met_bronnen,
                            common_vars = lijst_met_variabelen)

# dit bestand opslaan als een RDS-bestand
saveRDS(data_totaal, file.path(InputFolder, stringr::str_glue("data_{YEAR-1}.rds")))

# ook nadere selecties maken uit de data
data_sel <- data_totaal %>%
  dplyr::filter(!is.na(PS.exist), !is.na(SbiGecoordineerd)) %>%
  dplyr::filter(SbiGecoordineerd3D %in% sbiSelectie | SbiGecoordineerd4D %in% sbiSelectie) %>%
  dplyr::filter(GkSbsGecoordineerd1D %in% gkSelectie)

# dit bestand opslaan als een RDS-bestand
saveRDS(data_sel, file.path(InputFolder, stringr::str_glue("data_{YEAR-1}_sel.rds")))
