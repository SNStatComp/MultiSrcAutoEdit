# Functies data preparatie ----
#' Prepare data
#' - load data
#' - add helper variables for edit operations
#' - convert data to numeric
#'
#' @param use_eo use edit operations (TRUE / FALSE)
#'
#' @return input data error locate
#' @export
#'
#' @examples
#' prepare_data()
prepare_data <- function(file_name = FILE_NAME_INPUT_DATA,
                         use_eo = TRUE,
                         conf_vars) {
  data <- load_input(file_name)

  logger::log_info(stringr::str_glue("edit operations = {use_eo}"))
  if (use_eo) {
    data <- add_variables_edit_operations(data)
  }

  data <- add_real_values_confrontation_variables(data, conf_vars)

  logger::log_info("convert data to numeric")
  data <- convert_columns_to_numeric(
    data = data,
    excluded_vars = data %>%
      dplyr::select(c(BE_ID, OG_ID,
                      dplyr::any_of(c(dplyr::starts_with('Sbi'),
                                      dplyr::starts_with('Gk'),
                                      dplyr::starts_with('pattern'))))) %>%
      colnames()
  )
  return(data)
}

load_input <- function(file_name) {
  logger::log_info(stringr::str_glue("load {file_name}"))
  if (grepl('[.]rds$', file_name)) {
    input_data <- readRDS(file_name)
  } else if (grepl('[.]csv$', file_name)) {
    input_data <- read.csv2(file_name, stringsAsFactors = FALSE)
  } else {
    stop('Unknown file format in load_input!')
  }
  return(input_data)
}

# Lege 'echte waarden' confrontatievariabelen afleiden
add_real_values_confrontation_variables <- function(data, conf_vars){
  new_columns <- replicate(n = length(conf_vars), NA_real_, simplify = FALSE)
  names(new_columns) <- paste0('Echt.', conf_vars)
  data <- data %>%
    dplyr::mutate(!!!new_columns)
  return(data)
}

convert_columns_to_numeric <- function(data, excluded_vars) {
  data <- data %>%
    dplyr::mutate(dplyr::across(-any_of(excluded_vars), ~ as.numeric(.))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric) & !any_of(excluded_vars), ~ ifelse(is.infinite(.x), NA, .x)))
}
