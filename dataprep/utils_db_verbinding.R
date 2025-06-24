#' Verbinding maken met database
#'
#' @param server server
#' @param database database
#' @param credhub naam credhubservice met sql gebruik/wachtwoord
#'
#' @return geeft een connectie terug
#' @export
db_verbinding_maken <- function(server, database) {

    logger::log_debug("login op {server} met trusted connectie")
    con <- DBI::dbConnect(
      odbc::odbc(),
      Driver    = "SQL Server",
      Server    = server,
      Database  = database
    )

  return(con)
}


#' Sluiten database connectie
#'
#' @param connectie
#'
#' @return -
#' @export
db_verbinding_sluiten <- function(connectie) {
  DBI::dbDisconnect(connectie)
}
