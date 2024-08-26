#' Run_Query
#'
#' This function queries a Linked Server within SQL Server
#'
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC odbcClose
#' @importFrom RODBC sqlQuery
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbGetQuery
#' @importFrom odbc odbc
#'
#' @param DSN The Data Source Name (EDW_IBMi, EDW_External, ETL, RStudio)
#' @param Query The Query Text or the Name of the Query File
#' @param File Default: TRUE. Determines if a Query is being passed through the function as a File or as Text
#' @param Linked.Server.Name Default: NULL. Otherwise, the name of the Linked Server
#' @param N Default: 5. This is the number of attempts the function will make to connect to the Server/Database
#' @param Sleep Default: 2. Milliseconds between each attempt
#'
#' @return A Dataframe containing the Query Results
#'
#' @examples Query <- "Select * from table"
#' Data <- Run_Query(DSN = "DSN",
#'                   Query = Query,
#'                   File = TRUE,
#'                   Linked.Server.Name = "name",)
#'
#' @export
Run_Query <- function(DSN, Query, File = TRUE, Linked.Server.Name = NULL, N = 5, Sleep = 2){
  if(File){
    Query <- readChar(con = Query, nchars = file.info(Query)$size[1])
    Query <- gsub(pattern = "\r", replacement = " ", x = Query)
    Query <- gsub(pattern = "\n", replacement = " ", x = Query)
    Query <- gsub(pattern = "ï»¿", replacement = " ", x = Query)
    Query <- gsub(pattern = "\\", replacement = " ", x = Query, fixed = TRUE)
  }


  counter = 1
  con <- "Failed to connect"

  if(!is.null(Linked.Server.Name)) {
    while (class(con) == "character" & counter <= N) {
      con <- tryCatch(RODBC::odbcConnect(DSN),
                      error = function(e){return("Failed to connect")})
      counter <- counter + 1
      if(suppressWarnings(class(con) == "character")) Sys.sleep(Sleep)
    }
  }else{
    while (class(con) == "character" & counter <= N) {
      con <- tryCatch(DBI::dbConnect(odbc::odbc(), DSN),
                      error = function(e){return("Failed to connect")})
      counter <- counter + 1
      if(suppressWarnings(class(con) == "character")) Sys.sleep(Sleep)
    }
  }

  if(!is.null(Linked.Server.Name)) {
    Query <- gsub("'", "''", Query)
    Query <- paste0("Exec ('", Query, "') at [", Linked.Server.Name, "]")
    OUT <- RODBC::sqlQuery(con, Query)
    RODBC::odbcClose(con)
    colnames(OUT) <- gsub("\"", "",colnames(OUT))
    return(OUT)
  }else{
    OUT <- DBI::dbGetQuery(conn = con, statement = Query)
    DBI::dbDisconnect(con)
    return(OUT)
  }
}
