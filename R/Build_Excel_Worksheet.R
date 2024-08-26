#' Build_Excel_Worksheet
#'
#' This function queries a Linked Server within SQL Server
#'
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx createStyle
#' @importFrom openxlsx addStyle
#' @importFrom openxlsx writeData
#' @importFrom openxlsx freezePane
#' @importFrom openxlsx setColWidths
#'
#' @param DSN The Data Source Name (EDW_IBMi, EDW_External)
#' @param query The Query Text
#' @param linked_server_name The Name of the Linked Server (EDWtoIBMi)
#'
#' @return A Dataframe containing the Query Results
#'
#' @examples Query <- "Select * from table"
#' Data <- Query_Linked_Server(DSN = "EDWtoIBMi",
#'                             query = Query,
#'                             linked_server_name = "EDWtoIBMi")
#'
#' @export
Build_Excel_Worksheet <- function(SheetName, Data, WorkBook, CRDB_Columns = c(), Numeric_Columns = c(), Currency_Columns = c()){
  options("openxlsx.numFmt" = "0.00")

  openxlsx::addWorksheet(wb = WorkBook,
                         sheetName = SheetName,
                         gridLines = FALSE,
                         tabColour = "black")

  NumFormat <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                     fontColour = "black",
                                     fontName = "calibri",
                                     fontSize = 11,
                                     borderStyle = "thin",
                                     borderColour = 'black',
                                     numFmt = "#,##0.00")

  CurrencyFormat <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                          fontColour = "black",
                                          fontName = "calibri",
                                          fontSize = 11,
                                          borderStyle = "thin",
                                          borderColour = 'black',
                                          numFmt = "CURRENCY")

  TableFormat <- openxlsx::createStyle(fgFill = "#ffff99",
                                       halign = "CENTER",
                                       textDecoration = "Bold",
                                       border = "TopBottomLeftRight",
                                       fontColour = "black",
                                       fontName = "calibri",
                                       fontSize = 11,
                                       borderStyle = "thin",
                                       borderColour = 'black')


  if(!is.null(CRDB_Columns)) {
    for (a in CRDB_Columns) {
      Data[,a] <- CRDB_Format(Data[,a])
    }
  }

  if(!is.null(Numeric_Columns)) {
    for (a in Numeric_Columns) {
      class(Data[,a]) <- "NUMERIC"
    }
  }

  if(!is.null(Currency_Columns)) {
    for (a in Currency_Columns) {
      class(Data[,a]) <- "CURRENCY"
    }
  }



  Classes <- lapply(Data, class)
  Classes <- as.data.frame(unlist(Classes))
  Classes$Field <- rownames(Classes)
  colnames(Classes) <- c("Class", "Field")
  Decimal_columns <- which(Classes$Class == "NUMERIC")
  Currency_columns <- which(Classes$Class == "CURRENCY")

  openxlsx::addStyle(wb = WorkBook,
                     sheet = SheetName,
                     style = NumFormat,
                     rows = 2:(nrow(Data)+1),
                     cols = Decimal_columns,
                     gridExpand = TRUE,
                     stack = TRUE)

  openxlsx::addStyle(wb = WorkBook,
                     sheet = SheetName,
                     style = CurrencyFormat,
                     rows = 2:(nrow(Data)+1),
                     cols = Currency_columns,
                     gridExpand = TRUE,
                     stack = TRUE)

  openxlsx::writeData(wb = WorkBook,
                      sheet = SheetName,
                      x = Data,
                      startRow = 1,
                      startCol = 1,
                      headerStyle = TableFormat,
                      borders = "all",
                      borderStyle = "thin",
                      rowNames = FALSE,
                      colNames = TRUE,
                      borderColour = "black",
                      name = SheetName,
                      withFilter = TRUE)


  openxlsx::freezePane(wb = WorkBook,
                       sheet = SheetName,
                       firstRow = TRUE)


  ColWidth <- lapply(Data, function(x) max(nchar(na.omit(x))))
  ColWidth <- as.data.frame(unlist(ColWidth))
  ColWidth$Field <- rownames(ColWidth)
  colnames(ColWidth) <- c("Width", "Field")


  width_vec_header <- ifelse(ColWidth$Width + 4 > nchar(colnames(Data)) + 4,
                             ColWidth$Width + 4,
                             nchar(colnames(Data)) + 4)

  openxlsx::setColWidths(wb = WorkBook,
                         sheet = SheetName,
                         cols = 1:ncol(Data),
                         widths = width_vec_header)

}
