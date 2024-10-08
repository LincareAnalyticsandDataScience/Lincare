% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Run_Query.R
\name{Run_Query}
\alias{Run_Query}
\title{Run_Query}
\usage{
Run_Query(DSN, Query, File = TRUE, Linked.Server.Name = NULL, N = 5, Sleep = 2)
}
\arguments{
\item{DSN}{The Data Source Name (EDW_IBMi, EDW_External, ETL, RStudio)}

\item{Query}{The Query Text or the Name of the Query File}

\item{File}{Default: TRUE. Determines if a Query is being passed through the function as a File or as Text}

\item{Linked.Server.Name}{Default: NULL. Otherwise, the name of the Linked Server}

\item{N}{Default: 5. This is the number of attempts the function will make to connect to the Server/Database}

\item{Sleep}{Default: 2. Milliseconds between each attempt}
}
\value{
A Dataframe containing the Query Results
}
\description{
This function queries a Linked Server within SQL Server
}
\examples{
Query <- "Select * from table"
Data <- Run_Query(DSN = "DSN",
                  Query = Query,
                  File = TRUE,
                  Linked.Server.Name = "name",)

}
