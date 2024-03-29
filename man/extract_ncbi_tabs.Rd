% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_ncbi.R
\name{extract_ncbi_tabs}
\alias{extract_ncbi_tabs}
\title{Extract Tables from NCBI Output}
\usage{
extract_ncbi_tabs(
  file,
  lines = readLines(file),
  col_names = c("description", "scientific_name", "common_name", "taxid", "max_score",
    "total_score", "query_cover", "e_value", "per_ident", "acc_len", "accession")
)
}
\arguments{
\item{file}{file path to NCBI output file}

\item{lines}{the raw lines of the text file}

\item{col_names}{character vector of column names to assign}
}
\value{
list, with one element per table
}
\description{
Extracts the tables from the raw text file output from NCBI.
}
\author{
Matt Espe
}
