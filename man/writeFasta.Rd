% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_fasta.R
\name{writeFasta}
\alias{writeFasta}
\title{Write FASTA}
\usage{
writeFasta(
  data,
  filename,
  sample_column = "SampleName",
  sequence_column = "ESVseq",
  ...
)
}
\arguments{
\item{data}{data.frame}

\item{filename}{character, the name of the file to write to}

\item{sample_column}{character, the name of the sample ID column in data}

\item{sequence_column}{character, the name of the sequence column in data}

\item{...}{addtional args passed to \code{writeLines()}}
}
\value{
filename, the path to the file that was written
}
\description{
This function is based on one originally authored by Katie
Karpenko for writing FASTA files out. The functionality is
identical to that function, but this version has been optimized.
}
\author{
Matt Espe and Katie Karpenko
}
