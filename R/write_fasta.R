##' This function is based on one originally authored by Katie
##' Karpenko for writing FASTA files out. The functionality is
##' identical to that function, but this version has been optimized.
##'
##' @title Write FASTA
##' @param data data.frame
##' @param filename character, the name of the file to write to
##' @param sample_column character, the name of the sample ID column in data
##' @param sequence_column character, the name of the sequence column in data
##' @param ... addtional args passed to \code{writeLines()}
##' @return filename, the path to the file that was written 
##' @author Matt Espe and Katie Karpenko
##' @export
writeFasta<-function(data, filename,
                     sample_column = "SampleName",
                     sequence_column = "ESVseq",
                     ...)
{
    ## Adapted from original function authored by Katie
    ## Original grew results in for-loop
    ## Below is vectorized approach
    fastaLines = as.vector(rbind(paste0(">", data[,sample_column]),
                                 data[,sequence_column]))

    writeLines(fastaLines, filename, ...)
    return(filename)
}
