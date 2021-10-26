# Create a fasta file from a CSV
##' Creates a Fasta file from a CSV 
##'
##' @title CSV to Fasta
##' @param csv_file file path to the CSV to read in
##' @param df data.frame, the results of read.csv
##' @param outfile name of output file
##' @param outdir directory to write output to. Defaults to current working directory 
##' @return file path to fasta file
##' @author Matt Espe
##' @export
csv_to_fasta = function(csv_file, df = read.csv(csv_file),
                        outfile = gsub("\\.csv$", "_fasta.txt", basename(csv_file)),
                        outdir = ".")
{
    text = construct_fasta(df)
    cat(text,
        file = file.path(outdir, outfile))
    return(file.path(outdir, outfile))
}

construct_fasta = function(df)
    
{
    sn = df[,grep("samplename", colnames(df), ignore.case = TRUE)]
    sq = df[,grep("sequence", colnames(df),  ignore.case = TRUE)]
    paste(paste0(">", sn), sq, sep = "\n", collapse = "\n")    
}
