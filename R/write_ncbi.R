##' Write tables extracted and possibly reduced to CSV files for
##' further analysis.
##' 
##' @title Write NCBI data to a CSV
##' @param x either a list or data.frame 
##' @param outdir the directory to write CSVs to
##' @param out_prefix the common prefix for all files
##' @param out_names the unique name to use for each table
##' @param ... addtional args passed to write.csv
##' @return the file paths of the CSVs written
##' @author Matt Espe
##' @export
write_ncbi_tabs = function(x, outdir = ".", out_prefix = "NCBI_reduced_",
                           out_names = names(x), ...)
{
    if(is.data.frame(x) && is.null(out_names))
        stop("Please provide a name for the table")

    full_out = file.path(outdir, paste0(out_prefix, out_names, ".csv"))

    if(is.data.frame(x))
        write.csv(x, full_out, row.names = FALSE, ...)
    
    mapply(function(df, file) {write.csv(df, file, row.names = FALSE, ...)},
           df = x, file = full_out)

    return(full_out)
}
