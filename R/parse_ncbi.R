# parse the BLAST file output from NCBI

get_tbl_loc = function(lines)
{
    st = grep("^Sequences producing significant alignments:", lines)
    end = grep("^Alignments:", lines)
    return(cbind(start = st + 3, end = end - 2))
}

##' Extracts the tables from the raw text file output from NCBI.
##'
##' 
##' @title Extract Tables from NCBI Output
##' @param file file path to NCBI output file
##' @param lines the raw lines of the text file
##' @param col_widths integer vector giving the widths of the columns for the table
##' @param col_names character vector of column names to assign
##' @return list, with one element per table
##' @author Matt Espe
##' @export
extract_ncbi_tabs = function(file,
                             lines = readLines(file),
                             col_widths = c(66, 16, 16, 11, 7, 6, 6, 6, 7, 11, 10),                   
                             col_names = c("description", "scientific_name",
                                           "common_name", "taxid",
                                           "max_score", "total_score",
                                           "query_cover", "e_value",
                                           "per_ident", "acc_len", "accession"))       

{
    locs = get_tbl_loc(lines)
    lapply(seq(nrow(locs)), function(i){
        tt = read.fwf(textConnection(lines[locs[i,1]:locs[i,2]]),
                      header = FALSE,
                      widths = col_widths)
        colnames(tt) = col_names
        tt                        
    })
}

reduce_ncbi = function(df,
                       threshold)
{

}
