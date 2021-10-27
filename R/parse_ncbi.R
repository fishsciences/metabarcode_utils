# parse the BLAST file output from NCBI

get_sp_names = function(lines)
{
    x = grep("Query #[0-9]+:", lines, value = TRUE)
    sapply(strsplit(x, " "), "[", 3)
}


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
    sp = get_sp_names(lines)
    tabs = lapply(seq(nrow(locs)), function(i){
        tt = read.fwf(textConnection(lines[locs[i,1]:locs[i,2]]),
                      header = FALSE,
                      widths = col_widths)
        colnames(tt) = col_names
        tt$query_cover = as.numeric(gsub("%| ", "", tt$query_cover)) 
        tt                        
    })
    names(tabs) = sp
    tabs
}

##' Reduces the results of the NCBI BLASTN results. 
##'
##' 
##' @title Reduce NCBI BLAST results
##' @param df data.frame
##' @param keep_cols vector of column names to keep in the output.
##' @param accession_keep either "all" to keep all accessions, or
##'     "top" to only keep the accession with the top per_ident
##' @param query_threshold the threshold of query_coverage. Below this
##'     value, a string will be returned.
##' @param thresholds vector of threshold to cut off at. The last
##'     should be zero.
##' @return list of data.frames of reduced rows
##' @author Matt Espe
##' @export
reduce_ncbi = function(df,
                       keep_cols = c("scientific_name",
                                     "common_name", "taxid",
                                     "query_cover", "per_ident",
                                     "accession"),
                       accession_keep = c("all", "top"), 
                       query_threshold = 90,
                       thresholds = c(97, 80))
{
    if(class(df) == "list"){
        ans = lapply(df, reduce_ncbi, keep_cols = keep_cols,
                     accession_keep = accession_keep,
                     query_threshold = query_threshold,
                     thresholds = thresholds)
        names(ans) = names(df)
        return(ans)
    }
    
    df = df[,colnames(df) %in% keep_cols]
    if(max(df$query_cover, na.rm = TRUE) < query_threshold)
        return("<90% Coverage")

    if(max(df$per_ident, na.rm = TRUE) < min(thresholds))
        return(paste0("<", min(thresholds), "% Per Ident")

    for(th in thresholds){
        ans = subset(df, per_ident > th)
        if(nrow(ans)){
            ans = unique(subset(df, per_ident > th))
            return(switch(accession_keep,
                   all = ans,
                   top = get_top_accessions(ans)))

        }
    }
    stop("something went wrong - no records returned")
}

get_top_accessions = function(df)
{
    tmp = split(df, df$taxid)
    do.call(rbind, lapply(tmp, function(x) x[which.max(x$per_ident),]))
}

