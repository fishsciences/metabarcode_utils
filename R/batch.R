# Functions for batch processing files

##' These functions allow for batch processing MitoFish output. Each
##' takes a directory full of files.
##'
##' 
##' @title Batch process MitoFish output
##' @param file_dir character, the file path to the directory
##'     containing input files.
##' @param out_dir character, the file path to the output directory to
##'     write files to. If the directory does not exist, it will be
##'     created.
##' @param ... additional args passed to the processing function
##' @return list of processed output, in the case of mito_to_csv, or
##'     the file path to the written files, in the case of
##'     csv_to_fasta
##' @author Matt Espe
##' @export
batch_mito_to_csv = function(file_dir,
                             out_dir, ...)
{
    batch_generic(file_dir, out_dir, "\\.xlsx$", mito_to_csv, ...)
}

##' @rdname batch_mito_to_csv
##' @export
batch_csv_to_fasta = function(csv_dir,
                              out_dir, ...)
{
    batch_generic(csv_dir, out_dir, "\\.csv$", csv_to_fasta, ...)
}


##' @param file_dir the directory to look for raw NCBI output files
##' @param files vector, the full file path to the raw NCBI files
##' @rdname batch_reduce_ncbi
##' @export
batch_extract_ncbi_tabs = function(file_dir,
                                   files = list.files(file_dir,
                                                      pattern = "\\.txt$",
                                                      full.names = TRUE,
                                                      recursive = TRUE),
                                   ...)
{
    ans = lapply(files, function(f){
        try(extract_ncbi_tabs(f, outdir = out_dir, ...))
    })
    
    if(any(sapply(ans, is, "try-error")))
        warning("Error's encountered while processing some files. Inspect input directory and output.")
    
    names(ans) = files
    return(ans)
}

##' Convenience function to batch process NCBI tables.
##'
##' 
##' @title Batch reduce NCBI tables
##' @param tab_list a list of extracted tables created by
##'     \code{batch_extract_ncbi_tabs}
##' @param accession_filter character, one of "all" to keep all
##'     accessions, or "top" to only keep the top accession.
##' @param out_dir optional, if specified, the reduced tables will be
##'     written to CSV in the directory specified. If the directory
##'     does not exist, it will be created.
##' @param ... additional args passed to \code{reduce_nbci}
##' @return list of reducded tables
##' @author Matt Espe
##' @export
batch_reduce_ncbi = function(tab_list, accession_filter = c("all", "top"),
                             out_dir, ...)
{
    if(!dir.exists(out_dir))
        dir.create(out_dir)

    ans = lapply(tab_list, function(x){
        try(reduce_ncbi(x, accession_keep = accession_filter, ...))
    })

    is_err = sapply(ans, is, "try-error")
    if(any(is_err)){ 
        warning("Error's encountered while processing some tables. Inspect input.")   
        ans = ans[!is_err]
    }
    
    if(!missing(out_dir))
        lapply(ans, write_ncbi_tabs, outdir = out_dir)

    ans
}


batch_generic = function(file_dir,
                         out_dir,
                         file_pat,
                         fun, 
                         files = list.files(file_dir,
                                            pattern = file_pat,
                                            full.names = TRUE,
                                            recursive = TRUE))
{
    if(missing(out_dir))
        out_dir = tempdir()
    
    if(!dir.exists(out_dir))
        dir.create(out_dir)

    ans = lapply(files, function(f){
        try(fun(f, outdir = out_dir, ...))
    })
    
    if(any(sapply(ans, is, "try-error")))
        warning("Error's encountered while processing some files. Inspect input directory and output.")

    names(ans) = files
    return(ans)
}

                              
