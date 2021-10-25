# reads files from mitofish

##' Converts from a mitofish XLSX file to two CSVs. The hit CSV
##' contains with one row per hit, with the sample name designating
##' the stregth of the hit. The nohit CSV contains just the sequence of number of hits
##'
##' @title Mitofish to CSV
##' @param file file path to the xlsx file output from mitofish
##' @param outfile basename for output files, defaults to the name of the input file
##' @param outdir directory to write CSVs to. Defaults to current working directory
##' @return list, with a data.frame of hits and a data.frame for nohits
##' @author Matt Espe
##' @export
mitofish_to_csv = function(file, outfile = basename(file),
                           outdir = ".")
{
    ff = gsub("\\.xlsx?", "", outfile)
    
    sh = excel_sheets(file)
    st = get_sheet_type(sh)
    df = lapply(sh, function(x) as.data.frame(read_excel(file, sheet = x)))
    df = lapply(df, function(x) {
        colnames(x) = gsub(" ", "", colnames(x))
        x
    })
    names(df) = st
    
    hit = df[st != "NoHits"]
    nohit = df[st == "NoHits"][[1]]
    
    hit = mapply(process_hits, hit, names(hit), SIMPLIFY=FALSE)
    hit = do.call(rbind, hit)
    write.csv(hit, file.path(outdir, paste0(ff, "_hits.csv")), row.names = FALSE)
    write.csv(nohit[[1]], file.path(outdir, paste0(ff, "_nohit.csv")), row.names = FALSE)
    return(list(hits = hit, nohits = nohit))
}


get_sheet_type = function(x)
{
    gsub("BLASTN|%Similarity", "", x)
}

process_hits = function(df, st,
                        keep_cols = c("SampleName", "Species",
                                      "Family", "Totalread", "Sequence"))
{
    df = reduce_mitofish(df)
    df$SampleName = fix_sample_name(df, st)
    df[,keep_cols]
}


reduce_mitofish = function(d, split_col = d$Species,
                            size_var = "Size")
{
    tmp = split(d, split_col)
    do.call(rbind, lapply(tmp, function(df) {
        df[which.max(df[[size_var]]),]
        }))
}

fix_sample_name = function(df, sheet_type,
                           sample_col = df$`Samplename`,
                           sp_col = df$Species,
                           id_col = df$ID,
                           end_pattern = "_.*R$")
{
    x = gsub(end_pattern, "", sample_col)
    switch(sheet_type,
           ">97" = paste0(x, "_", sp_col, "_97"),
           "80-97" = paste0(x, "_", sp_col, "_80"),
           "NoHits" = paste0(x, "_NoHit_", seq(nrow(df))))
}
