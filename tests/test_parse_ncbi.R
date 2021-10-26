library(metabarcodeUtils)

f = system.file(package = "metabarcodeUtils", "sample_files", "R69MU8R7016-Alignment.txt")

tt = extract_ncbi_tabs(f)

ww = reduce_ncbi(tt[[2]], accession_keep = "top")
w2 = reduce_ncbi(tt[[2]], accession_keep = "all")

w3 = reduce_ncbi(tt, accession_keep = "top")

stopifnot(!any(duplicated(ww$taxid)))
stopifnot(nrow(w2) > nrow(ww))
stopifnot(class(w3) == "list")


