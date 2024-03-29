library(metabarcodeUtils)

f = system.file(package = "metabarcodeUtils", "sample_files", "R69MU8R7016-Alignment.txt")

tt = extract_ncbi_tabs(f)

ww = reduce_ncbi(tt[[2]], accession_keep = "top")
w2 = reduce_ncbi(tt[[2]], accession_keep = "all")

w3 = reduce_ncbi(tt, accession_keep = "top")

stopifnot(!any(duplicated(ww$taxid)))
stopifnot(nrow(w2) > nrow(ww))
stopifnot(class(w3) == "list")

d = tempdir()
write_ncbi_tabs(w3, d)
write_ncbi_tabs(w3[[1]], d)
write_ncbi_tabs(w3, d, combine = TRUE)
w3$Species3 = "<90% Coverage"
w3$Species4 = "<80% Per Ident"

write_ncbi_tabs(w3, d)

if(FALSE) { # these tests require large files

    f1 = "~/Downloads/S5GMB5JE013-Alignment.txt"

    tt = extract_ncbi_tabs(f1)

    f2 = "~/Downloads/S5GPWM4D013-Alignment.txt"
    tt = extract_ncbi_tabs(f2)
}
