library(metabarcodeUtils)

f = system.file(package = "metabarcodeUtils",
                "sample_files",
                "BTWIT1-20200429B.xlsx")

out_dir = tempdir()

tt = mitofish_to_csv(f, outdir = out_dir)

ff = list.files(out_dir)

stopifnot(length(ff) == 2)
stopifnot(length(tt) == 2)

# test to fasta

csv_to_fasta(file.path(out_dir, ff[1]), outdir = out_dir)
ff = list.files(out_dir)
stopifnot(length(ff) == 3)

