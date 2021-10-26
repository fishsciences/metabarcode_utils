library(metabarcodeUtils)

f = system.file(package = "metabarcodeUtils", "sample_files", "R69MU8R7016-Alignment.txt")

tt = extract_ncbi_tabs(f)
