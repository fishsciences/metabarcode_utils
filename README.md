# metabarcodeUtils

This simple package contains helper functions for the meta-barcoding workflow. 

# Installing the package

```
devtools::install_github("fishsciences/metabarcode_utils")
```

## Using the package

This package currently starts with a xlsx result file, renamed, from
the MitoFish tool. For testing, an example file is included with the
package.

```
f = system.file(package = "metabarcodeUtils",
                "sample_files",
                "BTWIT1-20200429B.xlsx")
```

This file can be parsed and converted into two CSV files (one for the
hits and one for the no hits) using the `mitofish_to_csv()` function:

```
tt = mitofish_to_csv(f, outdir = out_dir)
```

By default, this will return a list with two data.frames, and will
write the results to `outdir`. These results can then be further
processed into Fasta files with `csv_to_fasta()`:

```
csv_to_fasta(file.path(out_dir, ff[1]), outdir = out_dir)
```
