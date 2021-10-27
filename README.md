# metabarcodeUtils

This simple package contains helper functions for the meta-barcoding workflow. 

# Installing the package

```
devtools::install_github("fishsciences/metabarcode_utils")
```

## Using the package

### Summarizing MitoFish Output

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

### Summarizing NCBI BLAST Output

To extract and summarize the tables from a NCBI output file,

```
ncbi_tabs = extract_ncbi_tabs("R69MU8R7016-Alignment.txt")
```

This returns all the values in the table. To subset these according to:

 - Step 1.  Query cover matters.  If coverage is less than 90% (this
   may be too high, can we make this adjustable?) then we cannot
   identify the sequence to any organism.  The output should be “<90%
   coverage” for this “Query”

 
 - Step 2. Is Per Ident >97%.  If yes, then we need Scientific name,
   common name, query cover, Per ID, Accession#.  We only need one
   record of each species.

 
- Step 3. Is Per Ident >97% for multiple species.  If yes, then we
  need Scientific name, common name, query cover, Per ID, Accession#.
  We only need one record of each species.

 
- Step 4.  Is Per Ident between 80-96.9%.  If yes, then we need
  Scientific name, common name, query cover, Per ID, Accession# for
  all records.  We only need one record for each species.

- Step 5.  Is Per Ident less than 80%.  If yes, then output should be
  “<80% per ident”

This is accomplished with 

```
top_accessions = reduce_ncbi(ncbi_tabs, accession_keep = "top")
```

These tables can be written to a CSV file with,

```
d = create.dir("ncbi_output")
write_ncbi_tabs(top_accessions, "ncbi_output")
```
