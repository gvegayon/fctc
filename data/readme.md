# Processed data

In this folder we store processed data (so no raw anymore). All CSV files have NA
values represented by <NA>, so, in order to import these into R (for example) the
user should use the command.

```r
read.csv("file.csv", na="<NA>")
```

So far, we have the following datasets:

* `treaty_dates`: A data frame with 'signature' and 'ratification' dates per party.

* `political_shifts`: A data frame with relative political shift view as whether
  the incumbent party of year X is different from the incumbent party on the
  year the treaty was ratified.

* `party_attributes`: A data frame with information on GDP, membership, tobacco
  prod, tobacco mortality, etc. per member.

* `implementation`: Using the implementation data published online
  at the FCTC website, this generates a data frame with counts of how many 
  points have been implemented per country/year for articles 5, 6, 8, 11, and 13.

* `bloomberg`: A data frame with the number of projects (with amounts) funded
  by the Bloomberg initiative, by country/year. Includes projects especifically
  developed to contribute to the implementation of the FCTC.

The following datasets depend on the previous ones to be built:

* `model_data`: A data frame that is the merge between `party_attributes`, `political_shifts`,
  `implementation`, and `bloomberg`.
  
* `adjmat`: Holds `dgCMatrix` objects (sparse matrices), and depends on
  `party_attributes`.
  