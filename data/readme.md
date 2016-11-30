# Processed data

In this folder we store processed data (so no raw anymore). So far, we have the
following files:

* `treaty_dates`: A data frame with 'signature' and 'ratification' dates per party.

* `political_shifts`: A data frame with relative political shift view as whether
  the incumbent party of year X is different from the incumbent party on the
  year the treaty was ratified.

* `party_attributes`: A data frame with information on GDP, membership, tobacco
  prod, tobacco mortality, etc. per member.

* `fctc_implementation_sums`: Using the implementation data published online
  at the FCTC website, this generates a data frame with counts of how many 
  points have been implemented per country/year for articles 5, 6, 8, 11, and 13.

* `bloomberg`: A data frame with the number of projects (with amounts) funded
  by the Bloomberg initiative, by country/year. Includes projects especifically
  developed to contribute to the implementation of the FCTC.

