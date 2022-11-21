# PEPTools

PEPTools is an R package within the 
[pfmc-assessments](https://github.com/pfmc-assessments)
organization meant to house R functions that are applicable to 
assessors in general or used by multiple packages stored within the organization. 

## Installation

``` r
devtools::install_github("pfmc-assessments/PEPtools")
```

## Development protocol

  1. `git clone https://github.com/pfmc-assessments/PEPtools.git`
  2. `cd PEPtools`
  3. `git branch featurename`
  4. `git checkout featurename`
  5. `git add <file1> <file2> ...`
  6. `git commit`
  7. ...
  8. `git fetch`
  9. `git rebase origin/master`
  10. `git push origin featurename`
  11. navigate to [](https://github.com/pfmc-assessments/PEPtools) and 
  instigate a pull request for your branch featurename
  12. changes will be tested and then merged into the master branch