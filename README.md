# PEPTools

PEPTools is an R package within the 
[NWFSC-assess](https://github.com/nwfsc-assess)
repository directory meant to house R functions that are applicable to 
assessors in general or used by multiple packages stored within the directory. 

## Installation

``` r
devtools::install_github("nwfsc-assess/PEPtools")
```

## Development protocol

  1. `git clone https://github.com/nwfsc-assess/PEPtools.git`
  2. `cd PEPtools`
  3. `git branch featurename`
  4. `git checkout featurename`
  5. `git add <file1> <file2> ...`
  6. `git commit`
  7. ...
  8. `git fetch`
  9. `git rebase origin/master`
  10. `git push origin featurename`
  11. navigate to [](https://github.com/nwfsc-assess/PEPtools) and 
  instigate a pull request for your branch featurename
  12. changes will be tested and then merged into the master branch