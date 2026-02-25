## sta380_repo
Private repo for 2026 STA380H5 final project

Team name: Chou Why did

Authors: Renfei Wu, Shiqi Tian, Chengxi Li, Liwen Yin

## Testing
  
In your R terminal:
```r
library(testthat)
source("R/data_simulation.R")
source("R/permutation_test.R")
source("R/bootstrap_stability.R")
source("R/evaluation.R")
testthat::test_dir("R/tests/testthat")
```

## Render Docs

- Render R Code's roxygen documentation  
  In your terminal:
  ```r
  R -e 'roxygen2::roxygenise()'
  ```

- Render Rmd vignette for R code
  In your terminal:
  ```bash
  Rscript -e "rmarkdown::render('vignette.Rmd')"
  ```
