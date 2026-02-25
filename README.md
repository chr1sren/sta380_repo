## sta380_repo
Private repo for 2026 STA380H5 final project

Team name: Chou Why did

Authors: Renfei Wu, Shiqi Tian, Chengxi Li, Liwen Yin

## Initialization

Clone this repo to your machine. In RStudio:  
File → New Project → Existing Directory → select this repo.

## Installation

Install required packages in your R console:
```r
install.packages(c("testthat", "roxygen2", "rmarkdown", "devtools"))
```

## Testing
  
In your R console:
```r
library(testthat)
source("R/data_simulation.R")
source("R/permutation_test.R")
source("R/evaluation.R")
testthat::test_dir("R/tests/testthat")
```

## Render Docs

- Render R Code's roxygen documentation  
  Run in your terminal:
  ```bash
  R -e 'roxygen2::roxygenise()'
  ```
  Or run in your R console:
  ```r
  roxygen2::roxygenise()
  ```

- Render Rmd vignette for R code
  In your terminal:
  ```bash
  Rscript -e "rmarkdown::render('docs/vignette.Rmd')"
  ```
