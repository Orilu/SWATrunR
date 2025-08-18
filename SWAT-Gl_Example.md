# Example: using SWATrunR to calibrate SWAT-GL model<img src="man/figures/swatrunr_hex.svg" align="right" />





## Installation

You can install the `SWATrunR` from the package's GitHub repository: 

```r
# If the package remotes is not installed, run first:
install.packages("remotes")

remotes::install_github('Orilu/SWATrunR', ref = 'remove_legacy_gl')
```


## Calibration example

```r
library(SWATrunR)
library(lhs)
library(hydroGOF)
library(dplyr)

path_2012 <- "C:/your file path"

#Load observations
obs1 <-""
```
## Parameter definition

First, you can define the normal parameters and their respective ranges defined in the classical SWAT model, for instance:
```r
swat_params <- tibble("CN2.mgt | change = pctchg" = c(-20, 20),
                      "LAT_TTIME.hru | change = absval" = c(30, 100),
                      "GWQMN.gw | change = absval"= c(0, 1000),
                      "ALPHA_BF.gw | change = absval"= c(0.05, 1),                      
                      "TLAPS.sub | change = absval" = c(-8, -6),
                      "PLAPS.sub | change = absval" = c(20, 500))
```

Then you also define the new parameters included in the new SWAT-GL model related to glacier and snow dynamics (the latest snow parameters at each elevation band per subbasin):

```r
extra_params <- tibble(
  GLMTMP = c(0, 4),
  GLMFMX = c(2, 6),
  GLMFMN = c(0, 2),
  f_frze= c(0.05, 0.3),
  f_accu= c(0.1, 0.4),
  SFTMP = c(1, 4),
  SMTMP = c(0.5, 2),
  SMFMX = c(3, 5),
  SMFMN = c(0.1, 2),
  TIMP = c(0.3, 0.9)
)
```
Then, you can use any parameter sampling that you prefer; in this example, we are going to use Latin Hypercube Sampling (LHS):

```r
#number of simulations
n_simulations <- 1500

#sampling
sample_lhs <- function(par_bound, n_sample) {
  n_par <- ncol(par_bound)
  randomLHS(n_sample, n_par) %>%
    as_tibble() %>%
    set_names(names(par_bound)) %>%
    purrr::map2_df(par_bound, ~ (.x * diff(.y) + .y[1]))
}

swat_lhs <- sample_lhs(swat_params, n_simulations)
extra_lhs <- sample_lhs(extra_params, n_simulations)
```

After this, you can run the calibration using the same code as in  `SWATrunR` using the `run_swat2012` function, only adding the following extra line: "extra_params ="

```r
q_iter1<- run_swat2012(project_path = path_2012, 
                       output = list(q_1=define_output(file = "rch",
                                                         variable = "FLOW_OUT",
                                                         unit = 2),
                                     q_2=define_output(file = "rch",
                                                           variable = "FLOW_OUT",
                                                           unit = 3),
                        parameter = swat_lhs,
                        start_date = "2000-01-01",
                        end_date = "2024-12-31",
                        years_skip = 3, 
                        n_thread= 10, 
                        extra_params = extra_lhs #line to include new parameters
                        )
```
