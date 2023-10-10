
# ARBART

<!-- badges: start -->
<!-- badges: end -->

The goal of ARBART is to implement autoregressive Bayesian Additive Regression Tree Models for Limited Dependent Variables (AR binary Probit BART, AR Ordered Probit BART, AR Tobit BART)

## Installation


``` r
library(devtools)
install_github("EoghanONeill/ARBART")
```

## Example

Example taken from ``SMCS4`` package
``` r
library(ARBART)
## basic example code


set.seed(120912)
nt <- 100
xtemp <- cbind(rnorm(nt,sd=1),rnorm(nt,sd=1))
theta <- cbind(
  rnorm(1,mean=0,sd=sqrt(.2))+c(0,cumsum(rnorm(nt-1,sd=sqrt(.01)))),
  rnorm(1,mean=0,sd=sqrt(.2))+c(0,cumsum(rnorm(nt-1,sd=sqrt(.01)))),
  rnorm(1,mean=0,sd=sqrt(.2))+c(0,cumsum(rnorm(nt-1,sd=sqrt(.01))))
)
y2 <- rbinom(nt,prob=1-pnorm(rowSums(cbind(1,xtemp)*theta)),size=1)

arpbart <- ARProbitbartNOCovars_fullcond(y.train = y2,
                              seq_z_draws = 0,
                              print.opt = 1,
                              n.burnin = 0,
                              n.iter  = 100L)

```

