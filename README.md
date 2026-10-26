
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

arpbart <- ARPbartX_FFBS_emptynodes(y.train = as.matrix(y2),
                                        X.train = xtemp,
                                        X.test = xtemp[c(ncol(xtemp),ncol(xtemp)) ,],
                                        print.opt = 100L, num_z_iters = 1L,
                                        n.burnin = 500L,
                                        n.iter  = 2000L, validate_rebuilt_trees = FALSE,
                                        num_test_periods = 2)

```

