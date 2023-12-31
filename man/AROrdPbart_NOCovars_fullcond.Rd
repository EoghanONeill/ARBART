% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AROrdPbart_NOCovars_fullcond.R
\name{AROrdPbart_NOCovars_fullcond}
\alias{AROrdPbart_NOCovars_fullcond}
\title{Auto-regressive Ordered Probit BART Model without exogenous covariates}
\usage{
AROrdPbart_NOCovars_fullcond(
  y.train = NULL,
  n.iter = 1000L,
  n.burnin = 100L,
  num_test_periods = 1L,
  num_lags = 1L,
  indiv.offsets = FALSE,
  num_z_iters = 10L,
  keep_zmat = FALSE,
  noise_in_pred = 1L,
  n.trees = 50L,
  n.burn = 0L,
  n.samples = 1L,
  n.thin = 1L,
  n.chains = 1L,
  n.threads = 1L,
  printEvery = 100L,
  printCutoffs = 0L,
  rngKind = "default",
  rngNormalKind = "default",
  rngSeed = NA_integer_,
  updateState = TRUE,
  print.opt = 100L,
  seq_z_draws = 1
)
}
\arguments{
\item{y.train}{A vector (or matrix for panel data) of ordered integer outcomes. Rows correspond to time periods, columns correspond to variables.}

\item{n.iter}{Number of iterations excluding burnin.}

\item{n.burnin}{Number of burnin iterations.#'}

\item{num_test_periods}{Number of future time periods to predict.}

\item{num_lags}{Number of lags of latent outcome to be included as potential splitting variables. Currently must be equal to 1.}

\item{indiv.offsets}{Include individual-specific offsets? Only relevant if yttrain contains more than one column. Currently not supported.}

\item{num_z_iters}{Number of Gibbs iterations for latent outcome z samples per overall MCMC iteration.}

\item{keep_zmat}{Boolean. If equal to TRUE output the draws of Zmat for training data and test data}

\item{noise_in_pred}{If equal to 1, keep noise in test prediction calculations.}

\item{n.trees}{(dbarts option) A positive integer giving the number of trees used in the sum-of-trees formulation.}

\item{n.chains}{(dbarts option) A positive integer detailing the number of independent chains for the dbarts sampler to use (more than one chain is unlikely to improve speed because only one sample for each call to dbarts).}

\item{n.threads}{(dbarts option) A positive integer controlling how many threads will be used for various internal calculations, as well as the number of chains. Internal calculations are highly optimized so that single-threaded performance tends to be superior unless the number of observations is very large (>10k), so that it is often not necessary to have the number of threads exceed the number of chains.}

\item{printEvery}{(dbarts option)If verbose is TRUE, every printEvery potential samples (after thinning) will issue a verbal statement. Must be a positive integer.}

\item{printCutoffs}{(dbarts option) A non-negative integer specifying how many of the decision rules for a variable are printed in verbose mode}

\item{rngKind}{(dbarts option) Random number generator kind, as used in set.seed. For type "default", the built-in generator will be used if possible. Otherwise, will attempt to match the built-in generator’s type. Success depends on the number of threads.}

\item{rngNormalKind}{(dbarts option) Random number generator normal kind, as used in set.seed. For type "default", the built-in generator will be used if possible. Otherwise, will attempt to match the built-in generator’s type. Success depends on the number of threads and the rngKind}

\item{rngSeed}{(dbarts option) Random number generator seed, as used in set.seed. If the sampler is running single-threaded or has one chain, the behavior will be as any other sequential algorithm. If the sampler is multithreaded, the seed will be used to create an additional pRNG object, which in turn will be used sequentially seed the threadspecific pRNGs. If equal to NA, the clock will be used to seed pRNGs when applicable.}

\item{updateState}{(dbarts option) Logical setting the default behavior for many sampler methods with regards to the immediate updating of the cached state of the object. A current, cached state is only useful when saving/loading the sampler.}

\item{print.opt}{Print every print.optnumber of Gibbsa samples.}
}
\value{
A list is returned containing the following elements:
\item{mu}{Training sample MCMC draws of sum-of-tree predictions of mean of latent vairable f(zlag,x). Array of dimensions  c(num_indiv, n.time_train, n.iter).}
\item{mu_test}{Test sample MCMC draws of sum-of-tree predictions of mean of latent vairable f(zlag,x). Array of dimensions  c(num_indiv, num_test_periods, n.iter).}
\item{prob.train.draws}{Training sample MCMC draws of probability outcome equals 1. Array of dimensions  c(num_indiv, n.time_train, n.iter). Normal CF of mu values.}
\item{prob.test.draws}{Test sample MCMC draws of probability outcome equals 1. Array of dimensions  c(num_indiv, num_test_periods, n.iter). Normal CDF of mu_test values.}
\item{Z.mat}{Samples from the posterior of the latent scores training observations. Dimensions c(n.time_train, num_indiv, n.iter).}
\item{Z.mat.test}{Samples from the posterior of the latent scores for test observations. Dimensions c(num_test_periods, num_indiv, n.iter).}
}
\description{
Auto-regressive (of order one) Ordered Probit BART Model without exogenous covariates
}
