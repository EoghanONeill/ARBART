


#' Auto-regressive Ordered Probit BART Model with exogenous covariates
#'
#' Auto-regressive (of order one) Ordered Probit BART Model with exogenous covariates
#' @import truncnorm
#' @import mvtnorm
#' @import dbarts
#' @importFrom Rcpp evalCpp
#' @param y.train A vector (or matrix for panel data) of ordered integer outcomes. Rows correspond to time periods, columns correspond to variables.
#' @param X.train A training data matrix with rows corresponding to time-individual specific observations and columns corresponding to variables. The first ncol(y.train) rows are for all individuals in the first time period, then rows ncol(y.train) to 2*ncol(y.train) are for all individuals in the next time period, and so on.
#' @param X.testing A test data matrix with rows corresponding to time-individual units and columns corresponding to variables. The first ncol(y.train) rows are for all individuals in the first test period, then rows ncol(y.train) to 2*ncol(y.train) are for all individuals in the next test period, and so on.
#' @param n.iter Number of iterations excluding burnin.
#' @param n.burnin Number of burnin iterations.#'
#' @param num_test_periods Number of future time periods to predict.
#' @param num_lags Number of lags of latent outcome to be included as potential splitting variables. Currently must be equal to 1.
#' @param indiv.offsets Include individual-specific offsets? Only relevant if yttrain contains more than one column. Currently not supported.
#' @param num_z_iters Number of Gibbs iterations for latent outcome z samples per overall MCMC iteration.
#' @param keep_zmat Boolean. If equal to TRUE output the draws of Zmat for training data and test data
#' @param noise_in_pred If equal to 1, keep noise in test prediction calculations.
#' @param n.trees (dbarts option) A positive integer giving the number of trees used in the sum-of-trees formulation.
#' @param printEvery (dbarts option)If verbose is TRUE, every printEvery potential samples (after thinning) will issue a verbal statement. Must be a positive integer.
#' @param printCutoffs (dbarts option) A non-negative integer specifying how many of the decision rules for a variable are printed in verbose mode
#' @param rngKind (dbarts option) Random number generator kind, as used in set.seed. For type "default", the built-in generator will be used if possible. Otherwise, will attempt to match the built-in generator’s type. Success depends on the number of threads.
#' @param rngNormalKind (dbarts option) Random number generator normal kind, as used in set.seed. For type "default", the built-in generator will be used if possible. Otherwise, will attempt to match the built-in generator’s type. Success depends on the number of threads and the rngKind
#' @param rngSeed (dbarts option) Random number generator seed, as used in set.seed. If the sampler is running single-threaded or has one chain, the behavior will be as any other sequential algorithm. If the sampler is multithreaded, the seed will be used to create an additional pRNG object, which in turn will be used sequentially seed the threadspecific pRNGs. If equal to NA, the clock will be used to seed pRNGs when applicable.
#' @param updateState (dbarts option) Logical setting the default behavior for many sampler methods with regards to the immediate updating of the cached state of the object. A current, cached state is only useful when saving/loading the sampler.
#' @param print.opt Print every print.optnumber of Gibbsa samples.
#' @return A list is returned containing the following elements:
#' \item{mu}{Training sample MCMC draws of sum-of-tree predictions of mean of latent vairable f(zlag,x). Array of dimensions  c(num_indiv, n.time_train, n.iter).}
#' \item{mu_test}{Test sample MCMC draws of sum-of-tree predictions of mean of latent vairable f(zlag,x). Array of dimensions  c(num_indiv, num_test_periods, n.iter).}
#' \item{prob.train.draws}{Training sample MCMC draws of probability outcome equals 1. Array of dimensions  c(num_indiv, n.time_train, n.iter). Normal CF of mu values.}
#' \item{prob.test.draws}{Test sample MCMC draws of probability outcome equals 1. Array of dimensions  c(num_indiv, num_test_periods, n.iter). Normal CDF of mu_test values.}
#' \item{Z.mat}{Samples from the posterior of the latent scores training observations. Dimensions c(n.time_train, num_indiv, n.iter).}
#' \item{Z.mat.test}{Samples from the posterior of the latent scores for test observations. Dimensions c(num_test_periods, num_indiv, n.iter).}
#' @useDynLib ARBART, .registration = TRUE
#' @export
AROrdPbartX_fullcond <- function(y.train = NULL,
                              X.train = matrix(NA, nrow = nrow(y.train)*ncol(y.train), ncol = 0),
                              X.test = NULL, # matrix(NA, nrow =0, ncol = 0),
                              n.iter=1000L,
                              n.burnin=100L,
                              num_test_periods = 1L,
                              num_lags = 1L, #initial.list = NULL,
                              indiv.offsets = FALSE,
                              num_z_iters = 10L,
                              keep_zmat = FALSE,
                              noise_in_pred = 1L,
                              n.trees = 50L,
                              n.burn = 0L,
                              # n.samples = 1L,
                              # n.thin = 1L,
                              # n.chains = 1L,
                              # n.threads = 1L,#guessNumCores(),
                              printEvery = 100L,
                              printCutoffs = 0L,
                              rngKind = "default",
                              rngNormalKind = "default",
                              rngSeed = NA_integer_,
                              updateState = TRUE,
                              print.opt = printEvery,
                              seq_z_draws = 1,
                              probit_k = 2.0,
                              reference_half_range = 3.0
){



  ######### Check inputs ##################

  if(num_test_periods < 1){ stop("num_test_periods must be >=1") }
  if(!is.integer(num_lags)){ stop("num_lags must be an integer.") }
  if(num_lags <1){ stop("num_lags must be an integer greater than or equal to 1.") }
  if(num_lags >1){ stop("Code not yet written for num_lags > 1.")}
  if (length(num_z_iters) != 1L || is.na(num_z_iters) || num_z_iters != as.integer(num_z_iters) || num_z_iters < 1L){stop("num_z_iters must be an integer greater than or equal to 1. It is the number of repeated samples of latent variable values per Gibbs iteration.")}
  #check that input values are binary 0 or 1.
  if (nrow(y.train) < 3L) stop("ARPbartX_fullcond currently requires at least three training periods.", call. = FALSE)

  # if(!(all(is.integer(y.train)))){
  #   stop("y.train contains non-integers")
  # }
  if (is.null(y.train) ||anyNA(y.train) ||any(!is.finite(y.train)) ||any(y.train != floor(y.train))) {stop("y.train must contain finite, non-missing integer-valued outcomes.")}
  y.train <- as.matrix(y.train)
  storage.mode(y.train) <- "integer"
  miny <- min(y.train)
  maxy <- max(y.train)

  num_categories <- maxy - miny + 1
  if (num_categories < 3L) {stop("Use the binary-probit function for a two-category outcome.")}
  if(!(all(y.train %in% miny:maxy))){stop("!(all(y.train %in% miny:maxy))")}
  for(y_ind in miny:maxy){if(sum(1*(y.train == y_ind) ) ==0){
      stop("There are zero training obserations for an integer between the minimum and maximum outcome values. Redefine outcome or use different training data.")
    }
  }
  # check input covariates
  X.train <- as.matrix(X.train)
  X.test <- as.matrix(X.test)
  y.train <- as.matrix(y.train)



  if(nrow(X.train) != ncol(y.train)*nrow(y.train)){
    print("nrow(X.train) = ")
    print(nrow(X.train))
    print("nrow(y.train) = ")
    print(nrow(y.train))
    print("ncol(y.train) = ")
    print(ncol(y.train))
    stop("nrow(X.train) != ncol(y.train)*nrow(y.train)")
  }
  if (is.null(X.test)){
    print("X.test is NULL, creating matrix of zeros ")
    X.test <- matrix(numeric(0), nrow = ncol(y.train) * num_test_periods, ncol = 0L)
  }
  if(nrow(X.test) != ncol(y.train)*num_test_periods){
    print("nrow(X.test) = ")
    print(nrow(X.test))
    print("num_test_periods = ")
    print(num_test_periods)
    print("ncol(y.train) = ")
    print(ncol(y.train))
    stop("nrow(X.test) != ncol(y.train)*num_test_periods")
  }

  y.train <- as.matrix(y.train)
  y.vec <- as.vector(t(y.train))
  y.train_from1 <- y.train - miny + 1
  y_from1.vec <- as.vector(t(y.train_from1))
  num_indiv <- ncol(y.train)
  n.time_train <- nrow(y.train)
  num_total <- n.time_train*num_indiv

  ######## create output list to be filled in #########################
  draw = list(
    mu = array(NA, dim = c(num_indiv, n.time_train, n.iter)),#,
    mu_test = array(NA, dim = c(num_indiv , num_test_periods , n.iter)),
    prob.train.draws = array(NA, dim = c(num_indiv, n.time_train, num_categories, n.iter)),#,
    prob.test.draws = array(NA, dim = c(num_indiv , num_test_periods , num_categories, n.iter)),
    thresholds = array(NA, dim = c(num_categories - 1 , n.iter))
  )

  if(keep_zmat==TRUE){
    draw$Z.mat = array(NA, dim = c(n.time_train, num_indiv, n.iter) )
    draw$Z.mat.test = array(NA, dim = c(num_test_periods, num_indiv, n.iter) )
  }

  ###### initial values for Z ###########################
  ## initial values for Z
  Z.mat <- matrix(NA, nrow = n.time_train, ncol = num_indiv)
  # Just independently sample from truncated distribution?
  # or take fixed values determined by truncation?
  # check standard probit BART initial values

  # # create offset
  # num_ones <- sum(y.train)
  # prop_ones <- mean(y.train)
  #
  # offsetz <- qnorm(prop_ones)

  offsetz <- 0 #qnorm(prop_ones)
  offsetz_vec <- rep(offsetz, num_indiv*n.time_train)
  offsetz_vec_test <- rep(offsetz, num_indiv*num_test_periods)
  num_ones_vec <- as.vector(sum(y.train))
  prop_ones_vec <- as.vector(mean(y.train))

  if(indiv.offsets == TRUE){stop("The code currently does not support indiv.offsets == TRUE") }
  # # alternative: individual-specific offsets
  # if((indiv.offsets == TRUE)){
  #   if(num_indiv > 1){
  #     num_ones_vec <- apply(y.train,2,sum)
  #     prop_ones_vec <- apply(y.train,2,mean)
  #     offsetz_vec <- qnorm(prop_ones_vec) # rep(0,num_indiv) #
  #   }else{
  #     offsetz_vec <- rep(offsetz, num_indiv)
  #     num_ones_vec <- as.vector(sum(y.train))
  #     prop_ones_vec <- as.vector(mean(y.train))
  #   }
  #
  # }else{
  #   offsetz_vec <- rep(offsetz, num_indiv)
  #   num_ones_vec <- as.vector(sum(y.train))
  #   prop_ones_vec <- as.vector(mean(y.train))
  # }

  # # Try naive pooled model (no dynamics, no covariates, just an intercept)
  # temp_ordprobMLE <- MASS::polr(ordered(y.train) ~ 1,
  #                               method = "probit")
  y_levels <- miny:maxy
  y_ord <- ordered(as.vector(y.train),levels = y_levels)
  temp_ordprobMLE <- MASS::polr(y_ord ~ 1,method = "probit",Hess = TRUE)
  if (length(temp_ordprobMLE$zeta) != num_categories - 1L ||
      any(!is.finite(temp_ordprobMLE$zeta))) {
    stop("Invalid threshold estimates returned by MASS::polr().")
  }

  # # MCMCpack multiplies thresholds by 0.588, This is not clear.
  # thresholds <- rep(NA, num_categories-1)
  # # thresholds[1] <- -Inf
  # thresholds[1] <- 0
  # # thresholds[num_categories+1] <- Inf
  # # thresholds[2:(num_categories-1)] <- temp_ordprobMLE$zeta[2:(num_categories-1)]
  #
  # # since we set one threshold to for identification, demean by first threhsold from polr function
  # thresholds[2:(num_categories-1)] <- temp_ordprobMLE$zeta[2:(num_categories - 1)] - temp_ordprobMLE$zeta[1]
  thresholds <- numeric(num_categories - 1L)
  thresholds[1L] <- 0
  if (num_categories > 2L) {
    thresholds[2:(num_categories - 1L)] <-temp_ordprobMLE$zeta[2:(num_categories - 1L)] - temp_ordprobMLE$zeta[1L]
  }
  if (length(thresholds) != num_categories - 1L ||any(!is.finite(thresholds)) ||any(diff(thresholds) <= 0)) {
    stop("Thresholds must be finite and strictly increasing.")
  }
  # ordinalbayes package uses something like
  # alpha.precision <- 1/alpha.var
  # coerce.precision <- 1/coerce.var
  # alpha <- numeric()
  # pi.0 <- table(Y)/length(Y)
  # tab <- table(Y)
  # alpha <- log(cumsum(pi.0)/(1 - cumsum(pi.0)))[1:(k - 1)]

  # # takes proportions
  # cat_props <- table(y.train)/length(y.train)
  # # then inverse transform from empirical CDF to Z thresholds
  # # and shift
  # thresholds <- c(0,
  #                 qnorm(cat_props)[1:(num_categories-1)])

  # takes proportions
  # then inverse transform from empirical CDF to Z thresholds
  # and shift
  # tempthresh <- qnorm(cumsum(table(y.train)/length(y.train)))
  # thresholds <- (tempthresh - tempthresh[1])[1:(length(tempthresh)-1)]

  # # fully vectorized, but does not allow for individual-specific offsets
  # one_inds <- which(y.train  ==1)
  #
  # if(indiv.offsets == FALSE){
  #   # Z.mat[one_inds] <- rtruncnorm(num_ones, a= 0, b = Inf, mean = offsetz, sd = 1)
  #   # perhaps can use binary to select instead
  #   Z.mat[y.train] <- rtruncnorm(num_ones, a= 0, b = Inf, mean = offsetz, sd = 1)
  #   Z.mat[1- y.train] <- rtruncnorm(1, a= -Inf, b = 0, mean = offsetz, sd = 1)
  #
  # }

  # This can probably be vectorized
  for(j in 1:num_indiv){
    #partly vectorized code
    # if((indiv.offsets == FALSE)|(num_indiv == 1)){
    Z.mat[y.train[,j]== miny,j] <- rtruncnorm(n = sum(y.train[,j]==miny),
                                              a= -Inf,
                                              b = 0,
                                              mean = 0,#offsetz,
                                              sd = 1)

    if (num_categories > 2L) {
      for(y_ind in (miny+1):(maxy-1)){
        Z.mat[y.train[,j]== y_ind,j] <- rtruncnorm(n = sum(y.train[,j]== y_ind),
                                                   a= thresholds[y_ind - miny],
                                                   b = thresholds[y_ind - miny +1],
                                                   mean = 0, #offsetz,
                                                   sd = 1)
      }
    }
    Z.mat[y.train[,j]==maxy,j] <- rtruncnorm(n = sum(y.train[,j] == maxy),
                                             a= thresholds[num_categories-1],
                                             b = Inf,
                                             mean = 0, # offsetz,
                                             sd = 1)

    #   Z.mat[y.train[,j]==1,j] <- rtruncnorm(sum(y.train[,j]), a= 0, b = Inf, mean = offsetz, sd = 1)
    #   Z.mat[y.train[,j]==0,j] <- rtruncnorm(n.time_train - sum(y.train[,j]), a= -Inf, b = 0, mean = offsetz, sd = 1)
    # }else{
    #   Z.mat[y.train[,j]==1,j] <- rtruncnorm(num_ones_vec[j], a= 0, b = Inf, mean = offsetz_vec[j], sd = 1)
    #   Z.mat[y.train[,j]==0,j] <- rtruncnorm(n.time_train - num_ones_vec[j], a= -Inf , b = 0, mean = offsetz_vec[j], sd = 1)
    # }

  } # end j loop

  # print("Z.mat = ")
  # print(Z.mat)
  Z.mat.test =  matrix(rnorm(num_test_periods*num_indiv), nrow = num_test_periods, ncol = num_indiv)
  # if num_indiv > 1, must vectorize for input to dbarts
  # the vectorized matrix might be more convenient to work with if each time period is a continguous block?
  Z.vec <- as.vector(t(Z.mat))
  Z.vec.test <- as.vector(t(Z.mat.test))

  Z_initial <-  Z.vec
  # Z_reference <- ifelse(t(y.train)==maxy, reference_half_range,
  #                       ifelse(t(y.train)==miny, -reference_half_range,
  #                              -reference_half_range + (t(y.train)- miny)*(2*reference_half_range)/(maxy-miny)
  #                              ) )
                        # Z_initial # as.numeric(Z.vec)
  Z_reference <- ifelse(y.vec==maxy, reference_half_range,
                        ifelse(y.vec==miny, -reference_half_range,
                               -reference_half_range + (y.vec- miny)*(2*reference_half_range)/(maxy-miny) ) )


  scale_min_fixed <- min(Z_reference)
  scale_max_fixed <- max(Z_reference)
  scale_range_fixed <- scale_max_fixed - scale_min_fixed
  # offset_current <- numeric(
  #   length(Z_reference)
  # )

  computational_offset <- Z_reference - Z_initial
  dbarts_offset <- computational_offset + offsetz_vec

  min_resp <- scale_min_fixed # min(sampler$data@y)
  max_resp <- scale_max_fixed # max(sampler$data@y)
  response_range <- scale_range_fixed # max_resp - min_resp

  ## initial values
  # alpha = rep(0, n.item)
  # beta = rep(0, p.cov)

  # print("create data matrices for dbarts")

  #create time period 1 initial values for all items and all rankers
  #[actually these values are not updated at al]
  #one possibility is all zeros, although then all T1 observations fall in one branc if split on Z
  # init_Z_t0 <- rep(0, n.item*n.ranker)
  #another possibility is normally distributed initial values
  # init_Z_t0 <- rnorm(n.item*n.ranker)

  # with initial value create vector of Z lags
  # Zlag.1.mat <- c(init_Z_t0, as.vector(Z.mat)[1:((n.time-1)*n.item*n.ranker)])

  #must do something similar if extend to two period lags, three period lags etc.


  # this has same format as Z.vec
  Zlag.mat <- matrix(NA, nrow = n.time_train*num_indiv, ncol = num_lags)
  for(t in 1:num_lags){
    init_Z_t0 <- rep(0, t*num_indiv)
    # init_Z_t0 <- rnorm(t*n.item*n.ranker)
    Zlag.mat[,t] <- c(init_Z_t0, Z_initial[1:((n.time_train-t)*num_indiv)])
  }

  #it is also necessary to create initial values for the test data (???????)

  Zlag.mat.test <- matrix(NA, nrow = num_test_periods*num_indiv, ncol = num_lags)

  # if(nrow(X.test) >0 ){
  for(t in 1:num_lags){
    if(num_test_periods > t ){
      #up to t time periods ahead can be filled in. Rest of test time periods filled in using repeated values of last observation in training data

      Zlag.mat.test[,t] <- c(Z_initial[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
                             rep(Z_initial[((n.time_train-1)*num_indiv+1):(n.time_train*num_indiv)],
                                 (num_test_periods-t) )  )

      # #other option is to set all unobservable values to zero
      # Zlag.mat.test[,t] <- c(as.vector(Z.mat)[((n.time-t)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],
      #                        rep(0,(num_test_periods-t)*n.item*n.ranker ))

    }else{
      #nothing to fill in if num_test_periods <= t
      Zlag.mat.test[,t] <- c(Z_initial[((n.time_train-t)*num_indiv+1):((n.time_train-(t - num_test_periods) )*num_indiv)]  )

    }
    # }
  }
  # }
  Xmat.train <- data.frame(y = as.vector(Z_reference), x = cbind(Zlag.mat, X.train) )
  # print(colnames(Xmat.train))

  if(nrow(X.test)>0 ){
    Xmat.test  <- data.frame(x = cbind( Zlag.mat.test, X.test ) )
    #must create predictions for future time periods using covariates for one period ahead, two periods ahead etc.
  }

  k_sig_mu <- reference_half_range*probit_k/3.0
  # df_for_dbart <- data.frame(y = as.vector(Z.vec), x = as.matrix(Zlag.mat) )

  control <- dbartsControl(updateState = updateState, verbose = FALSE,  keepTrainingFits = TRUE,
                           keepTrees = TRUE,
                           n.trees = n.trees,
                           n.burn = n.burn,
                           n.samples = 1L, # n.samples,
                           n.thin = 1L, # n.thin,
                           n.chains = 1L, # n.chains,
                           n.threads = 1L, # n.threads,
                           printEvery = printEvery,
                           printCutoffs = printCutoffs,
                           rngKind = rngKind,
                           rngNormalKind = rngNormalKind,
                           rngSeed = rngSeed)

  # print("begin dbarts")
  # if(nrow(X.test )==0){
  if(nrow(X.test )==0){
    sampler <- dbarts(y ~ .,
                      data = Xmat.train,
                      #test = Xmat.test,
                      control = control,
                      resid.prior = fixed(1), node.prior = normal(k_sig_mu),
                      sigma=1 #check if this is the correct approach for setting the variance to 1
    )
  }else{
    sampler <- dbarts(y ~ .,
                      data = Xmat.train,
                      test = Xmat.test,
                      control = control,
                      resid.prior = fixed(1), node.prior = normal(k_sig_mu),
                      sigma=1 #check if this is the correct approach for setting the variance to 1
    )
  }


  computational_offset <- Z_reference - Z_initial
  dbarts_offset <- computational_offset + offsetz_vec # genuine_offset_vec
  sampler$setOffset(offset = dbarts_offset, updateScale = FALSE)
  # sampler$setTestOffset (
  #   offset = offsetz_vec#,updateScale = FALSE
  # )

  # print("Line 321")
  #set the response.
  #Check that 0 is a reasonable initial value
  #perhaps makes more sense to use initial values of Z
  # sampler$setResponse(y = as.vector(Z.vec))
  # # sampler$setSigma(sigma = 1)
  #
  # min_resp <- min(Z.vec)
  # max_resp <- max(Z.vec)

  # scale offsets
  # scale zero

  # scalezero <- (0 - min_resp)/(max_resp - min_resp) - 0.5
  # scaled_offsetz_vec <- (offsetz_vec - min_resp)/(max_resp - min_resp) - 0.5

  #sampler$setPredictor(x= Xmat.train, column = 1, forceUpdate = TRUE)

  #mu = as.vector( alpha + X.mat %*% beta )
  # sampler$sampleTreesFromPrior()
  samplestemp <- sampler$run()

  # mutemp <- samplestemp$train[,1]
  #suppose there are a number of samples

  # print("sigma = ")
  # print(samplestemp$sigma)

  # mupreds <- sampler$predict(Xmat.train)
  f_train <- as.numeric(samplestemp$train[, 1L]) - dbarts_offset
  mutemp <- offsetz_vec + f_train # genuine_offset_vec + f_train
  mu <- mutemp

  if(n.burnin == 0){
    if(keep_zmat==TRUE){
      draw$Z.mat[,,1] <- Z.mat
      draw$Z.mat.test[,,1] <- Z.mat.test
    }
    # draw$alpha[,1] = alpha
    # draw$beta[,1] = beta
    draw$mu[, , 1] <- matrix(mu ,
                             nrow = num_indiv, ncol = n.time_train)
    draw$prob.train.draws[, , 1:num_categories, 1] <-
      pnorm( outer( X = - draw$mu[, , 1]  ,  Y =  c(thresholds, Inf ), FUN = "+")  ) -
      pnorm( outer( X = - draw$mu[, , 1]   ,  Y =  c(-Inf,thresholds ), FUN = "+")  )
    # draw$sigma2.alpha[1] = sigma2.alpha
    # draw$sigma2.beta[1] = sigma2.beta
    draw$thresholds[,1] <- thresholds
  }

  # print("Line 954")


  # df_for_dbart_test <- data.frame(#y = rep(NA, nrow(Zlag.mat.test)),
  #   x = Zlag.mat.test )

  # print("df_for_dbart_test = ")
  # print(df_for_dbart_test)
  #
  # print("Line 363")
  # should really instead marginalize out z_T for predictive distribution sample of Z_{T+1} as outlined in paper.
  temp_test_preds <- matrix(NA, nrow = num_indiv,ncol = num_test_periods)
  # temp_mu_test <- rep(NA,  nrow(df_for_dbart_test) )
  temp_mu_test <- rep(NA,  nrow(Xmat.test) )

  # temp_test_mat <- data.frame(#y = rep(NA,num_indiv),
  #   x = as.matrix(df_for_dbart_test[1:(num_indiv) , , drop=FALSE ]) ) #  matrix(NA, nrow = n.item*n.ranker,ncol = ncol(Xmat.test))
  #
  # colnames(temp_test_mat) <- colnames(df_for_dbart_test)

  temp_test_mat <- data.frame( x = as.matrix(Xmat.test[1:(num_indiv) , , drop=FALSE ]) ) #  matrix(NA, nrow = n.item*n.ranker,ncol = ncol(Xmat.test))
  colnames(temp_test_mat) <- colnames(Xmat.test)

  for(t1 in 1:num_test_periods){
    #produce a prediction
    # need to edit z_{t+1} predictions
    # testpredvec <- sampler$predict(temp_test_mat)
    # edit code below if Rstudio crashes
    # must obtain intersection matrix for t1 individual indiv
    testpredvec <- rep(NA, num_indiv)
    for(indiv in 1:num_indiv){
      indiv_ind <- (t1 - 1)*num_indiv+indiv
      list_inter_mats <- list()
      for(i in 1:n.trees){
        treeexample1 <- sampler$getTrees(treeNums = i,chainNums = 1,sampleNums = 1)
        rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))

        #must use covariates for individual indiv at time period t
        # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, Xmat.train[obs_indices[1],-1, drop = FALSE] )
        # if num_lags > 1 this would have to be edited (just first column dropped)
        # list_inter_mats[[i]] <- getPredictionsRangesForTree3_cpp(as.matrix(rebuilt_tree),
        #                                                          as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
        list_inter_mats[[i]] <-getPredictionsRangesForTreeMCP_cpp(as.matrix(rebuilt_tree),as.matrix(temp_test_mat[indiv, , drop = FALSE]))
      }
      intersectmat <- interNtreesB_cpp(list_inter_mats)
      # Biglist_intersectmats[[t]] <- intersectmat
      # obtain prediction from intersection matrix
      ktemp <- which((as.matrix(temp_test_mat[indiv,1])[1]  <= intersectmat[, 3]) )[1]
      # Then obtain the corresponding region mean value
      testpredvec[indiv] <- intersectmat[ktemp,1]
    }
    testpredvec <- (testpredvec + 0.5)*(max_resp - min_resp) + min_resp

    # print("Line 1065")
    # if(num_indiv == 1){
    #
    #   # print("min(Z.vec) = ")
    #   # print(min(Z.vec))
    #   # print("max(Z.vec) = ")
    #   # print(max(Z.vec))
    #   # print("Z.vec[length(Z.vec)] = ")
    #   # print(Z.vec[length(Z.vec)])
    #
    #   tempbind <- as.matrix(rbind(temp_test_mat,temp_test_mat))
    #   # sampler$predict(x.test = as.matrix(Z.vec[100]), offset.test = NULL)[1:num_indiv]
    #   #
    #   # testpredvec <- sampler$predict(x.test = as.matrix(rep(tempbind[1,1],100)), offset.test = NULL)[1:num_indiv]
    #   #
    #   # print("line 1078 testpredvec from predict = ")
    #   # print(testpredvec)
    #
    #   list_inter_mats <- list()
    #
    #   for(i in 1:n.trees){
    #     # print("Line 492")
    #     treeexample1 <- sampler$getTrees(treeNums = i,
    #                                      chainNums = 1,
    #                                      sampleNums = 1)
    #     # print("Line 496")
    #     # print("treeexample1 = ")
    #     # print(treeexample1)
    #     # # rebuilt_tree <- rebuildTree2(treeexample1)
    #     rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
    #     # print("Line 499")
    #     #
    #     #must use covariates for individual indiv at time period t
    #
    #     # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, as.matrix(df_for_dbart$x[obs_indices[1]]) )
    #
    #     # list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree$var == -1 , 5:7]
    #     list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree[,4] == -1 , 5:7, drop = FALSE]
    #     # print("Line 507")
    #
    #   }
    #
    #   intersectmat <- interNtreesB(list_inter_mats)
    #
    #   kpredtemp <- which((as.matrix(tempbind[1,])[1] < intersectmat[, 3]) )[1]
    #   # Then obtain the corresponding region mean value
    #
    #   testpredvec <- as.matrix(intersectmat[kpredtemp,1])
    #
    #   testpredvec <- (testpredvec + 0.5)*(max_resp - min_resp) + min_resp
    #
    #   # print("line 1112 testpredvec from mat rescaled  = ")
    #   # print(testpredvec)
    # }else{
    #
    #
    #   testpredvec <- sampler$predict(x.test = as.matrix(temp_test_mat), offset.test = NULL)
    #
    # }
    test_indices <- (t1 -1L)*num_indiv + seq_len(num_indiv)
    offset_current_test <-offsetz_vec_test[test_indices ]
    if(noise_in_pred==1){
      temp_test_preds[ , t1] <- testpredvec + offset_current_test + rnorm( num_indiv )
    }else{
      temp_test_preds[ , t1] <- testpredvec + offset_current_test
    }
    # print("Line 963")
    #update temp_test_mat
    #shift z columns to the right and fill in leftmost column

    #need to rewrite this if want to allow for no observed covariates

    if(num_lags ==1){
      # temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] ,
      #                                           as.matrix(df_for_dbart_test[(t1-1)*(num_indiv)  +  1:(num_indiv), c(-1), drop=FALSE ] )))
      if(t1 < num_test_periods){
        temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] ,
                                                  as.matrix(Xmat.test[(t1)*(num_indiv)  +  1:(num_indiv) ,(num_lags+1):(ncol(Xmat.test)), drop=FALSE ] )))
      }
    }else{
      print("num lags not equal to 1")
      # temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] , temp_test_mat[,1:(num_lags-1)] ,
      #                                           as.matrix(df_for_dbart_test[(t1-1)*(num_indiv)  +  1:(num_indiv),c(-1), drop=FALSE ] )))
      if(t1 < num_test_periods){
        temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] , temp_test_mat[,1:(num_lags-1)] ,
                                                  as.matrix(Xmat.test[(t1)*(num_indiv)  +  1:(num_indiv) ,(num_lags+1):(ncol(Xmat.test)), drop=FALSE ] )))
      }
    }

    # if(t1 == num_test_periods){
    #   temp_test_mat <- data.frame(#y = rep(NA,num_indiv),
    #                               x = as.matrix(df_for_dbart_test[1:(num_indiv) , , drop=FALSE ]) ) #  matrix(NA, nrow = n.item*n.ranker,ncol = ncol(Xmat.test))
    #   colnames(temp_test_mat) <- colnames(df_for_dbart_test)
    # }
    # colnames(temp_test_mat) <- colnames(df_for_dbart_test)
    colnames(temp_test_mat) <- colnames(Xmat.test)

    # print("colnames(temp_test_mat) = ")
    # print(colnames(temp_test_mat))


    #fill in temp_mu_test without noise
    temp_mu_test[(t1-1)*(num_indiv)  +  1:(num_indiv) ] <- testpredvec + offset_current_test
  }

  # print("Line 427")
  # can re-draw Z.mat and Z.mat.test here
  Zlag.mat.test <- matrix(NA, nrow = num_test_periods*num_indiv, ncol = num_lags)

  # if(nrow(X.test) >0 ){
  for(t in 1:num_lags){
    if(num_test_periods > t ){
      #up to t time periods ahead can be filled in. Rest of test time periods filled in using repeated values of last observation in training data
      # Zlag.mat.test[,t] <- c(as.vector(Z.mat)[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
      #                        as.vector(as.matrix(temp_test_mat))[1:((num_test_periods - t)*(num_indiv))  ] )
      # Zlag.mat.test[,t] <- c(as.vector(t(Z.mat))[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
      #                        as.vector(as.matrix(temp_test_mat))[1:((num_test_periods - t)*(num_indiv))  ] )
      # USE INITIAL PREDICTION INSTEAD OF FILLING IN WITH REPEATED VALUES OF LAST TRAINING OBSERVATION
      Zlag.mat.test[,t] <- c(as.vector(t(Z.mat))[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
                             as.vector(as.matrix(temp_test_preds))[1:((num_test_periods - t)*(num_indiv))  ] )
    }else{
      #nothing to fill in if num_test_periods <= t
      Zlag.mat.test[,t] <- c(as.vector(t(Z.mat))[((n.time_train-t)*num_indiv+1):((n.time_train-(t - num_test_periods) )*num_indiv)]  )

    }
    # }
  }
  # }
  # and redefine z mat test lags?
  if(n.burnin == 0){
    draw$mu_test[,,1] <- matrix(temp_mu_test , nrow = num_indiv, ncol = num_test_periods) #temp_mu_test
    draw$prob.test.draws[, , 1:num_categories, 1] <-
      pnorm( outer( X = - draw$mu_test[, , 1]  ,  Y =  c(thresholds, Inf ), FUN = "+")  ) -
      pnorm( outer( X = - draw$mu_test[, , 1]  ,  Y =  c(-Inf,thresholds ), FUN = "+")  )
    # draw$mu_test[,1] <- samplestemp$test[,1]
    if(keep_zmat==TRUE){
      draw$Z.mat.test[,,1]  <- t(matrix(data = as.vector(temp_test_preds), nrow = num_indiv, ncol = num_test_periods))
    }
  }
  # print("Line 828")
  max_lag_update_attempts <- 1000L
  ##################### Begin Gibbs sampler ##################################################
  #//////////////////////////////////////////////////////////////////////////
  ## Gibbs iteration
  iter <- 2
  breakcount <- 0

  # for(iter in 2:(n.burnin + n.iter)){
  while( iter <= (n.burnin + n.iter) ){
    # print("line 842")
    # print("iter = ")
    # print(iter)
    if(iter > n.burnin){
      iter_min_burnin <- iter - n.burnin
    }
    ########## sample threshold parameters #######################

    for(cat_ind in 2:(num_categories-1)){
      # if(cat_ind < num_categories-1){
        # thresholds[cat_ind] <- runif(1,
        #                              min = max(max(Z.vec[y_from1.vec == cat_ind]), thresholds[cat_ind-1]),
        #                              max = min(min(Z.vec[y_from1.vec == cat_ind + 1]), thresholds[cat_ind+1]))
        lower_threshold_bound <-max(max(Z.vec[y_from1.vec == cat_ind]), thresholds[cat_ind -1L])
        upper_threshold_bound <-min(min(Z.vec[y_from1.vec == cat_ind +1L]), if (cat_ind < num_categories -1L) {thresholds[cat_ind +1L]} else {Inf})
        if (!is.finite(lower_threshold_bound) || lower_threshold_bound >= upper_threshold_bound) {stop(sprintf(paste0("Empty threshold full conditional at ","iteration %d, threshold %d: ","lower=%g, upper=%g."),iter,cat_ind,lower_threshold_bound,upper_threshold_bound),call. = FALSE)}
        thresholds[cat_ind] <- runif(1L,min =lower_threshold_bound, max = upper_threshold_bound)

      # }else{
      #   thresholds[cat_ind] <- runif(1,
      #                                min = max(max(Z.vec[y_from1.vec == cat_ind]), thresholds[cat_ind-1]),
      #                                max = min(min(Z.vec[y_from1.vec == cat_ind + 1]), Inf))
      # }
    }
    if (thresholds[1L] != 0 ||anyNA(thresholds) ||any(!is.finite(thresholds)) ||any(diff(thresholds) <= 0)) {stop("Thresholds are non-finite, unidentified, or not strictly increasing.")}
    #################################################################
    # print("Line 454")
    Z.matold <- Z.mat
    # Z_mat_old <- Z.mat
    # Z_vec_old <- Z.vec
    # Zlag_old <- Zlag.mat
    # Xmat_train_old <- Xmat.train
    ########### Sample from full conditional distribution.
    #################### calculate intersection matrices ########################################

    # if  there are no covariates, the intersection matrices do not vary across time or individuals
    #### obtain intersection matrices for time period t = 1 ###############
    # create intersection matrices
    intersectmat_tmin1 <- NA
    list_item_intersectmats_tmin1 <- NA
    rebuilt_tree_list <- list()
    for(i in 1:n.trees){
      treeexample1 <- sampler$getTrees(treeNums = i,chainNums = 1,sampleNums = 1)

      if (anyNA(treeexample1$value) ||any(!is.finite(treeexample1$value))) {
        stop(sprintf(paste0("Non-finite value in dbarts tree at ","iter=%d, tree=%d; ","rows=%s; var=%s; n=%s; value=%s"),iter,i,
                     paste(which(is.na(treeexample1$value) |!is.finite(treeexample1$value)),collapse = ","),
                     paste(treeexample1$var, collapse = ","),
                     paste(treeexample1$n, collapse = ","),
                     paste(treeexample1$value, collapse = ",")),
             call. = FALSE
        )
      }
      # rebuilt_tree <- rebuildTree2(treeexample1)
      rebuilt_tree_list[[i]] <- rebuildTree2_cpp(as.matrix(treeexample1))
    }
    list_inter_mats <- list()
    # for(i in 1:n.trees){
    #   treeexample1 <- sampler$getTrees(treeNums = i,chainNums = 1,sampleNums = 1)
    #   # rebuilt_tree <- rebuildTree2(treeexample1)
    #   rebuilt_tree <- ARBART:::rebuildTree2_cpp(as.matrix(treeexample1))
    #   #must use covariates for individual indiv at time period t
    #
    #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, as.matrix(df_for_dbart$x[obs_indices[1]]) )
    #   # list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree$var == -1 , 5:7]
    #   list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree[,4] == -1 , 5:7, drop = FALSE]
    # }
    # intersectmat <- interNtreesB_cpp(list_inter_mats)
    # intersectmat <- cbind(intersectmat, rep(NA, nrow(intersectmat)))
    # # calculate one dimensional integrals
    # for(rowind in 1:nrow(intersectmat)){
    #   # ktemp <- nkt_mat[rowind,k_index]
    #   # tempmean <- intersectmat[ktemp,1]
    #   templower <- intersectmat[rowind,2]
    #   tempupper <- intersectmat[rowind,3]
    #   # ASSUMING PRIOR MEAN ALL ZEROS
    #   # These are the q0 integrals
    #   intersectmat[rowind, 4] <- pnorm(tempupper, mean = 0, sd = 1) - pnorm(templower, mean = 0, sd = 1)
    #   # tempintegralval <- tempintegralval*onedim_int
    # }
    # intersectmat_tmin1 <- intersectmat
    num_regions <- nrow(intersectmat)
    # print("Line 937")
    for(z_iter_ind in 1:num_z_iters){
      for(indiv in 1:num_indiv){
        # print("indiv = ")
        # print(indiv)
        ########### calculate qkt  ########################################
        ########### calculate qkt   for t = 1 ########################################
        Biglist_list_item_intersectmats <- list()
        Biglist_intersectmats <- list()
        ######## Period 1 Intersection Matrix #########################
        indiv_ind <- (1 - 1)*num_indiv+indiv
        # list_inter_mats <- list()
        # for(i in 1:n.trees){
        #   treeexample1 <- sampler$getTrees(treeNums = i, chainNums = 1, sampleNums = 1)
        #   rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
        #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, Xmat.train[obs_indices[1],-1, drop = FALSE] )
        #   list_inter_mats[[i]] <- getPredictionsRangesForTreeMCP_cpp(as.matrix(rebuilt_tree),
        #                                                            as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
        # }
        list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))

        intersectmat_tmin1 <- interNtreesB_cpp(list_inter_mats)
        intersectmat_tmin1 <- cbind(intersectmat_tmin1, rep(NA, nrow(intersectmat_tmin1)))
        # calculate one dimensional integrals
        for(rowind in 1:nrow(intersectmat_tmin1)){
          # ktemp <- nkt_mat[rowind,k_index]
          # tempmean <- intersectmat_tmin1[ktemp,1]
          templower <- intersectmat_tmin1[rowind,2]
          tempupper <- intersectmat_tmin1[rowind,3]
          # ASSUMING PRIOR MEAN ALL ZEROS
          intersectmat_tmin1[rowind, 4] <- pnorm(tempupper, mean = 0, sd = 1) - pnorm(templower, mean = 0, sd = 1)
          # tempintegralval <- tempintegralval*onedim_int
        }

        Biglist_intersectmats[[1]] <- intersectmat_tmin1
        ################ period 2 intersection matrix #################################################
        # create vector of indices for ranker indiv in time period 2
        indiv_ind <- (2 - 1)*num_indiv+indiv
        # list_inter_mats <- list()
        # for(i in 1:n.trees){
        #   treeexample1 <- sampler$getTrees(treeNums = i, chainNums = 1, sampleNums = 1)
        #   # rebuilt_tree <- rebuildTree2(treeexample1)
        #   rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
        #   #must use covariates for individual indiv at time period t
        #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, Xmat.train[obs_indices[1],-1, drop = FALSE] )
        #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3_cpp(as.matrix(rebuilt_tree),
        #   #                                                          as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
        #   list_inter_mats[[i]] <- getPredictionsRangesForTreeMCP_cpp(as.matrix(rebuilt_tree),
        #                                                            as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
        # }
        list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
        intersectmat <- interNtreesB_cpp(list_inter_mats)
        intersectmat <- cbind(intersectmat, rep(NA, nrow(intersectmat)))
        # calculate one dimensional integrals
        for(rowind in 1:nrow(intersectmat)){
          # ktemp <- nkt_mat[rowind,k_index]
          # tempmean <- intersectmat[ktemp,1]
          templower <- intersectmat[rowind,2]
          tempupper <- intersectmat[rowind,3]
          # ASSUMING PRIOR MEAN ALL ZEROS
          intersectmat[rowind, 4] <- pnorm(tempupper, mean = 0, sd = 1) - pnorm(templower, mean = 0, sd = 1)
          # tempintegralval <- tempintegralval*onedim_int
        }
        Biglist_intersectmats[[2]] <- intersectmat

        ##### t = 3 to T intersect matrices ######################
        # print("Begin loop for intersection matrices = ")
        for(t in 3:n.time_train){
          #### obtain intersection matrices for time period t ###############
          # print("t = ")
          # print(t)
          indiv_ind <- (t - 1)*num_indiv+indiv
          # list_inter_mats <- list()
          # for(i in 1:n.trees){
          #   treeexample1 <- sampler$getTrees(treeNums = i,
          #                                    chainNums = 1,
          #                                    sampleNums = 1)
          #   # rebuilt_tree <- rebuildTree2(treeexample1)
          #   rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
          #   #must use covariates for individual indiv at time period t
          #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, Xmat.train[obs_indices[1],-1, drop = FALSE] )
          #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3_cpp(as.matrix(rebuilt_tree),
          #   #                                                          as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
          #   list_inter_mats[[i]] <- getPredictionsRangesForTreeMCP_cpp(as.matrix(rebuilt_tree),
          #                                                            as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
          # }
          list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
          intersectmat <- interNtreesB_cpp(list_inter_mats)
          Biglist_intersectmats[[t]] <- intersectmat
        }

        ################## qkt for t=1 ? ##############
        intersectmat <- Biglist_intersectmats[[2]]
        intersectmat_tmin1 <- Biglist_intersectmats[[1]]
        num_regions <- nrow(intersectmat)
        num_regions_tmin1 <- nrow(intersectmat_tmin1)
        #QUESTION: LOOP OVER ITEMS THEN TIME OT TIME THEN ITEMS?
        # loop over items
        # Now loop over time periods
        # special case for t=1
        # probability matrix for sampling elements

        # let rows be period zero, and columns be period 1 (2?)
        # probmattemp <- matrix(0, nrow = num_regions_tmin1, ncol = num_regions)
        logprobmattemp <-  matrix(-Inf,nrow = num_regions_tmin1,ncol = num_regions)

        # loop over period 2 regions into which z_1 can fall
        tempbounds <- matrix(NA, nrow = num_regions, ncol = 2)
        # Trunc norm prob of next periods latent value conditional on region
        # Create intervals from interval t+1 latent values
        y_tp1 <- y.train_from1[2,indiv]
        # z in period 2 for individual indiv
        temp_ztp1 <- Z.vec[1*num_indiv + indiv]
        y_t <- y.train_from1[1,indiv]
        # if(y_t == 1){
        #   temp_lower3 <- 0 # scalezero # 0
        #   temp_upper3 <- Inf
        # }else{
        #   temp_lower3 <- -Inf
        #   temp_upper3 <- 0 # scalezero # 0
        # }
        temp_lower3 <- c(-Inf,thresholds)[y_t] # scalezero # 0
        temp_upper3 <- c(thresholds,Inf)[y_t]

        # for(k_ind in 1:num_regions){
        #   # obtain mean for truncated normal distribution
        #   temp_mean <- intersectmat[k_ind, 1]
        #   # want trunc norm probability of latent variable value for item_ind
        #   # in period t+1
        #   # temp_tnorm_prob <- dtruncnorm(temp_ztp1,
        #   #                               a=temp_lower,
        #   #                               b=Inf,
        #   #                               mean = temp_mean,
        #   #                               sd = 1)
        #   tempmeanfordens <- (temp_mean + 0.5)*(max_resp - min_resp) + min_resp
        #   temp_tnorm_prob <- fastnormdens(temp_ztp1,
        #                                   mean = tempmeanfordens,
        #                                   sd = 1)
        #
        #   temp_mean <- intersectmat[k_ind, 1]
        #   # now second term
        #   temp_lower2 <- intersectmat[k_ind, 2]
        #   temp_upper2 <- intersectmat[k_ind, 3]
        #
        #   if((temp_lower2 >= temp_upper3) | (temp_lower3 >= temp_upper2)){
        #     # intervals do not overlap, therefore assign probability zero
        #     # and skip to next iteration
        #     # these three lines are technically unnecessary
        #     probmattemp[, k_ind] <- rep(0,num_regions_tmin1)
        #     tempbounds[k_ind, 1] <- NA
        #     tempbounds[k_ind, 2] <- NA
        #     next
        #   }
        #
        #   temp_lower2 <- max(temp_lower2, temp_lower3)
        #   temp_upper2 <- min(temp_upper2, temp_upper3)
        #
        #   if(temp_lower2 >= temp_upper2){
        #     stop("Line 1581 temp_lower2 >= temp_upper2")
        #   }
        #   for(k0_ind in 1:num_regions_tmin1){
        #     #loop over all possible means
        #     temp_mean2 <- intersectmat_tmin1[k0_ind,1]
        #     # probability of being in intersection region
        #     temp_mean2_origscale <- (temp_mean2 + 0.5)*(max_resp - min_resp) + min_resp + offsetz_vec[indiv]
        #     # prob_t_region <- pnorm(temp_upper2 - temp_mean2) - pnorm(temp_lower2 - temp_mean2)
        #     prob_t_region <- pnorm(temp_upper2 - temp_mean2_origscale) - pnorm(temp_lower2 - temp_mean2_origscale)
        #     # probmattemp[k0_ind, k_ind] <- prob_t_region*
        #     #   temp_tnorm_prob *
        #     #   intersectmat_tmin1[k0_ind,4]
        #     probmattemp[k0_ind, k_ind] <- temp_tnorm_prob * prob_t_region * intersectmat_tmin1[k0_ind,4]
        #     if(probmattemp[k0_ind, k_ind] < 0){
        #       print("probmattemp[k0_ind, k_ind] = ")
        #       print(probmattemp[k0_ind, k_ind])
        #     }
        #
        #   } # end loop over k0
        #
        #   # save upper and lower bounds (mean saved in intersectmat)
        #   # or just obtain again later
        #   tempbounds[k_ind,1] <- temp_lower2
        #   tempbounds[k_ind,2] <- temp_upper2
        # } # end loop over k1

        response_range <- max_resp - min_resp
        if (!is.finite(min_resp) ||!is.finite(max_resp) ||!is.finite(response_range) ||response_range <= 0) {
          stop(sprintf(paste0("Invalid latent response scale at ","iter=%d, z_iter=%d, indiv=%d: ","min_resp=%g, max_resp=%g, range=%g"),
                       iter,z_iter_ind,indiv,min_resp,max_resp,response_range),call. = FALSE)}
        if (anyNA(intersectmat[, 1]) ||any(!is.finite(intersectmat[, 1]))) {
          bad_rows <- which(is.na(intersectmat[, 1]) |!is.finite(intersectmat[, 1]))
          stop(sprintf(paste0("Invalid regional mean before rescaling at ","iter=%d, z_iter=%d, indiv=%d; ","bad rows=%s; means=%s; lower=%s; upper=%s"),
                       iter,z_iter_ind,indiv,paste(bad_rows, collapse = ","),paste(intersectmat[, 1], collapse = ","),paste(intersectmat[, 2], collapse = ","),paste(intersectmat[, 3], collapse = ",")),call. = FALSE)
        }

        # tempmeanfordens <- (temp_mean + 0.5)*(max_resp - min_resp) + min_resp
        # tempmeanfordens <- (intersectmat[1:num_regions, 1] + 0.5)*(max_resp - min_resp) + min_resp
        temp_f_for_dens <- (intersectmat[1:num_regions, 1] + 0.5)*(max_resp - min_resp) + min_resp
        tempmeanfordens <- temp_f_for_dens + offsetz_vec[indiv]

        # temp_tnorm_logprobvec <- fastlognormdens(temp_ztp1,
        #                                          mean = (intersectmat[, 1] + 0.5)*(max_resp - min_resp) + min_resp,
        #                                          sd = 1)

        bad_regions <- which((intersectmat[1:num_regions, 2] >= temp_upper3) | (temp_lower3 >= intersectmat[1:num_regions, 3]))

        logprobmattemp[, bad_regions] <- -Inf #rep(-Inf,num_regions)
        tempbounds[bad_regions, 1] <- NA
        tempbounds[bad_regions, 2] <- NA

        good_regions <- setdiff(1:num_regions, bad_regions)
        temp_tnorm_logprobvec <- rep(NA, num_regions)
        temp_tnorm_logprobvec[good_regions] <- fastlognormdens(temp_ztp1,
                                                               mean = tempmeanfordens[good_regions],
                                                               sd = 1)
        q0 <- intersectmat_tmin1[, 4]
        dens_log <- temp_tnorm_logprobvec[good_regions]

        if (anyNA(q0) || any(q0 < 0) || any(!is.finite(q0) & q0 != 0)) {
          stop(sprintf(paste0("Invalid q0 interval mass at iter=%d: ","q0=%s; lower=%s; upper=%s"),
                       iter,paste(q0, collapse = ", "),paste(intersectmat_tmin1[, 2], collapse = ", "),paste(intersectmat_tmin1[, 3], collapse = ", ")))
        }

        if (anyNA(dens_log) || any(dens_log == Inf)) {
          stop(sprintf(paste0("Invalid transition log-density at iter=%d: ","z_tp1=%g; means=%s; logdens=%s"),
                       iter,temp_ztp1,paste(tempmeanfordens[good_regions], collapse = ", "),paste(dens_log, collapse = ", ")))
        }

        temp_mean2_origscale_vec <- (intersectmat_tmin1[1:num_regions_tmin1,1] + 0.5)*(max_resp - min_resp) +
          min_resp + offsetz_vec[indiv]
        tempbounds[good_regions,1] <- pmax(intersectmat[good_regions, 2], temp_lower3)
        tempbounds[good_regions,2] <- pmin(intersectmat[good_regions, 3], temp_upper3)
        region_logprobs_tnorm_denoms <- matrix(NA, nrow = num_regions_tmin1, ncol = num_regions)
        temp_norm_args_u <- matrix(NA, nrow = num_regions_tmin1, ncol = num_regions)
        temp_norm_args_u[, good_regions] <- matrix(tempbounds[good_regions,2],
                                                   nrow = num_regions_tmin1, ncol = length(good_regions),byrow =  TRUE ) -
          matrix(temp_mean2_origscale_vec, nrow = num_regions_tmin1, ncol = length(good_regions))
        temp_norm_args_l <- matrix(NA, nrow = num_regions_tmin1, ncol = num_regions)
        temp_norm_args_l[, good_regions] <- matrix(tempbounds[good_regions,1],
                                                   nrow = num_regions_tmin1, ncol = length(good_regions),byrow =  TRUE ) -
          matrix(temp_mean2_origscale_vec, nrow = num_regions_tmin1, ncol = length(good_regions))

        # region_logprobs_tnorm_denoms[, good_regions] <- log(pnorm(temp_norm_args_u[, good_regions]) - pnorm(temp_norm_args_l[, good_regions]))
        region_logprobs_tnorm_denoms[, good_regions] <- stable_log_diff_cdf_matrix(temp_norm_args_l[, good_regions, drop = FALSE],
                                                                                   temp_norm_args_u[, good_regions, drop = FALSE])

        logprobmattemp[1:num_regions_tmin1, good_regions] <- outer(log(intersectmat_tmin1[1:num_regions_tmin1,4]),
                                                                   temp_tnorm_logprobvec[good_regions],FUN = "+")+
          region_logprobs_tnorm_denoms[, good_regions, drop = FALSE]

        if(any(tempbounds[good_regions,1] >= tempbounds[good_regions,2])){
          stop(" line 1868 bounds badly defined")
        }

        logprobstemp <- as.vector(logprobmattemp)
        probstemp <- normalize_log_probs(logp = logprobstemp,iter = iter,indiv = indiv,
                                         z_iter = z_iter_ind,t = 1L,context = "initial-time joint-region draw")

        region_ind <- sample.int(num_regions_tmin1 * num_regions,
                                 size = 1L,replace = TRUE,prob = probstemp)

        # #sample a combination of k0 and k1
        # # if necessary can use column sums to sample k1, then k0
        # # however, this is probably unnecessary
        # region_ind <- sample.int(num_regions_tmin1*num_regions,
        #                          size = 1,
        #                          replace = TRUE,
        #                          prob = as.vector(probmattemp))



        # k0 region is sampled number modulo number of regions in period 1
        k0_region_ind <- ((region_ind -1) %% num_regions_tmin1 ) + 1
        # if(k0_region_ind ==0){
        #   k0_region_ind <- num_regions
        # }

        # k1 region is the ceiling of sampled number divided by number of regions in period 1
        # k1_region_ind <- ceiling(region_ind/num_regions)
        k1_region_ind <- (region_ind -1) %/% num_regions_tmin1 + 1

        temp_lower2 <- tempbounds[k1_region_ind,1]
        temp_upper2 <- tempbounds[k1_region_ind,2]
        temp_mean0 <- intersectmat_tmin1[k0_region_ind, 1]
        temp_mean0 <- (temp_mean0 + 0.5)*(max_resp - min_resp) + min_resp + offsetz_vec[indiv]

        zdraw_temp <- rtruncnorm(n = 1,
                                 a = temp_lower2,
                                 b = temp_upper2,
                                 mean = temp_mean0 ,
                                 sd = 1)

        #### BEGIN edits for repeated sampling until lag update does not result in terminal node

        z_old_temp <- Z.mat[1, indiv ]
        Z.mat[1, indiv ] <- zdraw_temp
        Z.vec[(1-1)*num_indiv + indiv] <- zdraw_temp

        check_lag_valid <- TRUE

        for(j in 1:num_lags){
          init_Z_t0 <- rep(0, j*num_indiv)
          Zlag.mat[,j] <- c(init_Z_t0, Z.vec[1:((n.time_train-j)*num_indiv)])
          if((sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) ){
            check_lag_valid <- FALSE
            Z.mat[1, indiv ] <- z_old_temp
            Z.vec[(1-1)*num_indiv + indiv] <- z_old_temp
            for(j2 in 1:j){
              init_Z_t0 <- rep(0, j2*num_indiv)
              Zlag.mat[,j2] <- c(init_Z_t0, Z.vec[1:((n.time_train-j2)*num_indiv)])
              doublecheck <- (sampler$setPredictor(x = Zlag.mat[,j2], column = j2) == FALSE)
              if(doublecheck){
                stop("cannot roll back")
              }
            }
            break
          }
        }

        lag_update_attempt <-  0L

        while (check_lag_valid == FALSE) {
          lag_update_attempt <- lag_update_attempt + 1L

          if ( lag_update_attempt > max_lag_update_attempts) {
            stop(sprintf(paste0("Unable to obtain a valid latent-lag update ","at iteration %d, time %d, individual %d."),
                iter,1,indiv),call. = FALSE)
          }

          region_ind <- sample.int(num_regions_tmin1*num_regions,
                                   size = 1,
                                   replace = TRUE,
                                   prob = as.vector(probstemp))

          # k0 region is sampled number modulo number of regions in period 1
          k0_region_ind <- ((region_ind -1) %% num_regions_tmin1 ) + 1
          # k1 region is the ceiling of sampled number divided by number of regions in period 1
          # k1_region_ind <- ceiling(region_ind/num_regions)
          k1_region_ind <- (region_ind -1) %/% num_regions_tmin1 + 1
          temp_lower2 <- tempbounds[k1_region_ind,1]
          temp_upper2 <- tempbounds[k1_region_ind,2]
          temp_mean0 <- intersectmat_tmin1[k0_region_ind, 1]
          temp_mean0 <- (temp_mean0 + 0.5)*(max_resp - min_resp) + min_resp + offsetz_vec[indiv]

          zdraw_temp <- rtruncnorm(n = 1,
                                   a = temp_lower2,
                                   b = temp_upper2,
                                   mean = temp_mean0 ,
                                   sd = 1)
          # z_old_temp <- Z.mat[1, indiv ]
          Z.mat[1, indiv ] <- zdraw_temp
          Z.vec[(1-1)*num_indiv + indiv] <- zdraw_temp

          check_lag_valid <- TRUE
          for(j in 1:num_lags){
            init_Z_t0 <- rep(0, j*num_indiv)
            Zlag.mat[,j] <- c(init_Z_t0, Z.vec[1:((n.time_train-j)*num_indiv)])
            if((sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) ){
              check_lag_valid <- FALSE
              Z.mat[1, indiv ] <- z_old_temp
              Z.vec[(1-1)*num_indiv + indiv] <- z_old_temp
              for(j2 in 1:j){
                init_Z_t0 <- rep(0, j2*num_indiv)
                Zlag.mat[,j2] <- c(init_Z_t0, Z.vec[1:((n.time_train-j2)*num_indiv)])
                doublecheck <- (sampler$setPredictor(x = Zlag.mat[,j2], column = j2) == FALSE)
                if(doublecheck){
                  stop("Failed to revert rejected z update.")
                }
              }
              break
            }
          }
        } # end while loop

        #### END edits for repeated sampling until lag update does not result in terminal node
        # Z.mat[item_ind,  indiv ] <- zdraw_temp
        Z.mat[1,  indiv ] <- zdraw_temp
        Z.vec[indiv] <- zdraw_temp
        #### qkt Begin Loop over time t > 1 ###################
        # loop over time periods for general case 1 < t < T
        for(t in 2:(n.time_train - 1)){
          intersectmat <- Biglist_intersectmats[[t+1]]
          intersectmat_tmin1 <- Biglist_intersectmats[[t]]
          num_regions <- nrow(intersectmat)
          temp_ztpmin1 <- as.vector(t(Z.mat))[(t-2)*num_indiv + indiv]
          # must find mean corresponding to z in period t-1 # This will be used in and after the loop over regions.
          # can directly obtain from dbarts # or find region # and use already saved region mean values

          # must find last lower bound that temp_ztpmin1 is greater than
          # or first upper bound that temp_ztpmin1 is below
          ktemp_tmin1 <- which((temp_ztpmin1 <= intersectmat_tmin1[, 3]) )[1]
          # Then obtain the corresponding region mean value
          temp_mean2 <- intersectmat_tmin1[ktemp_tmin1,1]

          # Calculate the probabilities for each region in this time period
          # the regions being looped over are actually period t+1 regions
          # Same regions for all time periods if there are no time varying covariates
          # However, the weights are individual and time period specific
          # loop through regions

          # first column is the probabilities
          # second column is the lower bounds
          # third column is the upper bounds
          # temp_region_probs <- matrix(0,
          #                             nrow = nrow(intersectmat),
          #                             ncol = 3)
          temp_region_logprobs <- matrix(-Inf,
                                         nrow = nrow(intersectmat),
                                         ncol = 3)
          # Trunc norm prob of next periods latent value conditional on region
          # Create intervals from interval t+1 latent values
          # maybe pointless?
          # temp_lower nad temp_upper not used until updated below
          y_tp1 <- y.train_from1[t,indiv]
          # if(y_tp1 == 1){
          #   temp_lower <- 0 # scalezero # 0
          #   temp_upper <- Inf
          # }else{
          #   temp_lower <- -Inf
          #   temp_upper <- 0 # scalezero # 0
          # }
          # temp_lower <- c(-Inf,thresholds)[y_tp1] # scalezero # 0
          # temp_upper <- c(thresholds,Inf)[y_tp1]
          temp_ztp1 <- Z.vec[t*num_indiv + indiv]
          y_t <- y.train_from1[t,indiv]

          # if(y_t == 1){
          #   temp_lower3 <- 0 # scalezero # 0
          #   temp_upper3 <- Inf
          # }else{
          #   temp_lower3 <- -Inf
          #   temp_upper3 <- 0 # scalezero # 0
          # }

          temp_lower3 <- c(-Inf,thresholds)[y_t] # scalezero # 0
          temp_upper3 <- c(thresholds,Inf)[y_t]
          temp_tnorm_logprobvec <- fastlognormdens(temp_ztp1,
                                                   mean = (intersectmat[, 1] + 0.5)*(max_resp - min_resp) + min_resp +
                                                     offsetz_vec[indiv],
                                                   sd = 1)

          for(k_ind in 1:num_regions){

            # # obtain mean for truncated normal distribution
            # temp_mean <- intersectmat[k_ind, 1]
            #
            # # temp_tnorm_prob <- dtruncnorm(temp_ztp1,
            # #                               a=temp_lower,
            # #                               b=Inf,
            # #                               mean = temp_mean,
            # #                               sd = 1)
            #
            # tempmeanfordens <- (temp_mean + 0.5)*(max_resp - min_resp) + min_resp
            #
            # temp_tnorm_prob <- fastnormdens(temp_ztp1,
            #                                 mean = tempmeanfordens,
            #                                 sd = 1)

            # Probability of z_t in intersection of
            # region k_ind (for period t+1)
            # and region defined by period t latent variables for other individuals
            # and rank for period t
            temp_tnorm_logprob  <- temp_tnorm_logprobvec[k_ind]


            # tildeC_ktminl corresponds to
            # period t+1 k_ind region intereval

            temp_lower2 <- intersectmat[k_ind, 2]
            temp_upper2 <- intersectmat[k_ind, 3]

            if((temp_lower2 > temp_upper3) | (temp_lower3 > temp_upper2)){
              # intervals do not overlap, therefore assign probability zero
              # and skip to next iteration
              # temp_region_probs[k_ind, 1] <- 0
              # temp_region_probs[k_ind, 2] <- NA
              # temp_region_probs[k_ind, 3] <- NA
              temp_region_logprobs[k_ind, 1] <- -Inf
              temp_region_logprobs[k_ind, 2] <- NA
              temp_region_logprobs[k_ind, 3] <- NA
              next
            }

            temp_lower2 <- max(temp_lower2, temp_lower3)
            temp_upper2 <- min(temp_upper2, temp_upper3)

            if(temp_lower2 >= temp_upper2){
              print("temp_lower2 = ")
              print(temp_lower2)
              print("temp_upper2 = ")
              print(temp_upper2)
              stop("Line 1917. temp_lower2 >= temp_upper2")
            }

            temp_mean2_origscale <- (temp_mean2 + 0.5)*(max_resp - min_resp) + min_resp + offsetz_vec[indiv]
            # prob_t_region <- pnorm(temp_upper2 - temp_mean2_origscale) - pnorm(temp_lower2 - temp_mean2_origscale)
            logprob_t_region <- stable_log_diff_cdf_all_tails(temp_lower2 - temp_mean2_origscale, temp_upper2 - temp_mean2_origscale)

            # prob_t_region <- prob_t_region*temp_tnorm_prob
            logprob_t_region <- logprob_t_region + temp_tnorm_logprob
            # prob_t_region <- temp_tnorm_prob

            # # probability of being in intersection region
            # temp_mean2_origscale <- (temp_mean2 + 0.5)*(max_resp - min_resp) + min_resp + offsetz_vec[indiv]
            # # temp_upper2_origscale <- (temp_upper2 + 0.5)*(max_resp - min_resp) + min_resp
            # # temp_lower2_origscale <- (temp_lower2 + 0.5)*(max_resp - min_resp) + min_resp
            #
            # # prob_t_region <- pnorm(temp_upper2 - temp_mean2) - pnorm(temp_lower2 - temp_mean2)
            # prob_t_region <- pnorm(temp_upper2 - temp_mean2_origscale) - pnorm(temp_lower2 - temp_mean2_origscale)
            # # prob_t_region <- pnorm(temp_upper2_origscale - temp_mean2_origscale) - pnorm(temp_lower2_origscale - temp_mean2_origscale)
            # prob_t_region <- prob_t_region*temp_tnorm_prob
            # prob_t_region <- temp_tnorm_prob

            # save region probability

            # and save region bounds (or maybe more memory efficient to obtain the region again)

            # must multiply by other previously obtained probabilities

            # print("prob_t_region = ")
            # print(prob_t_region)

            # temp_region_probs[k_ind, 1] <- prob_t_region
            # temp_region_probs[k_ind, 2] <- temp_lower2
            # temp_region_probs[k_ind, 3] <- temp_upper2
            temp_region_logprobs[k_ind, 1] <- logprob_t_region
            temp_region_logprobs[k_ind, 2] <- temp_lower2
            temp_region_logprobs[k_ind, 3] <- temp_upper2
          }

          if (num_lags !=1L) {stop("Optimized latent update currently requires num_lags == 1L.",call. = FALSE)}
          current_index <-as.integer((t -1L) *num_indiv +indiv)
          future_index <-as.integer(t *num_indiv +indiv)
          old_z_value <-Z.vec[current_index]
          old_lag_vector <-Zlag.mat[,1L]
          region_logweights <- as.numeric(temp_region_logprobs[seq_len(num_regions),1L])
          ## Exclude malformed or empty intervals immediately.
          malformed_region <-!is.finite(region_logweights) |
            is.na(temp_region_logprobs[seq_len(num_regions),2L]) |is.na(temp_region_logprobs[seq_len(num_regions),3L]) |
            temp_region_logprobs[seq_len(num_regions),2L] >=temp_region_logprobs[seq_len(num_regions),3L]
          region_logweights[malformed_region] <- -Inf
          old_region_ind <-which(old_z_value >temp_region_logprobs[seq_len(num_regions),2L] &old_z_value <=temp_region_logprobs[seq_len(num_regions),3L])
          if (length(old_region_ind) !=1L) {
            stop(sprintf(paste0("Old latent value %.17g is not uniquely assigned ","to a region at iteration %d, time %d, individual %d; ","matching regions=%s."),
                         old_z_value,iter,t,indiv,paste(old_region_ind,collapse = ",")),call. = FALSE)
          }


          if (!is.finite(region_logweights[old_region_ind])) {
            stop(sprintf(paste0("The current latent region has zero statistical weight ","at iteration %d, time %d, individual %d, region %d."),
                         iter,t,indiv,old_region_ind),call. = FALSE)
          }
          ## Cache region validity only for this latent update:
          ##  0 = unknown
          ## -1 = tested and invalid
          ##  1 = known valid
          # region_validity <-integer(num_regions)
          ## The current region is valid because it generates the sampler's
          ## current predictor allocation.
          # region_validity[old_region_ind] <-1L
          candidate_logweights <- region_logweights
          accepted_update <- FALSE
          number_invalid_regions_tested <- 0L
          while (!accepted_update) {
            if (!any(is.finite(candidate_logweights))) {
              stop(sprintf(paste0("No admissible latent-lag region remains at ","iteration %d, time %d, individual %d."),
                           iter,t,indiv),call. = FALSE)
            }
            candidate_region_probs <-normalize_log_probs(logp =candidate_logweights,iter =iter,indiv =indiv,z_iter =z_iter_ind,t =t,
                                                         context ="interior-time admissible region draw")
            region_ind <-sample.int(num_regions,size = 1L,replace = TRUE,prob =candidate_region_probs)
            zdraw_temp <-truncnorm::rtruncnorm(n = 1L,a =temp_region_logprobs[region_ind,2L],
                                               b =temp_region_logprobs[region_ind,3L],
                                               mean =temp_mean2_origscale +offsetz_vec[indiv],sd = 1)
            ## For one lag, only one predictor entry changes.
            candidate_lag_vector <-old_lag_vector
            candidate_lag_vector[future_index] <-zdraw_temp
            update_ok <- isTRUE(sampler$setPredictor(x =candidate_lag_vector,column = 1L))
            if (update_ok) {
              # region_validity[region_ind] <- 1L
              accepted_update <- TRUE
              break
            }
            ## Restore the previous predictor state immediately.
            rollback_ok <-sampler$setPredictor(x =old_lag_vector,column = 1L)
            if (!isTRUE(rollback_ok)) {stop(sprintf(paste0("Failed to restore predictor after rejecting region %d ","at iteration %d, time %d, individual %d."),region_ind,iter,t,indiv),call. = FALSE)}
            ## A rejected interior value means this entire tree-allocation
            ## region is invalid. Never sample it again in this update.
            # region_validity[region_ind] <--1L
            candidate_logweights[region_ind] <- -Inf
            number_invalid_regions_tested <-number_invalid_regions_tested +1L
            # if (number_invalid_regions_tested >=num_regions) {
            #   stop(sprintf(paste0("Every candidate region was rejected at iteration %d, ","time %d, individual %d."),
            #                iter,t,indiv),call. = FALSE)
            # }
          }
          Z.vec[current_index] <-zdraw_temp
          Z.mat[t,indiv] <-zdraw_temp
          Zlag.mat[future_index,1L] <-zdraw_temp


          # # sample a region using probabilities obtained above
          #
          # # print("Line 1903 before sample")
          #
          # region_ind <- sample.int(num_regions, 1, replace = TRUE, prob = temp_region_probs[,1])
          #
          # # print("Line 1914 after sample")
          #
          # temp_mean2_origscale <- (temp_mean2 + 0.5)*(max_resp - min_resp) + min_resp + offsetz_vec[indiv]
          #
          # zdraw_temp <- rtruncnorm(n = 1,
          #                          a=temp_region_probs[region_ind, 2],
          #                          b=temp_region_probs[region_ind, 3],
          #                          mean = temp_mean2_origscale,
          #                          sd = 1)
          #
          #
          # #### BEGIN edits for repeated sampling until lag update does not result in terminal node
          #
          #
          # z_old_temp <- Z.mat[t, indiv ]
          # Z.mat[t, indiv ] <- zdraw_temp
          # Z.vec[(t-1)*num_indiv + indiv] <- zdraw_temp
          #
          # check_lag_valid <- TRUE
          #
          # for(j in 1:num_lags){
          #   init_Z_t0 <- rep(0, j*num_indiv)
          #   Zlag.mat[,j] <- c(init_Z_t0, Z.vec[1:((n.time_train-j)*num_indiv)])
          #   if((sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) ){
          #     check_lag_valid <- FALSE
          #     Z.mat[t, indiv ] <- z_old_temp
          #     Z.vec[(t-1)*num_indiv + indiv] <- z_old_temp
          #     for(j2 in 1:j){
          #       init_Z_t0 <- rep(0, j2*num_indiv)
          #       Zlag.mat[,j2] <- c(init_Z_t0, Z.vec[1:((n.time_train-j2)*num_indiv)])
          #       doublecheck <- (sampler$setPredictor(x = Zlag.mat[,j2], column = j2) == FALSE)
          #       if(doublecheck){
          #         stop("cannot roll back")
          #       }
          #     }
          #     break
          #   }
          # }
          #
          # lag_update_attempt <-  0L
          #
          # while (check_lag_valid == FALSE) {
          #   lag_update_attempt <- lag_update_attempt + 1L
          #
          #   if ( lag_update_attempt > max_lag_update_attempts) {
          #     stop(sprintf(paste0("Unable to obtain a valid latent-lag update ","at iteration %d, time %d, individual %d."),
          #                  iter,t,indiv),call. = FALSE)
          #   }
          #
          #   region_ind <- sample.int(num_regions, 1, replace = TRUE, prob = temp_region_probs[,1])
          #   zdraw_temp <- rtruncnorm(n = 1,
          #                            a=temp_region_probs[region_ind, 2],
          #                            b=temp_region_probs[region_ind, 3],
          #                            mean = temp_mean2_origscale,
          #                            sd = 1)
          #   # z_old_temp <- Z.mat[t, indiv ]
          #   Z.mat[t, indiv ] <- zdraw_temp
          #   Z.vec[(t-1)*num_indiv + indiv] <- zdraw_temp
          #
          #   check_lag_valid <- TRUE
          #
          #   for(j in 1:num_lags){
          #     init_Z_t0 <- rep(0, j*num_indiv)
          #     Zlag.mat[,j] <- c(init_Z_t0, Z.vec[1:((n.time_train-j)*num_indiv)])
          #     if((sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) ){
          #       check_lag_valid <- FALSE
          #       Z.mat[t, indiv ] <- z_old_temp
          #       Z.vec[(t-1)*num_indiv + indiv] <- z_old_temp
          #       for(j2 in 1:j){
          #         init_Z_t0 <- rep(0, j2*num_indiv)
          #         Zlag.mat[,j2] <- c(init_Z_t0, Z.vec[1:((n.time_train-j2)*num_indiv)])
          #         doublecheck <- (sampler$setPredictor(x = Zlag.mat[,j2], column = j2) == FALSE)
          #         if(doublecheck){
          #           stop("Failed to revert rejected z update.")
          #         }
          #       }
          #       break
          #     }
          #   }
          # }

          #### END edits for repeated sampling until lag update does not result in terminal node
          # zdraw_temp <- rtruncnorm(n = 1,
          #                          a=temp_region_probs[region_ind, 2],
          #                          b=temp_region_probs[region_ind, 3],
          #                          mean = temp_mean2,
          #                          sd = 1)
          Z.mat[t, indiv ] <- zdraw_temp
          # jusyt need to update relevant element of Z.vec
          Z.vec[(t-1)*num_indiv + indiv] <- zdraw_temp
          # Z.vec <- as.vetor(t(Z.mat))
        } # end loop over time periods

        # check for special cases for n.time - 1, n.time - 2, n.time - 3
        # special case for t = n.time
        #### qkt for T ###################
        intersectmat_tmin1 <- Biglist_intersectmats[[n.time_train]]
        # num_regions <- nrow(intersectmat_tmin1)
        temp_ztpmin1 <- Z.vec[(n.time_train-2)*num_indiv + indiv]
        # must find mean corresponding to z in period t-1
        # This will be used in and after the loop over regions.# can directly obtain from dbarts# or find region
        # and use already saved region mean values

        # must find last lower bound that temp_ztpmin1 is greater than
        # for first upper bound that temp_ztpmin1 is below
        ktemp_tmin1 <- which(temp_ztpmin1 <= intersectmat_tmin1[, 3])[1]
        # Then obtain the corresponding region mean value
        temp_mean2 <- intersectmat_tmin1[ktemp_tmin1,1]
        # now find interval
        # tildeC_ktminl corresponds to # period t+1 k_ind region intereval
        y_t <- y.train_from1[n.time_train, indiv]
        # if(y_t == 1){
        #   temp_lower3 <- 0 # scalezero # 0
        #   temp_upper3 <- Inf
        # }else{
        #   temp_lower3 <- -Inf
        #   temp_upper3 <- 0 # scalezero # 0
        # }
        temp_lower3 <- c(-Inf,thresholds)[y_t] # scalezero # 0
        temp_upper3 <- c(thresholds,Inf)[y_t]
        temp_mean2_origscale <- (temp_mean2 + 0.5)*(max_resp - min_resp) + min_resp
        # CHECK IF THESE BOUNDS ARE CORRECTLY DEFINED
        zdraw_temp <- rtruncnorm(n = 1,
                                 a = temp_lower3,
                                 b = temp_upper3,
                                 mean = temp_mean2_origscale + offsetz_vec[indiv],
                                 sd = 1)
        Z.mat[n.time_train, indiv ] <- zdraw_temp
        Z.vec[(n.time_train-1)*num_indiv + indiv] <- zdraw_temp
        # } # end loop over items
      } # end loop over individuals indiv in 1:num_indiv
      # print("line 1288")
      # print("z_iter_ind = ")
      # print(z_iter_ind)
    } # end loop over z_iter_ind

    if (anyNA(Z.vec) ||any(!is.finite(Z.vec))) {stop(sprintf("Non-finite Z at Gibbs iteration %d.",iter),call. = FALSE)}
    max_abs_Z <- max(abs(Z.vec))
    if (max_abs_Z > 25) {
      stop(sprintf(paste0("Latent-state explosion at iteration %d: ","min(Z)=%.6g, max(Z)=%.6g, max|Z|=%.6g."),
          iter,min(Z.vec),max(Z.vec),max_abs_Z),call. = FALSE)
    }

    ####### update Z lag matrix ####################
    Zlag.mat <- matrix(NA, nrow = n.time_train*num_indiv, ncol = num_lags)
    for(t in 1:num_lags){
      init_Z_t0 <- rep(0, t*num_indiv)
      # init_Z_t0 <- rnorm(t*n.item*n.ranker)
      Zlag.mat[,t] <- c(init_Z_t0, Z.vec[1:((n.time_train-t)*num_indiv)])
      Xmat.train[,t + 1L] <- Zlag.mat[, t]
    }

    # for (lag_index in seq_len(num_lags)) {
    #   Xmat.train[,lag_index + 1L] <- Zlag.mat[, lag_index]
    # }
    ######## check that new z values are consistent with the tree structures ###############

    # for(j in 1:num_lags){
    #   # sampler$setPredictor(x = Zlag.mat[,j], column = j, forceUpdate = TRUE)
    #
    #   sampler$setPredictor(x = Zlag.mat[,j], column = j,
    #                              forceUpdate = TRUE)
    #
    #   # if (!isTRUE(ok)) {
    #   #   stop("Forced dbarts predictor update unexpectedly failed.")
    #   # }
    #
    #   Xmat.train[, j + 1L] <- Zlag.mat[, j]
    # }


    # for(j in 1:num_lags){
    #   # sampler$setPredictor(x = Zlag.mat[,j], column = j, forceUpdate = TRUE)
    #
    #   while (sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) {
    #
    #     if(seq_z_draws==1){
    #       stop("updates still not consistent with tree structure")
    #     }
    #     # print("new z values not consistent with tree structure, must draw again")
    #
    #     # print("current values ")
    #     # print("sampler$data@x = ")
    #     # print(sampler$data@x)
    #     #
    #     # print(" Zlag.mat[,j] = ")
    #     # print( Zlag.mat[,j])
    #
    #
    #     # If this error message occurs
    #     # Check the conditions in the dbart package for setPredictor == FALSE
    #     # And if this is hypothetically possible, even with draws from the smoothing distribution,
    #     # and if it is not a bug
    #     # then need to go back to beginning of this iteration of the Gibbs sampler
    #     # and sample Zmat again
    #
    #
    #     temp_break <- 1
    #     break
    #     # stop("new z values not consistent with tree structure, must draw again")
    #
    #
    #     # #perhaps this can be rewritten to just re-draw the relevant column?
    #     # Z.mat <- GibbsUpLatentGivenRankindividual(pair.comp.ten = pair.comp.ten,
    #     #                                           Z.mat = Z.mat,
    #     #                                           mu = mu,
    #     #                                           weight.vec = rep(1, n.ranker*n.time),
    #     #                                           n.ranker = n.ranker*n.time,
    #     #                                           n.item = n.item )
    #     #
    #     # Zlag.mat <- matrix(NA, nrow = n.time*n.ranker*n.item, ncol = num_lags)
    #     #
    #     # for(t in 1:num_lags){
    #     #   init_Z_t0 <- rep(0, t*n.item*n.ranker)
    #     #   # init_Z_t0 <- rnorm(t*n.item*n.ranker)
    #     #
    #     #   Zlag.mat[,t] <- c(init_Z_t0, as.vector(Z.mat)[1:((n.time-t)*n.item*n.ranker)])
    #     #
    #     # }
    #     #
    #     # #check that Zmat and  Zlag.mat are updated outside the while loop and for-loop over j
    #
    #   }
    #
    #   if(temp_break==1){
    #     break
    #   }
    # }
    #
    # # print("sampler$data@x = ")
    # # print(sampler$data@x)
    #
    # # if need to draw z values again, go back to start of loop
    # if(temp_break==1){
    #   # if(breakcount == max_z_resamples){
    #   Z.mat <- Z.matold
    #   Z.vec <- as.vector(t(Z.mat))
    #
    #   # Zlag.mat <- matrix(NA, nrow = n.time*n.ranker*n.item, ncol = num_lags)
    #   #
    #   # for(t in 1:num_lags){
    #   #   init_Z_t0 <- rep(0, t*n.item*n.ranker)
    #   #   # init_Z_t0 <- rnorm(t*n.item*n.ranker)
    #   #
    #   #   Zlag.mat[,t] <- c(init_Z_t0, as.vector(Z.mat)[1:((n.time-t)*n.item*n.ranker)])
    #   #
    #   # }
    #
    #   Zlag.mat <- matrix(NA, nrow = n.time_train*num_indiv, ncol = num_lags)
    #
    #   for(t in 1:num_lags){
    #     init_Z_t0 <- rep(0, t*num_indiv)
    #     # init_Z_t0 <- rnorm(t*n.item*n.ranker)
    #
    #     Zlag.mat[,t] <- c(init_Z_t0, Z.vec[1:((n.time_train-t)*num_indiv)])
    #
    #   }
    #
    #   for(j in 1:num_lags){
    #     sampler$setPredictor(x = Zlag.mat[,j], column = j#, forceUpdate = TRUE
    #     )
    #   }
    #   for(j in 1:num_lags){
    #     Xmat.train[, j + 1L] <- Zlag.mat[, j]
    #   }
    #
    #   # }else{
    #   #   breakcount <- breakcount +1
    #   #   next
    #   # }
    #
    # }else{
    #   # Xmat.train[, j + 1L] <- Zlag.mat[, j]
    #   for(j in 1:num_lags){
    #     Xmat.train[, j + 1L] <- Zlag.mat[, j]
    #   }
    # }


    # temp_break <- 0
    #
    # for(j in 1:num_lags){
    #   # sampler$setPredictor(x = Zlag.mat[,j], column = j, forceUpdate = TRUE)
    #
    #   while (sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) {
    #
    #     if(seq_z_draws==1){
    #       stop("updates still not consistent with tree structure")
    #     }
    #     print("new z values not consistent with tree structure, must draw again")
    #
    #     # print("current values ")
    #     # print("sampler$data@x = ")
    #     # print(sampler$data@x)
    #     #
    #     # print(" Zlag.mat[,j] = ")
    #     # print( Zlag.mat[,j])
    #
    #
    #     # If this error message occurs
    #     # Check the conditions in the dbart package for setPredictor == FALSE
    #     # And if this is hypothetically possible, even with draws from the smoothing distribution,
    #     # and if it is not a bug
    #     # then need to go back to beginning of this iteration of the Gibbs sampler
    #     # and sample Zmat again
    #
    #
    #     temp_break <- 1
    #     break
    #     # stop("new z values not consistent with tree structure, must draw again")
    #
    #
    #     # #perhaps this can be rewritten to just re-draw the relevant column?
    #     # Z.mat <- GibbsUpLatentGivenRankindividual(pair.comp.ten = pair.comp.ten,
    #     #                                           Z.mat = Z.mat,
    #     #                                           mu = mu,
    #     #                                           weight.vec = rep(1, n.ranker*n.time),
    #     #                                           n.ranker = n.ranker*n.time,
    #     #                                           n.item = n.item )
    #     #
    #     # Zlag.mat <- matrix(NA, nrow = n.time*n.ranker*n.item, ncol = num_lags)
    #     #
    #     # for(t in 1:num_lags){
    #     #   init_Z_t0 <- rep(0, t*n.item*n.ranker)
    #     #   # init_Z_t0 <- rnorm(t*n.item*n.ranker)
    #     #
    #     #   Zlag.mat[,t] <- c(init_Z_t0, as.vector(Z.mat)[1:((n.time-t)*n.item*n.ranker)])
    #     #
    #     # }
    #     #
    #     # #check that Zmat and  Zlag.mat are updated outside the while loop and for-loop over j
    #
    #   }
    #
    #   if(temp_break==1){
    #     break
    #   }
    # }
    #
    # # print("sampler$data@x = ")
    # # print(sampler$data@x)
    #
    # # if need to draw z values again, go back to start of loop
    # if(temp_break==1){
    #   # if(breakcount == 10){
    #     Z.mat <- Z.matold
    #     Z.vec <- as.vector(t(Z.mat))
    #
    #
    #     # Zlag.mat <- matrix(NA, nrow = n.time*n.ranker*n.item, ncol = num_lags)
    #     #
    #     # for(t in 1:num_lags){
    #     #   init_Z_t0 <- rep(0, t*n.item*n.ranker)
    #     #   # init_Z_t0 <- rnorm(t*n.item*n.ranker)
    #     #
    #     #   Zlag.mat[,t] <- c(init_Z_t0, as.vector(Z.mat)[1:((n.time-t)*n.item*n.ranker)])
    #     #
    #     # }
    #
    #     Zlag.mat <- matrix(NA, nrow = n.time_train*num_indiv, ncol = num_lags)
    #
    #     for(t in 1:num_lags){
    #       init_Z_t0 <- rep(0, t*num_indiv)
    #       # init_Z_t0 <- rnorm(t*n.item*n.ranker)
    #
    #       Zlag.mat[,t] <- c(init_Z_t0, Z.vec[1:((n.time_train-t)*num_indiv)])
    #
    #     }
    #
    #     for(j in 1:num_lags){
    #       sampler$setPredictor(x = Zlag.mat[,j], column = j#, forceUpdate = TRUE
    #                            )
    #     }
    #     for(j in 1:num_lags){
    #       Xmat.train[, j + 1L] <- Zlag.mat[, j]
    #     }
    #
    #   # }else{
    #   #   breakcount <- breakcount +1
    #   #   next
    #   # }
    #
    # }else{
    #   for(j in 1:num_lags){
    #     Xmat.train[, j + 1L] <- Zlag.mat[, j]
    #   }
    # }

    stopifnot(isTRUE(all.equal(Z.vec,as.vector(t(Z.mat)),tolerance = 0)))
    breakcount <- 0
    # print("Line 2147")

    ##################### Sample sum-of-trees ##################################################
    #set the response.
    #Check that 0 is a reasonable initial value
    #perhaps makes more sense to use initial values of Z

    # # sampler$setResponse(y = as.vector(Z.mat))
    # sampler$setResponse(y = as.vector(Z.vec))
    #
    # min_resp <- min(Z.vec)
    # max_resp <- max(Z.vec)
    #
    # scalezero <- (0 - min_resp)/(max_resp - min_resp) - 0.5
    # scaled_offsetz_vec <- (offsetz_vec - min_resp)/(max_resp - min_resp) - 0.5


    computational_offset <- Z_reference - Z.vec
    dbarts_offset <- computational_offset + offsetz_vec # genuine_offset_vec
    sampler$setOffset( offset = dbarts_offset, updateScale = FALSE )

    if (!isTRUE(all.equal( as.numeric(sampler$data@y), as.numeric(Z_reference), tolerance = 0, check.attributes = FALSE))) {
      stop( sprintf("Fixed dbarts reference response changed at iteration %d.", iter), call. = FALSE)}

    # sampler$setSigma(sigma = 1)
    #sampler$setPredictor(x= df_for_dbart$x, column = 1, forceUpdate = TRUE)
    #mu = as.vector( alpha + X.mat %*% beta )
    samplestemp <- sampler$run()

    f_train <- as.numeric(samplestemp$train[, 1L]) - dbarts_offset
    mutemp <- offsetz_vec + f_train # genuine_offset_vec + f_train
    mu = mutemp

    # mutemp <- samplestemp$train[,1]
    #suppose there are a number of samples
    # mutemp <- sampler$predict(df_for_dbart)
    # print("sigma = ")
    # print(samplestemp$sigma)

    # print("line 2307 = ")
    #
    # print("sampler$data@x = ")
    # print(sampler$data@x)
    #
    # print("Zlag.mat= ")
    # print(Zlag.mat)

    # if(nrow(X.train)==n.item){
    #   #each n.ranker values of u should be equal,
    #   #so just take one mu value from each of these
    #   #this keeps the dimension of mu equal to n.item
    #   #so a new Gibbs sampler update does not have to be written for Z
    #
    #   #mu = mutemp[(1:n.item)]
    #   mu = mutemp[n.item+(1:n.item)]
    #
    #   # if(mutemp[1]!= mutemp[n.item+1]){
    #   if(mutemp[n.item+1]!= mutemp[n.item+n.item+1]){
    #     print("iteration number")
    #     print(iter)
    #     print("n.item = ")
    #     print(n.item)
    #     print("mutemp = ")
    #     print(mutemp)
    #     stop("mutemp[1]!= mutemp[n.item+1]")
    #   }
    #
    #   #mu = mutemp[(1:n.item)*n.ranker]
    # }else{
    #   if(nrow(X.train)==n.item*n.ranker*n.time){
    #     mu = mutemp
    #   }else{
    #     stop("nrow(X.train) not equal to n.item*n.ranker*n.time")
    #   }
    # }

    ##################### Store iteration output ##################################################

    #remove this? will be saved again later
    if(iter > n.burnin){
      # store value at this iteration
      if(keep_zmat==TRUE){
        # print("line 2230")
        draw$Z.mat[,,iter_min_burnin] = Z.mat
        draw$Z.mat.test[,,iter_min_burnin] = Z.mat.test
      }
      # draw$alpha[,iter_min_burnin] = alpha
      # draw$beta[,iter_min_burnin] = beta
      draw$mu[,,iter_min_burnin] = matrix(mu, nrow = num_indiv, ncol = n.time_train) # mu
      draw$prob.train.draws[, , 1:num_categories, iter_min_burnin] <-
        pnorm( outer( X = - draw$mu[, , iter_min_burnin],  Y =  c(thresholds, Inf ), FUN = "+")  ) -
        pnorm( outer( X = - draw$mu[, , iter_min_burnin],  Y =  c(-Inf,thresholds ), FUN = "+")  )# draw$sigma2.alpha[iter_min_burnin] = sigma2.alpha      # draw$sigma2.alpha[iter_min_burnin] = sigma2.alpha
      # draw$sigma2.beta[iter_min_burnin] = sigma2.beta
      draw$thresholds[,iter_min_burnin] <- thresholds
    }

    if( nrow(X.test) > 0 ){
      temp_test_mat <- matrix(NA,nrow = num_indiv,ncol =  ncol(Xmat.test))
      for(t  in 1:num_lags){
        if(noise_in_pred ==1){
          # temp_test_mat[,t] <- as.vector(mu)[((n.time-t)*n.item*n.ranker+1):((n.time-t +1) *n.item*n.ranker)]
          temp_test_mat[,t] <- Z.vec[((n.time_train-t)*num_indiv+1):((n.time_train-t +1)*num_indiv)]
        }else{
          # temp_test_mat[,t] <- as.vector(Z.mat)[((n.time-t)*n.item*n.ranker+1):((n.time-t +1)*n.item*n.ranker)]
          # temp_test_mat[,t] <- as.vector(mu)[((n.time-t)*n.item*n.ranker+1):((n.time-t +1) *n.item*n.ranker)]
          temp_test_mat[,t] <- Z.vec[((n.time_train-t)*num_indiv+1):((n.time_train-t +1)*num_indiv)]
        }
      }
      # temp_test_mat[, (num_lags+1):ncol(Xmat.test)] <- as.matrix(Xmat.test[1:(n.item*n.ranker) ,
      #                                                                      (num_lags+1):ncol(Xmat.test)])
      temp_test_mat[, (num_lags+1):ncol(Xmat.test)] <- as.matrix(Xmat.test[1:(num_indiv) , (num_lags+1):ncol(Xmat.test)])
      temp_test_mat <- data.frame(x = temp_test_mat)
      colnames(temp_test_mat) <- colnames(Xmat.test)
      # temp_test_mat <- Xmat.test[1:(n.item*n.ranker) ,]  #  matrix(NA, nrow = n.item*n.ranker,ncol = ncol(Xmat.test))
      # temp_test_preds <- matrix(NA, nrow = n.item*n.ranker,ncol = num_test_periods)
      temp_test_preds <- matrix(NA, nrow = num_indiv, ncol = num_test_periods)
      # print("Line 5488")
      # temp_mu_test <- rep(NA,  nrow(df_for_dbart_test) )
      temp_mu_test <- rep(NA,  nrow(Xmat.test) )
      # for t1 ==1, alternative is to draw from predictive distribtuion for Z_{T+1}
      # as outlined in AR-BART paper
      # if(iter < 5){
      #   print("temp_test_mat = " )
      #   print(temp_test_mat)
      # }
      # print("Line 2192")
      for(t1 in 1:num_test_periods){
        #produce a prediction

        # temp_test_mat <- data.frame(#y = rep(NA, nrow(temp_test_mat)), x = temp_test_mat)
        # colnames(temp_test_mat) <- colnames(df_for_dbart_test)
        # # must use original column names to prevent an error in the predict function
        # # colnames(temp_test_mat) <- colnames(df_for_dbart_test)
        #
        # # print("Line 2200")
        # # print("t1 = ")
        # # print(t1)
        # # print("colnames(temp_test_mat) = ")
        # # print(colnames(temp_test_mat))
        # #
        # # print("colnames(df_for_dbart) = ")
        # # print(colnames(df_for_dbart))
        # #
        # # print("df_for_dbart = ")
        # # print(df_for_dbart)
        # #
        # # print("class(temp_test_mat) = ")
        # # print(class(temp_test_mat))
        # #
        # # print("temp_test_mat = ")
        # # print(temp_test_mat)
        # #
        # # # temp_test_mat <- as.matrix(temp_test_mat)
        # #
        # # print("class(temp_test_mat) = ")
        # # print(class(temp_test_mat))
        # #
        # # print("colnames(df_for_dbart)")
        # # print(colnames(df_for_dbart))
        # #
        # # print("colnames(temp_test_mat)")
        # # print(colnames(temp_test_mat))
        # #
        # # testpredvec <- sampler$predict(x = df_for_dbart)
        # #
        # # print("testpredvec = ")
        # # print(testpredvec)
        #
        # if(num_indiv ==1){
        #   # print("Line 2264")
        #   #
        #   # print("min(Z.vec) = ")
        #   # print(min(Z.vec))
        #   # print("max(Z.vec) = ")
        #   # print(max(Z.vec))
        #   #
        #   # print("Z.vec[length(Z.vec)] = ")
        #   # print(Z.vec[length(Z.vec)])
        #
        #   tempbind <- as.matrix(rbind(temp_test_mat,temp_test_mat))
        #   colnames(tempbind) <- colnames(df_for_dbart_test)
        #
        #   # print("tempbind = ")
        #   # print(tempbind)
        #   #
        #   # #
        #   # # print("sampler$state[[1]]@trees = ")
        #   # # print(sampler$state[[1]]@trees)
        #   # #
        #   # # print("sampler$state")
        #   # # print(sampler$state)
        #   #
        #   # print("sampler$getTrees() = ")
        #   # print(sampler$getTrees())
        #   #
        #   # print("num_indiv = ")
        #   # print(num_indiv)
        #   #
        #   # testpredvec <- sampler$predict(x.test = as.matrix(tempbind[1,]), offset.test = NULL )[1:num_indiv]
        #
        #   list_inter_mats <- list()
        #
        #   for(i in 1:n.trees){
        #     # print("Line 492")
        #     treeexample1 <- sampler$getTrees(treeNums = i,
        #                                      chainNums = 1,
        #                                      sampleNums = 1)
        #     # print("Line 496")
        #     # print("treeexample1 = ")
        #     # print(treeexample1)
        #     # # rebuilt_tree <- rebuildTree2(treeexample1)
        #     rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
        #     # print("Line 499")
        #     #
        #     #must use covariates for individual indiv at time period t
        #
        #     # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, as.matrix(df_for_dbart$x[obs_indices[1]]) )
        #
        #     # list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree$var == -1 , 5:7]
        #     list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree[,4] == -1 , 5:7, drop = FALSE]
        #     # print("Line 507")
        #
        #   }
        #
        #   intersectmat <- interNtreesB(list_inter_mats)
        #
        #   kpredtemp <- which((as.matrix(tempbind[1,])[1] < intersectmat[, 3]) )[1]
        #   # Then obtain the corresponding region mean value
        #   testpredvec <- as.matrix(intersectmat[kpredtemp,1])
        #
        #
        #   testpredvec <- (testpredvec + 0.5)*(max_resp - min_resp) + min_resp
        #
        #
        #   # temp_mean2_debug <- sampler$predict(x.test = as.matrix(rep(as.matrix(tempbind[1,1]),100)),
        #   #                                     offset.test = NULL)[1:num_indiv]
        #   #
        #   # print("line 2194 temp_mean2_debug from predict = ")
        #   # print(temp_mean2_debug)
        #   #
        #   # print("line 2061 testpredvec = ")
        #   # print(testpredvec)
        #
        # }else{
        #   testpredvec <- sampler$predict(x.test = as.matrix(temp_test_mat), offset.test = NULL)
        # }
        #
        #
        # # print("testpredvec = ")
        # # print(testpredvec)
        #
        # # print("Line 2279")
        #
        #
        # # print("testpredvec = ")
        # # print(testpredvec)
        # #
        # # print("Line 2283")
        # testpredvec <- sampler$predict(temp_test_mat)

        rebuilt_tree_list <- vector("list", n.trees)
        for (i in seq_len(n.trees)) {
          treeexample1 <- sampler$getTrees(treeNums = i, chainNums = 1L, sampleNums = 1L)
          if (anyNA(treeexample1$value) || any(!is.finite(treeexample1$value))) {
            stop(sprintf("Non-finite value in updated dbarts tree at iteration %d, tree %d.", iter, i), call. = FALSE)
          }
          rebuilt_tree_list[[i]] <- rebuildTree2_cpp(as.matrix(treeexample1))
        }
        testpredvec <- rep(NA, num_indiv)

        for(indiv in 1:num_indiv){
          indiv_ind <- (t1 - 1)*num_indiv+indiv

          # list_inter_mats <- list()
          #
          # for(i in 1:n.trees){
          #   treeexample1 <- sampler$getTrees(treeNums = i, chainNums = 1,sampleNums = 1)
          #   # rebuilt_tree <- rebuildTree2(treeexample1)
          #   rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
          #   #must use covariates for individual indiv at time period t
          #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, Xmat.train[obs_indices[1],-1, drop = FALSE] )
          #   # if num_lags > 1 this would have to be edited (just first column dropped)
          #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3_cpp(as.matrix(rebuilt_tree),
          #   #                                                          as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
          #   list_inter_mats[[i]] <- getPredictionsRangesForTreeMCP_cpp(as.matrix(rebuilt_tree),as.matrix(temp_test_mat[indiv, , drop = FALSE]))
          # }
          # intersectmat <- interNtreesB_cpp(list_inter_mats)
          list_inter_mats <- getListintermats_cpp(rebuilt_tree_list ,as.matrix(temp_test_mat[indiv, , drop = FALSE]))
          intersectmat <- interNtreesB_cpp(list_inter_mats)
          # Biglist_intersectmats[[t]] <- intersectmat
          # obtain prediction from intersection matrix
          ktemp <- which((as.matrix(temp_test_mat[indiv,1])[1]  <= intersectmat[, 3]) )[1]
          # Then obtain the corresponding region mean value
          testpredvec[indiv] <- intersectmat[ktemp,1]
        }
        testpredvec <- (testpredvec + 0.5)*(max_resp - min_resp) + min_resp
        #fill in temp_test_preds with noise
        test_indices <- (t1 -1L)*num_indiv + seq_len(num_indiv)
        offset_current_test <- offsetz_vec_test[test_indices ]
        if(noise_in_pred ==1){
          temp_test_preds[ , t1] <- testpredvec + rnorm( num_indiv )+ offset_current_test
        }else{
          temp_test_preds[ , t1] <- testpredvec + offset_current_test #+ rnorm( num_indiv )
        }

        #update temp_test_mat
        #shift z columns to the right and fill in leftmost column
        #need to rewrite this if want to allow for no observed covariates
        # temp_test_mat <-  data.frame( x= cbind(  temp_test_preds[ , t1] , temp_test_mat[,1:(num_lags-1)] ,Xmat.test[(t1-1)*(n.item*n.ranker)  +  1:(n.item*n.ranker) ,(num_lags+1):(ncol(Xmat.test))] ) )

        if(t1 != num_test_periods){
          if(num_lags ==1){
            temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] ,
                                                      as.matrix(Xmat.test[(t1)*(num_indiv)  +  1:(num_indiv) ,
                                                                          (num_lags+1):(ncol(Xmat.test))]) ))

          }else{
            temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] ,
                                                      temp_test_mat[,1:(num_lags-1)] ,
                                                      as.matrix(Xmat.test[(t1)*(num_indiv)  +  1:(num_indiv) ,
                                                                          (num_lags+1):(ncol(Xmat.test))]) ))
          }
        }
        # colnames(temp_test_mat) <- colnames(Xmat.test)
        colnames(temp_test_mat) <- colnames(Xmat.test)
        # fill in temp_mu_test without noise
        temp_mu_test[(t1-1)*(num_indiv)  +  1:(num_indiv) ] <- testpredvec + offset_current_test
      }

      #also update Zlag.mat.test ?
      #perhaps this is unnecessary here?
      # print("Line 5545")
      # Zlag.mat.test <- matrix(NA, nrow = num_test_periods*n.ranker*n.item, ncol = num_lags)
      Zlag.mat.test <- matrix(NA, nrow = num_test_periods*num_indiv, ncol = num_lags)
      # if(nrow(X.test) >0 ){
      for(t in 1:num_lags){
        if(num_test_periods > t ){
          # # up to t time periods ahead can be filled in. Rest of test time periods filled in using repeated values of last observation in training data
          #
          # Zlag.mat.test[,t] <- c(Z.vec[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
          #                        rep(Z.vec[((n.time_train-1)*num_indiv+1):(n.time_train*num_indiv)],
          #                            (num_test_periods-t) )  )
          #
          # # #other option is to set all unobservable values to zero

          # USE PREDICTIONs INSTEAD OF FILLING IN WITH REPEATED VALUES OF LAST TRAINING OBSERVATION
          Zlag.mat.test[,t] <- c(Z.vec[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
                                 as.vector(as.matrix(temp_test_preds))[1:((num_test_periods - t)*(num_indiv))  ] )
        }else{
          #nothing to fill in if num_test_periods <= t
          Zlag.mat.test[,t] <- c(Z.vec[((n.time_train-t)*num_indiv+1):((n.time_train-(t - num_test_periods) )*num_indiv)]  )
        }
        # }
      }
      # }
      # if(nrow(X.test)>0){
      #   for(j in 1:num_lags){
      #     #perhaps this should be removed for when Z is updated properly below
      #     sampler$setTestPredictor(x = Zlag.mat.test[,j], column = j)
      #   }
      #
      # }
      if(iter > n.burnin){
        draw$mu_test[,,iter_min_burnin] <- matrix(temp_mu_test, nrow = num_indiv, ncol = num_test_periods) # temp_mu_test
        draw$prob.test.draws[, , 1:num_categories, iter_min_burnin] <-
          pnorm( outer( X = - draw$mu_test[, , iter_min_burnin],  Y =  c(thresholds, Inf ), FUN = "+")  ) -
          pnorm( outer( X = - draw$mu_test[, , iter_min_burnin],  Y =  c(-Inf,thresholds ), FUN = "+")  )
        # draw$mu_test[,iter_min_burnin] <- samplestemp$test[,1]
        if(keep_zmat==TRUE){
          # draw$Z.mat.test[,,iter_min_burnin]  <- matrix(data = as.vector(temp_test_preds), nrow = n.item, ncol = n.ranker*num_test_periods)
          draw$Z.mat.test[,,iter_min_burnin]  <- t(matrix(data = as.vector(temp_test_preds), nrow = num_indiv, ncol = num_test_periods))
        }
      }
    }
    # print iteration number
    if(iter %% print.opt == 0){
      print(paste("Gibbs Iteration", iter))
      # print(c(sigma2.alpha, sigma2.beta))
    }
    iter <- iter+1
  } # end iterations of Gibbs sampler
  return(draw)
}



