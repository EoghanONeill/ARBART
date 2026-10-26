


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
AROrdPbartX_fullcond_emptynodes <- function(y.train = NULL,
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
                                 print.opt = 100L,
                                 probit_k = 2.0,
                                 reference_half_range = 3.0, # max_lag_update_attempts = 10000L,
                                 prune_existing_empty_leaves  = FALSE,
                                 sigma2 = 1,
                                 alpha = 0.95,
                                 beta = 2,
                                 node_min_size = 1L,
                                 trans_prob = c(0.3, 0.3, 0.4),
                                 splitting_rules = "continuous",
                                 max_bad_trees = 50L,
                                 sparse = FALSE,
                                 alpha_prior = FALSE,
                                 alpha_a = 0.5,
                                 alpha_b = 1,
                                 sigma_mu_prior = FALSE,
                                 validate_rebuilt_trees = FALSE){



  ######### Check inputs ##################
  if (length(print.opt) != 1L || is.na(print.opt) || print.opt != as.integer(print.opt) || print.opt < 1L) {stop("print.opt must be a positive integer.")}
  print.opt <- as.integer(print.opt)
  if(num_test_periods < 1){ stop("num_test_periods must be >=1") }
  if(!is.integer(num_lags)){ stop("num_lags must be an integer.") }
  if(num_lags <1){ stop("num_lags must be an integer greater than or equal to 1.") }
  if(num_lags >1){ stop("Code not yet written for num_lags > 1.")}
  if (length(num_z_iters) != 1L || is.na(num_z_iters) || num_z_iters != as.integer(num_z_iters) || num_z_iters < 1L){stop("num_z_iters must be an integer greater than or equal to 1. It is the number of repeated samples of latent variable values per Gibbs iteration.")}
  #check that input values are binary 0 or 1.

  # if(!(all(is.integer(y.train)))){
  #   stop("y.train contains non-integers")
  # }
  if (is.null(y.train) ||anyNA(y.train) ||any(!is.finite(y.train)) ||any(y.train != floor(y.train))) {stop("y.train must contain finite, non-missing integer-valued outcomes.")}
  y.train <- as.matrix(y.train)
  storage.mode(y.train) <- "integer"
  miny <- min(y.train)
  maxy <- max(y.train)
  if (nrow(y.train) < 3L) stop("AROrdPbartX_fullcond_emptynodes currently requires at least three training periods.", call. = FALSE)

  num_categories <- maxy - miny + 1
  if (num_categories < 3L) {stop("Use the binary-probit function for a two-category outcome.")}
  if(!(all(y.train %in% miny:maxy))){stop("!(all(y.train %in% miny:maxy))")}
  for(y_ind in miny:maxy){if(sum(1*(y.train == y_ind) ) ==0){
    stop("There are zero training obserations for an integer between the minimum and maximum outcome values. Redefine outcome or use different training data.")
  }
  }
  # check input covariates
  X.train <- as.matrix(X.train)
  if (is.null(X.test)) {
    X.test <- matrix(numeric(0),nrow = ncol(y.train) * num_test_periods,ncol = 0L)
  } else {
    X.test <- as.matrix(X.test)
  }
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
  y_ord <- ordered(as.vector(t(y.train)),levels = y_levels)
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

  Z_initial <-  Z.vec


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
  Xmat.train <- data.frame(y = as.vector(Z.vec), x = cbind(Zlag.mat, X.train) )
  Xmat.train.no.y <- cbind(Zlag.mat, X.train)
  # print(colnames(Xmat.train))

  if(nrow(X.test)>0 ){
    Xmat.test  <- data.frame(x = cbind( Zlag.mat.test, X.test ) )
    # Xmat.test.no.y <- cbind( Zlag.mat.test, X.test )
    #must create predictions for future time periods using covariates for one period ahead, two periods ahead etc.
  }


  var_count = rep(0, ncol(Xmat.train.no.y))
  # var_count_store = matrix(0, ncol = ncol(Xmat.train.no.y), nrow = store_size)
  # s_prob_store = matrix(0, ncol = ncol(Xmat.train.no.y), nrow = store_size)
  # var_count_store <- matrix(0,nrow = n.iter,ncol = ncol(Xmat.train.no.y))
  # s_prob_store <- matrix(0,nrow = n.iter,ncol = ncol(Xmat.train.no.y))
  tree_fits_store = matrix(0, ncol = n.trees, nrow = length(as.vector(Z.mat)))
  s = rep(1/ncol(Xmat.train.no.y), ncol(Xmat.train.no.y))
  p <- ncol(Xmat.train.no.y)
  # rho <- p # For DART
  if(alpha_prior){
    alpha_s <- p
  }else{
    alpha_s <- 1
  }
  alpha_scale <- p
  # actual_sig_mu <- reference_half_range/(probit_k*sqrt(n.trees)) #/3.0
  # sigma2_mu <- ((max(as.vector(Z.mat))-min(as.vector(Z.mat)))/(2 * k * sqrt(n.trees)))^2
  sigma2_mu <- ((reference_half_range)/( probit_k * sqrt(n.trees)))^2

  curr_trees = create_stump(num_trees = n.trees,
                            y = as.vector(Z.mat),
                            X = Xmat.train.no.y)
  # Initialise the new trees as current one
  new_trees = curr_trees
  # Initialise the predicted values to zero
  mutemp = get_predictions(curr_trees, Xmat.train.no.y, single_tree = n.trees == 1)
  mu <- mutemp + offsetz_vec

  Xmat.train.no.y <- as.matrix(Xmat.train.no.y) # to make other lines more efficient

  # if(n.burnin == 0){
  #   if(keep_zmat==TRUE){
  #     draw$Z.mat[,,1] <- Z.mat
  #     draw$Z.mat.test[,,1] <- Z.mat.test
  #   }
  #   # draw$alpha[,1] = alpha
  #   # draw$beta[,1] = beta
  #   draw$mu[, , 1] <- matrix(mu ,
  #                            nrow = num_indiv, ncol = n.time_train)
  #   draw$prob.train.draws[, , 1:num_categories, 1] <-
  #     pnorm( outer( X = - draw$mu[, , 1]  ,  Y =  c(thresholds, Inf ), FUN = "+")  ) -
  #     pnorm( outer( X = - draw$mu[, , 1]   ,  Y =  c(-Inf,thresholds ), FUN = "+")  )
  #   # draw$sigma2.alpha[1] = sigma2.alpha
  #   # draw$sigma2.beta[1] = sigma2.beta
  #   draw$thresholds[,1] <- thresholds
  # }

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
    testpredvec = get_predictions(curr_trees, temp_test_mat, single_tree = n.trees == 1)

    test_indices <- (t1 -1L)*num_indiv + seq_len(num_indiv)
    offset_current_test <- offsetz_vec_test[test_indices ]
    if(noise_in_pred==1){
      temp_test_preds[ , t1] <- testpredvec + offset_current_test + rnorm( num_indiv )
    }else{
      temp_test_preds[ , t1] <- testpredvec + offset_current_test
    }
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

    colnames(temp_test_mat) <- colnames(Xmat.test)
    #fill in temp_mu_test without noise
    temp_mu_test[(t1-1)*(num_indiv)  +  1:(num_indiv) ] <- testpredvec + offset_current_test
  }

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
  # if(n.burnin == 0){
  #   draw$mu_test[,,1] <- matrix(temp_mu_test , nrow = num_indiv, ncol = num_test_periods) #temp_mu_test
  #   draw$prob.test.draws[, , 1:num_categories, 1] <-
  #     pnorm( outer( X = - draw$mu_test[, , 1]  ,  Y =  c(thresholds, Inf ), FUN = "+")  ) -
  #     pnorm( outer( X = - draw$mu_test[, , 1]  ,  Y =  c(-Inf,thresholds ), FUN = "+")  )
  #   # draw$mu_test[,1] <- samplestemp$test[,1]
  #   if(keep_zmat==TRUE){
  #     draw$Z.mat.test[,,1]  <- t(matrix(data = as.vector(temp_test_preds), nrow = num_indiv, ncol = num_test_periods))
  #   }
  # }
  # print("Line 828")
  ##################### Begin Gibbs sampler ##################################################
  #//////////////////////////////////////////////////////////////////////////
  ## Gibbs iteration
  iter <- 1

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

    ########### Sample from full conditional distribution.
    #################### calculate intersection matrices ########################################

    # if  there are no covariates, the intersection matrices do not vary across time or individuals
    #### obtain intersection matrices for time period t = 1 ###############
    # create intersection matrices
    intersectmat_tmin1 <- NA
    # list_item_intersectmats_tmin1 <- NA
    rebuilt_tree_list <- list()

    if (validate_rebuilt_trees) {
      for (i in seq_len(n.trees)) {
        tm <- curr_trees[[i]]$tree_matrix
        rebuilt <- rebuildTreeFromChildren_cpp(tree = as.matrix(tm),
                                               terminal_col = match("terminal",colnames(tm)),
                                               node_size_col = match("node_size",colnames(tm)),
                                               split_variable_col = match("split_variable",colnames(tm)),
                                               split_value_col = match("split_value",colnames(tm)),
                                               mu_col = match("mu",colnames(tm)),
                                               child_left_col = match("child_left",colnames(tm)),
                                               child_right_col = match("child_right",colnames(tm)),
                                               tree_number = i)
        check_rows <- sample.int(nrow(Xmat.train.no.y),size = min(25L,nrow(Xmat.train.no.y)))
        for (row_index in check_rows) {
          x_row <- Xmat.train.no.y[row_index,,drop = FALSE]
          ranges <- getPredictionsRangesForTreeMCP_cpp(rebuilt,x_row)
          lag_value <- x_row[1L, 1L]
          region_index <- which(lag_value > ranges[, 2L] & lag_value <= ranges[, 3L])
          if (length(region_index) != 1L) {
            stop(sprintf(paste0("Lag value did not match exactly one interval. ","Tree=%d, observation=%d, matches=%s."),
                         i,row_index,paste(region_index, collapse = ",")))
          }
          interval_prediction <- ranges[region_index, 1L]
          direct_prediction <- get_predictions(curr_trees[[i]],x_row,single_tree = TRUE)
          if (!isTRUE(all.equal(as.numeric(interval_prediction),as.numeric(direct_prediction),tolerance = 1e-12,check.attributes = FALSE))) {
            stop(sprintf(paste0("Prediction mismatch for tree %d, observation %d: ","direct=%.17g, interval=%.17g."),
                         i,row_index,direct_prediction,interval_prediction))
          }
        }
      }
    }
    for(i in 1:n.trees){
      temptree <- curr_trees[[i]]$tree_matrix
      # treeexample1 <- cbind(1, i, temptree[, c('node_size','split_variable', 'split_value' ), drop = FALSE])
      # temp_na_inds <- is.na(temptree[,'split_variable'])
      # treeexample1[temp_na_inds , 4:5 ] <- cbind(-1, temptree[temp_na_inds,'mu'])
      # rebuilt_tree_list[[i]] <- rebuildTree2_cpp(as.matrix(treeexample1))
      rebuilt_tree_list[[i]] <- rebuildTreeFromChildren_cpp(tree = as.matrix(temptree),
                                                            terminal_col = match("terminal",colnames(temptree)),
                                                            node_size_col = match("node_size",colnames(temptree)),
                                                            split_variable_col = match("split_variable",colnames(temptree)),
                                                            split_value_col = match("split_value",colnames(temptree)),
                                                            mu_col = match("mu",colnames(temptree)),
                                                            child_left_col = match("child_left",colnames(temptree)),
                                                            child_right_col = match("child_right",colnames(temptree)),
                                                            tree_number = i)
      # pred_custom <- get_predictions(curr_trees[[i]],Xmat.train.no.y,single_tree = TRUE)
      # pred_rebuilt <- predict_from_rebuilt_tree(rebuilt_tree_list[[i]],Xmat.train.no.y)
      # stopifnot(isTRUE(all.equal(pred_custom,pred_rebuilt,tolerance = 1e-12,check.attributes = FALSE)))
    }

    list_inter_mats <- list()
    for(z_iter_ind in 1:num_z_iters){
      for(indiv in 1:num_indiv){
        # print("indiv = ")
        # print(indiv)
        ########### calculate qkt  ########################################
        ########### calculate qkt   for t = 1 ########################################
        # Biglist_list_item_intersectmats <- list()
        Biglist_intersectmats <- list()
        ######## Period 1 Intersection Matrix #########################
        indiv_ind <- (1 - 1)*num_indiv+indiv

        # list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
        # intersectmat_tmin1 <- interNtreesB_cpp(list_inter_mats)
        intersectmat_tmin1 <- getlistinter_create_intermat(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
        intersectmat_tmin1 <- cbind(intersectmat_tmin1, rep(NA, nrow(intersectmat_tmin1)))
        # calculate one dimensional integrals
        # for(rowind in 1:nrow(intersectmat_tmin1)){
        #   # ktemp <- nkt_mat[rowind,k_index]
        #   # tempmean <- intersectmat_tmin1[ktemp,1]
        #   templower <- intersectmat_tmin1[rowind,2]
        #   tempupper <- intersectmat_tmin1[rowind,3]
        #   # ASSUMING PRIOR MEAN ALL ZEROS
        #   intersectmat_tmin1[rowind, 4] <- pnorm(tempupper, mean = 0, sd = 1) - pnorm(templower, mean = 0, sd = 1)
        #   # tempintegralval <- tempintegralval*onedim_int
        # }
        # intersectmat_tmin1[, 4] <- pnorm(intersectmat_tmin1[,3], mean = 0, sd = 1) - pnorm( intersectmat_tmin1[,2], mean = 0, sd = 1)

        log_q1 <- stable_log_diff_cdf_vec(intersectmat_tmin1[, 2L], intersectmat_tmin1[, 3L])
        q1 <- normalize_log_probs(logp = log_q1,iter = iter,indiv = indiv,z_iter = z_iter_ind,t = 1L,context = "FFBS initial-region probabilities")
        intersectmat_tmin1[, 4]  <- q1 # exp(log_q1 - max(log_q1))
        Biglist_intersectmats[[1]] <- intersectmat_tmin1
        # ################ period 2 intersection matrix #################################################
        # # create vector of indices for ranker indiv in time period 2
        # indiv_ind <- (2 - 1)*num_indiv+indiv
        # list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
        # intersectmat <- interNtreesB_cpp(list_inter_mats)
        # # intersectmat <- getlistinter_create_intermat(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
        # intersectmat <- cbind(intersectmat, rep(NA, nrow(intersectmat)))
        # # calculate one dimensional integrals
        # for(rowind in 1:nrow(intersectmat)){
        #   # ktemp <- nkt_mat[rowind,k_index]
        #   # tempmean <- intersectmat[ktemp,1]
        #   templower <- intersectmat[rowind,2]
        #   tempupper <- intersectmat[rowind,3]
        #   # ASSUMING PRIOR MEAN ALL ZEROS
        #   intersectmat[rowind, 4] <- pnorm(tempupper, mean = 0, sd = 1) - pnorm(templower, mean = 0, sd = 1)
        #   # tempintegralval <- tempintegralval*onedim_int
        # }
        # Biglist_intersectmats[[2]] <- intersectmat

        ##### t = 3 to T intersect matrices ######################
        # print("Begin loop for intersection matrices = ")
        for(t in 2:n.time_train){
          #### obtain intersection matrices for time period t ###############
          # print("t = ")
          # print(t)
          indiv_ind <- (t - 1)*num_indiv+indiv
          # list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
          # intersectmat <- interNtreesB_cpp(list_inter_mats)
          intersectmat <- getlistinter_create_intermat(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
          Biglist_intersectmats[[t]] <- intersectmat
        }

        ################## qkt for t=1 ? ##############
        indiv_ind <- (1 - 1)*num_indiv+indiv
        future_index <- 1L * num_indiv + indiv
        current_index <- (1L - 1L) * num_indiv + indiv
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
        # y_tp1 <- y.train_from1[2,indiv]
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
        if (anyNA(intersectmat[, 1]) ||any(!is.finite(intersectmat[, 1]))) {
          bad_rows <- which(is.na(intersectmat[, 1]) |!is.finite(intersectmat[, 1]))
          stop(sprintf(paste0("Invalid regional mean before rescaling at ","iter=%d, z_iter=%d, indiv=%d; ","bad rows=%s; means=%s; lower=%s; upper=%s"),
                       iter,z_iter_ind,indiv,paste(bad_rows, collapse = ","),paste(intersectmat[, 1], collapse = ","),paste(intersectmat[, 2], collapse = ","),paste(intersectmat[, 3], collapse = ",")),call. = FALSE)
        }

        temp_f_for_dens <- intersectmat[1:num_regions, 1]
        tempmeanfordens <- temp_f_for_dens + offsetz_vec[current_index]

        # admissible_lower <- -Inf
        # admissible_upper <- Inf
        restricted_lower <- intersectmat[seq_len(num_regions),2L]
        restricted_upper <- intersectmat[seq_len(num_regions),3L]
        ## Intersect with the observed-outcome interval.
        tempbounds[, 1L] <- pmax(restricted_lower,temp_lower3)
        tempbounds[, 2L] <- pmin(restricted_upper,temp_upper3)
        good_regions <- which(tempbounds[, 1L] < tempbounds[, 2L])
        bad_regions <- setdiff(seq_len(num_regions),good_regions)
        logprobmattemp[, bad_regions] <- -Inf
        tempbounds[bad_regions, ] <- NA_real_
        if (length(good_regions) == 0L) {
          stop(sprintf(paste0("No admissible initial-time region at iteration %d, ","z iteration %d, individual %d; ","outcome interval=(%g,%g]."),
                       iter,z_iter_ind,indiv,temp_lower3,temp_upper3))
        }        # bad_regions <- which((intersectmat[1:num_regions, 2] >= temp_upper3) | (temp_lower3 >= intersectmat[1:num_regions, 3]))
        #
        # logprobmattemp[, bad_regions] <- -Inf #rep(-Inf,num_regions)
        # tempbounds[bad_regions, 1] <- NA
        # tempbounds[bad_regions, 2] <- NA
        #
        # good_regions <- setdiff(1:num_regions, bad_regions)
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

        temp_mean2_origscale_vec <- intersectmat_tmin1[1:num_regions_tmin1,1]  + offsetz_vec[current_index]
        # tempbounds[good_regions,1] <- pmax(intersectmat[good_regions, 2], temp_lower3)
        # tempbounds[good_regions,2] <- pmin(intersectmat[good_regions, 3], temp_upper3)
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

        if(any(tempbounds[good_regions,1] >= tempbounds[good_regions,2])){stop(" line 754 bounds badly defined")}

        logprobstemp <- as.vector(logprobmattemp)
        probstemp <- normalize_log_probs(logp = logprobstemp,iter = iter,indiv = indiv,
                                         z_iter = z_iter_ind,t = 1L,context = "initial-time joint-region draw")

        region_ind <- sample.int(num_regions_tmin1 * num_regions,
                                 size = 1L,replace = TRUE,prob = probstemp)

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
        temp_mean0 <- temp_mean0 + offsetz_vec[current_index]
        zdraw_temp <- rtruncnorm(n = 1,
                                 a = temp_lower2,
                                 b = temp_upper2,
                                 mean = temp_mean0,
                                 sd = 1)

        #### BEGIN edits for repeated sampling until lag update does not result in terminal node
        # z_old_temp <- Z.mat[1, indiv ]
        Z.mat[1, indiv ] <- zdraw_temp
        Z.vec[(1-1)*num_indiv + indiv] <- zdraw_temp
        for(t2 in 1:num_lags){
          # init_Z_t0 <- rep(0, t2*num_indiv)
          # # init_Z_t0 <- rnorm(t*n.item*n.ranker)
          # Zlag.mat[,t2] <- c(init_Z_t0, Z.vec[1:((n.time_train-t2)*num_indiv)])
          # Xmat.train[,t2 + 1L] <- Zlag.mat[, t2]
          Xmat.train[t2*num_indiv + (1-1)*num_indiv + indiv,t2 + 1L] <- Z.vec[(1-1)*num_indiv + indiv]
          Zlag.mat[t2*num_indiv + (1-1)*num_indiv + indiv ,t2] <- Z.vec[(1-1)*num_indiv + indiv]
        }

        #### END edits for repeated sampling until lag update does not result in terminal node
        # Z.mat[item_ind,  indiv ] <- zdraw_temp
        Z.mat[1,  indiv ] <- zdraw_temp
        Z.vec[indiv] <- zdraw_temp
        #### qkt Begin Loop over time t > 1 ###################
        # loop over time periods for general case 1 < t < T
        for(t in 2:(n.time_train - 1)){
          future_index <- t * num_indiv + indiv
          current_index <- (t - 1L) * num_indiv + indiv
          if (!isTRUE(all.equal(as.numeric(Xmat.train[future_index, 2L]),as.numeric(Z.vec[current_index]),tolerance = 0))) {
            stop(sprintf(paste0("Xmat.train lag and Z.vec disagree at iteration %d, time %d, individual %d."),iter,t,indiv))
          }
          if (!isTRUE(all.equal(as.numeric(Xmat.train[future_index, 2L]),as.numeric(Zlag.mat[future_index, 1L]),tolerance = 0,check.attributes = FALSE))) {
            stop(sprintf(paste0("Xmat.train and Zlag.mat disagree at iteration %d, ","time %d, individual %d: Xmat.train=%.17g, Zlag.mat=%.17g."),
                         iter,t,indiv,as.numeric(Xmat.train[future_index, 2L]),as.numeric(Zlag.mat[future_index, 1L])))
          }

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
          # y_tp1 <- y.train_from1[t,indiv]
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
          temp_tnorm_logprobvec <- fastlognormdens(temp_ztp1, mean = intersectmat[, 1] + offsetz_vec[current_index], sd = 1)

          # admissible_lower <- -Inf
          # admissible_upper <- Inf

          for(k_ind in 1:num_regions){
            # Probability of z_t in intersection of
            # region k_ind (for period t+1)
            # and region defined by period t latent variables for other individuals
            # and rank for period t
            temp_tnorm_logprob  <- temp_tnorm_logprobvec[k_ind]
            # tildeC_ktminl corresponds to
            # period t+1 k_ind region intereval
            temp_lower2 <- intersectmat[k_ind, 2]
            temp_upper2 <- intersectmat[k_ind, 3]

            if((temp_lower2 >= temp_upper3) | (temp_lower3 >= temp_upper2)){
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

            temp_mean2_origscale <- temp_mean2 + offsetz_vec[current_index]
            # prob_t_region <- pnorm(temp_upper2 - temp_mean2_origscale) - pnorm(temp_lower2 - temp_mean2_origscale)
            logprob_t_region <- stable_log_diff_cdf_all_tails(temp_lower2 - temp_mean2_origscale, temp_upper2 - temp_mean2_origscale)
            # prob_t_region <- prob_t_region*temp_tnorm_prob
            logprob_t_region <- logprob_t_region + temp_tnorm_logprob
            # prob_t_region <- temp_tnorm_prob

            # save region probability # and save region bounds (or maybe more memory efficient to obtain the region again)
            # must multiply by other previously obtained probabilities
            # temp_region_probs[k_ind, 1] <- prob_t_region
            # temp_region_probs[k_ind, 2] <- temp_lower2
            # temp_region_probs[k_ind, 3] <- temp_upper2
            temp_region_logprobs[k_ind, 1] <- logprob_t_region
            temp_region_logprobs[k_ind, 2] <- temp_lower2
            temp_region_logprobs[k_ind, 3] <- temp_upper2
          }


          if (!any(is.finite(temp_region_logprobs[, 1L]))) {
            stop(sprintf(paste0("No region overlaps the outcome and admissibility constraints ","at iteration %d, z iteration %d, time %d, individual %d; ","outcome interval=(%g,%g]."),
                         iter,z_iter_ind,t,indiv,temp_lower3,temp_upper3))
          }

          # sample a region using probabilities obtained above
          logprobstemp <- temp_region_logprobs[, 1]
          probstemp <- normalize_log_probs(logp = logprobstemp,iter = iter,indiv = indiv,z_iter = z_iter_ind,t = t,context = "interior-time region draw")
          temp_mean2_origscale <- temp_mean2 + offsetz_vec[current_index]
          region_ind <- sample.int(num_regions, size = 1L, replace = TRUE, prob = probstemp )

          zdraw_temp <- rtruncnorm(n = 1, a=temp_region_logprobs[region_ind, 2], b=temp_region_logprobs[region_ind, 3], mean = temp_mean2_origscale , sd = 1)

          #### BEGIN edits for repeated sampling until lag update does not result in terminal node
          Z.mat[t, indiv ] <- zdraw_temp
          Z.vec[(t-1)*num_indiv + indiv] <- zdraw_temp
          for(t2 in 1:num_lags){
            # init_Z_t0 <- rep(0, t2*num_indiv)
            # # init_Z_t0 <- rnorm(t*n.item*n.ranker)
            # Zlag.mat[,t2] <- c(init_Z_t0, Z.vec[1:((n.time_train-t2)*num_indiv)])
            # Xmat.train[,t2 + 1L] <- Zlag.mat[, t2]
            Xmat.train[t2*num_indiv + (t-1)*num_indiv + indiv,t2 + 1L] <- Z.vec[(t-1)*num_indiv + indiv]
            Zlag.mat[t2*num_indiv + (t-1)*num_indiv + indiv ,t2] <- Z.vec[(t-1)*num_indiv + indiv]
          }
          # RESAMPLING SHOULD NOT BE REQUIRED BECAUSE CAN HAVE EMPTY LEAVES

          #### END edits for repeated sampling until lag update does not result in terminal node
        } # end loop over time periods

        # check for special cases for n.time - 1, n.time - 2, n.time - 3
        # special case for t = n.time
        #### qkt for T ###################
        current_index <- (n.time_train - 1L) * num_indiv + indiv
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
        temp_mean2_origscale <- temp_mean2 + offsetz_vec[current_index]
        # CHECK IF THESE BOUNDS ARE CORRECTLY DEFINED
        zdraw_temp <- rtruncnorm(n = 1,
                                 a = temp_lower3,
                                 b = temp_upper3,
                                 mean = temp_mean2_origscale ,
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

    stopifnot(isTRUE(all.equal(Z.vec,as.vector(t(Z.mat)),tolerance = 0)))



    category_lower <- c(-Inf,thresholds)
    category_upper <- c(thresholds,Inf)
    latent_lower <- category_lower[y_from1.vec]
    latent_upper <- category_upper[y_from1.vec]
    bad_latent <- which(!(Z.vec > latent_lower &Z.vec <= latent_upper))
    if (length(bad_latent) > 0L) {
      stop(sprintf(paste0("Latent values violate ordered-probit category bounds ","at iteration %d; indices=%s; Z=%s; lower=%s; upper=%s."),
        iter,paste(bad_latent, collapse = ","),paste(Z.vec[bad_latent], collapse = ","),paste(latent_lower[bad_latent], collapse = ","),paste(latent_upper[bad_latent], collapse = ",")))
    }



    ##################### Sample sum-of-trees ##################################################
    #set the response.
    #Check that 0 is a reasonable initial value
    #perhaps makes more sense to use initial values of Z
    if (anyNA(Z.vec) || any(!is.finite(Z.vec))) {
      stop(sprintf("Non-finite latent value before tree update at Gibbs iteration %d",iter))
    }
    if (!isTRUE(all.equal(Z.vec, as.vector(t(Z.mat))))) {
      stop(sprintf("Z.vec and Z.mat disagree before tree update at iteration %d",iter))
    }

    # update training data matrix
    Xmat.train <- data.frame(y = Z.vec, # as.vector(Z.mat),
                             x = cbind(Zlag.mat, X.train) )
    Xmat.train.no.y <- cbind(Zlag.mat, X.train)
    if(nrow(X.test)>0 ){
      Xmat.test  <- data.frame(x = cbind( Zlag.mat.test, X.test ) )
      # Xmat.test.no.y <- cbind( Zlag.mat.test, X.test )
    }
    # Must update node sizes for original tree # update all node sizes before new proposal
    for (j in 1:n.trees) {
      # print("curr_trees[[j]] = ")
      # print(curr_trees[[j]] )
      curr_trees[[j]] <- fill_tree_details(curr_trees[[j]], Xmat.train.no.y)
      # must also update individual tree predictions for calcualtion of partial residuals
      tree_fits_store[,j]  = get_predictions(curr_trees[[j]], Xmat.train.no.y, single_tree = TRUE)
    }
    # option: prune all empty nodes from current trees before proposal? # must create new function for this
    # perhaps an issue is that there would not be a (umique) mu value # associated with a parent of an empty node
    # must update mu before and after trees updated because covariates have been updated
    mutemp = get_predictions(curr_trees, Xmat.train.no.y, single_tree = n.trees == 1)
    mu <- mutemp + offsetz_vec

    # Start looping through trees
    for (j in 1:n.trees) {
      current_partial_residuals = as.vector(Z.mat) - mu + tree_fits_store[,j]  # rep(offsetz_vec, n.time_train)
      # Propose a new tree via grow/change/prune/swap
      # type = sample_move(curr_trees[[j]], iter, 0, # nburn # no burnin number, so setting to 100, maybe this is not optimal
      #                    trans_prob)
      if (nrow(curr_trees[[j]]$tree_matrix) == 1L) {
        type <- "grow"
      }else{
        type <- sample( c("grow", "prune", "change"), size = 1L, prob = trans_prob )
      }

      # option: edit the proposal step so that any proposed tree does not contain empty nodes?
      # must create new function for this
      # Generate a new tree based on the current
      if(prune_existing_empty_leaves  == TRUE){
        new_trees[[j]] = noempty_update_tree(y = as.vector(Z.mat),
                                             X = Xmat.train.no.y,
                                             type = type,
                                             curr_tree = curr_trees[[j]],
                                             node_min_size = node_min_size,
                                             s = s,
                                             splitting_rules = splitting_rules,
                                             max_bad_trees = max_bad_trees)

      }else{
        new_trees[[j]] = update_tree(y = as.vector(Z.mat),
                                     X = Xmat.train.no.y,
                                     type = type,
                                     curr_tree = curr_trees[[j]],
                                     node_min_size = node_min_size,
                                     s = s,
                                     splitting_rules = splitting_rules,
                                     max_bad_trees = max_bad_trees)

      }

      if (new_trees[[j]]$proposal_succeeded) {

        # CURRENT TREE: compute the log of the marginalised likelihood + log of the tree prior
        l_old = tree_full_conditional(curr_trees[[j]],
                                      current_partial_residuals,
                                      sigma2,
                                      sigma2_mu) +
          get_tree_prior(curr_trees[[j]], alpha, beta)

        # NEW TREE: compute the log of the marginalised likelihood + log of the tree prior
        l_new = tree_full_conditional(new_trees[[j]],
                                      current_partial_residuals,
                                      sigma2,
                                      sigma2_mu) +
          get_tree_prior(new_trees[[j]], alpha, beta)

        # Exponentiate and multiply by the transition probabilities
        # if(type == 'grow'){
        #   # a = exp(l_new - l_old)*ratio_grow(curr_trees[[j]], new_trees[[j]] )
        #   log_proposal_ratio = log(ratio_grow(curr_trees[[j]], new_trees[[j]] ))
        # } else if(type == 'prune'){
        #   # a = exp(l_new - l_old)*ratio_prune(curr_trees[[j]], new_trees[[j]] )
        #   log_proposal_ratio = log(ratio_prune(curr_trees[[j]], new_trees[[j]] ))
        # } else{
        #   # a = exp(l_new - l_old)
        #   log_proposal_ratio = 0
        # }

        proposal_ratio <- if (type == "grow") {
          ratio_grow(curr_trees[[j]],new_trees[[j]])
        } else if (type == "prune") {
          ratio_prune(curr_trees[[j]],new_trees[[j]])
        } else {
          1
        }
        if (length(proposal_ratio) != 1L ||is.na(proposal_ratio) ||proposal_ratio < 0) {
          stop(sprintf(paste0("Invalid proposal ratio for tree %d, move %s: %s."),j,type,paste(proposal_ratio, collapse = ",")))
        }
        log_proposal_ratio <- if (proposal_ratio == 0) {
          -Inf
        }else{
          log(proposal_ratio)
        }
        if (!is.finite(l_old) ||!is.finite(l_new)) {
          stop(sprintf(paste0("Non-finite tree log target at iteration %d, tree %d: ","old=%g, new=%g."),
                       iter,j,l_old,l_new))
        }
        vars_empty_pruned <- c()
        log_a <- l_new - l_old + log_proposal_ratio
        if (!is.finite(log_a) && log_a != -Inf) {
          stop("Non-finite Metropolis-Hastings log ratio.")
        }
        accepted <- log(runif(1L)) < min(0, log_a)
      }else{
        accepted <- FALSE
      }

      # if(a > runif(1)) {

      var_count_before <- var_count
      if(accepted) {
        old_tree <- curr_trees[[j]]
        curr_trees[[j]] = new_trees[[j]]
        # only account for pruning if  accept
        var_count <- count_tree_splits(curr_trees,p)
        var_count_manual <- var_count_before

        vars_empty_pruned <- new_trees[[j]]$vars_empty_pruned
        for(var_ind in vars_empty_pruned){
          var_count_manual[var_ind ] = var_count_manual[var_ind ] - 1
        }
        if (type =='change'){
          var_count_manual[curr_trees[[j]]$var[1] ] = var_count_manual[curr_trees[[j]]$var[1] ] - 1
          var_count_manual[curr_trees[[j]]$var[2] ] = var_count_manual[curr_trees[[j]]$var[2] ] + 1
        }
        if (type=='grow'){
          var_count_manual[curr_trees[[j]]$var ] = var_count_manual[curr_trees[[j]]$var ] + 1
        } # -1 because of the intercept in X
        if (type=='prune'){
          var_count_manual[curr_trees[[j]]$var ] = var_count_manual[curr_trees[[j]]$var ] - 1
        } # -1 because of the intercept in X


        if (!identical(as.integer(var_count_manual),as.integer(var_count))) {
          cat("\nVariable-count mismatch","\niteration:", iter,"\ntree:", j,"\nmove:", type,"\nmove variables:",paste(curr_trees[[j]]$var, collapse = ","),
              "\nempty-pruned variables:",paste(vars_empty_pruned, collapse = ","),"\ncount before:",paste(var_count_before, collapse = ","),
              "\nmanual count:",paste(var_count_manual, collapse = ","),"\nexact count:",paste(var_count, collapse = ","),"\n")
          cat("\nOld internal split variables:\n")
          print(old_tree$tree_matrix[old_tree$tree_matrix[, "terminal"] == 0L,"split_variable"])
          cat("\nNew internal split variables:\n")
          print(curr_trees[[j]]$tree_matrix[curr_trees[[j]]$tree_matrix[, "terminal"] == 0L,"split_variable"])
          stop("Manual variable-count update disagrees with the trees.")
        }
        #
        # var_count2 <- count_tree_splits(curr_trees,p)
        # if(!all(var_count2 == var_count)){
        #   stop("Line 1093. !all(var_count2 == var_count)")
        # }

      }
      ### might need to edit simulate_mu to find right terminal nodes #########
      # Update mu whether tree accepted or not
      curr_trees[[j]] = simulate_mu(curr_trees[[j]],
                                    current_partial_residuals,
                                    sigma2,
                                    sigma2_mu)
      # Updating BART predictions
      current_fit = get_predictions(curr_trees[[j]], Xmat.train.no.y, single_tree = TRUE)
      mu = mu - tree_fits_store[,j] # subtract the old fit
      mu = mu + current_fit # add the new fit
      tree_fits_store[,j] = current_fit # update the new fit
    } # End loop through trees

    Xmat.train.no.y <- as.matrix(Xmat.train.no.y) # to make other lines more efficient
    if (sparse & iter > max(2L, floor(0.25 * n.burnin))) {
      s_update <- update_s(var_count, p, alpha_s)
      s <- s_update[[1]]
      if(alpha_prior){
        alpha_s <- update_alpha(s, alpha_scale, alpha_a, alpha_b, p, s_update[[2]])
      }
    }
    if(sigma_mu_prior){
      sigma2_mu <-  update_sigma_mu_par(curr_trees, sigma2_mu)
    }

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
        testpredvec = get_predictions(curr_trees, temp_test_mat, single_tree = n.trees == 1)

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



