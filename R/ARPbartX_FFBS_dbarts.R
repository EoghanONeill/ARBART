
#' Auto-regressive Probit BART Model with exogenous covariates
#'
#' Auto-regressive (of order one) Probit BART Model with exogenous covariates
#' @import truncnorm
#' @import mvtnorm
#' @import dbarts
#' @importFrom Rcpp evalCpp
#' @param y.train A vector (or matrix for panel data) of binary outcomes. Rows correspond to time periods, columns correspond to variables.
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
ARPbartX_FFBS_dbarts <- function(y.train = NULL,
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
                              # printEvery = 100L,
                              printCutoffs = 0L,
                              rngKind = "default",
                              rngNormalKind = "default",
                              rngSeed = NA_integer_,
                              updateState = TRUE,
                              print.opt = 100L, #printEvery,
                              seq_z_draws = 1,
                              probit_k = 2.0,
                              reference_half_range = 3.0, max_lag_update_attempts = 10000L,
                              reject_or_restrict_Z_props = "reject"#, max_z_resamples = 10
){

  if(!(reject_or_restrict_Z_props %in% c("reject", "restrict"))){
    print("reject_or_restrict_Z_props = ")
    print(reject_or_restrict_Z_props)
    stop("reject_or_restrict_Z_props must be equal to reject or restrict")
  }

  if(reject_or_restrict_Z_props == "restrict"){
    stop("Option restrict not currently supported, because restriction of backward samples
         requires recalculation of filter probabilities for each time-specific backward sample.")
  }

  ######### Check inputs ##################
  if(num_test_periods < 1){stop("num_test_periods must be >=1")}
  if(!is.integer(num_lags)){stop("num_lags must be an integer.")}
  if(num_lags <1){stop("num_lags must be an integer greater than or equal to 1.")}
  if(num_lags >1){stop("Code not yet written for num_lags > 1.")}
  if (length(num_z_iters) != 1L || is.na(num_z_iters) || num_z_iters != as.integer(num_z_iters) || num_z_iters < 1L){stop("num_z_iters must be an integer greater than or equal to 1. It is the number of repeated samples of latent variable values per Gibbs iteration.")}
  #check that input values are binary 0 or 1.
  if(!(all(y.train %in% c(0,1)))){stop("Input y.train values must be 0 or 1.")}
  if(length(unique(y.train))!= 2){stop("y.train must contain at least one 0 and at least one 1. Input y.train containing all zeros or all ones is not supported.")}
  # check input covariates
  X.train <- as.matrix(X.train)
  if (is.null(X.test)){
    print("X.test is NULL, creating matrix of zeros ")
    X.test <- matrix(numeric(0), nrow = ncol(y.train) * num_test_periods, ncol = 0L)
  }
  X.test <- as.matrix(X.test)
  y.train <- as.matrix(y.train)
  y.vec <- as.vector(t(y.train))
  if (nrow(y.train) < 3L) stop("ARPbartX_fullcond currently requires at least three training periods.", call. = FALSE)

  if(nrow(X.train) != ncol(y.train)*nrow(y.train)){
    print("nrow(X.train) = ")
    print(nrow(X.train))
    print("nrow(y.train) = ")
    print(nrow(y.train))
    print("ncol(y.train) = ")
    print(ncol(y.train))
    stop("nrow(X.train) != ncol(y.train)*nrow(y.train)")
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
  if(indiv.offsets){
    for(col_ind in 1:ncol(y.train)){
      if(length(unique(y.train[,col_ind]))!= 2){
        stop("If indiv.offsets == TRUE, then for each column y.train[,col_ind] must contain at least one 0 and at least one 1. Input y.train[,col_ind] containing all zeros or all ones is not supported.")
      }
    }
  }

  num_indiv <- ncol(y.train)
  n.time_train <- nrow(y.train)
  num_total <- n.time_train*num_indiv

  ######## create output list to be filled in #########################
  draw = list(
    mu = array(NA, dim = c(num_indiv, n.time_train, n.iter)),#,
    mu_test = array(NA, dim = c(num_indiv , num_test_periods , n.iter)),
    prob.train.draws = array(NA, dim = c(num_indiv, n.time_train, n.iter)),#,
    prob.test.draws = array(NA, dim = c(num_indiv , num_test_periods , n.iter))
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

  # create offset
  num_ones <- sum(y.train)
  prop_ones <- mean(y.train)

  offsetz <- 0 # qnorm(prop_ones)

  # alternative: individual-specific offsets
  if((indiv.offsets == TRUE)){
    stop("The code currently does not support indiv.offsets == TRUE")
    if(num_indiv > 1){
      num_ones_vec <- apply(y.train,2,sum)
      prop_ones_vec <- apply(y.train,2,mean)
      offsetz_vec <- qnorm(prop_ones_vec) # rep(0,num_indiv) #
      genuine_offset_vec <- rep(offsetz_vec, n.time_train)
      genuine_offset_test <- rep(offsetz_vec, num_test_periods)
    }else{
      # genuine_offset_vec <- rep(offsetz, num_indiv)
      offsetz_vec <- c(offsetz)
      genuine_offset_vec <- rep(offsetz, num_indiv*n.time_train)
      genuine_offset_test <- rep(offsetz, num_indiv*num_test_periods)
      num_ones_vec <- as.vector(sum(y.train))
      prop_ones_vec <- as.vector(mean(y.train))
    }
  }else{
    offsetz_vec <- rep(offsetz, num_indiv)
    genuine_offset_vec <- rep(offsetz, num_indiv*n.time_train)
    genuine_offset_test <- rep(offsetz, num_indiv*num_test_periods)
    num_ones_vec <- as.vector(sum(y.train))
    prop_ones_vec <- as.vector(mean(y.train))
  }
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
    if((indiv.offsets == FALSE)|(num_indiv == 1)){
      Z.mat[which(y.train[,j]==1),j] <- reference_half_range # 0.5 # rtruncnorm(sum(y.train[,j]), a= 0, b = Inf, mean = offsetz, sd = 1)
      Z.mat[which(y.train[,j]==0),j] <- -reference_half_range # -0.5 # rtruncnorm(n.time_train - sum(y.train[,j]), a= -Inf, b = 0, mean = offsetz, sd = 1)
    }else{
      Z.mat[which(y.train[,j]==1),j] <- reference_half_range # 0.5 # rtruncnorm(num_ones_vec[j], a= 0, b = Inf, mean = offsetz_vec[j], sd = 1)
      Z.mat[which(y.train[,j]==0),j] <- -reference_half_range # -0.5 # rtruncnorm(n.time_train - num_ones_vec[j], a= -Inf , b = 0, mean = offsetz_vec[j], sd = 1)
    }
  } # end j loop

  # non-vectorized code
  # for(i in 1:(n.time_train)){
  #
  #   if(y.train[i,j]==1){
  #     #perhaps should set different initial mean here
  #     # i.e. first intialize the trees? or check how probit bart intializes the values
  #     # instead there is a binary offset
  #     if(indiv.offsets == FALSE){
  #       Z.mat[i,j] <- rtruncnorm(1, a= 0, b = Inf, mean = offsetz, sd = 1)
  #     }else{
  #       Z.mat[i,j] <- rtruncnorm(1, a= 0, b = Inf, mean = offsetz_vec[j], sd = 1)
  #     }
  #
  #   }else{
  #     if(indiv.offsets == FALSE){
  #       Z.mat[i,j] <- rtruncnorm(1, a= -Inf, b = 0, mean = offsetz, sd = 1)
  #     }else{
  #       Z.mat[i,j] <- rtruncnorm(1, a= -Inf, b = 0, mean = offsetz_vec[j], sd = 1)
  #     }
  #   }
  # } # end i loop


  Z.mat.test =  matrix(rnorm(num_test_periods*num_indiv), nrow = num_test_periods, ncol = num_indiv)
  # if num_indiv > 1, must vectorize for input to dbarts
  # the vectorized matrix might be more convenient to work with if each time period is a continguous block?
  Z.vec <- as.vector(t(Z.mat))
  Z.vec.test <- as.vector(t(Z.mat.test))
  z_init_half_range <- 3.0 # 0.5
  Z_initial <- ifelse(Z.vec > 0, z_init_half_range, - 1*z_init_half_range)
  Z_reference <- Z.vec # Z_initial # as.numeric(Z.vec)
  scale_min_fixed <- min(Z_reference)
  scale_max_fixed <- max(Z_reference)
  scale_range_fixed <- scale_max_fixed - scale_min_fixed
  computational_offset <- Z_reference - Z_initial
  dbarts_offset <- computational_offset + genuine_offset_vec

  min_resp <- scale_min_fixed # min(sampler$data@y)
  max_resp <- scale_max_fixed # max(sampler$data@y)
  response_range <- scale_range_fixed # max_resp - min_resp

  if (!isTRUE(all.equal(scale_min_fixed, -reference_half_range)) ||!isTRUE(all.equal(scale_max_fixed,  reference_half_range)) ||!isTRUE(all.equal(scale_range_fixed, 2*reference_half_range))) {
    stop(paste0("Initial reference response must have fixed range ","[-reference_half_range, reference_half_range]."),call. = FALSE)
  }
  Z.mat <- matrix( Z_initial,nrow = n.time_train, ncol = num_indiv, byrow = TRUE)
  Z.vec <- as.vector(t(Z.mat))

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
  # Zlag.1.mat <- c(init_Z_t0, as.vector(t(Z.mat))[1:((n.time-1)*n.item*n.ranker)])

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
      # Zlag.mat.test[,t] <- c(as.vector(t(Z.mat))[((n.time-t)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],
      #                        rep(0,(num_test_periods-t)*n.item*n.ranker ))
    }else{
      #nothing to fill in if num_test_periods <= t
      Zlag.mat.test[,t] <- c(Z_initial[((n.time_train-t)*num_indiv+1):((n.time_train-(t - num_test_periods) )*num_indiv)]  )
    }
    # }
  }
  # }

  Xmat.train <- data.frame(y = Z_reference,  x = cbind(Zlag.mat, X.train) )
  # print(colnames(Xmat.train))

  if(nrow(X.test)>0 ){
    Xmat.test  <- data.frame(x = cbind( Zlag.mat.test, X.test ) )
    #must create predictions for future time periods using covariates for one period ahead, two periods ahead etc.
  }

  # df_for_dbart <- data.frame(y = as.vector(Z.vec), x = as.matrix(Zlag.mat) )
  control <- dbartsControl(updateState = updateState, verbose = FALSE,  keepTrainingFits = TRUE,
                           keepTrees = TRUE,
                           n.trees = n.trees,
                           n.burn = n.burn,
                           n.samples = 1L, # n.samples,
                           n.thin = 1L, # n.thin,
                           n.chains = 1L, # n.chains,
                           n.threads = 1L, # n.threads,
                           printEvery = 1L, # verbose = FALSE,
                           printCutoffs = printCutoffs,
                           rngKind = rngKind,
                           rngNormalKind = rngNormalKind,
                           rngSeed = rngSeed)


  # print(colnames(Xmat.train))
  # print(colnames(Xmat.test))

  # print("begin dbarts")

  # k_sig_mu <- 2
  # if(edited_scale){
  #   k_sig_mu <- sqrt(2)/6
  # }

  # k_sig_mu <- (max(Z_reference) - min(Z_reference))/3.0 # 1/3 # 2 # 3
  k_sig_mu <- reference_half_range*probit_k/3.0

  # if(nrow(X.test )==0){

  if(nrow(X.test )==0){

    sampler <- dbarts(y ~ .,
                      data = Xmat.train,
                      #test = Xmat.test,
                      control = control,
                      # offset = dbarts_offset, # offset_current,
                      # offset.test = rep(
                      #   0,
                      #   nrow(Xmat.test)
                      # ),
                      resid.prior = fixed(1), node.prior = normal(k_sig_mu),
                      sigma=1 #check if this is the correct approach for setting the variance to 1
    )
  }else{
    sampler <- dbarts(y ~ .,
                      data = Xmat.train,
                      test = Xmat.test,
                      control = control,
                      # offset = dbarts_offset, # offset_current,
                      # offset.test = genuine_offset_test,
                      #   rep(
                      #   0,
                      #   nrow(Xmat.test)
                      # ),
                      resid.prior = fixed(1), node.prior = normal(k_sig_mu),
                      sigma=1 #check if this is the correct approach for setting the variance to 1
    )
  }

  # print("Line 321")
  #set the response.
  #Check that 0 is a reasonable initial value
  #perhaps makes more sense to use initial values of Z
  # sampler$setResponse(y = as.vector(Z.vec))

  computational_offset <- Z_reference - Z_initial
  dbarts_offset <- computational_offset + genuine_offset_vec

  sampler$setOffset(offset = dbarts_offset, updateScale = FALSE )
  sampler$setTestOffset(offset = genuine_offset_test#,updateScale = FALSE
  )
  # sampler$setSigma(sigma = 1)
  # min_resp <- min(Z.vec)
  # max_resp <- max(Z.vec)
  if (!isTRUE(all.equal(as.numeric(sampler$data@y),as.numeric(Z_reference),tolerance = 0,check.attributes = FALSE))) {
    stop(sprintf("sampler$data@y changed from the fixed refererce response.",0),call. = FALSE)
  }

  bart_target <- Z.vec - genuine_offset_vec
  reconstructed_target <- Z_reference - dbarts_offset

  if (!isTRUE(all.equal(reconstructed_target,bart_target,tolerance =100 *.Machine$double.eps,check.attributes =FALSE))) {
    stop(paste0("The dbarts offset does not reproduce ","the intended BART partial response."),call. = FALSE)
  }

  # min_resp <- min(sampler$data@y)
  # max_resp <- max(sampler$data@y)
  # response_range <- max_resp - min_resp
  #
  # # scale offsets
  # # scale zero
  #
  # scalezero <- (0 - min_resp)/(max_resp - min_resp) - 0.5
  # scaled_offsetz_vec <- (offsetz_vec - min_resp)/(max_resp - min_resp) - 0.5

  #sampler$setPredictor(x= Xmat.train, column = 1, forceUpdate = TRUE)

  #mu = as.vector( alpha + X.mat %*% beta )
  # sampler$sampleTreesFromPrior()
  samplestemp <- sampler$run()
  trees_after_run <- sampler$getTrees(chainNums = 1L,# sampleNums = 1L,
                                      current = TRUE)
  leaf_after_run <- trees_after_run$var == -1L

  if (any(trees_after_run$n[leaf_after_run] == 0L)) {
    stop(sprintf(paste0("Empty terminal node remains after dbarts run() ","at Gibbs iteration %d."),0),call. = FALSE)
  }
  if (any(!is.finite(trees_after_run$value[leaf_after_run]))) {
    stop(sprintf(paste0("Non-finite terminal-node parameter after dbarts run() ","at Gibbs iteration %d."),0),call. = FALSE)
  }
  if (anyNA(samplestemp$train) ||any(!is.finite(samplestemp$train))) {
    stop(sprintf("Non-finite dbarts fitted value at iteration %d.",0),call. = FALSE)
  }
  # mutemp <- samplestemp$train[,1]
  #suppose there are a number of samples
  # print("sigma = ")
  # print(samplestemp$sigma)

  # mupreds <- sampler$predict(Xmat.train)
  f_train <- as.numeric(samplestemp$train[, 1L]) - dbarts_offset
  mutemp <- genuine_offset_vec + f_train
  mu <- mutemp
  Xmat.train <- as.matrix(Xmat.train)

  # if(n.burnin == 0){
  #   if(keep_zmat==TRUE){
  #     draw$Z.mat[,,1] <- Z.mat
  #     draw$Z.mat.test[,,1] <- Z.mat.test
  #   }
  #   # draw$alpha[,1] = alpha
  #   # draw$beta[,1] = beta
  #   draw$mu[, , 1] <- matrix(mu, nrow = num_indiv, ncol = n.time_train)
  #   draw$prob.train.draws[, , 1] <- pnorm(draw$mu[, , 1])
  #   # draw$sigma2.alpha[1] = sigma2.alpha
  #   # draw$sigma2.beta[1] = sigma2.beta
  # }


  # should really instead marginalize out z_T for predictive distribution sample of Z_{T+1} as outlined in paper.
  temp_test_preds <- matrix(NA, nrow = num_indiv,ncol = num_test_periods)
  # temp_mu_test <- rep(NA,  nrow(df_for_dbart_test) )
  temp_mu_test <- rep(NA,  nrow(Xmat.test) )

  # print("colnames(temp_test_mat) = ")
  # print("Line 376")
  # print(colnames(temp_test_mat))

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
        treeexample1 <- sampler$getTrees(treeNums = i, chainNums = 1, sampleNums = 1#,
                                         # current = TRUE
        )
        if (anyNA(treeexample1$value) ||any(!is.finite(treeexample1$value))) {
          stop(sprintf(paste0("Non-finite value in dbarts tree at ","iter=%d, tree=%d; ","rows=%s; var=%s; n=%s; value=%s"),
                       0,i,paste(which(is.na(treeexample1$value) |!is.finite(treeexample1$value)),collapse = ","),
                       paste(treeexample1$var, collapse = ","),paste(treeexample1$n, collapse = ","),paste(treeexample1$value, collapse = ",")),call. = FALSE
          )
        }

        stored_tree <- sampler$getTrees(treeNums = i,chainNums = 1L,sampleNums = 1L)
        current_tree <- sampler$getTrees(treeNums = i,chainNums = 1L,current = TRUE)
        if (nrow(stored_tree) != nrow(current_tree) ||!identical(stored_tree$var, current_tree$var) ||!isTRUE(all.equal(
          stored_tree$value,current_tree$value,tolerance = 0,check.attributes = FALSE))) {
          stop(sprintf(paste0("Stored and current tree structures disagree for ","tree %d at iteration %d."),
                       i,iter))
        }

        rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
        #must use covariates for individual indiv at time period t
        # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, Xmat.train[obs_indices[1],-1, drop = FALSE] )
        # if num_lags > 1 this would have to be edited (just first column dropped)
        # list_inter_mats[[i]] <- getPredictionsRangesForTree3_cpp((rebuilt_tree),
        #                                                          as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
        list_inter_mats[[i]] <- getPredictionsRangesForTreeMCP_cpp( # getPredictionsRangesForTree3_cpp(
          as.matrix(rebuilt_tree),
          as.matrix(temp_test_mat[indiv, , drop = FALSE]))
      }

      intersectmat <- interNtreesB_cpp(list_inter_mats)
      # Biglist_intersectmats[[t]] <- intersectmat
      # obtain prediction from intersection matrix
      ktemp <- which((as.matrix(temp_test_mat[indiv,1])[1]  <= intersectmat[, 3]) )[1]
      # Then obtain the corresponding region mean value
      testpredvec[indiv] <- intersectmat[ktemp,1]
    }
    testpredvec <- (testpredvec + 0.5)*(max_resp - min_resp) + min_resp
    if(noise_in_pred==1){
      temp_test_preds[ , t1] <- testpredvec + rnorm( num_indiv ) + offsetz_vec
    }else{
      temp_test_preds[ , t1] <- testpredvec + offsetz_vec
    }
    # print("Line 963")
    #update temp_test_mat
    #shift z columns to the right and fill in leftmost column
    #need to rewrite this if want to allow for no observed covariates
    if (t1 < num_test_periods) {
      if(num_lags ==1){
        # temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] ,
        #                                           as.matrix(df_for_dbart_test[(t1-1)*(num_indiv)  +  1:(num_indiv), c(-1), drop=FALSE ] )))
        temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] ,
                                                  as.matrix(Xmat.test[(t1)*(num_indiv)  +  1:(num_indiv) ,(num_lags+1):(ncol(Xmat.test)), drop=FALSE ] )))
      }else{
        print("num lags not equal to 1")
        # temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] , temp_test_mat[,1:(num_lags-1)] ,
        #                                           as.matrix(df_for_dbart_test[(t1-1)*(num_indiv)  +  1:(num_indiv),c(-1), drop=FALSE ] )))
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
    #fill in temp_mu_test without noise
    temp_mu_test[(t1-1)*(num_indiv)  +  1:(num_indiv) ] <- testpredvec + offsetz_vec
  }
  # print("Line 427")
  # can re-draw Z.mat and Z.mat.test here
  Z.mat.test <-  t(temp_test_preds)
  # can re-draw Z.mat and Z.mat.test here
  Zlag.mat.test <- matrix(NA, nrow = num_test_periods*num_indiv, ncol = num_lags)
  # if(nrow(X.test) >0 ){
  for(t in 1:num_lags){
    if(num_test_periods > t ){
      #up to t time periods ahead can be filled in. Rest of test time periods filled in using repeated values of last observation in training data
      # # Zlag.mat.test[,t] <- c(as.vector(t(Z.mat))[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
      # #                        as.vector(as.matrix(temp_test_mat))[1:((num_test_periods - t)*(num_indiv))  ] )
      #
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
  #   draw$mu_test[,,1] <- matrix(temp_mu_test, nrow = num_indiv, ncol = num_test_periods) #temp_mu_test
  #   draw$prob.test.draws[,,1] <- pnorm(draw$mu_test[,,1])
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
  breakcount <- 0
  # for(iter in 2:(n.burnin + n.iter)){
  while( iter <= (n.burnin + n.iter) ){
    # print("line 842")
    # print("iter = ")
    # print(iter)
    if(iter > n.burnin){
      iter_min_burnin <- iter - n.burnin
    }
    # print("Line 454")

    Z.matold <- Z.mat
    # Z_mat_old <- Z.mat
    Z_vec_old <- Z.vec
    Zlag_old <- Zlag.mat
    Xmat_train_old <- Xmat.train

    ########### Sample from full conditional distribution.
    #################### calculate intersection matrices ########################################

    # if there are no covariates, the intersection matrices do not vary across time or individuals
    #### obtain intersection matrices for time period t = 1 ###############
    # create intersection matrices
    intersectmat_tmin1 <- NA
    list_item_intersectmats_tmin1 <- NA
    rebuilt_tree_list <- list()
    for(i in 1:n.trees){
      treeexample1 <- sampler$getTrees(treeNums = i,chainNums = 1,sampleNums = 1#,
                                       # current = TRUE
      )

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
      stored_tree <- sampler$getTrees(treeNums = i,chainNums = 1L,sampleNums = 1L)
      current_tree <- sampler$getTrees(treeNums = i,chainNums = 1L,current = TRUE)
      if (nrow(stored_tree) != nrow(current_tree) ||!identical(stored_tree$var, current_tree$var) ||!isTRUE(all.equal(
        stored_tree$value,current_tree$value,tolerance = 0,check.attributes = FALSE))) {
        stop(sprintf(paste0("Stored and current tree structures disagree for ","tree %d at iteration %d."),
                     i,iter))
      }

      rebuilt_tree_list[[i]] <- rebuildTree2_cpp(as.matrix(treeexample1))
    }
    # print("Line 488")
    list_inter_mats <- list()
    # for(i in 1:n.trees){
    #   # print("Line 492")
    #   # treeexample1 <- sampler$getTrees(treeNums = i,
    #   #                                  chainNums = 1,
    #   #                                  sampleNums = 1)
    #   # # print("Line 496")
    #   # # print("treeexample1 = ")
    #   # # print(treeexample1)
    #   # # rebuilt_tree <- rebuildTree2(treeexample1)
    #   # rebuilt_tree <- ARBART:::rebuildTree2_cpp(as.matrix(treeexample1))
    #   rebuilt_tree <- rebuilt_tree_list[[i]]
    #   # print("Line 499")
    #   #must use covariates for individual indiv at time period t
    #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, as.matrix(df_for_dbart$x[obs_indices[1]]) )
    #   # list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree$var == -1 , 5:7]
    #   list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree[,4] == -1 , 5:7, drop = FALSE]
    #   # print("Line 507")
    # }
    # # print("Line 510")
    # intersectmat <- interNtreesB_cpp(list_inter_mats)
    # # print("Line 1146")
    # intersectmat <- cbind(intersectmat, rep(NA, nrow(intersectmat)))
    #
    # # print("Line 518")
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
    #
    # intersectmat_tmin1 <- intersectmat

    # num_regions <- nrow(intersectmat)

    # print("Line 937")
    # for(indiv in 1:num_indiv){
    #
    #
    # }
    for(z_iter_ind in 1:num_z_iters){
      for(indiv in 1:num_indiv){
        # print("indiv = ")
        # print(indiv)
        # Z_indiv_old <- Z.mat[, indiv]
        # Z_vec_old <- Z.vec
        # Zlag_old <- Zlag.mat
        # Z_candidate <- Z.vec
        # Zmat_candidate <- Z.mat
        ########### calculate qkt  ########################################
        ########### calculate qkt   for t = 1 ########################################
        qkt_list <- list()
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
        #   # templower <- intersectmat_tmin1[rowind,2]
        #   # tempupper <- intersectmat_tmin1[rowind,3]
        #   # # ASSUMING PRIOR MEAN ALL ZEROS
        #   # intersectmat_tmin1[rowind, 4] <- pnorm(tempupper, mean = 0, sd = 1) - pnorm(templower, mean = 0, sd = 1)
        #   log_q1 <- stable_log_diff_cdf_all_tails(intersectmat_tmin1[, 2L], intersectmat_tmin1[, 3L])
        #   q1 <- exp(log_q1 - max(log_q1))
        #   q1 <- q1 / sum(q1)
        #   intersectmat_tmin1[, 4L] <- q1
        #   # tempintegralval <- tempintegralval*onedim_int
        # }
        # log_q1 <- stable_log_diff_cdf_all_tails(intersectmat_tmin1[, 2L], intersectmat_tmin1[, 3L])
        log_q1 <- stable_log_diff_cdf_vec(intersectmat_tmin1[, 2L], intersectmat_tmin1[, 3L])
        q1 <- normalize_log_probs(logp = log_q1,iter = iter,indiv = indiv,z_iter = z_iter_ind,t = 1L,context = "FFBS initial-region probabilities")
        # # q1 <- exp(log_q1 - max(log_q1))
        # # q1 <- q1 / sum(q1)
        # intersectmat_tmin1[, 4L] <- q1
        qkt_list[[1L]] <- cbind(region = seq_len(nrow(intersectmat_tmin1)),q = q1)
        Biglist_intersectmats[[1]] <- intersectmat_tmin1

        ################ period 2 intersection matrix #################################################

        # indiv_ind <- (2 - 1)*num_indiv+indiv
        # # list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
        # # intersectmat <- interNtreesB_cpp(list_inter_mats)
        #
        # intersectmat <- getlistinter_create_intermat(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
        #
        # intersectmat <- cbind(intersectmat, rep(NA, nrow(intersectmat)))
        #
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
        # intersectmat[, 4] <- stable_log_diff_cdf_vec(intersectmat[, 2L], intersectmat[, 3L])
        #
        # Biglist_intersectmats[[2]] <- intersectmat
        ##### t = 2 to T intersect matrices ######################

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


        # IF LATER NEED TO SAVE MEMORY, CAN COMPUTE
        # time-specific intersectmat within loop over time periods

        ################## qkt for t=1 ? ##############
        indiv_ind <- (1 - 1)*num_indiv+indiv
        future_index <- 1L * num_indiv + indiv

        # intersectmat <- Biglist_intersectmats[[2]]
        intersectmat <- Biglist_intersectmats[[1]]
        # num_regions <- nrow(intersectmat)
        num_regions_tmin1 <- nrow(intersectmat)

        # possible regions for z1 and prior probabilities
        # already in intersectmat

        # The initial columns correspond to regions.
        # There is just one indexing column for this univariate binary probit model
        # the last column is the probability of the region

        # This is the prior probability of z_0 being in the region
        # given the first period covariates x_1
        # where the transition equation is z_1 = f(z_0, x_1) + e_1
        # qkt_mat <- cbind(1:num_regions_tmin1, intersectmat[,4L] )
        # # normalizing to prevent probabilities going to zero
        # # normalizing constant is inconsequential because qkt_mat used later in normalized probabilities
        # qkt_mat[, 2L] <- qkt_mat[, 2L]/sum(qkt_mat[, 2L])
        # qkt_list[[1]] <- qkt_mat

        # this list might use a lot of memory
        # so it might be better to re-calculate in backaward sampler
        n_kt_ktmin1_list <- list()
        n_kt_ktmin1_list[[1]] <- q1 # qkt_mat[,1L, drop = FALSE] # no information from past, so just one column, unlike for t > 1

        ######## qkt for t > 1 #####
        # observed outcome in period 1 implies a constraint on z_1
        # which defines the set A_1 and enters the intersection
        # tildeC_k_2 = C_k_2 intersect A_1

        # all_possible_w_BS_t_LIST <- list()
        # all_tilde_Ckt_lowervecs_list <- list()
        # all_tilde_Ckt_uppervecs_list <- list()
        Ctilde_restrict_interval_list <- list()

        for(t in 2:(n.time_train)){
          indiv_ind <- (t - 2L)*num_indiv+indiv
          previous_obs_index <- (t - 2L) * num_indiv + indiv
          future_index <- (t - 1L) * num_indiv + indiv # t * num_indiv + indiv
          current_index <- (t - 2L) * num_indiv + indiv # (t - 1L) * num_indiv + indiv
          y_tmin1 <- y.train[t-1,indiv]
          if(y_tmin1 == 1){
            A_tmin1_lower <- 0 # scalezero # 0
            A_tmin1_upper <- Inf
          }else{
            A_tmin1_lower <- -Inf
            A_tmin1_upper <- 0 # scalezero # 0
          }
          intersectmat <- Biglist_intersectmats[[t]]
          intersectmat_tmin1 <- Biglist_intersectmats[[t-1]]
          num_regions <- nrow(intersectmat)
          num_regions_tmin1 <- nrow(intersectmat_tmin1)

          # maybe later edit this and above intersectmats to save log probabilities instead
          qkt_mat <- cbind(1:nrow(intersectmat), rep(0, nrow(intersectmat)) )
          # for(rowind in 1:nrow(qkt_mat) ){
          #   templower <- intersectmat[rowind,2]
          #   tempupper <- intersectmat[rowind,3]
          #   # discard region if does not overlap with [A_tmin1_lower, A_tmin1_upper]
          #   if((templower >= A_tmin1_upper )| (A_tmin1_lower >= tempupper)){
          #     qkt_mat[rowind] <- 0
          #     next
          #   }
          #   # num_Ktmin1 <- nrow(qkt_mat[[t-1]])
          #   # qm_1min1_vec <- (qkt_list[[t-1]])[,2L]
          #   # for(m_ind in 1:num_Ktmin1){
          #   #   tempmean <- intersectmat_tmin1[m_ind, 1L]
          #   #   tempintegralval <- pnorm(tempupper, mean = tempmean, sd = 1) - pnorm(templower, mean = tempmean, sd = 1)  # prod( pnorm(tempuppers, mean = tempmeans, sd = 1) - pnorm(templowers, mean = tempmeans, sd = 1) )
          #   #   qkt_mat[rowind,lastcol_ind] <- qkt_mat[rowind,lastcol_ind] +
          #   #     qm_1min1_vec[m_ind]*tempintegralval
          #   # }
          #   # can vectorize
          #   tempmeans <- intersectmat_tmin1[, 1L]
          #   qkt_mat[rowind, 2L] <- qkt_mat[rowind, 2L] + sum((qkt_list[[t-1]])[,2L]*(pnorm(tempupper, mean = tempmeans, sd = 1) - pnorm(templower, mean = tempmeans, sd = 1)))
          # }

          # perhaps can even vectorize the above loop?
          # (qkt_list[[t-1]])[,2L]*(pnorm(tempupper, mean = tempmeans, sd = 1) - pnorm(templower, mean = tempmeans, sd = 1))
          # matrix of intervals, rows correspond to region intervals, columns correpond to  (t minus 1)-region means
          # intervalprobs <- matrix(0, nrow = nrow(intersectmat), ncol = nrow(intersectmat_tmin1))
          # can check intervals or just re-set negative probabilities to zero
          # allowed_regions <- which(!((intersectmat[,2L] >= A_tmin1_upper )| (A_tmin1_lower >= intersectmat[,3L])))

          admissible_lower <- -Inf
          admissible_upper <- Inf
          if(reject_or_restrict_Z_props == "restrict"){

            list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[future_index,-1L, drop = FALSE]))
            all_trees_all_obs <- sampler$getTrees(chainNums = 1L, current = TRUE,newdata = Xmat.train[,-1L, drop = FALSE])
            all_trees_current_obs <- sampler$getTrees(chainNums = 1L,current = TRUE,newdata = Xmat.train[future_index, -1L, drop = FALSE])

            current_rows_by_tree <- split(seq_len(nrow(all_trees_current_obs)),all_trees_current_obs$tree)
            all_rows_by_tree <- split(seq_len(nrow(all_trees_all_obs)),all_trees_all_obs$tree)

            for (tree_index in seq_len(n.trees)) {
              ## Find the current leaf containing the affected observation,
              ## conditional on its complete current predictor row.

              # cannot just use ranges of lag of latent variable because
              # splits on other covariates allow for overlapping ranges in different leaves
              # must find one leaf to which observation being re-sampled belongs
              # current_leaf_lowers <- list_inter_mats[[tree_index]][,2L]
              # current_leaf_uppers <- list_inter_mats[[tree_index]][,3L]

              # find leaf to which observation being re-sampled currently belongs
              # by passing corresponding observation to gettrees
              # assuming Xmat.train[(t)*num_indiv + indiv, , drop = FALSE] is updated accordingly
              # mote Xmat.train contains the LAG of Z so using the period t + 1 observation indexed by (t)*num_indiv + indiv
              # tree_only_currobs <- sampler$getTrees(treeNums = tree_index,chainNums = 1,sampleNums = 1, current = TRUE,
              #                                       newdata = Xmat.train[(t)*num_indiv + indiv, , drop = FALSE])
              # tree_only_currobs <- all_trees_current_obs[all_trees_current_obs$tree == tree_index,,drop = FALSE]

              # find row with only non-empty leaf
              # nonemptyrow <- which(tree_only_currobs$var == -1L & tree_only_currobs$n > 0)
              # nonemptyrow <- which(tree_only_currobs$var == -1L & tree_only_currobs$n == 1)
              current_tree_rows <- current_rows_by_tree[[as.character(tree_index)]]# which(all_trees_current_obs$tree == tree_index)
              nonemptyrow <- which(all_trees_current_obs$var[current_tree_rows] == -1L & all_trees_current_obs$n[current_tree_rows] == 1)
              if(length(nonemptyrow)!=1){
                stop("Line 1314. Current observation not assigned to exactly one leaf. length(nonemptyrow)!=1")
              }

              # find  count of all training observations in the just obtained current observation leaf
              # by passing all training data to getTrees
              # tree_all_obs <- sampler$getTrees(treeNums = tree_index,chainNums = 1,sampleNums = 1, newdata = Xmat.train)
              # tree_all_obs <- sampler$getTrees(treeNums = tree_index,chainNums = 1,sampleNums = 1, current = TRUE, newdata = Xmat.train)

              # tree_all_obs <- all_trees_all_obs[all_trees_all_obs$tree == tree_index,,drop = FALSE]
              # curr_leaf_n <- tree_all_obs$n[nonemptyrow]
              curr_leaf_n <- all_trees_all_obs$n[all_rows_by_tree[[as.character(tree_index)]]][nonemptyrow]

              # curr_leaf_n <- all_trees_all_obs[all_trees_all_obs$tree == tree_index,
              #                                  which(names(all_trees_all_obs) %in% c("n")),
              #                                  drop = FALSE][nonemptyrow]

              if (length(curr_leaf_n) != 1L ||is.na(curr_leaf_n) ||curr_leaf_n < 1L) {
                stop(sprintf("Invalid current-leaf count in tree %d: %s.",tree_index,paste(curr_leaf_n, collapse = ",")))
              }
              # if(!(curr_leaf_n > 0 )){
              #   print("curr_leaf_n =")
              #   print(curr_leaf_n )
              #   stop("Line 1321. !(curr_leaf_n > 0 )")
              # }

              # if (current_leaf$n == 1L) {
              if (curr_leaf_n == 1L) {
                # must find lower and upper bound for z in this particular leaf.
                # there might be a quicker way of doing this, but can use existing ranges for this tree,
                # which are specific to the observation currently being sampled
                current_leaf_lowers <- list_inter_mats[[tree_index]][,2L]
                current_leaf_uppers <- list_inter_mats[[tree_index]][,3L]
                # # assuming lower and upper bounds are ordered from smallest to largest
                # # find first upper bound is greater than z
                # zless_inds <- which(Z.mat[t, indiv ] <= current_leaf_uppers)
                # z_leaf_ind <- zless_inds[1]
                # or, find index in a way that is unaffected by ordering and perhaps more bug-proof
                z_leaf_ind <- which((Z.mat[t - 1L, indiv ] > current_leaf_lowers) & (Z.mat[t - 1L, indiv ] <= current_leaf_uppers) )
                if(length(z_leaf_ind)!=1){stop("Line 1341. length(z_leaf_ind)!=1")}
                z_leaf_ind <- z_leaf_ind[1]
                z_leaf_lb <- current_leaf_lowers[z_leaf_ind]
                z_leaf_ub <- current_leaf_uppers[z_leaf_ind]

                # admissible_lower <- max(admissible_lower,current_leaf$lag_lower)
                # admissible_upper <- min(admissible_upper,current_leaf$lag_upper)
                admissible_lower <- max(admissible_lower,z_leaf_lb)
                admissible_upper <- min(admissible_upper,z_leaf_ub)
              }
            }

            if (!(admissible_lower < admissible_upper)) {
              stop("Singleton-leaf constraints have an empty intersection.")
            }

            #calculate region probabilities

            # restricted_lower <- pmax(temp_region_logprobs[, 2L],admissible_lower)
            # restricted_upper <- pmin(temp_region_logprobs[, 3L],admissible_upper)
            # admissible_region <- is.finite(temp_region_logprobs[, 1L]) & restricted_lower < restricted_upper
            # old_z <- Z.mat[t, indiv]
            old_z <- Z.mat[t - 1L, indiv]

            if (!(old_z > admissible_lower && old_z <= admissible_upper)) {
              stop(sprintf(paste0("Current latent lag %.17g is outside its derived ","admissible interval (%g, %g] at iteration %d, ","time %d, individual %d."),
                           old_z,admissible_lower,admissible_upper,iter,t,indiv))
            }
          }

          temp_lower2 <- intersectmat[, 2]
          temp_upper2 <- intersectmat[, 3]

          if(reject_or_restrict_Z_props == "restrict"){
            temp_lower2 <- pmax(temp_lower2,admissible_lower)
            temp_upper2 <- pmin(temp_upper2,admissible_upper)
          }
          Ctilde_lower <- pmax(temp_lower2,A_tmin1_lower)
          Ctilde_upper <- pmin(temp_upper2,A_tmin1_upper)

          if(reject_or_restrict_Z_props == "restrict"){
            Ctilde_restrict_interval_list[[t]] <- cbind(Ctilde_lower, Ctilde_upper)
          }

          allowed_regions <- which(!is.na(Ctilde_lower) &!is.na(Ctilde_upper) & Ctilde_lower < Ctilde_upper)
          # tempmeans <- intersectmat_tmin1[, 1L] + genuine_offset_vec[previous_obs_index]
          tempmeans <- (intersectmat_tmin1[, 1L] + 0.5)*(max_resp - min_resp) + min_resp + genuine_offset_vec[previous_obs_index]
          if (length(allowed_regions) == 0L) {
            stop(sprintf(paste0("No FFBS forward region at iteration %d, ","z iteration %d, time %d, individual %d."),
                         iter,z_iter_ind,t,indiv))
          }
          # intervalprobs[allowed_regions, ] <- (outer(pmin(intersectmat[allowed_regions,3L], A_tmin1_upper), tempmeans, pnorm, sd = 1) -
          # outer(pmax(intersectmat[allowed_regions,2L], A_tmin1_lower), tempmeans, pnorm, sd = 1))
          # saving memory by just saving intervals probabilities of allowed regions

          # temp_quantiles <- outer(c(pmin(intersectmat[allowed_regions,3L], A_tmin1_upper),
          #                           pmax(intersectmat[allowed_regions,2L], A_tmin1_lower)),
          #                         tempmeans, pnorm, sd = 1)
          # intervalprobs_allowed <- temp_quantiles[1:length(allowed_regions),] - temp_quantiles[length(allowed_regions) + 1:length(allowed_regions),]

          # intervalprobs_allowed <- (outer(pmin(intersectmat[allowed_regions,3L], A_tmin1_upper), tempmeans, pnorm, sd = 1) -
          #                             outer(pmax(intersectmat[allowed_regions,2L], A_tmin1_lower), tempmeans, pnorm, sd = 1))

          # Ctilde_lower_allowed <- pmax( intersectmat[allowed_regions, 2L], A_tmin1_lower )
          # Ctilde_upper_allowed <- pmin( intersectmat[allowed_regions, 3L], A_tmin1_upper )
          Ctilde_lower_allowed <- Ctilde_lower[allowed_regions]
          Ctilde_upper_allowed <- Ctilde_upper[allowed_regions]
          lower_args <- outer( Ctilde_lower_allowed, tempmeans, FUN = "-" )
          upper_args <- outer( Ctilde_upper_allowed, tempmeans, FUN = "-" )
          log_intervalprobs_allowed <- matrix( stable_log_diff_cdf_vec( as.vector(lower_args), as.vector(upper_args) ), nrow = nrow(lower_args), ncol = ncol(lower_args))
          intervalprobs_allowed <- exp(log_intervalprobs_allowed)
          if (anyNA(intervalprobs_allowed) ||any(!is.finite(intervalprobs_allowed)) ||any(intervalprobs_allowed < 0) || any(intervalprobs_allowed > 1)) {
            stop(sprintf(paste0("Invalid FFBS transition probabilities at ","iteration %d, z iteration %d, time %d, ","individual %d."), iter,z_iter_ind,t,indiv))
          }
          underflowed_transitions <- is.finite(log_intervalprobs_allowed) &intervalprobs_allowed == 0
          if (any(underflowed_transitions)) {
            warning(sprintf(paste0("Transition-probability underflow at iteration %d, ","z iteration %d, time %d, individual %d: ","%d of %d finite log probabilities became zero; ","smallest finite log probability=%.3f."),
                            iter,z_iter_ind,t,indiv,sum(underflowed_transitions),sum(is.finite(log_intervalprobs_allowed)),min(log_intervalprobs_allowed[is.finite(log_intervalprobs_allowed)])))
          }

          # # if memory efficiency is not important can save intervals for later use ion backward sampling
          # all_tilde_Ckt_lowervec <- rep(0, num_regions)
          # all_tilde_Ckt_uppervec <- rep(0, num_regions)
          # all_tilde_Ckt_lowervec <- pmax(intersectmat[allowed_regions,2L], A_tmin1_lower)
          # all_tilde_Ckt_uppervec <- pmin(intersectmat[allowed_regions,3L], A_tmin1_upper)
          # all_tilde_Ckt_lowervecs_list[[t]] <- all_tilde_Ckt_lowervec
          # all_tilde_Ckt_uppervecs_list[[t]] <- all_tilde_Ckt_uppervec
          # intervalprobs_allowed <- (outer(all_tilde_Ckt_uppervec, tempmeans, pnorm, sd = 1) - outer(all_tilde_Ckt_lowervec, tempmeans, pnorm, sd = 1))

          # within each row, multiply element-wise by (qkt_list[[t-1]])[,2L],
          # i.e. all elements of column 1 of intervalprobs are multiplied by (qkt_list[[t-1]])[1,2L]
          # i.e. all elements of column 2 of intervalprobs are multiplied by (qkt_list[[t-1]])[2,2L], and so on
          # then take the row-sums (across columns)
          # but this is equivalent to intervalprobs %*% (qkt_list[[t-1]])[,2L] ?
          # and do not need to calculate for regions that are not allowed
          # qkt_mat[allowed_regions, 2L] <- intervalprobs[allowed_regions, ] %*% (qkt_list[[t-1]])[,2L]

          # print("length(allowed_regions) = ")
          # print(length(allowed_regions))
          # print("length(tempmeans) = ")
          # print(length(tempmeans))
          # print("dim(intervalprobs_allowed) = ")
          # print(dim(intervalprobs_allowed))
          # print("dim(qkt_list[[t-1]]) = ")
          # print(dim(qkt_list[[t-1]]))
          # print("dim(intersectmat_tmin1) = ")
          # print(dim(intersectmat_tmin1))
          # print("dim(intersectmat) = ")
          # print(dim(intersectmat))
          # print("t = ")
          # print(t)
          # print("n.time_train = ")
          # print(n.time_train)
          qkt_mat[allowed_regions, 2L] <- intervalprobs_allowed %*% (qkt_list[[t-1]])[,2L]
          # normalizing to prevent probabilities going to zero
          # normalizing constant is inconsequential because qkt_mat used later in normalized probabilities
          qkt_mat[allowed_regions, 2L] <- qkt_mat[allowed_regions, 2L]/sum(qkt_mat[allowed_regions, 2L])
          qkt_list[[t]] <- qkt_mat

          # if memory efficiency not important, can also save list of n_kt_ktmin1 matrices
          intervalprobs <- matrix(0, nrow = nrow(intersectmat), ncol = nrow(intersectmat_tmin1))
          intervalprobs[allowed_regions, ] <- intervalprobs_allowed
          n_kt_ktmin1_list[[t]] <- intervalprobs
          # Actually the matrix product involves calculation of all possible backward sampling weights
          # therefore, if memory constraints were not an issue, it might be more efficient to save
          # all products calculated in the evaluation of intervalprobs_allowed %*% (qkt_list[[t-1]])[,2L]
          # this would be a 3d array
          # all_possible_w_BS_t <- outer(intervalprobs_allowed, (qkt_list[[t-1]])[,2L], `*`)
          # all_possible_w_BS_t_LIST[[t]] <- all_possible_w_BS_t
          # qkt_list[[t]] <- apply(all_possible_w_BS_t,MARGIN = 3, FUN = sum )


          # another option is need to make a bit more memory efficient
          # n_kt_ktmin1_list[[t]] <- list(allowed_regions = allowed_regions,probabilities = intervalprobs_allowed,num_regions = num_regions)

        } # end qkt loop over t

        ####### nkt calculations ##################

        # The qkt give probabilities of being in regions kt
        # whereas nkt_tp1 give probabilities of moving from region kt to ktp1
        # However, nkt_tp1 are intermediate values in the recursion that defines the qkt values.
        # THIS MIGHT BE USEFUL FOR THE BACKWARD SAMPLER

        # For the filtering distribution, we jsut require nkt values
        # these are probabilities of going from region kt to any zt allowed by the constraint.
        # i.e. the sum of nkt_tp1 for a given kt over all ktp1.
        # HOWEVER, it might be better to write this as nktmin1 because it is probability of going
        # from region ktmin1 to region A_t, so the k refers to the previous period and the constraint refers to the next period.

        # calculate all forward or backwards?
        # nkt only required for smoother (and period T filter)
        # but running a backward sampler
        ######### BACKWARD SAMPLER ##############
        k_path <- integer(n.time_train)

        #### period T draw from filter ##########
        # region weights determined by qkT
        qkt_mat <- qkt_list[[n.time_train]]
        intersectmat <- Biglist_intersectmats[[n.time_train]]
        # not normalizing because normalization should occur within the function sample.int
        # indiv_ind <- (t - 1)*num_indiv + indiv
        y_t <- y.train[n.time_train,indiv]
        if(y_t == 1){
          A_t_lower <- 0 # scalezero # 0
          A_t_upper <- Inf
        }else{
          A_t_lower <- -Inf
          A_t_upper <- 0 # scalezero # 0
        }

        # n_ktvec <- pnorm(A_t_upper, mean = intersectmat[,1L] + offsetz_vec[indiv], sd = 1) - pnorm(A_t_lower, mean = intersectmat[,1L] + offsetz_vec[indiv], sd = 1)
        # k_t <- sample.int(nrow(qkt_mat),size = 1,prob = qkt_mat[,2L]*n_ktvec)
        terminal_obs_index <- (n.time_train - 1L) * num_indiv + indiv
        regional_means <- (intersectmat[, 1L] + 0.5)*(max_resp - min_resp) + min_resp + genuine_offset_vec[terminal_obs_index]
        log_n_ktvec <- stable_log_diff_cdf_vec( A_t_lower - regional_means, A_t_upper - regional_means )
        n_ktvec <- exp(log_n_ktvec)
        log_q_T <- rep(-Inf,length(qkt_mat[, 2L]))
        positive_q <- qkt_mat[, 2L] > 0
        log_q_T[positive_q] <- log(qkt_mat[positive_q, 2L])
        log_terminal_weights <- log_q_T + log_n_ktvec
        terminal_probabilities <- normalize_log_probs( logp = log_terminal_weights, iter = iter, indiv = indiv, z_iter = z_iter_ind, t = n.time_train,
                                                       context = "FFBS terminal-region draw")

        k_t <- sample.int(n = nrow(qkt_mat), size = 1L, prob = terminal_probabilities )

        if(reject_or_restrict_Z_props == "reject" ){

          affected_row <- (n.time_train - 1L) * num_indiv + indiv
          candidate_k_t_feasible <- FALSE
          y_tmin1 <- y.train[n.time_train - 1L, indiv]
          if (y_tmin1 == 1L) {
            A_tmin1_lower <- 0
            A_tmin1_upper <- Inf
          } else {
            A_tmin1_lower <- -Inf
            A_tmin1_upper <- 0
          }
          kt_update_count <- 0
          while(candidate_k_t_feasible == FALSE){
            kt_update_count <- kt_update_count + 1
            if(kt_update_count == max_lag_update_attempts){stop("kt_update_count == max_lag_update_attempts")}
            # find admissible region
            admissible_lower <- -Inf
            admissible_upper <- Inf
            # if(reject_or_restrict_Z_props == "restrict"){

            list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[affected_row,-1L, drop = FALSE]))
            all_trees_all_obs <- sampler$getTrees(chainNums = 1L, current = TRUE,newdata = Xmat.train[,-1L, drop = FALSE])
            all_trees_current_obs <- sampler$getTrees(chainNums = 1L,current = TRUE,newdata = Xmat.train[affected_row, -1L, drop = FALSE])
            current_rows_by_tree <- split(seq_len(nrow(all_trees_current_obs)),all_trees_current_obs$tree)
            all_rows_by_tree <- split(seq_len(nrow(all_trees_all_obs)),all_trees_all_obs$tree)
            for (tree_index in seq_len(n.trees)) {
              current_tree_rows <- current_rows_by_tree[[as.character(tree_index)]]# which(all_trees_current_obs$tree == tree_index)
              nonemptyrow <- which(all_trees_current_obs$var[current_tree_rows] == -1L & all_trees_current_obs$n[current_tree_rows] == 1)
              if(length(nonemptyrow)!=1){
                stop("Line 1314. Current observation not assigned to exactly one leaf. length(nonemptyrow)!=1")
              }
              curr_leaf_n <- all_trees_all_obs$n[all_rows_by_tree[[as.character(tree_index)]]][nonemptyrow]
              if (length(curr_leaf_n) != 1L ||is.na(curr_leaf_n) ||curr_leaf_n < 1L) {
                stop(sprintf("Invalid current-leaf count in tree %d: %s.",tree_index,paste(curr_leaf_n, collapse = ",")))
              }
              if (curr_leaf_n == 1L) {
                current_leaf_lowers <- list_inter_mats[[tree_index]][,2L]
                current_leaf_uppers <- list_inter_mats[[tree_index]][,3L]
                z_leaf_ind <- which((Z.mat[n.time_train - 1L, indiv ] > current_leaf_lowers) & (Z.mat[n.time_train - 1L, indiv ] <= current_leaf_uppers) )
                if(length(z_leaf_ind)!=1){stop("Line 1341. length(z_leaf_ind)!=1")}
                z_leaf_ind <- z_leaf_ind[1]
                z_leaf_lb <- current_leaf_lowers[z_leaf_ind]
                z_leaf_ub <- current_leaf_uppers[z_leaf_ind]
                admissible_lower <- max(admissible_lower,z_leaf_lb)
                admissible_upper <- min(admissible_upper,z_leaf_ub)
              }
            }

            if (!(admissible_lower < admissible_upper)) {
              stop("Singleton-leaf constraints have an empty intersection.")
            }

            #calculate region probabilities

            # restricted_lower <- pmax(temp_region_logprobs[, 2L],admissible_lower)
            # restricted_upper <- pmin(temp_region_logprobs[, 3L],admissible_upper)
            # admissible_region <- is.finite(temp_region_logprobs[, 1L]) & restricted_lower < restricted_upper
            # old_z <- Z.mat[t, indiv]
            old_z <- Z.mat[n.time_train - 1L, indiv]

            if (!(old_z > admissible_lower && old_z <= admissible_upper)) {
              stop(sprintf(paste0("Current latent lag %.17g is outside its derived ","admissible interval (%g, %g] at iteration %d, ","time %d, individual %d."),
                           old_z,admissible_lower,admissible_upper,iter,n.time_train,indiv))
            }
            # }

            region_lower <- Biglist_intersectmats[[n.time_train]][ k_t, 2L ]
            region_upper <- Biglist_intersectmats[[n.time_train]][k_t,3L]
            future_valid_lower <- max(region_lower,A_tmin1_lower,admissible_lower)
            future_valid_upper <- min(region_upper,A_tmin1_upper,admissible_upper)
            candidate_k_t_feasible <- future_valid_lower < future_valid_upper
            if(candidate_k_t_feasible){
              # valid_k_t <- FALSE
            }else{
              # k_t <- sample.int(num_regions,size = 1,prob = w_BS_vec)
              k_t <- sample.int(n = nrow(qkt_mat), size = 1L, prob = terminal_probabilities )
              # valid_k_t <- TRUE
            }
          } # end while loop
        }



        # mu_T <- intersectmat[,1L] + offsetz_vec[indiv]
        # log_n_T <- stable_log_diff_cdf_all_tails(A_t_lower - mu_T,A_t_upper - mu_T)
        # log_region_weights_T <- log(qkt_mat[,2L]) + log_n_T
        # prob_region_T <- normalize_log_probs(logp = log_region_weights_T,iter = iter,indiv = indiv,z_iter = z_iter_ind,t = T_index,
        #   context = "FFBS terminal-region draw")
        # k_t <- sample.int(nrow(qkt_mat),size = 1,prob = prob_region_T)


        zdraw_temp <- rtruncnorm(n = 1,
                                 a = A_t_lower,
                                 b = A_t_upper,
                                 mean = (intersectmat[k_t,1L] + 0.5)*(max_resp - min_resp) + min_resp + genuine_offset_vec[terminal_obs_index],
                                 sd = 1)
        #### BEGIN edits for repeated sampling until lag update does not result in terminal node
        z_old_temp <- Z.mat[n.time_train, indiv ]
        Z.mat[n.time_train, indiv ] <- zdraw_temp
        Z.vec[(n.time_train-1)*num_indiv + indiv] <- zdraw_temp
        old_k_t <- k_path[n.time_train]
        old_k_tp1 <- old_k_t # k_tp1
        k_tp1 <- k_t
        k_path <- integer(n.time_train)
        k_path[n.time_train] <- k_t
        #### period t draw from backward sampler #############

        for(t in (n.time_train-1):1){
          intersectmat <- Biglist_intersectmats[[t]]
          intersectmat_tp1 <- Biglist_intersectmats[[t+1]]
          num_regions <- nrow(intersectmat)
          num_regions_tp1 <- nrow(intersectmat_tp1)
          current_obs_index <- (t - 1L) * num_indiv + indiv
          # z_tp1 <- Z.vec[t*num_indiv + indiv]
          # w_BS_mat <- matrix(0, nrow = nrow(num_regions_tp1), ncol = num_regions)
          # pointwise product
          w_BS_vec <- n_kt_ktmin1_list[[t+1]][k_tp1,] * qkt_list[[t]][,2L]
          # w_BS_vec <- w_BS_vec/qkt_list[[t+1]][k_tp1,2L]
          if (anyNA(w_BS_vec) || any(!is.finite(w_BS_vec)) || any(w_BS_vec < 0) || !any(w_BS_vec > 0) ) {
            stop(sprintf(paste0("Invalid FFBS backward weights at iteration %d, ","z iteration %d, time %d, individual %d, ","next region %d; weights=%s."),
                         iter,z_iter_ind,t,indiv,k_tp1,paste(w_BS_vec, collapse = ",")))
          }
          w_BS_vec <- w_BS_vec/sum(w_BS_vec)

          # # If using bit more memory efficient storage of nktp1_nkt from filtering step
          # transition_column <- numeric(num_regions)
          # match_row <- match(k_tp1,n_kt_ktmin1_list[[t + 1L]]$allowed_regions)
          # if (!is.na(match_row)) {
          #   transition_column <- n_kt_ktmin1_list[[t + 1L]]$probabilities[match_row,,drop = TRUE]
          # }
          # w_BS_vec <-transition_column * qkt_list[[t]][, 2L]
          # w_BS_vec <- w_BS_vec/sum(w_BS_vec)

          # indiv_ind <- (t - 1)*num_indiv + indiv
          y_t <- y.train[t,indiv]
          if(y_t == 1){
            A_t_lower <- 0 # scalezero # 0
            A_t_upper <- Inf
          }else{
            A_t_lower <- -Inf
            A_t_upper <- 0 # scalezero # 0
          }

          k_t <- sample.int(num_regions,size = 1,prob = w_BS_vec)

          if(reject_or_restrict_Z_props == "reject" & t > 1L){
            affected_row <- (t - 1L) * num_indiv + indiv
            candidate_k_t_feasible <- FALSE
            y_tmin1 <- y.train[t - 1L, indiv]
            if (y_tmin1 == 1L) {
              A_tmin1_lower <- 0
              A_tmin1_upper <- Inf
            } else {
              A_tmin1_lower <- -Inf
              A_tmin1_upper <- 0
            }

            kt_update_count <- 0
            while(candidate_k_t_feasible == FALSE){
              kt_update_count <- kt_update_count + 1
              if(kt_update_count == max_lag_update_attempts){stop("kt_update_count == max_lag_update_attempts")}
              # find admissible region
              admissible_lower <- -Inf
              admissible_upper <- Inf
              # if(reject_or_restrict_Z_props == "restrict"){

              list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[affected_row,-1L, drop = FALSE]))
              all_trees_all_obs <- sampler$getTrees(chainNums = 1L, current = TRUE,newdata = Xmat.train[,-1L, drop = FALSE])
              all_trees_current_obs <- sampler$getTrees(chainNums = 1L,current = TRUE,newdata = Xmat.train[affected_row, -1L, drop = FALSE])
              current_rows_by_tree <- split(seq_len(nrow(all_trees_current_obs)),all_trees_current_obs$tree)
              all_rows_by_tree <- split(seq_len(nrow(all_trees_all_obs)),all_trees_all_obs$tree)
              for (tree_index in seq_len(n.trees)) {
                current_tree_rows <- current_rows_by_tree[[as.character(tree_index)]]# which(all_trees_current_obs$tree == tree_index)
                nonemptyrow <- which(all_trees_current_obs$var[current_tree_rows] == -1L & all_trees_current_obs$n[current_tree_rows] == 1)
                if(length(nonemptyrow)!=1){
                  stop("Line 1314. Current observation not assigned to exactly one leaf. length(nonemptyrow)!=1")
                }
                curr_leaf_n <- all_trees_all_obs$n[all_rows_by_tree[[as.character(tree_index)]]][nonemptyrow]
                if (length(curr_leaf_n) != 1L ||is.na(curr_leaf_n) ||curr_leaf_n < 1L) {
                  stop(sprintf("Invalid current-leaf count in tree %d: %s.",tree_index,paste(curr_leaf_n, collapse = ",")))
                }
                if (curr_leaf_n == 1L) {
                  current_leaf_lowers <- list_inter_mats[[tree_index]][,2L]
                  current_leaf_uppers <- list_inter_mats[[tree_index]][,3L]
                  z_leaf_ind <- which((Z.mat[t - 1L, indiv ] > current_leaf_lowers) & (Z.mat[t - 1L, indiv ] <= current_leaf_uppers) )
                  if(length(z_leaf_ind)!=1){stop("Line 1341. length(z_leaf_ind)!=1")}
                  z_leaf_ind <- z_leaf_ind[1]
                  z_leaf_lb <- current_leaf_lowers[z_leaf_ind]
                  z_leaf_ub <- current_leaf_uppers[z_leaf_ind]
                  admissible_lower <- max(admissible_lower,z_leaf_lb)
                  admissible_upper <- min(admissible_upper,z_leaf_ub)
                }
              }

              if (!(admissible_lower < admissible_upper)) {
                stop("Singleton-leaf constraints have an empty intersection.")
              }

              #calculate region probabilities

              # restricted_lower <- pmax(temp_region_logprobs[, 2L],admissible_lower)
              # restricted_upper <- pmin(temp_region_logprobs[, 3L],admissible_upper)
              # admissible_region <- is.finite(temp_region_logprobs[, 1L]) & restricted_lower < restricted_upper
              # old_z <- Z.mat[t, indiv]
              old_z <- Z.mat[t - 1L, indiv]

              if (!(old_z > admissible_lower && old_z <= admissible_upper)) {
                stop(sprintf(paste0("Current latent lag %.17g is outside its derived ","admissible interval (%g, %g] at iteration %d, ","time %d, individual %d."),
                             old_z,admissible_lower,admissible_upper,iter,t,indiv))
              }
              # }

              region_lower <- Biglist_intersectmats[[t]][ k_t, 2L ]
              region_upper <- Biglist_intersectmats[[t]][k_t,3L]
              future_valid_lower <- max(region_lower,A_tmin1_lower,admissible_lower)
              future_valid_upper <- min(region_upper,A_tmin1_upper,admissible_upper)
              candidate_k_t_feasible <- future_valid_lower < future_valid_upper
              if(candidate_k_t_feasible){
                # valid_k_t <- FALSE
              }else{
                k_t <- sample.int(num_regions,size = 1,prob = w_BS_vec)
                # valid_k_t <- TRUE
              }
            } # end while loop
          }






          backward_lower <- max(intersectmat_tp1[k_tp1, 2L],A_t_lower)
          backward_upper <- min(intersectmat_tp1[k_tp1, 3L],A_t_upper)
          if(reject_or_restrict_Z_props == "restrict"){
            backward_lower <- max(Ctilde_restrict_interval_list[[t+1L]][k_tp1, 1L],A_t_lower)
            backward_upper <- min(Ctilde_restrict_interval_list[[t+1L]][k_tp1, 2L],A_t_upper)
          }
          if (is.na(backward_lower) ||is.na(backward_upper) ||backward_lower >= backward_upper) {
            stop(sprintf(paste0("Invalid FFBS backward interval at iteration %d, ","z iteration %d, time %d, individual %d; ","current region=%d, next region=%d, interval=(%g,%g]."),
                         iter,z_iter_ind,t,indiv,k_t,k_tp1,backward_lower,backward_upper))
          }

          zdraw_temp <- rtruncnorm(n = 1,
                                   a = backward_lower, # max(intersectmat_tp1[k_tp1,2L], A_t_lower),
                                   b = backward_upper, # min(intersectmat_tp1[k_tp1,3L], A_t_upper),
                                   mean = (intersectmat[k_t,1L] + 0.5)*(max_resp - min_resp) + min_resp + genuine_offset_vec[current_obs_index],
                                   sd = 1)

          #### BEGIN edits for repeated sampling until lag update does not result in terminal node
          z_old_temp <- Z.mat[t, indiv ]
          Z.mat[t, indiv ] <- zdraw_temp
          Z.vec[(t-1)*num_indiv + indiv] <- zdraw_temp
          old_k_t <- k_path[t]
          old_k_tp1 <-  k_tp1
          k_path[t] <- k_t
          k_tp1 <- k_t
          for(t2 in 1:num_lags){
            # init_Z_t0 <- rep(0, t2*num_indiv)
            # # init_Z_t0 <- rnorm(t*n.item*n.ranker)
            # Zlag.mat[,t2] <- c(init_Z_t0, Z.vec[1:((n.time_train-t2)*num_indiv)])
            # Xmat.train[,t2 + 1L] <- Zlag.mat[, t2]
            Xmat.train[t2*num_indiv + (t-1)*num_indiv + indiv,t2 + 1L] <- Z.vec[(t-1)*num_indiv + indiv]
          }

          check_lag_valid <- FALSE

          for(j in 1:num_lags){
            init_Z_t0 <- rep(0, j*num_indiv)
            Zlag.mat[,j] <- c(init_Z_t0, Z.vec[1:((n.time_train-j)*num_indiv)])
            if((sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) ){
              check_lag_valid <- FALSE
              if(reject_or_restrict_Z_props == "restrict"){
                print("Line 1822 predictor update not accepted and reject_or_restrict_Z_props == restrict")
                stop("Somehow made mistake in restricted sampling of latent variable")
              }
              Z.mat[t, indiv ] <- z_old_temp
              Z.vec[(t-1)*num_indiv + indiv] <- z_old_temp
              k_path[t] <- old_k_t
              k_tp1 <- old_k_tp1
              for(t2 in 1:num_lags){
                # init_Z_t0 <- rep(0, t2*num_indiv)
                # # init_Z_t0 <- rnorm(t*n.item*n.ranker)
                # Zlag.mat[,t2] <- c(init_Z_t0, Z.vec[1:((n.time_train-t2)*num_indiv)])
                # Xmat.train[,t2 + 1L] <- Zlag.mat[, t2]
                Xmat.train[t2*num_indiv + (t-1)*num_indiv + indiv,t2 + 1L] <- Z.vec[(t-1)*num_indiv + indiv]
              }
              for(j2 in 1:j){
                init_Z_t0 <- rep(0, j2*num_indiv)
                Zlag.mat[,j2] <- c(init_Z_t0, Z.vec[1:((n.time_train-j2)*num_indiv)])
                doublecheck <- (sampler$setPredictor(x = Zlag.mat[,j2], column = j2) == FALSE)
                if(doublecheck){
                  stop("cannot roll back")
                }
              }
              break
            }else{
              if(reject_or_restrict_Z_props == "reject" & t > 1L){
                # find admissible region for z_{t-1}
                # the z_t sample can imply that
                # there is at least one leaf that now contains only z_{t-1}
                # which impacts the acceptable regions
                # The might or might not make the z_{t-1} sample impossible
                # Must obtain admissible z_{t-1} region and check if
                # a feasible range still remains
                admissible_lag_lower <- -Inf
                admissible_lag_upper <- Inf
                # if(reject_or_restrict_Z_props == "restrict"){
                affected_row_lag <- (t - 1L) * num_indiv + indiv

                list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[affected_row_lag,-1L, drop = FALSE]))
                all_trees_all_obs <- sampler$getTrees(chainNums = 1L, current = TRUE,newdata = Xmat.train[,-1L, drop = FALSE])
                all_trees_current_obs <- sampler$getTrees(chainNums = 1L,current = TRUE,newdata = Xmat.train[affected_row_lag, -1L, drop = FALSE])
                current_rows_by_tree <- split(seq_len(nrow(all_trees_current_obs)),all_trees_current_obs$tree)
                all_rows_by_tree <- split(seq_len(nrow(all_trees_all_obs)),all_trees_all_obs$tree)
                for (tree_index in seq_len(n.trees)) {
                  current_tree_rows <- current_rows_by_tree[[as.character(tree_index)]]# which(all_trees_current_obs$tree == tree_index)
                  nonemptyrow <- which(all_trees_current_obs$var[current_tree_rows] == -1L & all_trees_current_obs$n[current_tree_rows] == 1)
                  if(length(nonemptyrow)!=1){
                    stop("Line 1314. Current observation not assigned to exactly one leaf. length(nonemptyrow)!=1")
                  }
                  curr_leaf_n <- all_trees_all_obs$n[all_rows_by_tree[[as.character(tree_index)]]][nonemptyrow]
                  if (length(curr_leaf_n) != 1L ||is.na(curr_leaf_n) ||curr_leaf_n < 1L) {
                    stop(sprintf("Invalid current-leaf count in tree %d: %s.",tree_index,paste(curr_leaf_n, collapse = ",")))
                  }
                  if (curr_leaf_n == 1L) {
                    current_leaf_lowers <- list_inter_mats[[tree_index]][,2L]
                    current_leaf_uppers <- list_inter_mats[[tree_index]][,3L]
                    z_leaf_ind <- which((Z.mat[t - 1L, indiv ] > current_leaf_lowers) & (Z.mat[t - 1L, indiv ] <= current_leaf_uppers) )
                    if(length(z_leaf_ind)!=1){stop("Line 1341. length(z_leaf_ind)!=1")}
                    z_leaf_ind <- z_leaf_ind[1]
                    z_leaf_lb <- current_leaf_lowers[z_leaf_ind]
                    z_leaf_ub <- current_leaf_uppers[z_leaf_ind]
                    admissible_lag_lower <- max(admissible_lag_lower,z_leaf_lb)
                    admissible_lag_upper <- min(admissible_lag_upper,z_leaf_ub)
                  }
                }
                y_tmin1 <- y.train[t - 1L, indiv]
                if (y_tmin1 == 1L) {
                  A_tmin1_lower <- 0
                  A_tmin1_upper <- Inf
                } else {
                  A_tmin1_lower <- -Inf
                  A_tmin1_upper <- 0
                }
                region_lower <- Biglist_intersectmats[[t]][ k_t, 2L ]
                region_upper <- Biglist_intersectmats[[t]][k_t,3L]
                overall_lower <- max(admissible_lag_lower, A_tmin1_lower, region_lower)
                overall_upper <- min(admissible_lag_upper, A_tmin1_upper, region_upper)
                if(overall_lower < overall_upper){
                  check_lag_valid <- TRUE
                  Xmat.train[, j + 1L] <- Zlag.mat[, j]
                  if (!isTRUE(all.equal(as.numeric(Xmat.train[, j + 1L]),as.numeric(Zlag.mat[, j]),tolerance = 0,check.attributes = FALSE))) {
                    stop("Xmat.train and Zlag.mat disagree after predictor update.")
                  }
                  if (!isTRUE(all.equal(as.numeric(sampler$data@x[, j]),as.numeric(Zlag.mat[, j]),tolerance = 0,check.attributes = FALSE))) {
                    stop("dbarts predictor and Zlag.mat disagree after predictor update.")
                  }
                }else{
                  check_lag_valid <- FALSE
                  if(reject_or_restrict_Z_props == "restrict"){
                    print("Line 1822 predictor update not accepted and reject_or_restrict_Z_props == restrict")
                    stop("Somehow made mistake in restricted sampling of latent variable")
                  }
                  Z.mat[t, indiv ] <- z_old_temp
                  Z.vec[(t-1)*num_indiv + indiv] <- z_old_temp
                  k_path[t] <- old_k_t
                  k_tp1 <- old_k_tp1
                  for(t2 in 1:num_lags){
                    # init_Z_t0 <- rep(0, t2*num_indiv)
                    # # init_Z_t0 <- rnorm(t*n.item*n.ranker)
                    # Zlag.mat[,t2] <- c(init_Z_t0, Z.vec[1:((n.time_train-t2)*num_indiv)])
                    # Xmat.train[,t2 + 1L] <- Zlag.mat[, t2]
                    Xmat.train[t2*num_indiv + (t-1)*num_indiv + indiv,t2 + 1L] <- Z.vec[(t-1)*num_indiv + indiv]
                  }
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
              }else{
                check_lag_valid <- TRUE
                Xmat.train[, j + 1L] <- Zlag.mat[, j]
                if (!isTRUE(all.equal(as.numeric(Xmat.train[, j + 1L]),as.numeric(Zlag.mat[, j]),tolerance = 0,check.attributes = FALSE))) {
                  stop("Xmat.train and Zlag.mat disagree after predictor update.")
                }
                if (!isTRUE(all.equal(as.numeric(sampler$data@x[, j]),as.numeric(Zlag.mat[, j]),tolerance = 0,check.attributes = FALSE))) {
                  stop("dbarts predictor and Zlag.mat disagree after predictor update.")
                }
              }
            }
          }

          lag_update_attempt <-  0L

          if(check_lag_valid == FALSE & reject_or_restrict_Z_props == "restrict"){
            print("check_lag_valid == FALSE and reject_or_restrict_Z_props == restrict")
            stop("Line 1850 Somehow made mistake in restricted sampling of latent variable")
          }

          while (check_lag_valid == FALSE) {
            lag_update_attempt <- lag_update_attempt + 1L

            if ( lag_update_attempt > max_lag_update_attempts) {
              stop(sprintf(paste0("Unable to obtain a valid latent-lag update ","at iteration %d, time %d, individual %d."),
                           iter,t,indiv),call. = FALSE)
            }
            k_t <- sample.int(num_regions,size = 1,prob = w_BS_vec)
            # indiv_ind <- (t - 1)*num_indiv + indiv
            y_t <- y.train[t,indiv]
            if(y_t == 1){
              A_t_lower <- 0 # scalezero # 0
              A_t_upper <- Inf
            }else{
              A_t_lower <- -Inf
              A_t_upper <- 0 # scalezero # 0
            }
            if(reject_or_restrict_Z_props == "reject" & t > 1L){
              affected_row <- (t - 1L) * num_indiv + indiv
              candidate_k_t_feasible <- FALSE
              y_tmin1 <- y.train[t - 1L, indiv]
              if (y_tmin1 == 1L) {
                A_tmin1_lower <- 0
                A_tmin1_upper <- Inf
              } else {
                A_tmin1_lower <- -Inf
                A_tmin1_upper <- 0
              }

              kt_update_count <- 0
              while(candidate_k_t_feasible == FALSE){
                kt_update_count <- kt_update_count + 1
                if(kt_update_count == max_lag_update_attempts){stop("kt_update_count == max_lag_update_attempts")}
                # find admissible region
                admissible_lower <- -Inf
                admissible_upper <- Inf
                # if(reject_or_restrict_Z_props == "restrict"){

                list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[affected_row,-1L, drop = FALSE]))
                all_trees_all_obs <- sampler$getTrees(chainNums = 1L, current = TRUE,newdata = Xmat.train[,-1L, drop = FALSE])
                all_trees_current_obs <- sampler$getTrees(chainNums = 1L,current = TRUE,newdata = Xmat.train[affected_row, -1L, drop = FALSE])
                current_rows_by_tree <- split(seq_len(nrow(all_trees_current_obs)),all_trees_current_obs$tree)
                all_rows_by_tree <- split(seq_len(nrow(all_trees_all_obs)),all_trees_all_obs$tree)
                for (tree_index in seq_len(n.trees)) {
                  current_tree_rows <- current_rows_by_tree[[as.character(tree_index)]]# which(all_trees_current_obs$tree == tree_index)
                  nonemptyrow <- which(all_trees_current_obs$var[current_tree_rows] == -1L & all_trees_current_obs$n[current_tree_rows] == 1)
                  if(length(nonemptyrow)!=1){
                    stop("Line 1314. Current observation not assigned to exactly one leaf. length(nonemptyrow)!=1")
                  }
                  curr_leaf_n <- all_trees_all_obs$n[all_rows_by_tree[[as.character(tree_index)]]][nonemptyrow]
                  if (length(curr_leaf_n) != 1L ||is.na(curr_leaf_n) ||curr_leaf_n < 1L) {
                    stop(sprintf("Invalid current-leaf count in tree %d: %s.",tree_index,paste(curr_leaf_n, collapse = ",")))
                  }
                  if (curr_leaf_n == 1L) {
                    current_leaf_lowers <- list_inter_mats[[tree_index]][,2L]
                    current_leaf_uppers <- list_inter_mats[[tree_index]][,3L]
                    z_leaf_ind <- which((Z.mat[t - 1L, indiv ] > current_leaf_lowers) & (Z.mat[t - 1L, indiv ] <= current_leaf_uppers) )
                    if(length(z_leaf_ind)!=1){stop("Line 1341. length(z_leaf_ind)!=1")}
                    z_leaf_ind <- z_leaf_ind[1]
                    z_leaf_lb <- current_leaf_lowers[z_leaf_ind]
                    z_leaf_ub <- current_leaf_uppers[z_leaf_ind]
                    admissible_lower <- max(admissible_lower,z_leaf_lb)
                    admissible_upper <- min(admissible_upper,z_leaf_ub)
                  }
                }

                if (!(admissible_lower < admissible_upper)) {
                  stop("Singleton-leaf constraints have an empty intersection.")
                }

                #calculate region probabilities

                # restricted_lower <- pmax(temp_region_logprobs[, 2L],admissible_lower)
                # restricted_upper <- pmin(temp_region_logprobs[, 3L],admissible_upper)
                # admissible_region <- is.finite(temp_region_logprobs[, 1L]) & restricted_lower < restricted_upper
                # old_z <- Z.mat[t, indiv]
                old_z <- Z.mat[t - 1L, indiv]

                if (!(old_z > admissible_lower && old_z <= admissible_upper)) {
                  stop(sprintf(paste0("Current latent lag %.17g is outside its derived ","admissible interval (%g, %g] at iteration %d, ","time %d, individual %d."),
                               old_z,admissible_lower,admissible_upper,iter,t,indiv))
                }
                # }

                region_lower <- Biglist_intersectmats[[t]][ k_t, 2L ]
                region_upper <- Biglist_intersectmats[[t]][k_t,3L]
                future_valid_lower <- max(region_lower,A_tmin1_lower,admissible_lower)
                future_valid_upper <- min(region_upper,A_tmin1_upper,admissible_upper)
                candidate_k_t_feasible <- future_valid_lower < future_valid_upper
                if(candidate_k_t_feasible){
                  # valid_k_t <- FALSE
                }else{
                  k_t <- sample.int(num_regions,size = 1,prob = w_BS_vec)
                  # valid_k_t <- TRUE
                }
              } # end while loop
            }




            backward_lower <- max(intersectmat_tp1[k_tp1, 2L],A_t_lower)
            backward_upper <- min(intersectmat_tp1[k_tp1, 3L],A_t_upper)
            if(reject_or_restrict_Z_props == "restrict"){
              backward_lower <- max(Ctilde_restrict_interval_list[[t+1L]][k_tp1, 1L],A_t_lower)
              backward_upper <- min(Ctilde_restrict_interval_list[[t+1L]][k_tp1, 2L],A_t_upper)
            }
            if (is.na(backward_lower) ||is.na(backward_upper) ||backward_lower >= backward_upper) {
              stop(sprintf(paste0("Invalid FFBS backward interval at iteration %d, ","z iteration %d, time %d, individual %d; ","current region=%d, next region=%d, interval=(%g,%g]."),
                           iter,z_iter_ind,t,indiv,k_t,k_tp1,backward_lower,backward_upper))
            }

            zdraw_temp <- rtruncnorm(n = 1,
                                     a = backward_lower, # max(intersectmat_tp1[k_tp1,2L], A_t_lower),
                                     b = backward_upper, # min(intersectmat_tp1[k_tp1,3L], A_t_upper),
                                     mean = (intersectmat[k_t,1L] + 0.5)*(max_resp - min_resp) + min_resp + genuine_offset_vec[current_obs_index],
                                     sd = 1)

            #### BEGIN edits for repeated sampling until lag update does not result in terminal node
            z_old_temp <- Z.mat[t, indiv ]
            Z.mat[t, indiv ] <- zdraw_temp
            Z.vec[(t-1)*num_indiv + indiv] <- zdraw_temp
            old_k_t <- k_path[t]
            old_k_tp1 <- k_tp1
            k_path[t] <- k_t
            k_tp1 <- k_t
            for(t2 in 1:num_lags){
              # init_Z_t0 <- rep(0, t2*num_indiv)
              # # init_Z_t0 <- rnorm(t*n.item*n.ranker)
              # Zlag.mat[,t2] <- c(init_Z_t0, Z.vec[1:((n.time_train-t2)*num_indiv)])
              # Xmat.train[,t2 + 1L] <- Zlag.mat[, t2]
              Xmat.train[t2*num_indiv + (t-1)*num_indiv + indiv,t2 + 1L] <- Z.vec[(t-1)*num_indiv + indiv]
            }

            check_lag_valid <- TRUE

            for(j in 1:num_lags){
              init_Z_t0 <- rep(0, j*num_indiv)
              Zlag.mat[,j] <- c(init_Z_t0, Z.vec[1:((n.time_train-j)*num_indiv)])
              if((sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) ){
                check_lag_valid <- FALSE
                if(reject_or_restrict_Z_props == "restrict"){
                  print("Line 1822 predictor update not accepted and reject_or_restrict_Z_props == restrict")
                  stop("Somehow made mistake in restricted sampling of latent variable")
                }
                Z.mat[t, indiv ] <- z_old_temp
                Z.vec[(t-1)*num_indiv + indiv] <- z_old_temp
                k_path[t] <- old_k_t
                k_tp1 <- old_k_tp1
                for(t2 in 1:num_lags){
                  # init_Z_t0 <- rep(0, t2*num_indiv)
                  # # init_Z_t0 <- rnorm(t*n.item*n.ranker)
                  # Zlag.mat[,t2] <- c(init_Z_t0, Z.vec[1:((n.time_train-t2)*num_indiv)])
                  # Xmat.train[,t2 + 1L] <- Zlag.mat[, t2]
                  Xmat.train[t2*num_indiv + (t-1)*num_indiv + indiv,t2 + 1L] <- Z.vec[(t-1)*num_indiv + indiv]
                }
                for(j2 in 1:j){
                  init_Z_t0 <- rep(0, j2*num_indiv)
                  Zlag.mat[,j2] <- c(init_Z_t0, Z.vec[1:((n.time_train-j2)*num_indiv)])
                  doublecheck <- (sampler$setPredictor(x = Zlag.mat[,j2], column = j2) == FALSE)
                  if(doublecheck){
                    stop("cannot roll back")
                  }
                }
                break
              }else{
                if(reject_or_restrict_Z_props == "reject" & t > 1L){
                  # find admissible region for z_{t-1}
                  # the z_t sample can imply that
                  # there is at least one leaf that now contains only z_{t-1}
                  # which impacts the acceptable regions
                  # The might or might not make the z_{t-1} sample impossible
                  # Must obtain admissible z_{t-1} region and check if
                  # a feasible range still remains
                  admissible_lag_lower <- -Inf
                  admissible_lag_upper <- Inf
                  # if(reject_or_restrict_Z_props == "restrict"){
                  affected_row_lag <- (t - 1L) * num_indiv + indiv

                  list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[affected_row_lag,-1L, drop = FALSE]))
                  all_trees_all_obs <- sampler$getTrees(chainNums = 1L, current = TRUE,newdata = Xmat.train[,-1L, drop = FALSE])
                  all_trees_current_obs <- sampler$getTrees(chainNums = 1L,current = TRUE,newdata = Xmat.train[affected_row_lag, -1L, drop = FALSE])
                  current_rows_by_tree <- split(seq_len(nrow(all_trees_current_obs)),all_trees_current_obs$tree)
                  all_rows_by_tree <- split(seq_len(nrow(all_trees_all_obs)),all_trees_all_obs$tree)
                  for (tree_index in seq_len(n.trees)) {
                    current_tree_rows <- current_rows_by_tree[[as.character(tree_index)]]# which(all_trees_current_obs$tree == tree_index)
                    nonemptyrow <- which(all_trees_current_obs$var[current_tree_rows] == -1L & all_trees_current_obs$n[current_tree_rows] == 1)
                    if(length(nonemptyrow)!=1){
                      stop("Line 1314. Current observation not assigned to exactly one leaf. length(nonemptyrow)!=1")
                    }
                    curr_leaf_n <- all_trees_all_obs$n[all_rows_by_tree[[as.character(tree_index)]]][nonemptyrow]
                    if (length(curr_leaf_n) != 1L ||is.na(curr_leaf_n) ||curr_leaf_n < 1L) {
                      stop(sprintf("Invalid current-leaf count in tree %d: %s.",tree_index,paste(curr_leaf_n, collapse = ",")))
                    }
                    if (curr_leaf_n == 1L) {
                      current_leaf_lowers <- list_inter_mats[[tree_index]][,2L]
                      current_leaf_uppers <- list_inter_mats[[tree_index]][,3L]
                      z_leaf_ind <- which((Z.mat[t - 1L, indiv ] > current_leaf_lowers) & (Z.mat[t - 1L, indiv ] <= current_leaf_uppers) )
                      if(length(z_leaf_ind)!=1){stop("Line 1341. length(z_leaf_ind)!=1")}
                      z_leaf_ind <- z_leaf_ind[1]
                      z_leaf_lb <- current_leaf_lowers[z_leaf_ind]
                      z_leaf_ub <- current_leaf_uppers[z_leaf_ind]
                      admissible_lag_lower <- max(admissible_lag_lower,z_leaf_lb)
                      admissible_lag_upper <- min(admissible_lag_upper,z_leaf_ub)
                    }
                  }
                  y_tmin1 <- y.train[t - 1L, indiv]
                  if (y_tmin1 == 1L) {
                    A_tmin1_lower <- 0
                    A_tmin1_upper <- Inf
                  } else {
                    A_tmin1_lower <- -Inf
                    A_tmin1_upper <- 0
                  }
                  region_lower <- Biglist_intersectmats[[t]][ k_t, 2L ]
                  region_upper <- Biglist_intersectmats[[t]][k_t,3L]
                  overall_lower <- max(admissible_lag_lower, A_tmin1_lower, region_lower)
                  overall_upper <- min(admissible_lag_upper, A_tmin1_upper, region_upper)
                  if(overall_lower < overall_upper){
                    check_lag_valid <- TRUE
                    Xmat.train[, j + 1L] <- Zlag.mat[, j]
                    if (!isTRUE(all.equal(as.numeric(Xmat.train[, j + 1L]),as.numeric(Zlag.mat[, j]),tolerance = 0,check.attributes = FALSE))) {
                      stop("Xmat.train and Zlag.mat disagree after predictor update.")
                    }
                    if (!isTRUE(all.equal(as.numeric(sampler$data@x[, j]),as.numeric(Zlag.mat[, j]),tolerance = 0,check.attributes = FALSE))) {
                      stop("dbarts predictor and Zlag.mat disagree after predictor update.")
                    }
                  }else{
                    check_lag_valid <- FALSE
                    if(reject_or_restrict_Z_props == "restrict"){
                      print("Line 1822 predictor update not accepted and reject_or_restrict_Z_props == restrict")
                      stop("Somehow made mistake in restricted sampling of latent variable")
                    }
                    Z.mat[t, indiv ] <- z_old_temp
                    Z.vec[(t-1)*num_indiv + indiv] <- z_old_temp
                    k_path[t] <- old_k_t
                    k_tp1 <- old_k_tp1
                    for(t2 in 1:num_lags){
                      # init_Z_t0 <- rep(0, t2*num_indiv)
                      # # init_Z_t0 <- rnorm(t*n.item*n.ranker)
                      # Zlag.mat[,t2] <- c(init_Z_t0, Z.vec[1:((n.time_train-t2)*num_indiv)])
                      # Xmat.train[,t2 + 1L] <- Zlag.mat[, t2]
                      Xmat.train[t2*num_indiv + (t-1)*num_indiv + indiv,t2 + 1L] <- Z.vec[(t-1)*num_indiv + indiv]
                    }
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
                }else{
                  check_lag_valid <- TRUE
                  Xmat.train[, j + 1L] <- Zlag.mat[, j]
                  if (!isTRUE(all.equal(as.numeric(Xmat.train[, j + 1L]),as.numeric(Zlag.mat[, j]),tolerance = 0,check.attributes = FALSE))) {
                    stop("Xmat.train and Zlag.mat disagree after predictor update.")
                  }
                  if (!isTRUE(all.equal(as.numeric(sampler$data@x[, j]),as.numeric(Zlag.mat[, j]),tolerance = 0,check.attributes = FALSE))) {
                    stop("dbarts predictor and Zlag.mat disagree after predictor update.")
                  }
                }
              }
            }
          }


        }

        for (t in seq_len(n.time_train - 1L)) {
          next_region <- k_path[t + 1L]
          lower <- Biglist_intersectmats[[t + 1L]][next_region, 2L]
          upper <- Biglist_intersectmats[[t + 1L]][next_region, 3L]
          z_t <- Z.mat[t, indiv]
          if (!(z_t > lower && z_t <= upper)) {
            stop(sprintf(paste0("FFBS region-path inconsistency at iteration %d, ","z iteration %d, time %d, individual %d: ","Z=%g, next region=%d, interval=(%g,%g]."),
                         iter,z_iter_ind,t,indiv,z_t,next_region,lower,upper))
          }
        }
        bad_binary <- which((y.train[, indiv] == 1L & Z.mat[, indiv] <= 0) |(y.train[, indiv] == 0L & Z.mat[, indiv] > 0))
        if (length(bad_binary) > 0L) {
          stop(sprintf(paste0("FFBS latent draw violates binary constraints at ","iteration %d, z iteration %d, individual %d, ","times=%s."),
                       iter,z_iter_ind,indiv,paste(bad_binary, collapse = ",")))
        }

        # } # end loop over items
      } # end loop over individuals indiv in 1:num_indiv
      if (!isTRUE(all.equal(Z.vec,as.vector(t(Z.mat)),tolerance = 0,check.attributes = FALSE))) {
        stop(sprintf("Z.vec and Z.mat disagree after FFBS at iteration %d.",iter))
      }
      # print("line 1288")
      # print("z_iter_ind = ")
      # print(z_iter_ind)
    } # end loop over z_iter_ind

    if (anyNA(Z.vec) ||any(!is.finite(Z.vec))) {
      stop(sprintf("Non-finite Z at Gibbs iteration %d.",iter),call. = FALSE)
    }

    max_abs_Z <- max(abs(Z.vec))

    if (max_abs_Z > 25) {stop(sprintf(paste0("Latent-state explosion at iteration %d: ","min(Z)=%.6g, max(Z)=%.6g, max|Z|=%.6g."),
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
    #     #   Zlag.mat[,t] <- c(init_Z_t0, as.vector(t(Z.mat))[1:((n.time-t)*n.item*n.ranker)])
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
    #     Z.mat <- Z.matold
    #     Z.vec <- as.vector(t(Z.mat))
    #
    #     # Zlag.mat <- matrix(NA, nrow = n.time*n.ranker*n.item, ncol = num_lags)
    #     #
    #     # for(t in 1:num_lags){
    #     #   init_Z_t0 <- rep(0, t*n.item*n.ranker)
    #     #   # init_Z_t0 <- rnorm(t*n.item*n.ranker)
    #     #
    #     #   Zlag.mat[,t] <- c(init_Z_t0, as.vector(t(Z.mat))[1:((n.time-t)*n.item*n.ranker)])
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
    #   # Xmat.train[, j + 1L] <- Zlag.mat[, j]
    #   for(j in 1:num_lags){
    #     Xmat.train[, j + 1L] <- Zlag.mat[, j]
    #   }
    # }
    stopifnot(isTRUE(all.equal(Z.vec,as.vector(t(Z.mat)),tolerance = 0)))
    breakcount <- 0
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
    # sampler$setResponse(y = as.vector(t(Z.mat)))
    # sampler$setResponse(y = as.vector(Z.vec))

    # offset_current <-
    #   Z_reference -
    #   Z.vec
    computational_offset <- Z_reference - Z.vec
    dbarts_offset <- computational_offset + genuine_offset_vec
    sampler$setOffset(offset = dbarts_offset,updateScale = FALSE)
    # min_resp <- min(Z.vec)
    # max_resp <- max(Z.vec)

    # if(sparse){
    #   tempmodel <- sampler$model
    #   tempmodel@tree.prior@splitProbabilities <- s_y
    #   sampler$setModel(newModel = tempmodel)
    # }
    if (!isTRUE(all.equal(as.numeric(sampler$data@y),as.numeric(Z_reference),tolerance = 0,check.attributes = FALSE))) {
      stop(sprintf("Fixed dbarts reference response changed at iteration %d.",iter),call. = FALSE)
    }
    # scalezero <- (0 - min_resp)/(max_resp - min_resp) - 0.5
    # scaled_offsetz_vec <- (offsetz_vec - min_resp)/(max_resp - min_resp) - 0.5

    # sampler$setSigma(sigma = 1)
    #sampler$setPredictor(x= df_for_dbart$x, column = 1, forceUpdate = TRUE)

    #mu = as.vector( alpha + X.mat %*% beta )
    samplestemp <- sampler$run(numBurnIn = 0L,numSamples = 1L)
    trees_after_run <- sampler$getTrees(chainNums = 1L,#sampleNums = 1L,
                                        current = TRUE)
    leaf_after_run <- trees_after_run$var == -1L
    if (any(trees_after_run$n[leaf_after_run] == 0L)) {
      stop(sprintf(paste0("Empty terminal node remains after dbarts run() ","at Gibbs iteration %d."),iter),call. = FALSE)
    }
    if (any(!is.finite(trees_after_run$value[leaf_after_run]))) {
      stop(sprintf(paste0("Non-finite terminal-node parameter after dbarts run() ","at Gibbs iteration %d."),iter),call. = FALSE)}
    if (anyNA(samplestemp$train) ||any(!is.finite(samplestemp$train))) {
      stop(sprintf("Non-finite dbarts fitted value at iteration %d.",iter),call. = FALSE)
    }

    f_train <- as.numeric(samplestemp$train[, 1L]) - dbarts_offset
    # f_train <- as.numeric(sampler$predict(x.test = as.matrix(Xmat.train[, -1L, drop = FALSE]),offset.test = NULL))
    mutemp <- genuine_offset_vec + f_train
    # mutemp <- as.numeric(sampler$predict(x.test = as.matrix(Xmat.train[, -1L, drop = FALSE]),offset.test = genuine_offset_vec))

    # mu_train <-as.numeric(fit$train[, 1L]) -computational_offset
    # mutemp <- samplestemp$train[,1]
    #suppose there are a number of samples
    # mutemp <- sampler$predict(df_for_dbart)
    # print("sigma = ")
    # print(samplestemp$sigma)
    mu = mutemp
    # if(sparse){
    #   tempcounts <- fcount(sampler$getTrees()$var)
    #   tempcounts <- tempcounts[tempcounts$x != -1, ]
    #   var_count_y <- rep(0, p_y)
    #   var_count_y[tempcounts$x] <- tempcounts$N
    # }

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
    #
    #   #mu = mutemp[(1:n.item)*n.ranker]
    # }else{
    #   if(nrow(X.train)==n.item*n.ranker*n.time){
    #     mu = mutemp
    #   }else{
    #     stop("nrow(X.train) not equal to n.item*n.ranker*n.time")
    #   }
    #
    # }

    ##################### Store iteration output ##################################################

    #remvove this? will be dsaved again later

    if(iter > n.burnin){
      # store value at this iteration
      if(keep_zmat==TRUE){
        # print("line 2230")
        draw$Z.mat[,,iter_min_burnin] = Z.mat
        # print("line 2234")
        draw$Z.mat.test[,,iter_min_burnin] = Z.mat.test
      }
      # draw$alpha[,iter_min_burnin] = alpha
      # draw$beta[,iter_min_burnin] = beta
      draw$mu[,,iter_min_burnin] = matrix(mu, nrow = num_indiv, ncol = n.time_train) # mu
      draw$prob.train.draws[,,iter_min_burnin] = pnorm(draw$mu[,,iter_min_burnin])
      # draw$sigma2.alpha[iter_min_burnin] = sigma2.alpha
      # draw$sigma2.beta[iter_min_burnin] = sigma2.beta
    }


    # if (sparse & (iter > floor(n.burnin * 0.5))) {
    #   # s_update_z <- update_s(var_count_z, p_z, alpha_s_z)
    #   # s_z <- s_update_z[[1]]
    #
    #   s_update_y <- update_s(var_count_y, p_y, alpha_s_y)
    #   s_y <- s_update_y[[1]]
    #
    #   if(alpha_split_prior){
    #     # alpha_s_z <- update_alpha(s_z, alpha_scale_z, alpha_a_z, alpha_b_z, p_z, s_update_z[[2]])
    #     alpha_s_y <- update_alpha(s_y, alpha_scale_y, alpha_a_y, alpha_b_y, p_y, s_update_y[[2]])
    #   }
    # }

    if( nrow(X.test) > 0 ){
      # print("line 2209")
      #
      # print("ncol(df_for_dbart_test) = ")
      # print(ncol(df_for_dbart_test))
      # print("nrow(df_for_dbart_test) = ")
      # print(nrow(df_for_dbart_test))
      temp_test_mat <- matrix(NA,nrow = num_indiv,ncol =  ncol(Xmat.test))

      for(t  in 1:num_lags){
        if(noise_in_pred ==1){
          # temp_test_mat[,t] <- as.vector(mu)[((n.time-t)*n.item*n.ranker+1):((n.time-t +1) *n.item*n.ranker)]
          temp_test_mat[,t] <- Z.vec[((n.time_train-t)*num_indiv+1):((n.time_train-t +1)*num_indiv)]
        }else{
          # temp_test_mat[,t] <- as.vector(t(Z.mat))[((n.time-t)*n.item*n.ranker+1):((n.time-t +1)*n.item*n.ranker)]
          # temp_test_mat[,t] <- as.vector(mu)[((n.time-t)*n.item*n.ranker+1):((n.time-t +1) *n.item*n.ranker)]
          temp_test_mat[,t] <- Z.vec[((n.time_train-t)*num_indiv+1):((n.time_train-t +1)*num_indiv)]
        }
      }
      # temp_test_mat[, (num_lags+1):ncol(Xmat.test)] <- as.matrix(Xmat.test[1:(n.item*n.ranker) ,
      #                                                                      (num_lags+1):ncol(Xmat.test)])

      temp_test_mat[, (num_lags+1):ncol(Xmat.test)] <- as.matrix(Xmat.test[1:(num_indiv) ,
                                                                           (num_lags+1):ncol(Xmat.test)])
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

        # temp_test_mat <- data.frame(#y = rep(NA, nrow(temp_test_mat)),
        #   x = temp_test_mat)
        #
        # colnames(temp_test_mat) <- colnames(df_for_dbart_test)
        #
        # # must use original column names to prevent an error in the predict function
        # # colnames(temp_test_mat) <- colnames(df_for_dbart_test)
        #
        # # print("Line 2200")
        # # print("t1 = ")
        # # print(t1)
        # # #
        # # #
        # #
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
        #
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
        #   intersectmat <- interNtreesB_cpp(list_inter_mats)
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
          treeexample1 <- sampler$getTrees(treeNums = i, chainNums = 1L, sampleNums = 1L#,
                                           # current = TRUE
          )
          if (anyNA(treeexample1$value) || any(!is.finite(treeexample1$value))) {
            stop(sprintf("Non-finite value in updated dbarts tree at iteration %d, tree %d.", iter, i), call. = FALSE)
          }
          stored_tree <- sampler$getTrees(treeNums = i,chainNums = 1L,sampleNums = 1L)
          current_tree <- sampler$getTrees(treeNums = i,chainNums = 1L,current = TRUE)
          if (nrow(stored_tree) != nrow(current_tree) ||!identical(stored_tree$var, current_tree$var) ||!isTRUE(all.equal(
            stored_tree$value,current_tree$value,tolerance = 0,check.attributes = FALSE))) {
            stop(sprintf(paste0("Stored and current tree structures disagree for ","tree %d at iteration %d."),
                         i,iter))
          }
          rebuilt_tree_list[[i]] <- rebuildTree2_cpp(as.matrix(treeexample1))
        }

        testpredvec <- rep(NA, num_indiv)

        for(indiv in 1:num_indiv){

          indiv_ind <- (t1 - 1)*num_indiv+indiv

          # list_inter_mats <- list()
          #
          # for(i in 1:n.trees){
          #
          #   # treeexample1 <- sampler$getTrees(treeNums = i,
          #   #                                  chainNums = 1,
          #   #                                  sampleNums = 1)
          #
          #   # rebuilt_tree <- rebuildTree2(treeexample1)
          #
          #   rebuilt_tree <- rebuilt_tree_list[[i]] # rebuildTree2_cpp(as.matrix(treeexample1))
          #
          #   #must use covariates for individual indiv at time period t
          #
          #   # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, Xmat.train[obs_indices[1],-1, drop = FALSE] )
          #
          #   # if num_lags > 1 this would have to be edited (just first column dropped)
          #
          #   list_inter_mats[[i]] <- getPredictionsRangesForTree3_cpp((rebuilt_tree),
          #                                                            as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]) )
          #
          # }
          # list_inter_mats <- getListintermats_cpp(rebuilt_tree_list , as.matrix(Xmat.train[indiv_ind,-1, drop = FALSE]))
          list_inter_mats <- getListintermats_cpp(rebuilt_tree_list ,as.matrix(temp_test_mat[indiv, , drop = FALSE]))
          intersectmat <- interNtreesB_cpp(list_inter_mats)
          # intersectmat <- getlistinter_create_intermat(rebuilt_tree_list ,as.matrix(temp_test_mat[indiv, , drop = FALSE]))

          # Biglist_intersectmats[[t]] <- intersectmat
          # obtain prediction from intersection matrix
          ktemp <- which((as.matrix(temp_test_mat[indiv,1])[1]  <= intersectmat[, 3]) )[1]
          # Then obtain the corresponding region mean value
          testpredvec[indiv] <- intersectmat[ktemp,1]
        }
        testpredvec <- (testpredvec + 0.5)*(max_resp - min_resp) + min_resp
        #fill in temp_test_preds with noise
        if(noise_in_pred ==1){
          temp_test_preds[ , t1] <- testpredvec + rnorm( num_indiv ) + offsetz_vec
        }else{
          temp_test_preds[ , t1] <- testpredvec + offsetz_vec #+ rnorm( num_indiv )
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
        temp_mu_test[(t1-1)*(num_indiv)  +  1:(num_indiv) ] <- testpredvec + offsetz_vec
      }
      #also update Zlag.mat.test ?
      #perhaps this is unnecessary here?
      # print("Line 5545")
      # Zlag.mat.test <- matrix(NA, nrow = num_test_periods*n.ranker*n.item, ncol = num_lags)
      Zlag.mat.test <- matrix(NA, nrow = num_test_periods*num_indiv, ncol = num_lags)
      # if(nrow(X.test) >0 ){
      for(t in 1:num_lags){
        if(num_test_periods > t ){
          # #up to t time periods ahead can be filled in. Rest of test time periods filled in using repeated values of last observation in training data
          # Zlag.mat.test[,t] <- c(Z.vec[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
          #                        rep(Z.vec[((n.time_train-1)*num_indiv+1):(n.time_train*num_indiv)],
          #                            (num_test_periods-t) )  )
          # # #other option is to set all unobservable values to zero
          # # Zlag.mat.test[,t] <- c(as.vector(t(Z.mat))[((n.time-t)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],
          # #                        rep(0,(num_test_periods-t)*n.item*n.ranker ))
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
      # }
      if(iter > n.burnin){
        draw$mu_test[,,iter_min_burnin] <- matrix(temp_mu_test, nrow = num_indiv, ncol = num_test_periods) # temp_mu_test
        draw$prob.test.draws[,,iter_min_burnin] <- pnorm(draw$mu_test[,,iter_min_burnin])
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


