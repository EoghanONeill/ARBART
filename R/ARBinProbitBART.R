


#' @export
fastnormdens <- function(x, mean = 0, sd = 0){
  (1/(sd*sqrt(2*pi)))*exp(-0.5*((x-mean)/sd)^2)
}



rebuildTree2 <- function(tree) {
  # Define a worker function that will be recursively called on every node.
  tree1 <- tree
  tree <- cbind(tree,
                rep(NA, nrow(tree)),
                rep(NA, nrow(tree)))

  colnames(tree) <- c(colnames(tree1), "lower", "upper")

  lower <- -Inf
  upper <- Inf
  rebuildTreeRecurse <- function(tree1, lower1, upper1, rowind) {
    # print('rowind = ')
    # print(rowind)
    node <- list(
      # value = tree1$value[1],
      # n = tree1$n[1],
      # lower = lower1,
      # upper = upper1,
      rowindtemp = rowind
    )
    # Check node if is a leaf, and if so return early.
    if (tree1$var[1] == -1) {
      node$n_nodes <- 1


      # node$lower <- lower1
      # node$upper <- upper1

      # tree$lower[rowind] <- lower1
      # tree$upper[rowind] <- upper1
      # print("adding lower and upper")

      tree[rowind,6] <<- lower1
      tree[rowind,7] <<- upper1

      # print(tree)

      return(node)
    }
    # node$var <- variableNames[tree1$var[1]]

    leftlower <- lower1

    # print("tree1$var[1]= ")
    # print(tree1$var[1])

    if(tree1$var[1] ==1){

      leftupper <- tree1$value[1]
      # print("in if statement leftupper = ")
      # print(leftupper)
    }else{
      leftupper <- upper1
    }

    # By removing the current row, we can recurse down the left branch.
    headOfLeftBranch <- tree1[-1,]

    # print("leftupper = ")
    # print(leftupper)
    #
    # print("enter left branch function")

    left <- rebuildTreeRecurse(headOfLeftBranch, leftlower, leftupper, node$rowindtemp +1)
    n_nodes.left <- left$n_nodes
    # left$n_nodes <- NULL
    # node$left <- left



    if(tree1$var[1] ==1){
      rightlower <- tree1$value[1]
    }else{
      rightlower <- lower1
    }
    rightupper <- upper1

    # The right branch is obtained by advancing past the left nodes.
    headOfRightBranch <- tree1[seq.int(2 + n_nodes.left, nrow(tree1)),]
    # print("enter right branch function")
    # print("rightlower = ")
    # print(rightlower)
    # print("rightupper = ")
    # print(rightupper)

    right <- rebuildTreeRecurse(headOfRightBranch, rightlower, rightupper, left$rowindtemp + 1)
    n_nodes.right <- right$n_nodes
    # right$n_nodes <- NULL
    # node$right <- right
    node$n_nodes <- 1L + n_nodes.left + n_nodes.right
    node$rowindtemp <- right$rowindtemp
    return(node)
  }
  # variableNames <- colnames(object$data@x)
  rowind <- 1
  result <- rebuildTreeRecurse(tree1, lower, upper, rowind)
  result$n_nodes <- NULL
  return(tree)
  # return(result)
}


##////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



getPredictionsRangesForTree3 <- function(tree, x) {
  # predictions <- rep(NA_real_, nrow(x))
  outputmat <-cbind(rep(NA_real_, nrow(x)),
                    rep(NA_real_, nrow(x)),
                    rep(NA_real_, nrow(x)))
  indices <- c(1)

  tree1 <- tree

  getPredictionsForTreeRecursive <- function(tree, go_bool) {
    # if (tree$var[1] == -1) {
    # print("tree = ")
    # print(tree)

    if (tree[1,4] == -1) {
      # Assigns in the calling environment by using <<-

      # print("tree$var[1] == -1, indices = ")
      # print(indices)
      #
      # print("outputmat = ")
      # print(outputmat)
      #
      # print("length(indices) = ")
      # print(length(indices))

      if(go_bool){
        # outputmat[indices,1] <<- tree$value[1]
        # outputmat[indices,2] <<- tree$lower[1]
        # outputmat[indices,3] <<- tree$upper[1]
        outputmat[indices,1] <<- tree[1,5]
        outputmat[indices,2] <<- tree[1,6]
        outputmat[indices,3] <<- tree[1,7]
        tempindices <- indices[1] + 1
        # print("tempindices = ")
        # print(tempindices)
        # print("indices[1] = ")
        # print(indices[1])
        indices[1] <<- tempindices

      }

      # print("indices = ")
      # print(indices)
      #
      # print("outputmat = ")
      # print(outputmat)

      return(1)
    }

    # print("tree = ")
    # print(tree)

    # if(tree$var[1] == 1){
    if (tree[1,4] == 1) {
      # add a row because there are two possible values of the latent variable lag

      # print("tree$var[1] == 1, indices = ")
      # print(indices)

      if(go_bool ){
        outputmat <<- rbind(outputmat,
                            c(NA,NA,NA))
      }

      # goesLeft <- x[indices, tree$var[1]] <= tree$value[1]
      headOfLeftBranch <- tree[-1, , drop = FALSE]

      # print("indices = ")
      #
      # print(indices)
      #
      # print("nrow(outputmat)) = ")
      #
      # print(nrow(outputmat))
      #
      # print("headOfLeftBranch = ")
      #
      # print(headOfLeftBranch)

      n_nodes.left <- getPredictionsForTreeRecursive(
        headOfLeftBranch, go_bool)

      # print("n_nodes.left = ")
      #
      # print(n_nodes.left)
      #
      # print("nrow(tree) = ")
      #
      # print(nrow(tree))

      headOfRightBranch <- tree[seq.int(2 + n_nodes.left, nrow(tree)), , drop = FALSE]


      # print("headOfRightBranch = ")
      #
      # print(headOfRightBranch)
      #
      # print("indices = ")
      #
      # print(indices)
      #
      #
      # print(" outputmat = ")
      #
      # print( outputmat)
      #
      # print("nrow(outputmat)) = ")
      #
      # print(nrow(outputmat))


      n_nodes.right <- getPredictionsForTreeRecursive(
        headOfRightBranch, go_bool)

      # if(length(indices) >0 ){
      #   n_nodes.right <- getPredictionsForTreeRecursive(
      #     headOfRightBranch, nrow(outputmat))
      # }else{
      #   n_nodes.right <- getPredictionsForTreeRecursive(
      #     headOfRightBranch, indices)
      # }

      return(1 + n_nodes.left + n_nodes.right)

    }
    # if(tree$var[1] > 1){
    # print("tree = ")
    # print(tree)

    if (tree[1,4] > 1) {
      # print("tree$var[1] > 1, indices = ")
      # print(indices)

      # goesLeft <- x[indices, tree$var[1]] <= tree$value[1]
      # goesLeft <- x[1, tree$var[1]] <= tree$value[1]

      # goesLeft <- x[1, tree$var[1]] <= tree$value[1]
      goesLeft <- x[1, tree[1,4] ]<= tree[1,5]
      headOfLeftBranch <- tree[-1, , drop = FALSE]


      # print("goesLeft & go_bool = ")
      #
      # print(goesLeft  & go_bool)

      n_nodes.left <- getPredictionsForTreeRecursive(
        headOfLeftBranch, goesLeft & go_bool)

      headOfRightBranch <- tree[seq.int(2 + n_nodes.left, nrow(tree)), , drop = FALSE]



      #
      # print("!goesLeft = ")
      #
      # print(!goesLeft)
      #
      # print("outputmat = ")
      # print(outputmat)
      #
      # print("headOfRightBranch = ")
      # print(headOfRightBranch)

      n_nodes.right <- getPredictionsForTreeRecursive(
        headOfRightBranch, (!goesLeft)& go_bool)
    }


    return(1 + n_nodes.left + n_nodes.right)
  }


  # print("nrow(x) = ")
  # print(nrow(x))

  getPredictionsForTreeRecursive(tree, TRUE)

  # getPredictionsForTreeRecursive(tree, seq_len(nrow(x)))

  return(outputmat)
}



##///////////////////////////////////////////////////////////////////////////////////////////////////////////



# All preds and Zlag intervals for one row of covariates ##########

getPredictionsRangesForTree2 <- function(tree, x) {
  # predictions <- rep(NA_real_, nrow(x))
  outputmat <-cbind(rep(NA_real_, nrow(x)),
                    rep(NA_real_, nrow(x)),
                    rep(NA_real_, nrow(x)))

  tree1 <- tree

  getPredictionsForTreeRecursive <- function(tree, indices) {
    if (tree$var[1] == -1) {
      # Assigns in the calling environment by using <<-

      # print("outputmat = ")
      # print(outputmat)
      #
      # print("length(indices) = ")
      # print(length(indices))

      if(length(indices)>0){
        outputmat[indices,1] <<- tree$value[1]
        outputmat[indices,2] <<- tree$lower[1]
        outputmat[indices,3] <<- tree$upper[1]
      }

      # print("indices = ")
      # print(indices)
      #
      # print("outputmat = ")
      # print(outputmat)

      return(1)
    }

    if(tree$var[1] == 1){
      # add a row because there are two possible values of the latent variable lag

      # print("tree$var[1] == 1, indices = ")
      # print(indices)

      if(length(indices) >0 ){
        outputmat <<- rbind(outputmat,
                            c(NA,NA,NA))
      }
      # goesLeft <- x[indices, tree$var[1]] <= tree$value[1]
      headOfLeftBranch <- tree[-1,]

      # print("indices = ")
      #
      # print(indices)
      #
      # print("nrow(outputmat)) = ")
      #
      # print(nrow(outputmat))

      n_nodes.left <- getPredictionsForTreeRecursive(
        headOfLeftBranch, indices)

      headOfRightBranch <- tree[seq.int(2 + n_nodes.left, nrow(tree)),]


      # print("indices = ")
      #
      # print(indices)
      #
      #
      # print(" nrow(outputmat) = ")
      #
      # print( nrow(outputmat))
      #
      # print("nrow(outputmat)) = ")
      #
      # print(nrow(outputmat))

      if(length(indices) >0 ){
        n_nodes.right <- getPredictionsForTreeRecursive(
          headOfRightBranch, nrow(outputmat))
      }else{
        n_nodes.right <- getPredictionsForTreeRecursive(
          headOfRightBranch, indices)
      }

      return(1 + n_nodes.left + n_nodes.right)

    }
    if(tree$var[1] > 1){
      # goesLeft <- x[indices, tree$var[1]] <= tree$value[1]
      goesLeft <- x[1, tree$var[1]] <= tree$value[1]
      headOfLeftBranch <- tree[-1,]


      # print("indices[goesLeft] = ")
      #
      # print(indices[goesLeft])

      n_nodes.left <- getPredictionsForTreeRecursive(
        headOfLeftBranch, indices[goesLeft])

      headOfRightBranch <- tree[seq.int(2 + n_nodes.left, nrow(tree)),]




      # print("indices[!goesLeft] = ")
      #
      # print(indices[!goesLeft])
      #
      # print("outputmat = ")
      # print(outputmat)
      #
      # print("headOfRightBranch = ")
      # print(headOfRightBranch)

      n_nodes.right <- getPredictionsForTreeRecursive(
        headOfRightBranch, indices[!goesLeft])
    }


    return(1 + n_nodes.left + n_nodes.right)
  }


  # print("nrow(x) = ")
  # print(nrow(x))

  getPredictionsForTreeRecursive(tree, seq_len(nrow(x)))
  return(outputmat)
}


inter2treesa <- function(intermat1, intermat2){


  #create a step function

  x1 <- c(-Inf, intermat1[,3])
  x2 <- c(-Inf, intermat2[,3])

  xs <- sort(unique(c(x1, x2)))

  y1 <- intermat1[,1]
  y2 <- intermat2[,1]



  ypredvec <- rep(NA, length(xs)-1)
  for(i in 2:length(xs)){

    tempval <- xs[i]
    tempinds <- which((x1 <tempval ))
    temp_ind1 <- tempinds[length(tempinds)]

    tempinds <- which((x2 <tempval ))

    temp_ind2 <- tempinds[length(tempinds)]

    ypredvec[i-1] <- y1[temp_ind1] + y2[temp_ind2]

  }

  newmat <- matrix(NA, nrow = length(xs)-1, ncol = 3)

  newmat[,1] <- ypredvec
  newmat[,2] <- xs[1:(length(xs) - 1)]
  newmat[,3] <- xs[2:(length(xs))]

  return(newmat)

}




interNtreesA <- function(inter_list){

  temp_mat <- inter_list[[1]]
  for(i in 2:length(inter_list)){

    temp_mat <- inter2treesa(temp_mat,inter_list[[i]] )

  }

  return(temp_mat)

}



interNtreesB <- function(inter_list){


  #create a step function
  xlist <- list()
  for(i in 1:length(inter_list)){
    xlist[[i]] <- c(-Inf, (inter_list[[i]])[,3])
  }

  # x1 <- c(-Inf, intermat1[,3])
  # x2 <- c(-Inf, intermat2[,3])

  xs <- sort(unique(unlist(xlist)))


  ylist <- list()

  for(i in 1:length(inter_list)){
    ylist[[i]] <-  (inter_list[[i]])[,1]
  }

  # y1 <- intermat1[,1]
  # y2 <- intermat2[,1]

  ypredvec <- rep(NA, length(xs)-1)

  # tempindvec <- rep(NA, length(inter_list))

  for(i in 2:length(xs)){


    tempval <- xs[i]

    ypredvec[i-1] <- 0
    for(j in 1:length(inter_list)){
      tempinds <- which((xlist[[j]] <tempval ))

      temp_ind <- tempinds[length(tempinds)]

      ypredvec[i-1] <- ypredvec[i-1] + (ylist[[j]])[temp_ind]
    }

    # tempinds <- which((x1 <tempval ))
    # temp_ind1 <- tempinds[length(tempinds)]
    #
    # tempinds <- which((x2 <tempval ))
    #
    # temp_ind2 <- tempinds[length(tempinds)]

    # ypredvec[i-1] <- y1[temp_ind1] + y2[temp_ind2]

  }

  newmat <- matrix(NA, nrow = length(xs)-1, ncol = 3)

  newmat[,1] <- ypredvec
  newmat[,2] <- xs[1:(length(xs) - 1)]
  newmat[,3] <- xs[2:(length(xs))]


  return(newmat)

}


#' Auto-regressive Probit BART Model without exogenous covariates
#'
#' Auto-regressive (of order one) Probit BART Model without exogenous covariates
#' @import truncnorm
#' @import mvtnorm
#' @import dbarts
#' @importFrom Rcpp evalCpp
#' @param y.train A vector (or matrix for panel data) of binary outcomes. Rows correspond to time periods, columns correspond to variables.
#' @param n.iter Number of iterations excluding burnin.
#' @param n.burnin Number of burnin iterations.#'
#' @param num_test_periods Number of future time periods to predict.
#' @param num_lags Number of lags of latent outcome to be included as potential splitting variables. Currently must be equal to 1.
#' @param indiv.offsets Include individual-specific offsets? Only relevant if yttrain contains more than one column. Currently not supported.
#' @param num_z_iters Number of Gibbs iterations for latent outcome z samples per overall MCMC iteration.
#' @param keep_zmat Boolean. If equal to TRUE output the draws of Zmat for training data and test data
#' @param noise_in_pred If equal to 1, keep noise in test prediction calculations.
#' @param n.trees (dbarts option) A positive integer giving the number of trees used in the sum-of-trees formulation.
#' @param n.chains (dbarts option) A positive integer detailing the number of independent chains for the dbarts sampler to use (more than one chain is unlikely to improve speed because only one sample for each call to dbarts).
#' @param n.threads  (dbarts option) A positive integer controlling how many threads will be used for various internal calculations, as well as the number of chains. Internal calculations are highly optimized so that single-threaded performance tends to be superior unless the number of observations is very large (>10k), so that it is often not necessary to have the number of threads exceed the number of chains.
#' @param printEvery (dbarts option)If verbose is TRUE, every printEvery potential samples (after thinning) will issue a verbal statement. Must be a positive integer.
#' @param printCutoffs (dbarts option) A non-negative integer specifying how many of the decision rules for a variable are printed in verbose mode
#' @param rngKind (dbarts option) Random number generator kind, as used in set.seed. For type "default", the built-in generator will be used if possible. Otherwise, will attempt to match the built-in generator’s type. Success depends on the number of threads.
#' @param rngNormalKind (dbarts option) Random number generator normal kind, as used in set.seed. For type "default", the built-in generator will be used if possible. Otherwise, will attempt to match the built-in generator’s type. Success depends on the number of threads and the rngKind
#' @param rngSeed (dbarts option) Random number generator seed, as used in set.seed. If the sampler is running single-threaded or has one chain, the behavior will be as any other sequential algorithm. If the sampler is multithreaded, the seed will be used to create an additional pRNG object, which in turn will be used sequentially seed the threadspecific pRNGs. If equal to NA, the clock will be used to seed pRNGs when applicable.
#' @param updateState (dbarts option) Logical setting the default behavior for many sampler methods with regards to the immediate updating of the cached state of the object. A current, cached state is only useful when saving/loading the sampler.
#' @param print.opt Print every print.optnumber of Gibbsa samples.
#' @return A list is returned containing the following elements:
#' \item{ydraws_train}{Samples from the posterior of binary outcomes for training observations.}
#' \item{ydraws_train}{Samples from the posterior of binary outcomes for test observations}
#' @useDynLib ARBART, .registration = TRUE
#' @export
ARProbitbartNOCovars_fullcond <- function(y.train = NULL,
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
                                          n.samples = 1L,
                                          n.thin = 1L,
                                          n.chains = 1L,
                                          n.threads = 1L,#guessNumCores(),
                                          printEvery = 100L,
                                          printCutoffs = 0L,
                                          rngKind = "default",
                                          rngNormalKind = "default",
                                          rngSeed = NA_integer_,
                                          updateState = TRUE,
                                          print.opt = 100L,
                                          seq_z_draws = 1
                                          ){



  ######### Check inputs ##################

  if(num_test_periods < 1){
    stop("num_test_periods must be >=1")
  }

  if(!is.integer(num_lags)){
    stop("num_lags must be an integer.")
  }
  if(num_lags <1){
    stop("num_lags must be an integer greater than or equal to 1.")
  }
  if(num_lags >1){
    stop("Code not yet written for num_lags > 1.")
  }

  #check that input values are binary 0 or 1.

  if(!(all(y.train %in% c(0,1)))){
    stop("Input y.train values must be 0 or 1.")
  }

  y.train <- as.matrix(y.train)


  num_indiv <- ncol(y.train)

  n.time_train <- nrow(y.train)

  num_total <- n.time_train*num_indiv

  # #length of mu is number of individuals in panel
  #
  # length_mu <- ncol(y.train)
  #
  # ## length_mu can be 1, so will need to check if dimensions of matrix/array objects are dropped anywhere


  ######## create output list to be filled in #########################

  draw = list(
    # Z.mat = array(NA, dim = c(n.item, n.ranker*n.time, n.iter)),
    #alpha = array(NA, dim = c(n.item, n.iter)),
    #beta = array(NA, dim = c(p.cov, n.iter)),
    #if the x values do not vary over rankers, then there will only be n.item unique x values
    mu = array(NA, dim = c(num_indiv, n.time_train, n.iter)),#,
    mu_test = array(NA, dim = c(num_indiv , num_test_periods , n.iter))
    #
    #
    #can have mu of dimension n.item*n.ranker to operationalize rnanker-specific mu values, then need to edit gibbs update of Z
    #mu = array(NA, dim = c(n.item*n.ranker, n.iter))#,
    # sigma2.alpha = rep(NA, n.iter),
    # sigma2.beta = rep(NA, n.iter)
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
  if(indiv.offsets == TRUE){
    num_ones_vec <- apply(y.train,2,sum)
    prop_ones_vec <- apply(y.train,2,mean)
    offsetz_vec <- rep(0,num_indiv) # qnorm(prop_ones_vec)

  }else{
    offsetz_vec <- rep(offsetz, num_indiv)
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
    if(indiv.offsets == FALSE){

      Z.mat[,y.train[,j]] <- rtruncnorm(sum(y.train[,j]), a= 0, b = Inf, mean = offsetz, sd = 1)
      Z.mat[,1-y.train[,j]] <- rtruncnorm(n.time_train - sum(y.train[,j]), a= -Inf, b = 0, mean = offsetz, sd = 1)

    }else{
      Z.mat[,y.train[,j]] <- rtruncnorm(num_ones_vec[j], a= 0, b = Inf, mean = offsetz_vec[j], sd = 1)
      Z.mat[,1-y.train[,j]] <- rtruncnorm(n.time_train - num_ones_vec[j], a= -Inf , b = 0, mean = offsetz_vec[j], sd = 1)
    }

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

  } # end j loop


  Z.mat.test =  matrix(rnorm(num_test_periods*num_indiv),
                       nrow = n.time_train,
                       ncol = num_indiv)


  # if num_indiv > 1, must vectorize for input to dbarts
  # the vectorized matrix might be more convenient to work with if each time period is a continguous block?
  Z.vec <- as.vector(t(Z.mat))
  Z.vec.test <- as.vector(t(Z.mat.test))


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

    Zlag.mat[,t] <- c(init_Z_t0, Z.vec[1:((n.time_train-t)*num_indiv)])

  }

  #it is also necessary to create initial values for the test data (???????)

  Zlag.mat.test <- matrix(NA, nrow = num_test_periods*num_indiv, ncol = num_lags)

  # if(nrow(X.test) >0 ){
  for(t in 1:num_lags){
    # if(t==1){
    #   #repeating the last period values
    #   Zlag.mat.test[,t] <- rep(as.vector(Z.mat)[((n.time-1)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],num_test_periods )
    #
    #   #other option is to set all unobservable values to zero
    #   # Zlag.mat.test[,t] <- c(as.vector(Z.mat)[((n.time-1)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],
    #   #                        rep(0,(num_test_periods-t)*n.item*n.ranker ))
    #
    #
    # }else{

    if(num_test_periods > t ){
      #up to t time periods ahead can be filled in. Rest of test time periods filled in using repeated values of last observation in training data

      Zlag.mat.test[,t] <- c(Z.vec[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
                             rep(Z.vec[((n.time_train-1)*num_indiv+1):(n.time_train*num_indiv)],
                                 (num_test_periods-t) )  )

      # #other option is to set all unobservable values to zero
      # Zlag.mat.test[,t] <- c(as.vector(Z.mat)[((n.time-t)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],
      #                        rep(0,(num_test_periods-t)*n.item*n.ranker ))

    }else{
      #nothing to fill in if num_test_periods <= t
      Zlag.mat.test[,t] <- c(Z.vec[((n.time_train-t)*num_indiv+1):((n.time_train-(t - num_test_periods) )*num_indiv)]  )

    }
  # }
  }
  # }


  df_for_dbart <- data.frame(y = as.vector(Z.vec), x = as.matrix(Zlag.mat) )

  control <- dbartsControl(updateState = updateState, verbose = FALSE,  keepTrainingFits = TRUE,
                           keepTrees = TRUE,
                           n.trees = n.trees,
                           n.burn = n.burn,
                           n.samples = n.samples,
                           n.thin = n.thin,
                           n.chains = n.chains,
                           n.threads = n.threads,
                           printEvery = printEvery,
                           printCutoffs = printCutoffs,
                           rngKind = rngKind,
                           rngNormalKind = rngNormalKind,
                           rngSeed = rngSeed)


  # print(colnames(Xmat.train))
  # print(colnames(Xmat.test))

  # print("begin dbarts")


  # if(nrow(X.test )==0){
  sampler <- dbarts(y ~ .,
                    data = df_for_dbart,
                    #test = Xmat.test,
                    control = control,
                    resid.prior = fixed(1),
                    sigma=1 #check if this is the correct approach for setting the variance to 1
  )

  # print("df_for_dbart = ")
  # print(df_for_dbart)
  #
  # print("sampler$data= ")
  # print(sampler$data)

  # print("sampler$state[[1]]@trees = ")
  # print(sampler$state[[1]]@trees)

  # print("Line 321")
  #set the response.
  #Check that 0 is a reasonable initial value
  #perhaps makes more sense to use initial values of Z
  sampler$setResponse(y = as.vector(Z.vec))
  # sampler$setSigma(sigma = 1)

  min_resp <- min(Z.vec)
  max_resp <- max(Z.vec)


  # scale offsets
  # scale zero

  scalezero <- (0 - min_resp)/(max_resp - min_resp) - 0.5
  scaled_offsetz_vec <- (offsetz_vec - min_resp)/(max_resp - min_resp) - 0.5



  #sampler$setPredictor(x= Xmat.train, column = 1, forceUpdate = TRUE)

  #mu = as.vector( alpha + X.mat %*% beta )
  sampler$sampleTreesFromPrior()
  samplestemp <- sampler$run()

  mutemp <- samplestemp$train[,1]
  #suppose there are a number of samples

  # print("sigma = ")
  # print(samplestemp$sigma)

  # mupreds <- sampler$predict(Xmat.train)

  mu <- mutemp


  # print("line 919")
  # print("sampler$data@x = ")
  # print(sampler$data@x)

  # print("Zlag.mat= ")
  # print(Zlag.mat)


  # print("df_for_dbart = ")
  # print(df_for_dbart)
  #
  # print("sample1$data= ")
  # print(sampler$data)
  #
  #
  # temptrees <- sampler$state[[1]]@trees
  # print("temptrees = ")
  # print(temptrees)
  #
  #
  # print("sampler$state[[1]]@trees = ")
  # print(sampler$state[[1]]@trees)
  #
  # print("sampler$state")
  # print(sampler$state)


  if(n.burnin == 0){
    if(keep_zmat==TRUE){
      draw$Z.mat[,,1] <- Z.mat
      draw$Z.mat.test[,,1] <- Z.mat.test
    }
    # draw$alpha[,1] = alpha
    # draw$beta[,1] = beta
    draw$mu[, , 1] <- matrix(mu, nrow = num_indiv, ncol = n.time_train)
    # draw$sigma2.alpha[1] = sigma2.alpha
    # draw$sigma2.beta[1] = sigma2.beta
  }

  # print("Line 954")


  df_for_dbart_test <- data.frame(#y = rep(NA, nrow(Zlag.mat.test)),
                                  x = Zlag.mat.test )

  # print("df_for_dbart_test = ")
  # print(df_for_dbart_test)
  #
  # print("Line 363")


  # should really instead marginalize out z_T for predictive distribution sample of Z_{T+1} as outlined in paper.



  temp_test_preds <- matrix(NA, nrow = num_indiv,ncol = num_test_periods)

  temp_mu_test <- rep(NA,  nrow(df_for_dbart_test) )

  # print("colnames(temp_test_mat) = ")
  # print("Line 376")

  # print(colnames(temp_test_mat))

  for(t1 in 1:num_test_periods){
    #produce a prediction

    # need to edit z_{t+1} predictions

    temp_test_mat <- data.frame(#y = rep(NA,num_indiv),
                                x = as.matrix(df_for_dbart_test[1:(num_indiv) , , drop=FALSE ]) ) #  matrix(NA, nrow = n.item*n.ranker,ncol = ncol(Xmat.test))

    colnames(temp_test_mat) <- colnames(df_for_dbart_test)

    # print("colnames(temp_test_mat) = ")
    # print(colnames(temp_test_mat) )
    #
    # print("(temp_test_mat) = ")
    # print((temp_test_mat) )
    #
    #
    #
    # print("t1 = ")
    # print(t1)
    # #
    # #
    # print("colnames(temp_test_mat) = ")
    # print(colnames(temp_test_mat))
    #
    # print("colnames(df_for_dbart) = ")
    # print(colnames(df_for_dbart))
    #
    #
    # print("class(df_for_dbart) = ")
    # print(class(df_for_dbart))
    #
    # print("df_for_dbart = ")
    # print(df_for_dbart)
    #
    # print("class(temp_test_mat) = ")
    # print(class(temp_test_mat))
    #
    # print("temp_test_mat = ")
    # print(temp_test_mat)
    #
    #
    # # temp_test_mat <- as.matrix(temp_test_mat)
    #
    # print("class(temp_test_mat) = ")
    # print(class(temp_test_mat))
    #
    # print("temp_test_mat = ")
    # print(temp_test_mat)
    #
    # print("colnames(df_for_dbart)")
    # print(colnames(df_for_dbart))
    #
    # print("colnames(temp_test_mat)")
    # print(colnames(temp_test_mat))
    #
    # testpredvec <- sampler$predict(x = df_for_dbart)
    #
    # print("testpredvec = ")
    # print(testpredvec)
    #

    # print("Line 1065")

    if(num_indiv == 1){

      # print("min(Z.vec) = ")
      # print(min(Z.vec))
      # print("max(Z.vec) = ")
      # print(max(Z.vec))
      # print("Z.vec[length(Z.vec)] = ")
      # print(Z.vec[length(Z.vec)])

      tempbind <- as.matrix(rbind(temp_test_mat,temp_test_mat))

      # print("tempbind = ")
      # print(tempbind)
      #
      # print("sampler$state[[1]]@trees = ")
      # print(sampler$state[[1]]@trees)
      #
      # print("sampler$state")
      # print(sampler$state)


      # print("num_indiv = ")
      # print(num_indiv)
      #
      # print("sampler$getTrees() = ")
      # print(sampler$getTrees())

      # sampler$predict(x.test = as.matrix(Z.vec[100]), offset.test = NULL)[1:num_indiv]
      #
      # testpredvec <- sampler$predict(x.test = as.matrix(rep(tempbind[1,1],100)), offset.test = NULL)[1:num_indiv]
      #
      # print("line 1078 testpredvec from predict = ")
      # print(testpredvec)

      list_inter_mats <- list()

      for(i in 1:n.trees){
        # print("Line 492")
        treeexample1 <- sampler$getTrees(treeNums = i,
                                         chainNums = 1,
                                         sampleNums = 1)
        # print("Line 496")
        # print("treeexample1 = ")
        # print(treeexample1)
        # # rebuilt_tree <- rebuildTree2(treeexample1)
        rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
        # print("Line 499")
        #
        #must use covariates for individual indiv at time period t

        # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, as.matrix(df_for_dbart$x[obs_indices[1]]) )

        # list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree$var == -1 , 5:7]
        list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree[,4] == -1 , 5:7, drop = FALSE]
        # print("Line 507")

      }

      intersectmat <- interNtreesB(list_inter_mats)

      kpredtemp <- which((as.matrix(tempbind[1,])[1] < intersectmat[, 3]) )[1]
      # Then obtain the corresponding region mean value

      testpredvec <- as.matrix(intersectmat[kpredtemp,1])

      testpredvec <- (testpredvec + 0.5)*(max_resp - min_resp) + min_resp

      # print("line 1112 testpredvec from mat rescaled  = ")
      # print(testpredvec)
    }else{
      testpredvec <- sampler$predict(x.test = as.matrix(temp_test_mat), offset.test = NULL)
    }


    # print("Line 1061")


    #fill in temp_test_preds with noise
    # print("testpredvec = ")
    # print(testpredvec)
    #
    #
    # print("nrow(temp_test_preds) = ")
    # print(nrow(temp_test_preds))
    # print("ncol(temp_test_preds) = ")
    # print(ncol(temp_test_preds))

    if(noise_in_pred==1){
      temp_test_preds[ , t1] <- testpredvec + rnorm( num_indiv )
    }else{
      temp_test_preds[ , t1] <- testpredvec
    }


    # print("Line 963")

    #update temp_test_mat
    #shift z columns to the right and fill in leftmost column

    #need to rewrite this if want to allow for no observed covariates

    if(num_lags ==1){
      temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] ,
                                                as.matrix(df_for_dbart_test[(t1-1)*(num_indiv)  +  1:(num_indiv), c(-1), drop=FALSE ] )))

    }else{
      print("num lags not equal to 1")
      temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] , temp_test_mat[,1:(num_lags-1)] ,
                                                as.matrix(df_for_dbart_test[(t1-1)*(num_indiv)  +  1:(num_indiv),c(-1), drop=FALSE ] )))

    }

    if(t1 == num_test_periods){
      temp_test_mat <- data.frame(#y = rep(NA,num_indiv),
                                  x = as.matrix(df_for_dbart_test[1:(num_indiv) , , drop=FALSE ]) ) #  matrix(NA, nrow = n.item*n.ranker,ncol = ncol(Xmat.test))
      colnames(temp_test_mat) <- colnames(df_for_dbart_test)
    }



    # print("Line 980")
    # print("colnames(temp_test_mat) = ")
    # print(colnames(temp_test_mat))
    #
    # print("colnames(df_for_dbart) = ")
    # print(colnames(df_for_dbart))
    #
    # print("colnames(df_for_dbart_test) = ")
    # print(colnames(df_for_dbart_test))

    colnames(temp_test_mat) <- colnames(df_for_dbart_test)

    # print("colnames(temp_test_mat) = ")
    # print(colnames(temp_test_mat))


    #fill in temp_mu_test without noise
    temp_mu_test[(t1-1)*(num_indiv)  +  1:(num_indiv) ] <- testpredvec
  }


  # print("Line 427")


  # can re-draw Z.mat and Z.mat.test here

  # and redefine z mat test lags?

  if(n.burnin == 0){
    draw$mu_test[,,1] <- matrix(temp_mu_test, nrow = num_indiv, ncol = num_test_periods) #temp_mu_test

    # draw$mu_test[,1] <- samplestemp$test[,1]
    if(keep_zmat==TRUE){
      draw$Z.mat.test[,,1]  <- matrix(data = as.vector(temp_test_preds), nrow = num_indiv, ncol = num_test_periods)
    }
  }
  # print("Line 442")


  ##################### Begin Gibbs sampler ##################################################
  #//////////////////////////////////////////////////////////////////////////
  ## Gibbs iteration

  iter <- 2
  breakcount <- 0


  # for(iter in 2:(n.burnin + n.iter)){
  while( iter <= (n.burnin + n.iter) ){


    # print("Line 454")

    Z.matold <- Z.mat


    ########### Sample from full conditional distribution.



    #################### calculate intersection matrices ########################################

    # since there are no covariates, the intersection matrices do not vary across time or individuals

    #### obtain intersection matrices for time period t = 1 ###############




    # create intersection matrices


    intersectmat_tmin1 <- NA
    list_item_intersectmats_tmin1 <- NA

    # create vector of indices for ranker indiv in time period 1

    # this part is not really necessary
    # ind_start <- (1 - 1)*n.ranker*n.item+n.item*(indiv-1) + 1
    # ind_end <- (1 - 1)*n.ranker*n.item+n.item*indiv
    # obs_indices <- ind_start:ind_end

    # obs_indices[1] could jsut be replaced by [1] below\
    # because the only variable is zlag, so the other covariates are not used

    # print("Line 488")
    list_inter_mats <- list()

    for(i in 1:n.trees){
      # print("Line 492")
      treeexample1 <- sampler$getTrees(treeNums = i,
                                       chainNums = 1,
                                       sampleNums = 1)
      # print("Line 496")
      # print("treeexample1 = ")
      # print(treeexample1)
      # rebuilt_tree <- rebuildTree2(treeexample1)
      rebuilt_tree <- ARBART:::rebuildTree2_cpp(as.matrix(treeexample1))
      # print("Line 499")

      #must use covariates for individual indiv at time period t

      # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, as.matrix(df_for_dbart$x[obs_indices[1]]) )

      # list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree$var == -1 , 5:7]
      list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree[,4] == -1 , 5:7, drop = FALSE]
      # print("Line 507")

    }


    # print("Line 510")

    intersectmat <- interNtreesB(list_inter_mats)

    # print("Line 1146")

    intersectmat <- cbind(intersectmat, rep(NA, nrow(intersectmat)))

    # print("Line 518")


    # calculate one dimensional integrals
    for(rowind in 1:nrow(intersectmat)){
      # ktemp <- nkt_mat[rowind,k_index]
      # tempmean <- intersectmat[ktemp,1]
      templower <- intersectmat[rowind,2]
      tempupper <- intersectmat[rowind,3]

      # ASSUMING PRIOR MEAN ALL ZEROS

      # These are the q0 integrals
      intersectmat[rowind, 4] <- pnorm(tempupper, mean = 0, sd = 1) - pnorm(templower, mean = 0, sd = 1)
      # tempintegralval <- tempintegralval*onedim_int
    }

    intersectmat_tmin1 <- intersectmat



    num_regions <- nrow(intersectmat)

    # print("Line 1169")

    for(z_iter_ind in 1:num_z_iters){


      for(indiv in 1:num_indiv){

      # print("indiv = ")
      # print(indiv)
      ########### calculate qkt  ########################################

      ########### calculate qkt   for t = 1 ########################################


      ### calculate qkt integrals for time period t = 1  ################

      # These integrals are already calculated and saved as intersectmat[ktemp,4]

      # for(item_ind in 1:n.item){

        #QUESTION: LOOP OVER ITEMS THEN TIME OR TIME THEN ITEMS?

        # loop over items

        # Now loop over time periods

        # special case for t=1

        # probability matrix for sampling elements

        # let rows be period zero, and columns be period 1 (2?)

        probmattemp <- matrix(0,
                              nrow = num_regions,
                              ncol = num_regions)

        # loop over period 2 regions into which z_1 can fall


        tempbounds <- matrix(NA,
                             nrow = num_regions,
                             ncol = 2)

        y_tp1 <- y.train[2,indiv]


        # Trunc norm prob of next periods latent value conditional on region

        # Create intervals from interval t+1 latent values

        # rankvec_tp1 <- ranks_mat[,  1*n.ranker + indiv]

        # inds for j ranked below i in t+1



        # belowrank_ind <- which(rankvec_tp1 == rankvec_tp1[item_ind] - 1 )

        #max of latent variables for j ranked below i in t+1
        # Z.mat

        # if(length(belowrank_ind) ==0){
        #   temp_lower <- -Inf
        # }else{
        #   # Check that this is the period t+1 latent variable value, not period t
        #   temp_lower <- as.vector(Z.mat)[1*n.item*n.ranker+
        #                                    n.item*(indiv - 1) +
        #                                    belowrank_ind]
        # }

        # inds for j ranked above i in t+1

        # aboverank_ind <- which(rankvec_tp1 == rankvec_tp1[item_ind] + 1)
        #
        # #min of latent variables for j ranked below i in period t+1
        #
        # if(length(aboverank_ind) ==0){
        #   temp_upper <- Inf
        # }else{
        #   # Check that this is the period t+1 latent variable value, not period t
        #
        #   temp_upper <- as.vector(Z.mat)[1*n.item*n.ranker+
        #                                    n.item*(indiv - 1) +
        #                                    aboverank_ind]
        # }


        # z in period 2 for individual indiv

        temp_ztp1 <- Z.vec[1*num_indiv +
                             indiv]


        y_t <- y.train[1,indiv]

        # rankvec_t <- ranks_mat[,  (1-1)*n.ranker + indiv]

        # if(any(order(rankvec_t) !=
        #    order(as.vector(Z.mat)[(1-1)*n.item*n.ranker +
        #                           n.item*(indiv-1) +
        #                           1:n.item])) ){
        #
        #   # print("order(rankvec_t) = ")
        #   # print(order(rankvec_t))
        #   #
        #   # print("order(as.vector(Z.mat)[(1-1)*n.item*n.ranker +
        #   #                         n.item*(indiv - 1) +
        #   #                         1:n.item])  = ")
        #
        #   print(order(as.vector(Z.mat)[(1-1)*n.item*n.ranker +
        #                                  n.item*(indiv - 1) +
        #                                  1:n.item]) )
        #
        #   # print("as.vector(Z.mat)[(1-1)*n.item*n.ranker +
        #   #                                n.item*(indiv - 1) +
        #   #                                1:n.item] = ")
        #
        #   print(as.vector(Z.mat)[(1-1)*n.item*n.ranker +
        #                            n.item*(indiv - 1) +
        #                            1:n.item])
        #
        # }

        # inds for j ranked below i in t

        # belowrank_ind <- which(rankvec_t == rankvec_t[item_ind] - 1 )
        #
        # #max of latent variables for j ranked below i in t
        # # Z.mat
        #
        # if(length(belowrank_ind) ==0){
        #   temp_lower3 <- -Inf
        # }else{
        #   temp_lower3 <- as.vector(Z.mat)[(1-1)*n.item*n.ranker +
        #                                     n.item*(indiv - 1) +
        #                                     belowrank_ind]
        # }
        #
        # # inds for j ranked above i in t
        #
        # aboverank_ind <- which(rankvec_t == rankvec_t[item_ind] + 1)
        #
        # #min of latent variables for j ranked below i in period t
        #
        # if(length(aboverank_ind) ==0){
        #   temp_upper3 <- Inf
        # }else{
        #   temp_upper3 <- as.vector(Z.mat)[(1-1)*n.item*n.ranker +
        #                                     n.item*(indiv - 1) +
        #                                     aboverank_ind]
        # }


        if(y_t == 1){
          temp_lower3 <- 0 # scalezero # 0
          temp_upper3 <- Inf
        }else{
          temp_lower3 <- -Inf
          temp_upper3 <- 0 # scalezero # 0
        }


        # probabilities of current period 2 z values for each possible period 2 region
        # in which the period 1 latent variable can be located.
        # These will be used as part of the z1 weight calculations



        # tempzfordens <- (temp_ztp1 + offsetz_vec[indiv] - min_resp)/(max_resp - min_resp) - 0.5

        # tempmeanfordens <- (intersectmat[1:num_regions, 1] + offsetz_vec[indiv] - min_resp)/(max_resp - min_resp) - 0.5

        tempmeanfordens <- (intersectmat[1:num_regions, 1] + 0.5)*(max_resp - min_resp) + min_resp

        # must account for offset

        temp_tnorm_probvec <- fastnormdens(temp_ztp1,
                                           mean = tempmeanfordens + offsetz_vec[indiv],
                                           sd = 1)

        # temp_tnorm_probvec <- fastnormdens(tempzfordens,
        #                                    mean = intersectmat[1:num_regions, 1]+
        #                                      scaled_offsetz_vec[indiv],
        #                                    sd = 1)

        # temp_tnorm_probvec <- fastnormdens(temp_ztp1,
        #                                    mean = intersectmat[1:num_regions, 1]+
        #                                      offsetz_vec[indiv],
        #                                    sd = 1)

        # loop over k_i2 for a particular i

        for(k_ind in 1:num_regions){
          # obtain mean for truncated normal distribution
          # temp_mean <- intersectmat[k_ind, 1]

          # want trunc norm probability of latent variable value for item_ind
          # in period t+1

          # temp_tnorm_prob <- dtruncnorm(temp_ztp1,
          #                               a=temp_lower,
          #                               b=Inf,
          #                               mean = temp_mean,
          #                               sd = 1)

          # temp_tnorm_prob <- fastnormdens(temp_ztp1,
          #                          mean = temp_mean,
          #                          sd = 1)

          temp_tnorm_prob <- temp_tnorm_probvec[k_ind]

          # temp_mean <- intersectmat[k_ind, 1]

          # now second term


          temp_lower2 <- intersectmat[k_ind, 2]
          temp_upper2 <- intersectmat[k_ind, 3]


          if((temp_lower2 > temp_upper3) | (temp_lower3 > temp_upper2)){
            # intervals do not overlap, therefore assign probability zero
            # and skip to next iteration

            # print("k_ind = ")
            # print(k_ind)

            # print("ncol(temp_region_probs) = ")
            # print(ncol(temp_region_probs))

            # print("nrow(temp_region_probs) = ")
            # print(nrow(temp_region_probs))


            # these three lines are technically unnecessary
            probmattemp[, k_ind] <- rep(0,num_regions)
            tempbounds[k_ind, 1] <- NA
            tempbounds[k_ind, 2] <- NA

            next

          }


          # temp_lower2 <- max(temp_lower2, temp_lower3)
          # temp_upper2 <- min(temp_upper2, temp_upper3)

          # if(temp_lower2 > temp_upper2){
          #
          #   print("as.vector(Z.mat)[(1-1)*n.item*n.ranker +
          #                                   n.item*(indiv - 1) +
          #                                   1:n.item]")
          #
          #   print(as.vector(Z.mat)[(1-1)*n.item*n.ranker +
          #                            n.item*(indiv - 1) +
          #                            1:n.item])
          #
          #   print("item_ind = ")
          #   print(item_ind)
          #
          #   print("rankvec_t = ")
          #   print(rankvec_t)
          #
          #   stop("Line 1581 temp_lower2 > temp_upper2")
          # }

          # print("Line 790")
          # loop over k1

          for(k0_ind in 1:num_regions){

            #loop over all possible means
            temp_mean2 <- intersectmat[k0_ind,1]

            # probability of being in intersection region

            # prob_t_region <- pnorm(temp_upper2 - temp_mean2) - pnorm(temp_lower2 - temp_mean2)


            # probmattemp[k0_ind, k_ind] <- prob_t_region*
            #   temp_tnorm_prob *
            #   intersectmat[k0_ind,4]

            # probabilities for k1 and k2 combinations

            probmattemp[k0_ind, k_ind] <- temp_tnorm_prob * intersectmat[k0_ind,4]

            if(probmattemp[k0_ind, k_ind] < 0){
              # print("probmattemp[k0_ind, k_ind] = ")
              # print(probmattemp[k0_ind, k_ind])

              # print("prob_t_region = ")
              # print(prob_t_region)

              # print("temp_tnorm_prob = ")
              # print(temp_tnorm_prob)
              #
              # print("intersectmat[k0_ind,4] = ")
              # print(intersectmat[k0_ind,4])
              #
              # print("temp_upper2 = ")
              # print(temp_upper2)
              #
              # print("temp_lower2 = ")
              # print(temp_lower2)
              #
              # print("temp_mean2 = ")
              # print(temp_mean2)


            }


          } # end loop over k0

          # save upper and lower bounds (mean saved in intersectmat)
          # or just obtain again later

          tempbounds[k_ind,1] <- max(temp_lower3,temp_lower2)
          tempbounds[k_ind,2] <- min(temp_upper3,temp_upper2)

          # if(y_t==1){
          #   tempbounds[k_ind,1] <- max(0,temp_lower2)
          #   tempbounds[k_ind,2] <- temp_upper2
          # }else{
          #   tempbounds[k_ind,1] <- temp_lower2
          #   tempbounds[k_ind,2] <- min(0,temp_upper2)
          # }

        } # end loop over k1

        # sample a combination of k0 and k1
        # if necessary can use column sums to sample k1, then k0
        # however, this is probably unnecessary


        # print("Line 1621 before sample")

        region_ind <- sample.int((num_regions^2),
                                 size = 1,
                                 replace = TRUE,
                                 prob = as.vector(probmattemp))


        # print("Line 1629 after sample")

        # k0 region is sampled number modulo number of regions
        k0_region_ind <- (region_ind - 1) %% num_regions + 1
        # if(k0_region_ind ==0){
        #   k0_region_ind <- num_regions
        # }

        # k1 region is the ceiling of sampled number divided by number of regions
        # k1_region_ind <- ceiling(region_ind/num_regions)
        k1_region_ind <- (region_ind - 1) %/% num_regions + 1


        # print("k1_region_ind = ")
        # print(k1_region_ind)

        temp_lower2 <- tempbounds[k1_region_ind,1]
        temp_upper2 <- tempbounds[k1_region_ind,2]

        # print("num_regions = ")
        # print(num_regions)
        #
        # print("region_ind = ")
        # print(region_ind)
        #
        # print("k0_region_ind = ")
        # print(k0_region_ind)

        temp_mean0 <- intersectmat[k0_region_ind, 1] # +offsetz_vec[indiv]

        temp_mean0 <- (temp_mean0 + 0.5)*(max_resp - min_resp) + min_resp

        # temp_lower2 <- (temp_lower2 + 0.5)*(max_resp - min_resp) + min_resp
        # temp_upper2 <- (temp_upper2 + 0.5)*(max_resp - min_resp) + min_resp

        # print("temp_mean0 = ")
        # print(temp_mean0)
        #
        # print("temp_lower2 = ")
        # print(temp_lower2)
        #
        # print("temp_upper2 = ")
        # print(temp_upper2)

        zdraw_temp <- rtruncnorm(n = 1,
                                 a = temp_lower2,
                                 b = temp_upper2,
                                 mean = temp_mean0 + offsetz_vec[indiv],
                                 sd = 1)

        # zdraw_temp <- (zdraw_temp + 0.5)*(max_resp - min_resp) + min_resp

        # z draw for time 1 individual indiv

        Z.mat[1,  indiv ] <- zdraw_temp

        # print("Z.vec[(1-1)*num_indiv + indiv] = ")
        # print(Z.vec[(1-1)*num_indiv + indiv])
        #
        # print("zdraw_temp = ")
        # print(zdraw_temp)

        Z.vec[(1-1)*num_indiv + indiv] <- zdraw_temp

        # Z.vec <- as.vector(t(Z.mat))
        # print("Line 918")
        # loop over time periods for general case 1 < t < T

        for(t in 2:(n.time_train - 1)){

          temp_ztpmin1 <- Z.vec[(t-2)*num_indiv +
                                             indiv]

          # must find mean corresponding to z in period t-1
          # This will be used in and after the loop over regions.
          # can directly obtain from dbarts
          # or find region
          # and use already saved region mean values


          # must find last lower bound that temp_ztpmin1 is greater than
          # or first upper bound that temp_ztpmin1 is below
          ktemp_tmin1 <- which((temp_ztpmin1 < intersectmat[, 3]) )[1]
          # Then obtain the corresponding region mean value
          temp_mean2 <- intersectmat[ktemp_tmin1,1]

          # print("temp_mean2 = ")
          # print(temp_mean2)
          #
          # print("ktemp_tmin1 = ")
          # print(ktemp_tmin1)



          # Calculate the probabilities for each region in this time period
          # the regions being looped over are actually period t+1 regions

          # Same regions for all time periods if there are no time varying covariates

          # However, the weights are individual and time period specific

          # loop through regions


          # are these region probabilities used anywhere ?

          # first column is the probabilities
          # second column is the lower bounds
          # third column is the upper bounds
          temp_region_probs <- matrix(0,
                                      nrow = nrow(intersectmat),
                                      ncol = 3)

          # Trunc norm prob of next periods latent value conditional on region

          # Create intervals from interval t+1 latent values

          # rankvec_tp1 <- ranks_mat[,  (t)*n.ranker + indiv]
          #
          # # inds for j ranked below i in t+1
          #
          # belowrank_ind <- which(rankvec_tp1 == rankvec_tp1[item_ind] - 1 )
          #
          # #max of latent variables for j ranked below i in t+1
          # # Z.mat
          #
          # if(length(belowrank_ind) ==0){
          #   temp_lower <- -Inf
          # }else{
          #   # Check that this is the period t+1 latent variable value, not period t
          #   temp_lower <- as.vector(Z.mat)[t*n.item*n.ranker+
          #                                    n.item*(indiv - 1) +
          #                                    belowrank_ind]
          # }
          #
          # # inds for j ranked above i in t+1
          #
          # aboverank_ind <- which(rankvec_tp1 == rankvec_tp1[item_ind] + 1)
          #
          # #min of latent variables for j ranked below i in period t+1
          #
          # if(length(aboverank_ind) ==0){
          #   temp_upper <- Inf
          # }else{
          #   # Check that this is the period t+1 latent variable value, not period t
          #
          #   temp_upper <- as.vector(Z.mat)[t*n.item*n.ranker+
          #                                    n.item*(indiv - 1) +
          #                                    aboverank_ind]
          # }


          # want trunc norm probability of latent variable value for item_ind
          # in period t+1

          temp_ztp1 <- Z.vec[t*num_indiv +
                               indiv]

          # temp_ztp1 <- as.vector(Z.mat)[t*n.item*n.ranker+
          #                                 n.item*(indiv - 1) +
          #                                 item_ind]
          #
          # rankvec_t <- ranks_mat[,  (t-1)*n.ranker + indiv]

          y_t <- y.train[t,indiv]



          # if(any(order(rankvec_t) !=
          #    order(as.vector(Z.mat)[(t-1)*n.item*n.ranker +
          #                           n.item*(indiv - 1) +
          #                           1:n.item]) )){
          #
          #   print("order(rankvec_t) = ")
          #   print(order(rankvec_t))
          #
          #   print("order(as.vector(Z.mat)[(1-1)*n.item*n.ranker +
          #                         n.item*(indiv - 1) +
          #                         1:n.item])  = ")
          #
          #   print(order(as.vector(Z.mat)[(t-1)*n.item*n.ranker +
          #                                  n.item*(indiv - 1) +
          #                                  1:n.item]) )
          #
          #   print("as.vector(Z.mat)[(t-1)*n.item*n.ranker +
          #                                n.item*(indiv - 1) +
          #                                1:n.item] = ")
          #
          #   print(as.vector(Z.mat)[(t-1)*n.item*n.ranker +
          #                            n.item*(indiv - 1) +
          #                            1:n.item])
          #
          #
          # }





          # inds for j ranked below i in t

          # belowrank_ind <- which(rankvec_t == rankvec_t[item_ind] - 1 )
          #
          # #max of latent variables for j ranked below i in t
          # # Z.mat
          #
          # if(length(belowrank_ind) ==0){
          #   temp_lower3 <- -Inf
          # }else{
          #   temp_lower3 <- as.vector(Z.mat)[(t-1)*n.item*n.ranker +
          #                                     n.item*(indiv - 1) +
          #                                     belowrank_ind]
          # }
          #
          # # inds for j ranked above i in t
          #
          # aboverank_ind <- which(rankvec_t == rankvec_t[item_ind] + 1)
          #
          # #min of latent variables for j ranked below i in period t
          #
          # if(length(aboverank_ind) ==0){
          #   temp_upper3 <- Inf
          # }else{
          #   temp_upper3 <- as.vector(Z.mat)[(t-1)*n.item*n.ranker +
          #                                     n.item*(indiv - 1) +
          #                                     aboverank_ind]
          # }



          if(y_t == 1){
            temp_lower3 <- 0 # scalezero # 0
            temp_upper3 <- Inf
          }else{
            temp_lower3 <- -Inf
            temp_upper3 <- 0 # scalezero # 0
          }


          # tempzfordens <- (temp_ztp1 + offsetz_vec[indiv] - min_resp)/(max_resp - min_resp) - 0.5


          tempmeanfordens <- (intersectmat[1:num_regions, 1] + 0.5)*(max_resp - min_resp) + min_resp

          temp_tnorm_probvec <- fastnormdens(temp_ztp1,
                                             mean = tempmeanfordens + offsetz_vec[indiv],
                                             sd = 1)

          # temp_tnorm_probvec <- fastnormdens(temp_ztp1 + offsetz_vec[indiv],
          #                                    mean = intersectmat[1:num_regions, 1],
          #                                    sd = 1)


          for(k_ind in 1:num_regions){
            # obtain mean for truncated normal distribution
            # temp_mean <- intersectmat[k_ind, 1]

            # temp_tnorm_prob <- dtruncnorm(temp_ztp1,
            #                               a=temp_lower,
            #                               b=Inf,
            #                               mean = temp_mean,
            #                               sd = 1)


            # temp_tnorm_prob <- fastnormdens(temp_ztp1,
            #                          mean = temp_mean,
            #                          sd = 1)

            temp_tnorm_prob <- temp_tnorm_probvec[k_ind]

            # Probability of z_t in intersection of
            # region k_ind (for period t+1)
            # and region defined by period t latent variables for other individuals
            # and rank for period t


            # tildeC_ktminl corresponds to
            # period t+1 k_ind region intereval

            temp_lower2 <- intersectmat[k_ind, 2]
            temp_upper2 <- intersectmat[k_ind, 3]



            # print("temp_lower2 = ")
            # print(temp_lower2)
            #
            # print("temp_upper2 = ")
            # print(temp_upper2)
            #
            # print("temp_lower3 = ")
            # print(temp_lower3)
            #
            # print("temp_upper3 = ")
            # print(temp_upper3)


            if((temp_lower2 > temp_upper3) | (temp_lower3 > temp_upper2)){
              # intervals do not overlap, therefore assign probability zero
              # and skip to next iteration

              # print("k_ind = ")
              # print(k_ind)

              # print("ncol(temp_region_probs) = ")
              # print(ncol(temp_region_probs))

              # print("nrow(temp_region_probs) = ")
              # print(nrow(temp_region_probs))

              temp_region_probs[k_ind, 1] <- 0
              temp_region_probs[k_ind, 2] <- NA
              temp_region_probs[k_ind, 3] <- NA


              next
            }


            # print("Line 1163")
            temp_lower2 <- max(temp_lower2, temp_lower3)
            temp_upper2 <- min(temp_upper2, temp_upper3)


            if(temp_lower2 > temp_upper2){

              print("as.vector(Z.mat)[(t-1)*n.item*n.ranker +
                                            n.item*(indiv - 1) +
                                            1:n.item]")
              print(as.vector(Z.mat)[(t-1)*n.item*n.ranker +
                                       n.item*(indiv - 1) +
                                       1:n.item])
              print("item_ind = ")
              print(item_ind)

              print("rankvec_t = ")
              print(rankvec_t)


              print("temp_lower2 = ")
              print(temp_lower2)

              print("temp_upper2 = ")
              print(temp_upper2)

              stop("Line 1917. temp_lower2 > temp_upper2")
            }



            # probability of being in intersection region

            # prob_t_region <- pnorm(temp_upper2 - temp_mean2) - pnorm(temp_lower2 - temp_mean2)


            # print("temp_upper2 = ")
            # print(temp_upper2)
            # print("temp_lower2 = ")
            # print(temp_lower2)
            #
            # print("temp_mean2 = ")
            # print(temp_mean2)
            #
            #
            # print("prob_t_region = ")
            # print(prob_t_region)
            #
            # print("temp_tnorm_prob = ")
            # print(temp_tnorm_prob)

            # prob_t_region <- prob_t_region*temp_tnorm_prob
            prob_t_region <- temp_tnorm_prob

            # save region probability

            # and save region bounds (or maybe more memory efficient to obtain the region again)

            # must multiply by other previously obtained probabilities

            # print("prob_t_region = ")
            # print(prob_t_region)

            temp_region_probs[k_ind, 1] <- prob_t_region
            temp_region_probs[k_ind, 2] <- temp_lower2
            temp_region_probs[k_ind, 3] <- temp_upper2


          }


          if(any(temp_region_probs[,1] < 0)){
            print("temp_region_probs[,1] =  ")
            print(temp_region_probs[,1])
            stop("negative probabiltiies")
          }

          # sample a region using probabilities obtained above

          # print("Line 1903 before sample")

          region_ind <- sample.int(num_regions, 1, replace = TRUE, prob = temp_region_probs[,1])

          # print("Line 1914 after sample")

          temp_mean2_origscale <- (temp_mean2 + 0.5)*(max_resp - min_resp) + min_resp

          # temp_mean2_debug <- sampler$predict(x.test = as.matrix(rep(temp_ztpmin1,100)), offset.test = NULL)[1:num_indiv]
          #
          # print("line 2061 temp_mean2_debug from predict = ")
          # print(temp_mean2_debug)
          #
          # print("line 2061 temp_mean2_origscale = ")
          # print(temp_mean2_origscale)



          # temp_lower0 <- (temp_region_probs[region_ind, 2] + 0.5)*(max_resp - min_resp) + min_resp
          # temp_upper0 <- (temp_region_probs[region_ind, 3] + 0.5)*(max_resp - min_resp) + min_resp

          temp_lower0 <- temp_region_probs[region_ind, 2]
          temp_upper0 <- temp_region_probs[region_ind, 3]

          zdraw_temp <- rtruncnorm(n = 1,
                                   a=temp_lower0,
                                   b=temp_upper0,
                                   mean = temp_mean2_origscale + offsetz_vec[indiv],
                                   sd = 1)

          # zdraw_temp <- rtruncnorm(n = 1,
          #                          a=temp_region_probs[region_ind, 2],
          #                          b=temp_region_probs[region_ind, 3],
          #                          mean = temp_mean2 + scaled_offsetz_vec[indiv],
          #                          sd = 1)


          # zdraw_temp <- (zdraw_temp + 0.5)*(max_resp - min_resp) + min_resp


          # Z.mat[item_ind, (t - 1)*n.ranker + indiv ] <- zdraw_temp
          Z.mat[t, indiv ] <- zdraw_temp

          # print("Z.vec[(n.time_train-1)*num_indiv + indiv] = ")
          # print(Z.vec[(n.time_train-1)*num_indiv + indiv])
          #
          # print("zdraw_temp =  ")
          # print(zdraw_temp)

          # jusyt need to update relevant element of Z.vec
          Z.vec[(t-1)*num_indiv + indiv] <- zdraw_temp
          # Z.vec <- as.vetor(t(Z.mat))



        } # end loop over time periods

        # check for special cases for n.time - 1, n.time - 2, n.time - 3

        # special case for t = n.time

        # just a truncated normal draw


        # temp_ztpmin1 <- as.vector(Z.mat)[(n.time-2)*n.item*n.ranker+
        #                                    n.item*(indiv - 1) +
        #                                    item_ind]

        temp_ztpmin1 <- Z.vec[(n.time_train-2)*num_indiv +
                                indiv]


        # must find mean corresponding to z in period t-1
        # This will be used in and after the loop over regions.
        # can directly obtain from dbarts
        # or find region
        # and use already saved region mean values


        # must find last lower bound that temp_ztpmin1 is greater than
        # for first upper bound that temp_ztpmin1 is below
        ktemp_tmin1 <- which(temp_ztpmin1 < intersectmat[, 3])[1]
        # Then obtain the corresponding region mean value
        temp_mean2 <- intersectmat[ktemp_tmin1,1]

        # now find interval


        # tildeC_ktminl corresponds to
        # period t+1 k_ind region intereval

        # rankvec_t <- ranks_mat[,  (n.time-1)*n.ranker + indiv]

        y_t <- y.train[n.time_train,indiv]



        # # inds for j ranked below i in T
        #
        # belowrank_ind <- which(rankvec_t == rankvec_t[item_ind] - 1 )
        #
        # #max of latent variables for j ranked below i in T
        # # Z.mat
        #
        # if(length(belowrank_ind) ==0){
        #   temp_lower3 <- -Inf
        # }else{
        #   temp_lower3 <- as.vector(Z.mat)[(n.time-1)*n.item*n.ranker +
        #                                     n.item*(indiv - 1) +
        #                                     belowrank_ind]
        # }
        #
        # # inds for j ranked above i in T
        #
        # aboverank_ind <- which(rankvec_t == rankvec_t[item_ind] + 1)
        #
        # #min of latent variables for j ranked below i in period T
        #
        # if(length(aboverank_ind) ==0){
        #   temp_upper3 <- Inf
        # }else{
        #
        #   temp_upper3 <- as.vector(Z.mat)[(n.time-1)*n.item*n.ranker +
        #                                     n.item*(indiv - 1) +
        #                                     aboverank_ind]
        # }

        if(y_t == 1){
          temp_lower3 <- 0 # scalezero # 0
          temp_upper3 <- Inf
        }else{
          temp_lower3 <- -Inf
          temp_upper3 <- 0 # scalezero # 0
        }


        # print("Line 1343")
        # CHECK IF THESE BOUNDS ARE CORRECTLY DEFINED

        temp_mean2_origscale <- (temp_mean2 + 0.5)*(max_resp - min_resp) + min_resp

        # temp_mean2_debug <- sampler$predict(x.test = as.matrix(rep(temp_ztpmin1,100)), offset.test = NULL)[1:num_indiv]
        #
        # print("line 2194 temp_mean2_debug from predict = ")
        # print(temp_mean2_debug)
        #
        # print("line 2061 temp_mean2_origscale = ")
        # print(temp_mean2_origscale)



        temp_lower0 <- temp_lower3 # (temp_lower3 + 0.5)*(max_resp - min_resp) + min_resp
        temp_upper0 <- temp_upper3 # (temp_upper3 + 0.5)*(max_resp - min_resp) + min_resp


        zdraw_temp <- rtruncnorm(n = 1,
                                 a = temp_lower0,
                                 b = temp_upper0,
                                 mean = temp_mean2_origscale + offsetz_vec[indiv],
                                 sd = 1)

        # zdraw_temp <- rtruncnorm(n = 1,
        #                          a = temp_lower3,
        #                          b = temp_upper3,
        #                          mean = temp_mean2 + scaled_offsetz_vec[indiv],
        #                          sd = 1)

        # zdraw_temp <- (zdraw_temp + 0.5)*(max_resp - min_resp) + min_resp

        # Z.mat[item_ind, (n.time -1)*n.ranker + indiv ] <- zdraw_temp


        # Z.mat[item_ind, (t - 1)*n.ranker + indiv ] <- zdraw_temp
        Z.mat[n.time_train, indiv ] <- zdraw_temp

        # print("Z.vec[(n.time_train-1)*num_indiv + indiv] = ")
        # print(Z.vec[(n.time_train-1)*num_indiv + indiv])
        #
        # print("zdraw_temp =  ")
        # print(zdraw_temp)

        # just need to update relevant element of Z.vec
        Z.vec[(n.time_train-1)*num_indiv + indiv] <- zdraw_temp
        # Z.vec <- as.vetor(t(Z.mat))


      # } # end loop over items

      } # end loop over individuals indiv in 1:num_indiv

    } # end loop over z_iter_ind



    ####### update Z lag matrix ####################


    Zlag.mat <- matrix(NA, nrow = n.time_train*num_indiv, ncol = num_lags)

    for(t in 1:num_lags){
      init_Z_t0 <- rep(0, t*num_indiv)
      # init_Z_t0 <- rnorm(t*n.item*n.ranker)

      Zlag.mat[,t] <- c(init_Z_t0, Z.vec[1:((n.time_train-t)*num_indiv)])

    }


    ######## check that new z values are consistent with the tree structures ###############

    temp_break <- 0

    for(j in 1:num_lags){
      # sampler$setPredictor(x = Zlag.mat[,j], column = j, forceUpdate = TRUE)

      while (sampler$setPredictor(x = Zlag.mat[,j], column = j) == FALSE) {

        if(seq_z_draws==1){
          stop("updates still not consistent with tree structure")
        }
        print("new z values not consistent with tree structure, must draw again")

        # print("current values ")
        # print("sampler$data@x = ")
        # print(sampler$data@x)
        #
        # print(" Zlag.mat[,j] = ")
        # print( Zlag.mat[,j])


        # If this error message occurs
        # Check the conditions in the dbart package for setPredictor == FALSE
        # And if this is hypothetically possible, even with draws from the smoothing distribution,
        # and if it is not a bug
        # then need to go back to beginning of this iteration of the Gibbs sampler
        # and sample Zmat again


        temp_break <- 1
        break
        # stop("new z values not consistent with tree structure, must draw again")


        # #perhaps this can be rewritten to just re-draw the relevant column?
        # Z.mat <- GibbsUpLatentGivenRankindividual(pair.comp.ten = pair.comp.ten,
        #                                           Z.mat = Z.mat,
        #                                           mu = mu,
        #                                           weight.vec = rep(1, n.ranker*n.time),
        #                                           n.ranker = n.ranker*n.time,
        #                                           n.item = n.item )
        #
        # Zlag.mat <- matrix(NA, nrow = n.time*n.ranker*n.item, ncol = num_lags)
        #
        # for(t in 1:num_lags){
        #   init_Z_t0 <- rep(0, t*n.item*n.ranker)
        #   # init_Z_t0 <- rnorm(t*n.item*n.ranker)
        #
        #   Zlag.mat[,t] <- c(init_Z_t0, as.vector(Z.mat)[1:((n.time-t)*n.item*n.ranker)])
        #
        # }
        #
        # #check that Zmat and  Zlag.mat are updated outside the while loop and for-loop over j

      }

      if(temp_break==1){
        break
      }
    }

    # print("sampler$data@x = ")
    # print(sampler$data@x)

    # if need to draw z values again, go back to start of loop
    if(temp_break==1){
      if(breakcount == 10){
        Z.mat <- Z.matold
        Z.vec <- as.vector(t(Z.mat))

        # Zlag.mat <- matrix(NA, nrow = n.time*n.ranker*n.item, ncol = num_lags)
        #
        # for(t in 1:num_lags){
        #   init_Z_t0 <- rep(0, t*n.item*n.ranker)
        #   # init_Z_t0 <- rnorm(t*n.item*n.ranker)
        #
        #   Zlag.mat[,t] <- c(init_Z_t0, as.vector(Z.mat)[1:((n.time-t)*n.item*n.ranker)])
        #
        # }

        Zlag.mat <- matrix(NA, nrow = n.time_train*num_indiv, ncol = num_lags)

        for(t in 1:num_lags){
          init_Z_t0 <- rep(0, t*num_indiv)
          # init_Z_t0 <- rnorm(t*n.item*n.ranker)

          Zlag.mat[,t] <- c(init_Z_t0, Z.vec[1:((n.time_train-t)*num_indiv)])

        }

      }else{
        breakcount <- breakcount +1
        next
      }

    }

    breakcount <- 0

    # print("Line 1475")

    ##################### Sample sum-of-trees ##################################################


    #set the response.
    #Check that 0 is a reasonable initial value
    #perhaps makes more sense to use initial values of Z

    # sampler$setResponse(y = as.vector(Z.mat))
    sampler$setResponse(y = as.vector(Z.vec))

    min_resp <- min(Z.vec)
    max_resp <- max(Z.vec)

    scalezero <- (0 - min_resp)/(max_resp - min_resp) - 0.5
    scaled_offsetz_vec <- (offsetz_vec - min_resp)/(max_resp - min_resp) - 0.5

    # sampler$setSigma(sigma = 1)
    #sampler$setPredictor(x= df_for_dbart$x, column = 1, forceUpdate = TRUE)

    #mu = as.vector( alpha + X.mat %*% beta )
    samplestemp <- sampler$run()

    mutemp <- samplestemp$train[,1]
    #suppose there are a number of samples

    # mutemp <- sampler$predict(df_for_dbart)
    # print("sigma = ")
    # print(samplestemp$sigma)

    mu = mutemp

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
    #
    #
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
        draw$Z.mat[,,iter] = Z.mat
        draw$Z.mat.test[,,iter] = Z.mat.test
      }
      # draw$alpha[,iter] = alpha
      # draw$beta[,iter] = beta
      draw$mu[,,iter] = matrix(mu, nrow = num_indiv, ncol = n.time_train) # mu
      # draw$sigma2.alpha[iter] = sigma2.alpha
      # draw$sigma2.beta[iter] = sigma2.beta
    }









    # print("line 2209")
    #
    # print("ncol(df_for_dbart_test) = ")
    # print(ncol(df_for_dbart_test))
    # print("nrow(df_for_dbart_test) = ")
    # print(nrow(df_for_dbart_test))

    temp_test_mat <- matrix(NA,
                            nrow = num_indiv,
                            ncol = num_lags)

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



    # temp_test_mat <- Xmat.test[1:(n.item*n.ranker) ,]  #  matrix(NA, nrow = n.item*n.ranker,ncol = ncol(Xmat.test))

    # temp_test_preds <- matrix(NA, nrow = n.item*n.ranker,ncol = num_test_periods)
    temp_test_preds <- matrix(NA, nrow = num_indiv, ncol = num_test_periods)

    # print("Line 5488")


    temp_mu_test <- rep(NA,  nrow(df_for_dbart_test) )


    # for t1 ==1, alternative is to draw from predictive distribtuion for Z_{T+1}
    # as outlined in AR-BART paper


    # if(iter < 5){
    #   print("temp_test_mat = " )
    #   print(temp_test_mat)
    # }
    # print("Line 2192")

    for(t1 in 1:num_test_periods){
      #produce a prediction

      temp_test_mat <- data.frame(#y = rep(NA, nrow(temp_test_mat)),
                                  x = temp_test_mat)

      colnames(temp_test_mat) <- colnames(df_for_dbart_test)

      # must use original column names to prevent an error in the predict function
      # colnames(temp_test_mat) <- colnames(df_for_dbart_test)

      # print("Line 2200")
      # print("t1 = ")
      # print(t1)
      # #
      # #
      #
      # print("colnames(temp_test_mat) = ")
      # print(colnames(temp_test_mat))
      #
      # print("colnames(df_for_dbart) = ")
      # print(colnames(df_for_dbart))
      #
      # print("df_for_dbart = ")
      # print(df_for_dbart)
      #
      # print("class(temp_test_mat) = ")
      # print(class(temp_test_mat))
      #
      # print("temp_test_mat = ")
      # print(temp_test_mat)
      #
      # # temp_test_mat <- as.matrix(temp_test_mat)
      #
      # print("class(temp_test_mat) = ")
      # print(class(temp_test_mat))
      #
      # print("colnames(df_for_dbart)")
      # print(colnames(df_for_dbart))
      #
      # print("colnames(temp_test_mat)")
      # print(colnames(temp_test_mat))
      #
      # testpredvec <- sampler$predict(x = df_for_dbart)
      #
      # print("testpredvec = ")
      # print(testpredvec)

      if(num_indiv ==1){
        # print("Line 2264")
        #
        # print("min(Z.vec) = ")
        # print(min(Z.vec))
        # print("max(Z.vec) = ")
        # print(max(Z.vec))
        #
        # print("Z.vec[length(Z.vec)] = ")
        # print(Z.vec[length(Z.vec)])

        tempbind <- as.matrix(rbind(temp_test_mat,temp_test_mat))
        colnames(tempbind) <- colnames(df_for_dbart_test)

        # print("tempbind = ")
        # print(tempbind)
        #
        # #
        # # print("sampler$state[[1]]@trees = ")
        # # print(sampler$state[[1]]@trees)
        # #
        # # print("sampler$state")
        # # print(sampler$state)
        #
        # print("sampler$getTrees() = ")
        # print(sampler$getTrees())
        #
        # print("num_indiv = ")
        # print(num_indiv)
        #
        # testpredvec <- sampler$predict(x.test = as.matrix(tempbind[1,]), offset.test = NULL )[1:num_indiv]



        list_inter_mats <- list()

        for(i in 1:n.trees){
          # print("Line 492")
          treeexample1 <- sampler$getTrees(treeNums = i,
                                           chainNums = 1,
                                           sampleNums = 1)
          # print("Line 496")
          # print("treeexample1 = ")
          # print(treeexample1)
          # # rebuilt_tree <- rebuildTree2(treeexample1)
          rebuilt_tree <- rebuildTree2_cpp(as.matrix(treeexample1))
          # print("Line 499")
          #
          #must use covariates for individual indiv at time period t

          # list_inter_mats[[i]] <- getPredictionsRangesForTree3(rebuilt_tree, as.matrix(df_for_dbart$x[obs_indices[1]]) )

          # list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree$var == -1 , 5:7]
          list_inter_mats[[i]] <- rebuilt_tree[rebuilt_tree[,4] == -1 , 5:7, drop = FALSE]
          # print("Line 507")

        }

        intersectmat <- interNtreesB(list_inter_mats)

        kpredtemp <- which((as.matrix(tempbind[1,])[1] < intersectmat[, 3]) )[1]
        # Then obtain the corresponding region mean value
        testpredvec <- as.matrix(intersectmat[kpredtemp,1])


        testpredvec <- (testpredvec + 0.5)*(max_resp - min_resp) + min_resp


        # temp_mean2_debug <- sampler$predict(x.test = as.matrix(rep(as.matrix(tempbind[1,1]),100)),
        #                                     offset.test = NULL)[1:num_indiv]
        #
        # print("line 2194 temp_mean2_debug from predict = ")
        # print(temp_mean2_debug)
        #
        # print("line 2061 testpredvec = ")
        # print(testpredvec)

      }else{
        testpredvec <- sampler$predict(x.test = as.matrix(temp_test_mat), offset.test = NULL)
      }


      # print("testpredvec = ")
      # print(testpredvec)

      # print("Line 2279")


      # print("testpredvec = ")
      # print(testpredvec)
      #
      # print("Line 2283")

      #fill in temp_test_preds with noise
      if(noise_in_pred ==1){
        temp_test_preds[ , t1] <- testpredvec + rnorm( num_indiv )
      }else{
        temp_test_preds[ , t1] <- testpredvec #+ rnorm( num_indiv )
      }


      #update temp_test_mat
      #shift z columns to the right and fill in leftmost column

      #need to rewrite this if want to allow for no observed covariates

      # temp_test_mat <-  data.frame( x= cbind(  temp_test_preds[ , t1] , temp_test_mat[,1:(num_lags-1)] ,Xmat.test[(t1-1)*(n.item*n.ranker)  +  1:(n.item*n.ranker) ,(num_lags+1):(ncol(Xmat.test))] ) )

      if(t1 != num_test_periods){

        if(num_lags ==1){
          temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] #,
                                                    # as.matrix(Xmat.test[(t1)*(n.item*n.ranker)  +  1:(n.item*n.ranker),])
                                                    )
                                       )

        }else{
          temp_test_mat <-  data.frame(x =  cbind(  temp_test_preds[ , t1] ,
                                                    temp_test_mat[,1:(num_lags-1)] #,
                                                    # as.matrix(Xmat.test[(t1)*(n.item*n.ranker)  +  1:(n.item*n.ranker),])
                                                    )
                                       )
        }
      }

      # colnames(temp_test_mat) <- colnames(Xmat.test)

      # fill in temp_mu_test without noise
      temp_mu_test[(t1-1)*(num_indiv)  +  1:(num_indiv) ] <- testpredvec


    }

    #also update Zlag.mat.test ?
    #perhaps this is unnecessary here?

    # print("Line 5545")


    # Zlag.mat.test <- matrix(NA, nrow = num_test_periods*n.ranker*n.item, ncol = num_lags)
    Zlag.mat.test <- matrix(NA, nrow = num_test_periods*num_indiv, ncol = num_lags)


    # if(nrow(X.test) >0 ){
    for(t in 1:num_lags){
      # if(t==1){
      #   #repeating the last period values
      #   Zlag.mat.test[,t] <- rep(as.vector(Z.mat)[((n.time-1)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],num_test_periods )
      #
      #   #other option is to set all unobservable values to zero
      #   # Zlag.mat.test[,t] <- c(as.vector(Z.mat)[((n.time-1)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],
      #   #                        rep(0,(num_test_periods-t)*n.item*n.ranker ))
      #
      #
      # }else{

      # if(num_test_periods > t ){
      #   #up to t time periods ahead can be filled in. Rest of test time periods filled in using repeated values of last observation in training data
      #
      #   Zlag.mat.test[,t] <- c(as.vector(Z.mat)[((n.time-t)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],
      #                          as.vector(as.matrix(temp_test_mat))[1:((num_test_periods - t)*(n.item*n.ranker))  ] )
      #
      #
      #
      # }else{
      #   #nothing to fill in if num_test_periods <= t
      #   Zlag.mat.test[,t] <- c(as.vector(Z.mat)[((n.time-t)*n.item*n.ranker+1):((n.time-(t - num_test_periods) )*n.item*n.ranker)]  )
      #
      # }

      if(num_test_periods > t ){
        #up to t time periods ahead can be filled in. Rest of test time periods filled in using repeated values of last observation in training data

        Zlag.mat.test[,t] <- c(Z.vec[((n.time_train-t)*num_indiv+1):(n.time_train*num_indiv)],
                               rep(Z.vec[((n.time_train-1)*num_indiv+1):(n.time_train*num_indiv)],
                                   (num_test_periods-t) )  )

        # #other option is to set all unobservable values to zero
        # Zlag.mat.test[,t] <- c(as.vector(Z.mat)[((n.time-t)*n.item*n.ranker+1):(n.time*n.item*n.ranker)],
        #                        rep(0,(num_test_periods-t)*n.item*n.ranker ))

      }else{
        #nothing to fill in if num_test_periods <= t
        Zlag.mat.test[,t] <- c(Z.vec[((n.time_train-t)*num_indiv+1):((n.time_train-(t - num_test_periods) )*num_indiv)]  )

      }


      # }
    }
    # }

    # if(nrow(X.test)>0){
    #
    #
    #   for(j in 1:num_lags){
    #
    #     #perhaps this should be removed for when Z is updated properly below
    #     sampler$setTestPredictor(x = Zlag.mat.test[,j], column = j)
    #
    #   }
    #
    # }


    if(iter > n.burnin){
      draw$mu_test[,,iter] <- matrix(temp_mu_test, nrow = num_indiv, ncol = num_test_periods) # temp_mu_test
      # draw$mu_test[,iter] <- samplestemp$test[,1]

      if(keep_zmat==TRUE){
        # draw$Z.mat.test[,,iter]  <- matrix(data = as.vector(temp_test_preds), nrow = n.item, ncol = n.ranker*num_test_periods)
        draw$Z.mat.test[,,iter]  <- matrix(data = as.vector(temp_test_preds), nrow = num_indiv, ncol = num_test_periods)

      }
    }
    # draw$mu_test[,1] <- samplestemp$test[,1]

    # }else{
    #   draw$mu_test[,1] <- initial.list$mu_test
    # }




    # if( nrow(X.test) >0 ){
    #   draw$mu_test[,iter] <- samplestemp$test[,1]
    # }


    # print iteration number
    if(iter %% print.opt == 0){
      print(paste("Gibbs Iteration", iter))
      # print(c(sigma2.alpha, sigma2.beta))
    }

    iter <- iter+1

  } # end iterations of Gibbs sampler

  return(draw)

}


