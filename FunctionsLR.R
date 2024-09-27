# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  n <- nrow(X) # save variable of nrows as n, number of observations
  p <- ncol(X) # save variable of ncols as p, number of predictors
  ntest <- nrow(Xt) # save variable of nrows of Xtest as ntest (num obs of Xtest)
  K <- length(y)
  tX <- t(X) # compute transpose of X once to be accessed
  
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X is 1s, if not - display appropriate message and stop execution.
  if((X[ , 1] != rep(1,n)) ){
    stop("Error: check that the first column of X is 1s.")
  }
  # Check that the first column of Xt is 1s, if not - display appropriate message and stop execution.
  if((Xt[ , 1] != rep(1,ntest)) ){
    stop("Error: check that the first column of X test is 1s.")
  }
  # Check for compatibility of dimensions between X and Y
  if(n != nrow(Y)){
    stop("Error: check that the dimensions of X and Y are compatible for matrix multiplication.")
  }
  # Check for compatibility of dimensions between Xt and Yt - CHECK THIS!!
  if(ntest != nrow(Ytest)){
    stop("Error: check that the dimensions of Xtest and Ytest are compatible for matrix multiplication.")
  }
  # Check for compatibility of dimensions between X and Xt
  
  # Check eta is positive
  if(eta <= 0){
    stop("Error: Eta must be positive! Change your value of eta.")
  }
  # Check lambda is non-negative
  if(lambda < 0){
    stop("Error: lambda must be nonnegative! Change your value of lambda.")
  }
  
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if(is.null(beta_init)){
    beta_mat <- matrix(nrow = p, ncol = K)
  }
  if((is.null(beta_init) == FALSE) && ((nrow(beta_init)!= p) || ncol(beta_init) != K)){
    stop("Error: Check that the dimensions of beta are p x K.")
  }
  
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  fvec <- vector(mode = "numeric", length = nIter + 1) #initialize objective function 
  pk <- exp(X %*% beta_init)/(RowSum(tX %*% beta_init))
  # Calculate current objective value
  Xb <-  X %*% beta_init
  fvec[1] <-   sum(-y * (Xb) + log(1 + exp(Xb)))
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
 
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
  
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}