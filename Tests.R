# This is a script to save your own tests for the function
source("FunctionsLR.R")

# Load the letter data
#########################
# Training data
letter_train <- read.table("Data/letter-train.txt",
                           header = F,
                           colClasses = "numeric")
Y <- letter_train[, 1]
X <- as.matrix(letter_train[, -1])

# Testing data
letter_test <- read.table("Data/letter-test.txt",
                          header = F,
                          colClasses = "numeric")
Yt <- letter_test[, 1]
Xt <- as.matrix(letter_test[, -1])

# save variables that will be reaccessed
n <- nrow(X)
p <- ncol(X)
ntest <- nrow(Xt)
length(Y)

# Add column for an intercept to X and Xt
X <- cbind(rep(1, n), X)
Xt <- cbind(rep(1, ntest), Xt)

# Edge case tests at the beginning of the file

# Case 1: Check that the first column of X and Xt are 1s
sum((X[, 1] == rep(1, n))) == n #check first column of X
sum(Xt[, 1] == rep(1, ntest)) == ntest #check first column of Xt
LRMultiClass(
  X,
  Y,
  Xt,
  Yt,
  numIter = 50,
  eta = 0.1,
  lambda = 1,
  beta_init = NULL
)
