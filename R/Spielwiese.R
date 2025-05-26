test <- matrix(rep(1:20, 5), nrow = 10, ncol = 10)
test
test[-1, ] - test[nrow(test), ]
