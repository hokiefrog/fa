# factor analysis

# examples 9.7 and 9.10, p. 510 (Wichern and Johnson)
# using table 8.4, p. 473
# factor analysis of stock data using ML method

setwd("~/applied_multivariate_statistical_analysis/Wichern_data/")
dat <- read.table("T8-4.DAT")
names(dat) <- c("jpm","citibank","wf","rdc","exxon")

# help(factanal)
X <- as.matrix(dat)
stock_fa <- factanal(x=X,factor=2, scores="regression")
# reproduce Fig 9.4
plot(stock_fa$scores, pch=19, main="Figure 9.4")
abline(h=0,v=0)

# Estimation is maximum likelihood
# Uniquenesses = specific variances
# Loadings are rotated
# Test: hypothesis that n factors are sufficient.

# extract loadings and specific variances
stock_fa$loadings
stock_fa$uniquenesses

# example 9.14, 9.520
# factor analysis of chicken-bone data
# read in lower triangle of symmetric correlation matrix
# source for code: http://r.789695.n4.nabble.com/how-to-convert-the-lower-triangle-of-a-matrix-to-a-symmetric-matrix-td823271.html
dat2 <- scan("E9-14.dat")
x <- diag(6) 
x[upper.tri(x, diag=TRUE)] <- dat2 
x <- x + t(x) - diag(diag(x)) 

chick_fa <- factanal(factors=3, covmat=x, scores="regression")
plot(chick_fa$scores[], pch=19, main="Figure 9.4")
abline(h=0,v=0)
