
#' Fit a LMM with a L2 penalty on the fixed effects
#'
#' @param y character vector of the output variable name
#' @param x character vector of the fixed effects variable names
#' @param grp character vector of the random intercept variable name
#' @param data data frame
#' @param lambda L2 penalty
#'
lmm_fit <- function(y,x,grp,data,lambda=0.0) {
  
  all_vars <- c(x,y,grp)
  data_comp <- na.omit(data[,..all_vars])
  if (lambda == 0) {
    X <- as.matrix(cbind(rep(1,nrow(data_comp)),data_comp[,..x]))
  } else {
    X <- as.matrix(data_comp[,..x])
  }
  
  Z <- model.matrix(~ data_comp[[grp]] -1)
  y <- data_comp[[y]]
  n <- length(y)
  pr <- ncol(Z)
  pf <- ncol(X) 
  
  ## optimise
  pf <- ncol(X)
  mod <- optim(c(0,0), 
               llm_lik,
               hessian=TRUE, X=X,Z=Z,y=y,lambda=lambda) 
  solve(mod$hessian) 
  b <- attr(llm_lik(mod$par,X,Z,y,lambda),"b") 
  
  ## covariance for fixed effects
  sigma_b <- exp(mod$par[1]) 
  sigma <- exp(mod$par[2]) 
  ipsi <- c(rep(0.0,pf),rep(1/sigma_b^2,pr)) 
  beta_cov <- solve(t(X) %*% solve((Z %*% t(Z))*sigma_b^2 + diag(rep(sigma^2,n))) %*% X)
  
  ## return
  beta = b[1:pf]
  if (lambda == 0) {
    names(beta) <- c("intercept",x)
  } else {
    names(beta) <- c(x)
  }
 
  b = b[(pf+1):length(b)]
  #names(b)
  list(beta = beta,
       beta_se = sqrt(diag(beta_cov)),
       b = b,
       theta = exp(mod$par),
       hessian = solve(mod$hessian),
       n = n)
}


#' Likelihood for fitting a LMM with a L2 penalty on the fixed effects
#'
#'
#'
llm_lik <- function(theta,X,Z,y,lambda) { 
  ## untransform parameters... 
  sigma_b <- exp(theta[1]) 
  sigma <- exp(theta[2]) 
  ## extract dimensions... 
  n <- length(y)
  pr <- ncol(Z)
  pf <- ncol(X) 
  ## obtain \hat \beta, \hat b... 
  X1 <- cbind(X,Z) 
  ipsi <- c(rep(lambda,pf),rep(1/sigma_b^2,pr)) 
  b1 <- solve(crossprod(X1)/sigma^2+diag(ipsi), t(X1)%*%y/sigma^2) 
  ## compute log|Z’Z/sigma^2 + I/sigma.b^2|... 
  ldet <- sum(log(diag(chol(crossprod(Z)/sigma^2 + diag(ipsi[-(1:pf)]))))) 
  ## compute log profile likelihood... 
  l <- (-sum((y-X1%*%b1)^2)/sigma^2 - sum(b1[(pf+1):(length(b1))]^2)/sigma_b^2 - sum(b1[1:pf]^2)*lambda -
          n*log(sigma^2) - pr*log(sigma_b^2) - 2*ldet - n*log(2*pi))/2 
  attr(l,"b") <- as.numeric(b1) ## return \hat beta and \hat b 
  -l 
}

