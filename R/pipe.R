#' Pipe NCVREG Results into Confidence Intervals and Significance Testing
#'
#' This function processes the output of a cross-validated `ncvreg` fit, extracting the estimated coefficients, computing standard errors, and producing confidence intervals along with significance testing.
#'
#' @param cv_fit An object of class `cv.ncvreg` containing the cross-validated fit. Must include components `fit`, `lambda.min`, and other relevant information.
#' @param alpha A numeric value specifying the significance level for confidence intervals. Default is 0.05.
#' @param X (Optional) The design matrix. If NULL, it is extracted from `cv_fit`.
#' @param y (Optional) The response vector. If NULL, it is extracted from `cv_fit`.
#'
#' @return A data.frame containing the following columns:
#' \describe{
#'   \item{estimate}{The estimated coefficients (divided by rescale).}
#'   \item{lower}{The lower bounds of the confidence intervals.}
#'   \item{upper}{The upper bounds of the confidence intervals.}
#'   \item{significance}{Adjusted p-values for significance testing, corrected using the Benjamini-Hochberg method.}
#'   \item{original_pvals}{The original p-values before adjustment.}
#' }
#'
#' @details The function assumes the input `cv_fit` contains all necessary components, including the design matrix `X` and the response vector `y`. The function recalculates the response mean, residuals, and applies significance testing using the Benjamini-Hochberg correction.
#'
#' @seealso \code{\link[ncvreg]{ncvreg}}, \code{\link[ncvreg]{cv.ncvreg}}
#'
#' @examples
#' \dontrun{
#' library(ncvreg)
#' X <- matrix(rnorm(100*20), 100, 20)
#' y <- rnorm(100)
#' cvfit <- cv.ncvreg(X, y, family="gaussian")
#' result <- pipe_ncvreg(cvfit)
#' print(result)
#' }
#'
#' @export
pipe_ncvreg <- function(
    X, y, cv_fit, lambda, sigma = NULL, alpha = 0.05, penalty = "lasso", studentize = FALSE,
    correction, correct_bias = FALSE, original_n = FALSE
    # true_beta = NULL
) {
  
  
  
  if (!missing(cv_fit) && class(cv_fit) != "cv.ncvreg") {
    stop("cv_fit must be an opbject of class cv.ncvreg.")
  }
  
  # Check if cv_fit$fit contains X and y, or if they are supplied
  if (missing(X) & missing(y) & missing(cv_fit)) {
    stop("You must supply X and y or cv_fit as an object of class cv.ncvreg")
  }
  
  if (missing(X)) {
    if (missing(cv_fit)) {
      stop("You must supply X or cv_fit as an object of class cv.ncvreg")
    } else if (missing(X) & !("X") %in% names(cv_fit$fit)) {
      stop("fit object in cv_fit missing X, please rerun cv.ncvreg with returnX = TRUE or supply X directly") 
    }
  }
  
  if (missing(y) & missing(cv_fit)) {
    stop("You must supply y or cv_fit as an object of class cv.ncvreg")
  }
  
  if (!missing(cv_fit)) {
    if (!missing(y)) {
      message("Both cv_fit$fit and user-supplied y are provided; using cv_fit$fit's y.")
      y <- cv_fit$fit$y
    }
    if (!missing(X)) {
      message("Both cv_fit$fit and user-supplied X are provided; using cv_fit$fit's X.")
      X <- cv_fit$fit$X
    }
  }
  
  if (missing(X)) {
    X <- cv_fit$fit$X
  }
  if (missing(y)) {
    y <- cv_fit$fit$y
  }
  
  ## Need to clean up logic, currently doesn't make sense
  if (missing(cv_fit)) {
    cv_fit <- cv.ncvreg(X, y, penalty = penalty)
    X <- cv_fit$fit$X
    y <- cv_fit$fit$y
  }
  
  if (missing(lambda)) {
    lambda <- cv_fit$lambda.min
  }
  
  n <- nrow(X)
  p <- ncol(X)
  #orig_x <- t(t(X)*attr(X, "scale") + attr(X, "center"))
  #errs <- (y - (orig_x%*%true_beta))
  
  bh_lambda <- coef(cv_fit$fit, lambda = lambda)
  rescale <- attr(X, "scale")
  bh_lambda <- bh_lambda[-1] * rescale
  signs_s <- sign(bh_lambda)
  intercept <- mean(y - as.numeric(X %*% bh_lambda))
  yhat <- intercept + as.numeric(X %*% bh_lambda)
  #print(cv_fit$fit$linear.predictors[,cv_fit$min])
  #print(yhat)
  
  
  s <- bh_lambda != 0
  sh_lh <- sum(s)
  if (is.null(sigma)) {
    sigma_h <- sqrt((n - sh_lh)^(-1) * sum((y - yhat)^2))
  } else {
    sigma_h <- sigma
  }
  # sigma_h <- sqrt(cv_fit$cve[cv_fit$min])
  
  partial_residuals <- (y - yhat) + (X * matrix(bh_lambda, nrow = n, ncol = p, byrow=TRUE))

  b_bar <- (1/n)*colSums(X * partial_residuals)
  
  if (sh_lh > 0) {
    Xs <- X[,s,drop =FALSE]
    p_sh_default <- Xs %*% solve(t(Xs) %*% Xs, tol = 1e-12) %*% t(Xs)
    q_sh_default <- diag(n) - p_sh_default
  } else {
    q_sh_default <- diag(n)
  }
  
  ses <- numeric(p)
  # bias_correction <- numeric(p)
  if (!original_n) {
    
    for (j in 1:p) {
      
      Xj <- X[,j,drop=FALSE]
      
      if (!s[j]) {
        
        s_j <- s
        sh_j <- sh_lh
        q_sh <- q_sh_default
        Xsj <- X[,s_j,drop=FALSE]
        
      } else {
        s_j <- s
        s_j[j] <- FALSE
        sh_j <- sum(s_j)
        
        if (sh_j > 0) {
          
          Xsj <- X[,s_j,drop=FALSE]
          Xsj2i <- solve(t(Xsj) %*% Xsj, tol = 1e-12)
          q_sh <- diag(n) - (Xsj %*% Xsj2i %*% t(Xsj))
  
        } else {
  
          Xsj <- X[,s_j,drop=FALSE]
          q_sh <- diag(n)
  
        }
          
      }
      
      # all_beta <- true_beta * rescale
      # bias_err <- (1/n) * t(X[,j]) %*% errs
      # bias_err2 <- (t(X[,j,drop=FALSE]) %*% q_sh %*% X[,j,drop=FALSE])^(-1) * (t(X[,j,drop=FALSE]) %*% q_sh %*% errs)
      # delta <- (1/n)*t(Xsj)%*%(y - X[,j,drop=FALSE]*b_bar[j] - Xsj %*% bh_lambda[s_j])
      # bias1[j] <- bias_err2 + (t(X[,j,drop=FALSE]) %*% q_sh %*% X[,j,drop=FALSE])^(-1) * (t(X[,j,drop=FALSE]) %*% q_sh %*% X[,-j,drop=FALSE] %*% all_beta[-j]) + (t(X[,j,drop=FALSE]) %*% q_sh %*% X[,j,drop=FALSE])^(-1) * (n*t(X[,j,drop=FALSE]) %*% Xsj %*% solve(t(Xsj) %*% Xsj, tol = 1e-12) %*% delta)
      # bias_correction[j] <- (t(X[,j,drop=FALSE]) %*% q_sh %*% X[,j,drop=FALSE])^(-1) * (n*t(X[,j,drop=FALSE]) %*% Xsj %*% solve(t(Xsj) %*% Xsj, tol = 1e-12) %*% delta)
      # bias2[j] <- bias_err + (1/n)*t(X[,j,drop=FALSE]) %*% X[,-j,drop=FALSE] %*% (all_beta[-j] - bh_lambda[-j])
      
      ses[j] <- sigma_h * sqrt((t(X[,j,drop=FALSE]) %*% q_sh %*% X[,j,drop=FALSE])^(-1))
      
      
    }
    
  } else {
    ses <- sigma_h / sqrt(n)
  }
  
  # if (correct_bias) b_bar <- (b_bar - bias_correction)
  ts <- b_bar / ses
  ps <- 2 * (1 - pnorm(abs(ts)))
  qs <- p.adjust(ps, method = "BH")
  if (!missing(correction) && correction == "holm") alpha <- alpha / (ncol(X) - order(ps) + 1)
  if (!studentize) {
    widths <- abs(qnorm(alpha / 2)) * ses
  } else { 
    widths <- abs(qt(alpha / 2, df = n - sh_lh)) * ses
  }
  
  data.frame(
    variable = colnames(X),
    estimate = b_bar / rescale,
    lower = (b_bar - widths) / rescale,
    upper = (b_bar + widths) / rescale,
    significance = qs,
    original_pvals = ps,
    lambda = lambda,
    betahat = bh_lambda / rescale,
    rescale_factor = rescale
  )
  
}