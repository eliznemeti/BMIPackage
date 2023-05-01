#' Select random samples
#'
#' @param x A vector or a data frame.
#' @param n The number of items to select.
#' @param replace Logical, whether to sample with replacement.
#' @return A vector or a data frame containing random samples from x, depending on the input type.
#' @examples
#' rando(1:10, n = 5)
#' @export
rando <- function(x, n = 1, replace = T) {
  if (is.data.frame(x)) {
    return(x[sample(nrow(x), size = n, replace = replace), ])
  } else {
    return(sample(x, size = n, replace = replace))
  }
}

#' Identify minimum value
#'
#' @param x A numeric vector.
#' @param na.rm Logical, whether to remove NA values.
#' @return A logical vector indicating the minimum values in x (TRUE if the value is a minimum, FALSE otherwise).
#' @examples
#' is_min(c(1, 2, 3, NA), na.rm = TRUE)
#' @export
is_min <- function(x, na.rm = T) {
  min_x <- min(x, na.rm = na.rm)
  x == min_x
}

#' Identify maximum value
#'
#' @param x A numeric vector.
#' @param na.rm Logical, whether to remove NA values.
#' @return A logical vector indicating the maximum values in x (TRUE if the value is a maximum, FALSE otherwise).
#' @examples
#' is_max(c(1, 2, 3, NA), na.rm = TRUE)
#' @export
is_max <- function(x, na.rm = T) {
  max_x <- max(x, na.rm = na.rm)
  x == max_x
}

#' Repeat matrix
#'
#' @param x A matrix.
#' @param M The number of times to repeat rows.
#' @param N The number of times to repeat columns.
#' @return A matrix where the rows and columns of x are repeated M and N times, respectively.
#' @examples
#' rep_mat(matrix(1:4, nrow = 2), M = 2, N = 3)
#' @export
rep_mat <- function(x, M, N) {
  temp <- matrix(rep(x, each = M), nrow = nrow(x) * M)
  matrix(rep(temp, each = N), nrow = nrow(temp))
}

#' Extract classes of columns
#'
#' @param x A data frame.
#' @return A character vector of class names for each column in x.
#' @examples
#' classes(data.frame(a = 1:3, b = letters[1:3]))
#' @export
classes <- function(x) {
  sapply(x, class)
}


#' Scale a data frame
#'
#' @param x A data frame.
#' @param center Logical, whether to center the columns.
#' @param scale Logical, whether to scale the columns.
#' @return A data frame where the numeric columns have been scaled (centered and/or standardized).
#' @examples
#' df_scale(data.frame(a = 1:3, b = 4:6))
#' @export
df_scale <- function(x, center = T, scale = T) {
  x_num <- x[sapply(x, is.numeric)]  # only select numeric columns
  data.frame(scale(x_num, center = center, scale = scale))
}

#' Compute log-likelihood for normal distribution
#'
#' @param x A numeric vector.
#' @param mean The mean of the normal distribution.
#' @param sd The standard deviation of the normal distribution.
#' @return A numeric value representing the log-likelihood of the data given the specified normal distribution.
#' @examples
#' log_likelihood_norm(rnorm(100), mean = 0, sd = 1)
#' @importFrom stats dnorm
#' @export
log_likelihood_norm <- function(x, mean, sd) {
  sum(dnorm(x, mean = mean, sd = sd, log = TRUE))
}

#' #' Compute log-likelihood for uniform distribution
#'
#' @param x A numeric vector.
#' @param min The minimum value of the uniform distribution.
#' @param max The maximum value of the uniform distribution.
#' @return A numeric value representing the log-likelihood of the data given the specified uniform distribution.
#' @examples
#' log_likelihood_unif(runif(100), min = 0, max = 1)
#' @importFrom stats dunif
#' @export
log_likelihood_unif <- function(x, min, max) {
  sum(dunif(x, min = min, max = max, log = TRUE))
}

#' Compute log-likelihood for chi-squared distribution
#'
#' @param x A numeric vector.
#' @param df Degrees of freedom.
#' @return A numeric value representing the log-likelihood of the data given the specified chi-squared distribution.
#' @examples
#' log_likelihood_chisq(rchisq(100, df = 1), df = 1)
#' @importFrom stats dchisq
#' @export
log_likelihood_chisq <- function(x, df) {
  sum(dchisq(x, df = df, log = TRUE))
}

#' Compute log-likelihood for F distribution
#'
#' @param x A numeric vector.
#' @param df1 Numerator degrees of freedom.
#' @param df2 Denominator degrees of freedom.
#' @return A numeric value representing the log-likelihood of the data given the specified F distribution.
#' @examples
#' log_likelihood_f(rf(100, df1 = 1, df2 = 1), df1 = 1, df2 = 1)
#' @importFrom stats df
#' @export
log_likelihood_f <- function(x, df1, df2) {
  sum(stats::df(x, df1 = df1, df2 = df2, log = TRUE))
}

#' Compute log-likelihood for t distribution
#'
#' @param x A numeric vector.
#' @param df Degrees of freedom.
#' @return A numeric value representing the log-likelihood of the data given the specified t distribution.
#' @examples
#' log_likelihood_t(rt(100, df = 1), df = 1)
#' @importFrom stats dt
#' @export
log_likelihood_t <- function(x, df) {
  sum(dt(x, df = df, log = TRUE))
}

#' Compute sensitivity
#'
#' @param pred A logical vector of predictions.
#' @param truth A logical vector of true values.
#' @return A numeric value (between 0 and 1) representing the sensitivity of the predictions.
#' @examples
#' sensitivity(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, TRUE, FALSE))
#' @export
sensitivity <- function(pred,truth) {
  sum(pred & truth) / sum(truth)
}

#' Compute specificity
#'
#' @param pred A logical vector of predictions.
#' @param truth A logical vector of true values.
#' @return A numeric value (between 0 and 1) representing the specificity of the predictions.
#' @examples
#' specificity(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, TRUE, FALSE))
#' @export
specificity <- function(pred,truth) {
  sum(!pred & !truth) / sum(!truth)
}

#' Compute precision
#'
#' @param pred A logical vector of predictions.
#' @param truth A logical vector of true values.
#' @return A numeric value (between 0 and 1) representing the precision of the predictions.
#' @examples
#' precision(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, TRUE, FALSE))
#' @export
precision <- function(pred,truth) {
  sum(pred & truth) / sum(pred)
}

#' Compute recall
#'
#' @param pred A logical vector of predictions.
#' @param truth A logical vector of true values.
#' @return A numeric value (between 0 and 1) representing the recall of the predictions.
#' @examples
#' recall(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, TRUE, FALSE))
#' @export
recall <- function(pred,truth) {
  sum(pred & truth) / sum(truth)
}

#' Compute accuracy
#'
#' @param pred A logical vector of predictions.
#' @param truth A logical vector of true values.
#' @return A numeric value (between 0 and 1) representing the accuracy of the predictions.
#' @examples
#' accuracy(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, TRUE, FALSE))
#' @export
accuracy <- function(pred,truth) {
  sum(pred == truth) / length(truth)
}

#' Compute F1 score
#'
#' @param pred A logical vector of predictions.
#' @param truth A logical vector of true values.
#' @return A numeric value (between 0 and 1) representing the F1 score of the predictions.
#' @examples
#' f1(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, TRUE, FALSE))
#' @export
f1 <- function(pred,truth) {
  2 * (recall(pred,truth) * precision(pred,truth)) / (recall(pred,truth) + precision(pred,truth))
}

#' Compute minimum sample size per group
#'
#' @param d The effect size.
#' @param power The statistical power.
#' @return An integer value representing the minimum sample size per group needed to detect the effect size with the specified power.
#' @examples
#' minimum_n_per_group(d = 0.5, power = 0.8)
#' @importFrom pwr pwr.t.test
#' @export
minimum_n_per_group <- function(d, power = 0.8) {
  round(pwr::pwr.t.test(d = d, power = power, type = "two.sample")$n)
}

#' Compute R-squared
#'
#' @param pred A numeric vector of predictions.
#' @param truth A numeric vector of true values.
#' @return A numeric value (between 0 and 1) representing the R-squared of the predictions against the true values.
#' @examples
#' r2(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5))
#' @importFrom stats lm
#' @export
r2 <- function(pred,truth) {
  summary(stats::lm(truth~pred))$r.squared
}

#' Compute adjusted R-squared
#'
#' @param pred A numeric vector of predictions.
#' @param truth A numeric vector of true values.
#' @param n_p The number of predictors.
#' @return A numeric value (between 0 and 1) representing the adjusted R-squared of the predictions against the true values.
#' @examples
#' adj_R2(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5), n_p = 1)
#' @export
adj_R2 <- function(pred,truth,n_p) {
  summary(lm(truth~pred))$adj.r.squared
}

