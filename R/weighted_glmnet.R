#' Weighted linear model
#'
#' @param x predictors matrix
#' @param y target vector
#' @param ... passed to `glmnet`
#' @param weight_column name of weights column in `x`
#'
#' @return 'glmnet' object
#' @export
#'
weighted_glmnet <- function(x, y, ..., weight_column = "weights") {
  if(weight_column %in% colnames(x)) {
    wght_col <- which(colnames(x) == weight_column)
    weights <- x[,wght_col]
    x <- x[, - wght_col]
  } else {
    rlang::warn("No weights provided. Using equal weights.")
    weights <- rep(1, nrow(x))
  }

  object <- glmnet::glmnet(x, y, weights = weights, ...)

  object$weight_column <- weight_column
  class(object) <- c("weighted_glmnet", class(object))

  object
}

#' @export
predict.weighted_glmnet <- function(object, newx, ...) {
  if(object$weight_column %in% colnames(newx)) {
    wght_col <- which(colnames(newx) == object$weight_column)
    newx <- newx[, - wght_col]
  }

  glmnet::predict.glmnet(object, newx, ...)
}

