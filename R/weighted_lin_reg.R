#' General Interface for Weighted Linear Regression
#'
#'
#' `weighted_lin_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  package aigoraML. The main
#'  arguments for the model are:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The mixture amounts of different types of
#'   regularization (see below). Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and arguments can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  the only possible value for this model is "regression".
#' @param penalty The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'  This value is used for all PLS components for X.
#' @param mixture The mixture amounts of different types of
#'   regularization (see below). Note that this will be ignored for some engines.
#' @details The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"weighted_glmnet"`  (the default)
#' }
#'
#' @export
#'
#' @examples
#' weighted_lin_reg() %>%
#'   set_engine("weighted_glmnet") %>%
#'   set_mode("regression") %>%
#'   translate()
#'
weighted_lin_reg <-
  function(mode = "regression",
           penalty = NULL,
           mixture = NULL) {

    # Check for correct mode
    if (mode  != "regression") {
      rlang::abort("`mode` should be 'regression'")
    }

    args <- list(
      penalty = rlang::enquo(penalty),
      mixture = rlang::enquo(mixture)
    )

    parsnip::new_model_spec(
      "weighted_lin_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }


#' @param object A weighted linear regression model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- weighted_lin_reg(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#' @method update weighted_lin_reg
#' @rdname weighted_lin_reg
#' @export
update.weighted_lin_reg <-
  function(object,
           parameters = NULL,
           penalty = NULL, mixture = NULL,
           fresh = FALSE, ...) {

    object <- parsnip:::update.linear_reg(object,
                                          parameters = parameters,
                                          penalty = !! penalty,
                                          mixture = !! mixture,
                                          fresh = fresh,
                                          ...)
    parsnip::new_model_spec(
      "weighted_lin_reg",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

#' @export
translate.weighted_lin_reg <- function(x, engine = x$engine, ...) {
  x <- parsnip::translate.default(x, engine, ...)

  if (engine %in% c("glmnet", "weighted_glmnet")) {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    x$method$fit$args$lambda <- NULL
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
  }

  x
}


#' @export
min_grid.weighted_lin_reg <- tune:::min_grid.linear_reg
