make_weighted_lin_reg <- function() {
  parsnip::set_new_model("weighted_lin_reg")
  parsnip::set_model_mode(model = "weighted_lin_reg", mode = "regression")
  parsnip::set_model_engine("weighted_lin_reg", "regression", "weighted_glmnet")
  parsnip::set_dependency("weighted_lin_reg", "weighted_glmnet", "glmnet")

  parsnip::set_fit(
    model = "weighted_lin_reg",
    eng = "weighted_glmnet",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      # func = c(pkg = "glmnet", fun = "glmnet"),
      func = c(pkg = "aigoraML", fun = "weighted_glmnet"),
      defaults = list(family = "gaussian")
    )
  )

  parsnip::set_encoding(
    model = "weighted_lin_reg",
    eng = "weighted_glmnet",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = TRUE
    )
  )

  parsnip::set_model_arg(
    model = "weighted_lin_reg",
    eng = "weighted_glmnet",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "weighted_lin_reg",
    eng = "weighted_glmnet",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "weighted_lin_reg",
    eng = "weighted_glmnet",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = parsnip:::organize_glmnet_pred,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(as.matrix(new_data[, rownames(object$fit$beta), drop = FALSE])),
          type = "response",
          s = quote(object$spec$args$penalty)
        )
    )
  )

  parsnip::set_pred(
    model = "weighted_lin_reg",
    eng = "weighted_glmnet",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(object = quote(object$fit),
             newx = quote(as.matrix(new_data)))
    )
  )
}
