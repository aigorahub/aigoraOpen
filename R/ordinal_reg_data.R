make_ordinal_reg <- function() {
  parsnip::set_new_model("ordinal_reg")
  parsnip::set_model_mode(model = "ordinal_reg", mode = "classification")
  parsnip::set_model_engine("ordinal_reg", mode = "classification", eng = "glmnetcr")
  parsnip::set_dependency("ordinal_reg", eng = "glmnetcr", pkg = "glmnetcr")

  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "glmnetcr",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "glmnetcr",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    # has_submodel = TRUE,
    has_submodel = TRUE
  )

  parsnip::set_fit(
    model = "ordinal_reg",
    eng = "glmnetcr",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "glmnetcr", fun = "glmnetcr"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "ordinal_reg",
    eng = "glmnetcr",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = TRUE
    )
  )

  parsnip::set_pred(
    model = "ordinal_reg",
    eng = "glmnetcr",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = organize_glmnetcr_class,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "ordinal_reg",
    eng = "glmnetcr",
    mode = "classification",
    type = "prob",
    value = parsnip::pred_value_template(
      post = organize_glmnetcr_prob,
      func = c(fun = "predict"),
      # Now everything else is put into the `args` slot
      object = quote(object$fit),
      newx = quote(new_data)
    )
  )

}
