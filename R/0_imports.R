# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines ordinal_reg in the model database
  make_ordinal_reg()
  # This defines weighted_glmnet method for linear_reg in the model database
  make_weighted_lin_reg()
 }

#' @importFrom parsnip multi_predict
#' @export
parsnip::multi_predict

#' @importFrom parsnip predict_numeric
#' @export
parsnip::predict_numeric

#' @importFrom parsnip predict_raw
#' @export
parsnip::predict_raw

#' @importFrom parsnip translate
#' @export
parsnip::translate

#' @importFrom tune min_grid
#' @export
tune::min_grid

