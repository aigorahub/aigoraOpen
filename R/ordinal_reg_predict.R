
#' @export
#' @rdname predict
get_prob_for_lambda <- function(x, object, lambda) {
  if(any(lambda  == object$fit$lambda)) {
    res <- tibble::as_tibble(x$probs[, , which(lambda  == object$fit$lambda)])
  } else if(lambda > max(object$fit$lambda)) {
    rlang::warn(glue::glue("Chosen penalty = {lambda} is greater than the largest available value. ",
                           "Returning predictions for penalty = {max(object$fit$lambda)}."))
    res <- tibble::as_tibble(x$probs[, , which.max(object$fit$lambda)])
  } else if(lambda < min(object$fit$lambda)) {
    rlang::warn(glue::glue("Chosen penalty = {lambda} is smaller than the smallest available value. ",
                           "Returning predictions for penalty = {min(object$fit$lambda)}."))
    res <- tibble::as_tibble(x$probs[, , which.min(object$fit$lambda)])
  } else {
    res <- (x$probs[, , min(which(lambda > object$fit$lambda))] +
              x$probs[, , max(which(lambda < object$fit$lambda))]) / 2
  }

  res
}

organize_glmnetcr_class <- function(x, object) {
  # browser()

  if (dim(x$class)[2] == 1) {
    res <- x$class[,1]
  } else if (!is.null(object$spec$args$penalty)) {
    if(length(object$spec$args$penalty) == 1) {
      probs <- get_prob_for_lambda(x, object, object$spec$args$penalty)
      res <- colnames(probs)[apply(probs, 1, which.max)]
    } else {
      res <- tibble::tibble()
      for(lambda in object$spec$args$penalty) {
        probs <- get_prob_for_lambda(x, object, lambda)
        res_tmp <- tibble::tibble(
          values = colnames(probs)[apply(probs, 1, which.max)],
          lambda = lambda
        )
        res <- rbind(res, res_tmp)
      }
    }
  } else {
    n <- dim(x$class)[1]
    res <- utils::stack(as.data.frame(x$class))
    res$lambda <- rep(object$fit$lambda, each = n)
    res <- res[, colnames(res) %in% c("values", "lambda")]
  }
  res
}

organize_glmnetcr_prob <- function(x, object) {

  if (dim(x$probs)[3] == 1) {
    res <- tibble::as_tibble(x$probs[, , 1])
  } else if (!is.null(object$spec$args$penalty)) {
    if(length(object$spec$args$penalty) == 1) {
      res <- tibble::as_tibble(get_prob_for_lambda(x, object, object$spec$args$penalty))
    } else {
      res <- tibble::tibble()
      for(lambda in object$spec$args$penalty) {
        res_tmp <- tibble::as_tibble(get_prob_for_lambda(x, object, lambda))
        res_tmp$lambda <- lambda
        res <- rbind(res, res_tmp)
      }
    }
  } else {
    n <- dim(x$probs)[1]
    res <- tibble::as_tibble(apply(x$probs, 2, rbind))
    res$lambda <- rep(object$fit$lambda, each = n)
  }
  res
}

check_glmnetcr_penalty <- function (penalty = NULL, object, multi = FALSE)
{
  if (all(class(penalty) == c("quosure", "formula"))) {
    penalty <- rlang::get_expr(rlang::eval_tidy(penalty))
  }
  if (is.null(penalty)) {
    penalty <- object$fit$lambda
  }
  if (!multi) {
    if (length(penalty) != 1)
      rlang::abort(glue::glue("`penalty` should be a single numeric value. `multi_predict()` ",
                              "can be used to get multiple predictions per row of data.",
      ))
  }

  if (length(object$fit$lambda) == 1 && penalty != object$fit$lambda)
    rlang::abort(glue::glue("The glmnetcr model was fit with a single penalty value of ",
                            "{object$fit$lambda}. Predicting with a value of {penalty} ",
                            "will give incorrect results from `glmnetcr()`."))
  penalty
}


#' @export
#' @rdname predict
predict._glmnetcr <- function(object, new_data, type = NULL, opts = list(), penalty = NULL, multi = FALSE, ...) {
  if (any(names(rlang::enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
    penalty <- object$spec$args$penalty
  }

  # if(rlang::is_lang(penalty)) {
  #   penalty <- rlang::eval_
  # }

  object$spec$args$penalty <- check_glmnetcr_penalty(penalty, object, multi)

  object$spec <- parsnip::eval_args(object$spec)
  parsnip::predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}

#' @export
#' @rdname multi_predict
multi_predict._glmnetcr <-
  function(object, new_data, type = NULL, penalty = NULL, ...) {
    if (any(names(rlang::enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    if (rlang::is_quosure(penalty))
      penalty <- rlang::eval_tidy(penalty)

    dots <- list(...)

    object$spec$args$penalty <- check_glmnetcr_penalty(penalty, object, multi = TRUE)

    if (is.null(type))
      type <- "class"
    if (!(type %in% c("class", "prob"))) {
      rlang::abort("`type` should be either 'class' or 'prob'.")
    }

    object$spec <- parsnip::eval_args(object$spec)
    pred <- parsnip::predict.model_fit(object, new_data = new_data, type = type, opts = dots)

    if (type == "class") {
      pred <- pred$.pred_class
      colnames(pred) <- c(".pred_class", "penalty")
    } else if (type == "prob") {
      colnames(pred)[ncol(pred)] <- "penalty"
    }

    .row <- rep(1:nrow(new_data), length(unique(pred$penalty)))
    pred <- split(pred, .row)
    names(pred) <- NULL
    tibble::tibble(.pred = pred)
  }


#' These are internal functions not meant to be directly called by the user.
#'
#' @param object fitted model
#' @param new_data new data
#' @param ... other arguments
#'
#' @export
predict_class._glmnetcr <- function(object, new_data, ...) {
  if (any(names(rlang::enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- parsnip::eval_args(object$spec)
  parsnip::predict_class.model_fit(object, new_data = new_data, ...)
}
