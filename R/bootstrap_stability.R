#' Bootstrap selection frequency for each feature.
#'
#' @param X n x p numeric matrix of data.
#' @param Y Length-n vector of class labels (0 and 1).
#' @param select_fun A function that takes (X, Y) and returns selected indices.
#' @param B Number of bootstrap resamples.
#' @param seed Optional random seed.
#' @return Numeric vector of length p giving selection frequency per feature.
#' @importFrom stats runif
#' @export
bootstrap_selection_frequency <- function(X, Y, select_fun, B = 100, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(X)
  p <- ncol(X)
  count_selected <- integer(p)
  for (b in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    X_b <- X[idx, , drop = FALSE]
    Y_b <- Y[idx]
    selected <- select_fun(X_b, Y_b)
    for (j in selected) count_selected[j] <- count_selected[j] + 1
  }
  count_selected / B
}

#' Select features by bootstrap stability threshold.
#'
#' @param X n x p numeric matrix.
#' @param Y Length-n class labels (0 and 1).
#' @param select_fun Function (X, Y) -> selected indices.
#' @param B Number of bootstrap runs.
#' @param threshold Minimum selection frequency to include a feature.
#' @param seed Optional random seed.
#' @return Integer vector of selected feature indices.
#' @export
select_features_bootstrap <- function(X, Y, select_fun, B = 100, threshold = 0.5, seed = NULL) {
  freq <- bootstrap_selection_frequency(X, Y, select_fun, B = B, seed = seed)
  out <- which(freq >= threshold)
  attr(out, 'frequency') <- freq
  out
}
