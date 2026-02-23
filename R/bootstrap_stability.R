#' Bootstrap selection frequency for each feature.
#'
#' @description Runs the feature selection procedure on each of B bootstrap
#'   samples (resample rows of (X, Y) with replacement) and computes
#'   f_j = (# times feature j is selected) / B.
#'
#' @param X n x p numeric matrix of data.
#' @param Y Length-n vector of class labels (0 and 1).
#' @param select_fun A function that takes (X, Y) and returns an integer
#'   vector of selected feature indices (1-based). E.g. a wrapper that calls
#'   select_features_permutation with fixed stat_fun and alpha_level.
#' @param B Number of bootstrap resamples.
#' @param seed Optional random seed.
#' @return Numeric vector of length p giving selection frequency per feature.
#'
#' @examples
#' set.seed(1)
#' sim <- simulate_mixture_data(n = 80, p = 15, n_relevant = 4, alpha = 0.8, seed = 3)
#' select_wrapper <- function(X, Y) {
#'   select_features_permutation(X, Y, stat_mean_diff, n_shuffles = 199, alpha_level = 0.05)
#' }
#' freq <- bootstrap_selection_frequency(sim$X, sim$Y, select_wrapper, B = 50, seed = 42)
#' which(freq > 0.5)
#'
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
#' @description Computes bootstrap selection frequencies and returns features
#'   with frequency >= threshold.
#'
#' @param X n x p numeric matrix.
#' @param Y Length-n class labels (0 and 1).
#' @param select_fun Function (X, Y) -> selected indices.
#' @param B Number of bootstrap runs.
#' @param threshold Minimum selection frequency (0 to 1) to include a feature.
#' @param seed Optional random seed.
#' @return Integer vector of selected feature indices; attribute "frequency"
#'   contains the length-p frequency vector.
#'
#' @examples
#' set.seed(1)
#' sim <- simulate_mixture_data(n = 80, p = 15, n_relevant = 4, alpha = 0.8, seed = 3)
#' select_wrapper <- function(X, Y) {
#'   select_features_permutation(X, Y, stat_cvm, n_shuffles = 199, alpha_level = 0.05)
#' }
#' sel <- select_features_bootstrap(sim$X, sim$Y, select_wrapper, B = 50, threshold = 0.5, seed = 42)
#'
#' @export
select_features_bootstrap <- function(X, Y, select_fun, B = 100, threshold = 0.5, seed = NULL) {
  freq <- bootstrap_selection_frequency(X, Y, select_fun, B = B, seed = seed)
  out <- which(freq >= threshold)
  attr(out, "frequency") <- freq
  out
}
