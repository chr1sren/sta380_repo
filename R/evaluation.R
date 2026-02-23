#' Empirical precision and recall for feature selection.
#'
#' @description Given ground truth relevant set S and selected set S_hat,
#'   computes precision = |S cap S_hat| / |S_hat| and recall = |S cap S_hat| / |S|
#'   (with convention 0/0 = 0 for empty denominator).
#'
#' @param S_truth Integer vector of ground truth relevant feature indices (1-based).
#' @param S_hat Integer vector of selected feature indices (1-based).
#' @return A list with components \code{precision} and \code{recall} (scalars).
#'
#' @examples
#' evaluate_precision_recall(S_truth = c(1, 3, 5), S_hat = c(1, 2, 3, 5))
#' evaluate_precision_recall(S_truth = c(1, 2), S_hat = integer(0))
#'
#' @export
evaluate_precision_recall <- function(S_truth, S_hat) {
  S_truth <- as.integer(S_truth)
  S_hat <- as.integer(S_hat)
  n_hat <- length(S_hat)
  n_truth <- length(S_truth)
  n_correct <- length(intersect(S_truth, S_hat))
  precision <- if (n_hat == 0) 0 else n_correct / n_hat
  recall <- if (n_truth == 0) 0 else n_correct / n_truth
  list(precision = precision, recall = recall)
}


#' F1 score from precision and recall.
#'
#' @description F1 = 2 * precision * recall / (precision + recall), or 0 if both are 0.
#'
#' @param precision Scalar in [0, 1].
#' @param recall Scalar in [0, 1].
#' @return Scalar F1 in [0, 1].
#'
#' @examples
#' f1_score(0.8, 0.6)
#'
#' @export
f1_score <- function(precision, recall) {
  if (precision + recall == 0) return(0)
  2 * precision * recall / (precision + recall)
}
