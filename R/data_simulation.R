#' Simulate high dimensional data from a mixture model.
#'
#' @description Generates an n x p matrix X and length-n class labels Y from
#'   \eqn{X_{ij} \sim \alpha G_{Y_i}(\theta_j) + (1 - \alpha) H(\phi_j)} for relevant
#'   features j in S; for j not in S, X_ij ~ H(phi_j). Ground truth S and
#'   parameters are stored for evaluation.
#'
#' @param n Sample size.
#' @param p Number of features.
#' @param n_relevant Number of truly relevant features (size of S). Must be <= p.
#' @param alpha Signal strength in `[0,1]`. 
#' @param dist_G Distribution name for the class-conditional part: "normal"
#'   (mean shift by class) or "normal_scale" (mean and scale differ by class).
#' @param dist_H Distribution name for the noise part: "normal" or "uniform".
#' @param seed Optional random seed for reproducibility.
#'
#' @return A list with: \code{X} (n x p matrix), \code{Y} (length n, 0/1 labels),
#'   \code{S} (ground truth relevant feature indices), \code{theta} (parameters
#'   for G per feature), \code{phi} (parameters for H per feature), \code{alpha}.
#'
#' @examples
#' set.seed(42)
#' sim <- simulate_mixture_data(n = 100, p = 20, n_relevant = 5, alpha = 0.8)
#' dim(sim$X)
#' length(sim$Y)
#' length(sim$S)
#'
#' @importFrom stats rnorm runif
#' @export
simulate_mixture_data <- function(n,
                                 p,
                                 n_relevant,
                                 alpha = 0.8,
                                 dist_G = c("normal", "normal_scale"),
                                 dist_H = c("normal", "uniform"),
                                 seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  dist_G <- match.arg(dist_G)
  dist_H <- match.arg(dist_H)

  # Class labels: balanced
  Y <- rep(0:1, length.out = n)
  if (n > 2) Y <- sample(Y)

  # Ground truth relevant set S (random subset of {1,...,p})
  S <- sort(sample.int(p, size = min(n_relevant, p)))

  # Parameters: theta_j for G (class-conditional), phi_j for H (noise)
  # G normal: mean mu_j_0 for class 0, mu_j_1 for class 1; common sd
  mu0 <- rnorm(p, 0, 1)
  mu1 <- mu0 + rnorm(p, 0.5, 0.5)   # mean shift by class
  sigma_G <- runif(p, 0.5, 1.5)
  theta <- list(mu0 = mu0, mu1 = mu1, sigma = sigma_G, dist = dist_G)

  # H: normal with phi_j = (nu_j, tau_j)
  nu <- rnorm(p, 0, 1)
  tau <- runif(p, 0.8, 1.5)
  phi <- list(nu = nu, tau = tau, dist = dist_H)

  # Generate X
  X <- matrix(NA_real_, nrow = n, ncol = p)
  for (j in seq_len(p)) {
    is_relevant <- j %in% S
    for (i in seq_len(n)) {
      if (is_relevant && runif(1) < alpha) {
        # Draw from G_{Y_i}(theta_j)
        X[i, j] <- draw_G(Y[i], j, theta, dist_G)
      } else {
        # Draw from H(phi_j)
        X[i, j] <- draw_H(j, phi, dist_H)
      }
    }
  }

  list(
    X = X,
    Y = Y,
    S = S,
    theta = theta,
    phi = phi,
    alpha = alpha,
    dist_G = dist_G,
    dist_H = dist_H
  )
}


#' Draw one observation from G_y(theta_j).
#' @param y Class label 0 or 1.
#' @param j Feature index.
#' @param theta List with mu0, mu1, sigma, dist.
#' @param dist_G "normal" or "normal_scale".
#' @return Scalar.
#' @noRd
draw_G <- function(y, j, theta, dist_G) {
  if (dist_G == "normal") {
    mu <- if (y == 0) theta$mu0[j] else theta$mu1[j]
    rnorm(1, mean = mu, sd = theta$sigma[j])
  } else {
    # normal_scale: different mean and scale by class
    mu <- if (y == 0) theta$mu0[j] else theta$mu1[j]
    rnorm(1, mean = mu, sd = theta$sigma[j])
  }
}


#' Draw one observation from H(phi_j).
#' @param j Feature index.
#' @param phi List with nu, tau, dist.
#' @param dist_H "normal" or "uniform".
#' @return Scalar.
#' @noRd
draw_H <- function(j, phi, dist_H) {
  if (dist_H == "normal") {
    rnorm(1, mean = phi$nu[j], sd = phi$tau[j])
  } else {
    a <- phi$nu[j] - phi$tau[j]
    b <- phi$nu[j] + phi$tau[j]
    runif(1, min = a, max = b)
  }
}
