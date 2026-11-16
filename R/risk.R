
rk_lambda_scale <- 5

rk_weights <- function(n, rho) {
  q <- (seq_len(n) - 0.5) / n
  w <- exp(-rk_lambda_scale * rho * q)
  w / mean(w)
}

rk_score <- function(surplus, rho = 0) {
  s <- sort(surplus)
  sum(s * rk_weights(length(s), rho)) / length(s)
}

rk_summary <- function(surplus, rho = 0) {
  q <- stats::quantile(surplus, c(0.1, 0.9), names = FALSE)
  data.frame(e_surplus = mean(surplus), q10 = q[1], q90 = q[2], score = rk_score(surplus, rho), p_negative = mean(surplus < 0), rho = rho)
}

rk_plain_surplus <- function(W, A, p, mult = 1) {
  L <- ncol(W); d <- (1 + p$r)^-(seq_len(L) - 1); gr <- (1 + p$g)^(seq_len(L) - 1)
  as.vector(W %*% (mult * p$vpw * gr * d)) - A * sum(d)
}

rk_walkaway <- function(make_contract, W, P, p, rho = 0, plain = FALSE, upper = 1e8) {
  if (plain) {
    L <- nrow(make_contract(1)$seasons)
    Wl <- st_extend_paths(W, P, L)$war
    d <- (1 + p$r)^-(seq_len(L) - 1); gr <- (1 + p$g)^(seq_len(L) - 1)
    return(rk_score(as.vector(Wl %*% (p$vpw * gr * d)), rho) / sum(d))
  }
  f <- function(A) rk_score(st_price(make_contract(A), W, P, p)$paths$surplus, rho)
  lo <- f(0)
  if (lo <= 0) return(0)
  hi <- f(upper)
  if (hi > 0) return(upper)
  stats::uniroot(f, c(0, upper), tol = upper * 1e-5)$root
}
