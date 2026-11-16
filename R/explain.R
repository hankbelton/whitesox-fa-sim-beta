
ex_shapley <- function(f, levers) {
  k <- length(levers)
  masks <- 0:(2^k - 1)
  on <- function(m) as.logical(bitwAnd(m, 2^(0:(k - 1))))
  v <- vapply(masks, function(m) f(on(m)), 0)
  sz <- vapply(masks, function(m) sum(on(m)), 0)
  ph <- vapply(seq_len(k), function(i) {
    b <- 2^(i - 1)
    without <- masks[bitwAnd(masks, b) == 0]
    sum(vapply(without, function(m) {
      s <- sz[m + 1]; factorial(s) * factorial(k - s - 1) / factorial(k) * (v[m + b + 1] - v[m + 1])
    }, 0))
  }, 0)
  list(base = v[1], total = v[2^k], contrib = stats::setNames(ph, levers))
}

ex_make_contract <- function(spec) {
  g <- function(x, d) if (is.null(x) || length(x) == 0 || is.na(x)) d else x
  L <- as.integer(spec$L); opt <- g(spec$option, "none")
  function(A) {
    bonus_total <- g(spec$bonus_share, 0) * A * L
    sal <- A - bonus_total / L
    S <- data.frame(season = seq_len(L), salary = rep(sal, L), kind = "guaranteed", buyout = 0, vest_threshold = 0,
                    vest_basis = "prior", if_not_vested = "void")
    if (opt != "none") S <- rbind(S, data.frame(season = L + 1L, salary = g(spec$opt_salary_share, 1) * A, kind = opt,
                                                buyout = if (opt == "player_option") 0 else g(spec$opt_buyout_share, 0.1) * A,
                                                vest_threshold = g(spec$vest_threshold, 0), vest_basis = "prior", if_not_vested = "void"))
    oo <- as.integer(g(spec$opt_out_after, 0L))
    ds <- g(spec$defer_share, 0)
    defer <- if (ds > 0) data.frame(earned_season = seq_len(nrow(S)), pay_season = seq_len(nrow(S)) + as.integer(g(spec$defer_lag, 10L)), amount = ds * S$salary) else NULL
    list(seasons = S, opt_outs = if (oo >= 1 && oo < L) oo else integer(),
         bonus = if (bonus_total > 0) data.frame(pay_season = 1L, amount = bonus_total) else NULL, deferrals = defer)
  }
}

ex_is_plain <- function(spec) {
  g <- function(x, d) if (is.null(x) || length(x) == 0 || is.na(x)) d else x
  g(spec$option, "none") == "none" && g(spec$opt_out_after, 0) == 0 && g(spec$defer_share, 0) == 0 && g(spec$bonus_share, 0) == 0
}

ex_levers <- c("projection, aging and injury (WAR paths)", "GM conviction", "player revenue multiplier", "risk appetite", "contract structure")
ex_placeholders <- c("Sox win value and window (placeholder: market price per win)", "park fit", "needs bonus")

ex_walkaway_waterfall <- function(W, P, market_aav, spec, conviction = 0, mult = 1, rho = 0, p = st_defaults()) {
  L <- as.integer(spec$L)
  ex <- st_extend_paths(W, P, max(L + 1L, ncol(W))); Wx <- ex$war; Px <- ex$pt
  gr <- (1 + p$g)^(seq_len(ncol(Wx)) - 1)
  Wmkt <- matrix(market_aav / (p$vpw * gr), nrow(Wx), ncol(Wx), byrow = TRUE)
  Pmkt <- matrix(Inf, nrow(Wx), ncol(Wx))
  upper <- 4 * market_aav + 5e7
  f <- function(on) {
    Wu <- if (on[1]) Wx else Wmkt; Pu <- if (on[1]) Px else Pmkt
    if (on[2] && conviction != 0) Wu <- Wu + conviction * (Pu > 0)
    pu <- p; if (on[3]) pu$vpw <- p$vpw * mult
    sp <- if (on[5]) spec else list(L = L)
    rk_walkaway(ex_make_contract(sp), Wu, Pu, pu, rho = if (on[4]) rho else 0, plain = ex_is_plain(sp), upper = upper)
  }
  sh <- ex_shapley(f, ex_levers)
  rows <- data.frame(step = c("market price (Nov 3 lock, this length)", ex_levers, ex_placeholders, "Sox walkaway AAV"),
                     contribution = c(sh$base, sh$contrib, rep(0, length(ex_placeholders)), sh$total),
                     kind = c("start", rep("lever", length(ex_levers)), rep("placeholder", length(ex_placeholders)), "end"))
  rows$cumulative <- c(sh$base, sh$base + cumsum(c(sh$contrib, rep(0, length(ex_placeholders)))), sh$total)
  attr(rows, "check") <- abs(sh$base + sum(sh$contrib) - sh$total)
  rows
}
