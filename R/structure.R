
st_defaults <- function(rates_path = "data/cbt_rates.csv") {
  r <- 0.0387
  if (file.exists(rates_path)) { x <- utils::read.csv(rates_path); r <- as.numeric(x[[2]][which.max(as.integer(x[[1]]))]) }
  ppw <- 0.0402 * 174.0e6
  list(ppw = ppw, floor = 0.0045 * 174.0e6, g = 0.0385, r = r, vpw = ppw, value_basis = "placeholder: market price per win")
}

st_extend_paths <- function(war, pt, L) {
  Y <- ncol(war)
  if (Y >= L) return(list(war = war[, seq_len(L), drop = FALSE], pt = pt[, seq_len(L), drop = FALSE], extended = 0L))
  mu <- colMeans(war); step <- if (Y >= 2) mu[Y] - mu[Y - 1] else 0
  W <- cbind(war, matrix(0, nrow(war), L - Y)); P <- cbind(pt, matrix(pt[, Y], nrow(pt), L - Y))
  for (y in (Y + 1):L) W[, y] <- W[, y - 1] + step
  list(war = W, pt = P, extended = L - Y)
}

st_update_proj <- function(war, j, mu) {
  L <- ncol(war); w <- pmax(1, 5 - (seq_len(j) - 1))
  rec <- as.vector(war[, j:1, drop = FALSE] %*% w) / sum(w)
  base <- 0.5 * rec + 0.5 * mu[j + 1]
  outer(base, mu[(j + 1):L] - mu[j + 1], `+`)
}

st_price <- function(contract, war, pt = NULL, p = st_defaults()) {
  S <- as.data.frame(contract$seasons); N <- nrow(S)
  for (v in c("buyout", "vest_threshold")) if (is.null(S[[v]])) S[[v]] <- 0
  S$buyout[is.na(S$buyout)] <- 0
  if (is.null(S$vest_basis)) S$vest_basis <- "prior"; if (is.null(S$if_not_vested)) S$if_not_vested <- "void"
  if (is.null(pt)) pt <- matrix(Inf, nrow(war), ncol(war))
  ex <- st_extend_paths(war, pt, N); W <- ex$war; P <- ex$pt; n <- nrow(W)
  mu <- colMeans(W)
  d <- (1 + p$r)^-(seq_len(N + 30) - 1)
  gr <- (1 + p$g)^(seq_len(N) - 1)
  vpw <- p$vpw * gr; ppw <- p$ppw * gr; fl <- p$floor * gr
  defer <- if (is.null(contract$deferrals) || !NROW(contract$deferrals)) data.frame(earned_season = integer(), pay_season = integer(), amount = numeric()) else as.data.frame(contract$deferrals)
  bonus <- if (is.null(contract$bonus) || !NROW(contract$bonus)) data.frame(pay_season = integer(), amount = numeric()) else as.data.frame(contract$bonus)
  opt_outs <- if (is.null(contract$opt_outs)) integer() else as.integer(contract$opt_outs)
  active <- rep(TRUE, n); cost <- numeric(n); value <- numeric(n)
  exercised <- list()
  for (s in seq_len(N)) {
    k <- S$kind[s]
    if (k != "guaranteed") {
      dec <- rep(FALSE, n)
      proj <- if (s >= 2) st_update_proj(W, s - 1, mu)[, 1] else rep(mu[1], n)
      club_rule <- function() vpw[s] * proj > S$salary[s] - S$buyout[s]
      if (k == "player_option") dec <- S$salary[s] > fl[s] + ppw[s] * pmax(0, proj)
      if (k == "club_option") dec <- club_rule()
      if (k == "mutual_option") dec <- rep(FALSE, n)
      if (k == "vesting_option") {
        ptv <- if (s < 2) rep(Inf, n) else if (S$vest_basis[s] == "cumulative") rowSums(P[, 1:(s - 1), drop = FALSE]) else P[, s - 1]
        vest <- ptv >= S$vest_threshold[s]
        dec <- vest | (!vest & S$if_not_vested[s] == "club_option" & club_rule())
        exercised[[paste0("vested_season_", s)]] <- mean(vest[active])
      }
      exercised[[paste0(k, "_season_", s)]] <- if (any(active)) mean(dec[active]) else NA_real_
      declined <- active & !dec
      cost[declined] <- cost[declined] + d[s] * S$buyout[s]
      active <- active & dec
    }
    df_s <- sum(defer$amount[defer$earned_season == s])
    cost[active] <- cost[active] + d[s] * (S$salary[s] - df_s)
    for (i in which(defer$earned_season == s)) cost[active] <- cost[active] + d[defer$pay_season[i]] * defer$amount[i]
    value[active] <- value[active] + d[s] * vpw[s] * W[active, s]
    if (s %in% opt_outs && s < N) {
      rest <- (s + 1):N
      proj <- st_update_proj(W, s, mu)
      mkt <- as.vector(sweep(sweep(pmax(proj, 0), 2, ppw[rest], `*`), 2, fl[rest], `+`) %*% d[rest])
      sal <- sum(d[rest] * S$salary[rest])
      out <- active & mkt > sal
      exercised[[paste0("opt_out_after_", s)]] <- if (any(active)) mean(out[active]) else NA_real_
      active <- active & !out
    }
  }
  if (nrow(bonus)) cost <- cost + sum(d[bonus$pay_season] * bonus$amount)
  surplus <- value - cost
  Lb <- sum(S$kind %in% c("guaranteed", "player_option"))
  ev_plain <- mean(as.vector(sweep(W[, seq_len(Lb), drop = FALSE], 2, vpw[seq_len(Lb)] * d[seq_len(Lb)], `*`) %*% rep(1, Lb)))
  ge <- (ev_plain - mean(surplus)) / sum(d[seq_len(Lb)])
  list(summary = data.frame(e_cost = mean(cost), e_value = mean(value), e_surplus = mean(surplus), p_negative = mean(surplus < 0),
                            ge_aav = ge, base_term = Lb, seasons_extended = ex$extended, value_basis = p$value_basis),
       exercise = unlist(exercised), paths = data.frame(cost = cost, value = value, surplus = surplus))
}

st_plain <- function(L, aav) list(seasons = data.frame(season = seq_len(L), salary = rep(aav, L), kind = "guaranteed"))
