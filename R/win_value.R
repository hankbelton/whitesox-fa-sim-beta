
wv_repl_wins <- 47.7
wv_sharing <- c(low = 0.55, base = 0.48, high = 0.40)
wv_post_mult <- c(low = 1, base = 1.5, high = 2)
wv_counted <- c(wc = 2.4, ds = 3, cs = 4, ws = 4)

wv_rev_per_fan <- function(braves, att, lp, forbes, avg2027 = 174.0e6) {
  b <- data.table::as.data.table(braves)[line_item == "baseball_event", .(season = as.integer(fiscal_year),
        ev = data.table::fcoalesce(as.numeric(value_usd_millions), as.numeric(later_filing_value_usd_millions)) * 1e6)][!is.na(ev)]
  b <- merge(b, data.table::as.data.table(att)[bref == "ATL", .(season, attendance)], by = "season")
  b <- merge(b, data.table::as.data.table(lp)[, .(season, avg = as.numeric(avg_payroll_nominal))], by = "season")
  b[, per_fan_2027 := ev / attendance * avg2027 / avg]
  fb <- data.table::as.data.table(forbes); gv <- function(cl, f) as.numeric(fb[club == cl & field == f, value])
  a25 <- data.table::as.data.table(att)[season == 2025L]
  r_gate <- (gv("CHW", "gate_receipts_musd") / a25[bref == "CHW", attendance]) / (gv("ATL", "gate_receipts_musd") / a25[bref == "ATL", attendance])
  r_tix <- gv("CHW", "average_ticket_price_usd") / gv("ATL", "average_ticket_price_usd")
  base <- mean(b[season %in% 2022:2025, per_fan_2027])
  list(by_year = b, ratio_gate = r_gate, ratio_ticket = r_tix, braves_base = base,
       per_fan = c(low = min(b$per_fan_2027) * r_gate, base = base * r_gate, high = max(b$per_fan_2027) * max(r_gate, r_tix)))
}

wv_postseason <- function(att, pool, lp, seasons = 2022:2025, avg2027 = 174.0e6) {
  a <- data.table::as.data.table(att)[season %in% seasons]
  a[, `:=`(berth = as.integer(made_playoffs %in% c(TRUE, "TRUE", 1)), bye = as.integer(first_round == "DS"))]
  m_berth <- stats::glm(berth ~ wins, binomial, data = a)
  m_bye <- stats::glm(bye ~ wins, binomial, data = a[berth == 1])
  g_round <- function(r, bye) { wc <- if (bye) 0 else wv_counted[["wc"]]
    switch(r, lost_wc = wc, lost_ds = wc + wv_counted[["ds"]], lost_cs = wc + wv_counted[["ds"]] + wv_counted[["cs"]],
           lost_ws = , won_ws = wc + wv_counted[["ds"]] + wv_counted[["cs"]] + wv_counted[["ws"]], 0) }
  b <- a[berth == 1][, games := mapply(g_round, playoff_round, bye == 1)]
  eg <- c(bye = mean(b[bye == 1, games]), nobye = mean(b[bye == 0, games]))
  p <- data.table::as.data.table(pool)[year %in% seasons, .(G = total_pool[1] / 0.6), by = year]
  p <- merge(p, data.table::as.data.table(lp)[, .(year = season, avg = as.numeric(avg_payroll_nominal))], by = "year")
  n_games <- 4 * wv_counted[["wc"]] + 4 * wv_counted[["ds"]] + 2 * wv_counted[["cs"]] + wv_counted[["ws"]]
  p[, net_per_game_2027 := G / n_games * 0.4 * 0.5 * avg2027 / avg]
  list(m_berth = m_berth, m_bye = m_bye, games = eg, net_per_game = mean(p$net_per_game_2027), by_year = p, rows = nrow(a))
}

wv_curve <- function(fans, per_fan, post, r = 0.0387, setting = "base", berth_lift_fans = 114000, W = 40:115) {
  f <- data.table::as.data.table(fans)
  fn <- function(pat) { i <- grepl(pat, f$part); stats::approx(f$W[i], f$fans_per_win[i], xout = W, rule = 2)$y }
  f_now <- fn("^this"); f_next <- fn("^next")
  disc <- c(low = r + 0.02, base = r, high = max(0, r - 0.01))[[setting]]
  keep <- 1 - wv_sharing[[setting]] * 29 / 30
  pf <- per_fan[[setting]]
  gate_mv <- (f_now + f_next / (1 + disc)) * pf * keep
  nd <- data.frame(wins = W)
  pb <- stats::predict(post$m_berth, nd, type = "response"); py <- stats::predict(post$m_bye, nd, type = "response")
  post_rev <- pb * (py * post$games[["bye"]] + (1 - py) * post$games[["nobye"]]) * post$net_per_game * wv_post_mult[[setting]] * keep +
    pb * berth_lift_fans * pf * keep / (1 + disc)
  cum_gate <- cumsum(c(0, gate_mv[-1]))
  R <- cum_gate + post_rev
  R <- R - stats::approx(W, R, xout = wv_repl_wins)$y
  data.table::data.table(setting = setting, W = W, gate_mv = gate_mv, p_berth = pb, post_rev = post_rev, R = R,
                         mv = c(NA, diff(R)))
}

wv_value <- function(war, wb, curve) {
  f <- function(x) { lo <- min(curve$W); hi <- max(curve$W)
    y <- stats::approx(curve$W, curve$R, xout = pmin(pmax(x, lo), hi))$y
    sl_hi <- tail(curve$mv, 1); sl_lo <- curve$mv[2]
    y + pmax(0, x - hi) * sl_hi + pmin(0, x - lo) * sl_lo }
  f(wb + war) - f(wb)
}

wv_equiv_war <- function(W, wb, curve, vpw, mult = 1) {
  W <- as.matrix(W); out <- W
  for (s in seq_len(ncol(W))) out[, s] <- mult * wv_value(W[, s], wb[min(s, length(wb))], curve) / vpw
  out
}

wv_sox_baseline <- function(sox_paths, seasons = 2027:2031, extra = 0) {
  s <- data.table::as.data.table(sox_paths)[season %in% seasons, .(war = sum(pmax(p50, 0), na.rm = TRUE)), by = season]
  s[, wins := wv_repl_wins + war + extra][order(season)]
}

wv_st_price <- function(contract, war, weq, pt = NULL, p = st_defaults()) {
  S <- as.data.frame(contract$seasons); N <- nrow(S)
  for (v in c("buyout", "vest_threshold")) if (is.null(S[[v]])) S[[v]] <- 0
  S$buyout[is.na(S$buyout)] <- 0
  if (is.null(S$vest_basis)) S$vest_basis <- "prior"; if (is.null(S$if_not_vested)) S$if_not_vested <- "void"
  if (is.null(pt)) pt <- matrix(Inf, nrow(war), ncol(war))
  ex <- st_extend_paths(war, pt, N); W <- ex$war; P <- ex$pt; n <- nrow(W)
  E <- st_extend_paths(weq, pt, N)$war
  mu <- colMeans(W); mue <- colMeans(E)
  d <- (1 + p$r)^-(seq_len(N + 30) - 1); gr <- (1 + p$g)^(seq_len(N) - 1)
  vpw <- p$vpw * gr; ppw <- p$ppw * gr; fl <- p$floor * gr
  defer <- if (is.null(contract$deferrals) || !NROW(contract$deferrals)) data.frame(earned_season = integer(), pay_season = integer(), amount = numeric()) else as.data.frame(contract$deferrals)
  bonus <- if (is.null(contract$bonus) || !NROW(contract$bonus)) data.frame(pay_season = integer(), amount = numeric()) else as.data.frame(contract$bonus)
  opt_outs <- if (is.null(contract$opt_outs)) integer() else as.integer(contract$opt_outs)
  active <- rep(TRUE, n); cost <- numeric(n); value <- numeric(n); exercised <- list()
  for (s in seq_len(N)) {
    k <- S$kind[s]
    if (k != "guaranteed") {
      dec <- rep(FALSE, n)
      proj <- if (s >= 2) st_update_proj(W, s - 1, mu)[, 1] else rep(mu[1], n)
      proje <- if (s >= 2) st_update_proj(E, s - 1, mue)[, 1] else rep(mue[1], n)
      club_rule <- function() vpw[s] * proje > S$salary[s] - S$buyout[s]
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
    value[active] <- value[active] + d[s] * vpw[s] * E[active, s]
    if (s %in% opt_outs && s < N) {
      rest <- (s + 1):N
      projr <- st_update_proj(W, s, mu)
      mkt <- as.vector(sweep(sweep(pmax(projr, 0), 2, ppw[rest], `*`), 2, fl[rest], `+`) %*% d[rest])
      out <- active & mkt > sum(d[rest] * S$salary[rest])
      exercised[[paste0("opt_out_after_", s)]] <- if (any(active)) mean(out[active]) else NA_real_
      active <- active & !out
    }
  }
  if (nrow(bonus)) cost <- cost + sum(d[bonus$pay_season] * bonus$amount)
  surplus <- value - cost
  Lb <- sum(S$kind %in% c("guaranteed", "player_option"))
  ev_plain <- mean(as.vector(sweep(E[, seq_len(Lb), drop = FALSE], 2, vpw[seq_len(Lb)] * d[seq_len(Lb)], `*`) %*% rep(1, Lb)))
  ge <- (ev_plain - mean(surplus)) / sum(d[seq_len(Lb)])
  list(summary = data.frame(e_cost = mean(cost), e_value = mean(value), e_surplus = mean(surplus), p_negative = mean(surplus < 0),
                            ge_aav = ge, base_term = Lb, seasons_extended = ex$extended, value_basis = "revenue model (R/win_value.R)"),
       exercise = unlist(exercised), paths = data.frame(cost = cost, value = value, surplus = surplus))
}

wv_walkaway <- function(make_contract, W, weq, P, p, rho = 0, plain = FALSE, upper = 1e8) {
  if (plain) return(rk_walkaway(make_contract, weq, P, p, rho = rho, plain = TRUE))
  f <- function(A) rk_score(wv_st_price(make_contract(A), W, weq, P, p)$paths$surplus, rho)
  lo <- f(0); if (lo <= 0) return(0)
  hi <- f(upper); if (hi > 0) return(upper)
  stats::uniroot(f, c(0, upper), tol = upper * 1e-5)$root
}
