
rf_variants <- list(`v2+b` = c(rplen = FALSE, closer = FALSE, ceiling = FALSE),
                    R1 = c(rplen = TRUE, closer = TRUE, ceiling = FALSE),
                    R2 = c(rplen = FALSE, closer = FALSE, ceiling = TRUE),
                    R3 = c(rplen = TRUE, closer = TRUE, ceiling = TRUE))
rf_ceiling_s <- 0.25
rf_v2b <- c(prev = FALSE, track = TRUE, split = TRUE)

rf_closer <- function(d) as.numeric(d$role == "RP" & data.table::fcoalesce(as.numeric(d$sv3), 0) >= 30)

rf_X <- function(d, b, stage, spec, v) {
  X <- v2f_X(d, b, stage, spec, rf_v2b)
  if (v[["closer"]]) {
    had <- "f3_sv" %in% colnames(X)
    if (had) X <- X[, colnames(X) != "f3_sv", drop = FALSE]
    if (had || stage == "aav") X <- cbind(X, closer = rf_closer(d))
  }
  if (v[["rplen"]] && stage == "aav") {
    rp <- as.numeric(d$role == "RP")
    X <- cbind(X, len1_RP = X[, "len1"] * rp, len2_RP = X[, "len2"] * rp, len_age_RP = X[, "len_age"] * rp)
  }
  X
}

rf_soft_cap <- function(mu, is_rp, c, s = rf_ceiling_s) ifelse(is_rp, c - s * log1p(exp((c - mu) / s)), mu)

rf_fit_stage <- function(d, b, stage, spec, levels, v) {
  X <- rf_X(d, b, stage, spec, v)
  if (stage == "aav") fit_cens(X, winter_dummies(d$winter, levels), v2_Z(d, spec$spread), d$y, d$cens)
  else fit_years(X, winter_dummies(d$winter, levels, drop_first = TRUE), d$years7)
}

rf_run <- function(d, te, w, ky, ka, v, ndraw = 2000, label = "v2+b", obs = TRUE) {
  b <- v2_basis(d); levels <- sort(unique(d$winter))
  fy <- rf_fit_stage(d, b, "years", ky, levels, v); fa <- rf_fit_stage(d, b, "aav", ka, levels, v)
  hy <- heat_for(winter_levels(fy, "years", levels), w, "forecast"); ha <- heat_for(winter_levels(fa, "aav", levels), w, "forecast")
  P <- pred_years(fy, rf_X(te, b, "years", ky, v), hy)
  Za <- v2_Z(te, ka$spread)
  cmax <- max(d[role == "RP" & !cens, y]); is_rp <- te$role == "RP"
  cap <- function(p) { if (v[["ceiling"]]) p$mu <- rf_soft_cap(p$mu, is_rp, cmax); p }
  seven <- stats::median(d[years >= 7, pmin(years, 10)])
  at_len <- function(L) { t2 <- data.table::copy(te); t2[, `:=`(years = L, lyr = log(pmin(L, 10)))]; cap(pred_aav(fa, rf_X(t2, b, "aav", ka, v), Za, ha)) }
  menu <- lapply(c(1:6, seven), at_len)
  Lmed <- years_quant(P, 0.5)
  if (!obs) {
    mu <- vapply(seq_len(nrow(te)), function(i) menu[[Lmed[i]]]$mu[i], 0)
    return(data.table::data.table(name = te$name, mlbam_id = te$mlbam_id, branch = te$branch, role = te$role, years_med = Lmed, aav_med_share = exp(mu)))
  }
  pa <- cap(pred_aav(fa, rf_X(te, b, "aav", ka, v), Za, ha))
  set.seed(v2_seed)
  sims <- t(vapply(seq_len(nrow(te)), function(i) {
    k <- sample.int(7, ndraw, replace = TRUE, prob = P[i, ])
    mu <- vapply(menu, function(m) m$mu[i], 0)[k]; sd <- vapply(menu, function(m) m$sd[i], 0)[k]
    a <- stats::rnorm(ndraw, mu, sd); L <- c(1:6, seven)[k]
    c(stats::quantile(a, c(0.1, 0.5, 0.9)), stats::quantile(a + log(L), c(0.1, 0.5, 0.9)))
  }, numeric(6)))
  rbind(
    te[, .(signing_id, winter, role, model = label, outcome = "years", med = Lmed, obs = years7, cens = FALSE)],
    te[, .(signing_id, winter, role, model = label, outcome = "aav_at_years", med = pa$mu, obs = y, cens)],
    te[, .(signing_id, winter, role, model = label, outcome = "aav_uncond", med = sims[, 2], obs = y, cens)],
    te[, .(signing_id, winter, role, model = label, outcome = "total", med = sims[, 5], obs = ltotal, cens)])
}

rf_backtest <- function(all, screen, variants = rf_variants, targets = v2_target_winters, workers = 7L) {
  old <- future::plan(future::multisession, workers = workers); on.exit(future::plan(old), add = TRUE)
  fs <- lapply(targets, function(w) future::future({
    suppressWarnings(suppressMessages({ for (f in list.files("R", full.names = TRUE)) source(f) }))
    scr <- data.table::as.data.table(screen)[span_target == w]
    ky <- v2_kept_spec(scr, "years", TRUE); ka <- v2_kept_spec(scr, "aav", TRUE)
    d <- all[winter < w]; te <- all[winter == w]
    data.table::rbindlist(lapply(names(variants), function(n) rf_run(d, te, w, ky, ka, variants[[n]], label = n)))
  }, seed = TRUE, globals = list(all = all, screen = screen, variants = variants, w = w)))
  data.table::rbindlist(lapply(fs, future::value))
}

rf_scores <- function(bt, tiers, payroll, threshold = 15e6) {
  s <- data.table::copy(data.table::as.data.table(bt))[, signing_id := as.character(signing_id)]
  s <- merge(s, data.table::as.data.table(tiers)[, .(signing_id = as.character(signing_id), tier)], by = "signing_id", all.x = TRUE)
  pa <- s[outcome == "aav_at_years", .(signing_id, model, pred = exp(med) * payroll)]
  base_hi <- pa[model == "v2+b" & pred > threshold, signing_id]
  u <- s[outcome == "years" | !cens]
  s1 <- "S1: RP predicted > $15M (union)"
  data.table::rbindlist(lapply(setdiff(unique(s$model), "v2+b"), function(m) {
    hi <- union(base_hi, pa[model == m & pred > threshold, signing_id])
    keep <- list(u$role == "RP" & u$signing_id %in% hi, u$role == "RP", rep(TRUE, nrow(u)), u$tier %in% "star")
    names(keep) <- c(s1, "RP", "all", "stars")
    want <- list(c(s1, "aav_at_years"), c(s1, "total"), c("RP", "aav_at_years"), c("RP", "total"),
                 c("all", "aav_at_years"), c("all", "total"), c("all", "years"), c("stars", "total"))
    data.table::rbindlist(lapply(c(m, "v2+b"), function(mm) data.table::rbindlist(lapply(want, function(k) {
      x <- u[keep[[k[1]]] & u$model == mm & u$outcome == k[2]]
      data.table::data.table(model = mm, subset = k[1], outcome = k[2], n = nrow(x), mae = mean(abs(x$obs - x$med)), bias = mean(x$obs - x$med))
    }))))[, compared_with := m][]
  }))
}
rf_rule <- function(sc, tol = 5e-4) {
  r <- data.table::rbindlist(lapply(unique(sc$compared_with), function(m) {
    x <- sc[compared_with == m]; g <- function(mm, sub, oc, k = "mae") x[model == mm & subset == sub & outcome == oc][[k]]
    s1 <- "S1: RP predicted > $15M (union)"
    data.table::data.table(variant = m, s1_n = g(m, s1, "aav_at_years", "n"), s1_bias = g(m, s1, "aav_at_years", "bias"), s1_bias_base = g("v2+b", s1, "aav_at_years", "bias"),
                           s1_mae = g(m, s1, "aav_at_years"), s1_mae_base = g("v2+b", s1, "aav_at_years"),
                           total_all = g(m, "all", "total"), total_all_base = g("v2+b", "all", "total"),
                           rp_total = g(m, "RP", "total"), rp_total_base = g("v2+b", "RP", "total"))
  }))
  r[, `:=`(c1 = abs(s1_bias) < abs(s1_bias_base), c2 = total_all <= total_all_base + tol, c3 = rp_total <= rp_total_base + tol)]
  r[, passes := c1 & c2 & c3]
  ok <- r[passes == TRUE][order(total_all)]
  pick <- if (nrow(ok)) { best <- ok$total_all[1]; ok[total_all <= best + 0.001][order(s1_mae)]$variant[1] } else "v2+b"
  r[, adopted := variant == pick][]
}

rf_cap_prices <- function(tool, payroll, record_rp) {
  x <- data.table::copy(data.table::as.data.table(tool)); c0 <- log(record_rp / payroll)
  is_rp <- x$role %in% c("Reliever", "RP")
  capd <- function(v) ifelse(is_rp & !is.na(v) & v > 0, exp(rf_soft_cap(log(v / payroll), rep(TRUE, length(v)), c0)) * payroll, v)
  for (k in intersect(c("model_total", sprintf("total_%dy", 1:10)), names(x))) data.table::set(x, j = k, value = as.numeric(x[[k]]))
  for (k in intersect(c("model_aav", "model_aav_low", "model_aav_high", "qo_model_aav", sprintf("aav_%dy", 1:10)), names(x))) data.table::set(x, j = k, value = capd(as.numeric(x[[k]])))
  for (L in 1:10) { a <- sprintf("aav_%dy", L); t <- sprintf("total_%dy", L); if (all(c(a, t) %in% names(x))) data.table::set(x, j = t, value = x[[a]] * L) }
  if (all(c("model_total", "model_years", "model_aav") %in% names(x))) x[is_rp, model_total := model_aav * as.numeric(model_years)]
  x[, reliever_ceiling := is_rp][]
}
