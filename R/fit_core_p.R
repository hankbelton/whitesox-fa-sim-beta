
FIT_FILL_RATE <- local({ f <- "data/fill_rate.csv"; if (file.exists(f)) { x <- data.table::fread(f); setNames(x$fill_rate, x$season) } else NULL })
FIT_SLOT_UNITS <- c(SP = 150 / 180, RP = 60 / 180)
fit_pool_fill <- function(pool, rates = FIT_FILL_RATE, groups = alt_groups, seasons = 2027:2031, cap_pa = 650) {
  pool <- data.table::copy(data.table::as.data.table(pool)); if (is.null(rates)) return(pool)
  if (!"pt" %in% names(pool)) pool[, pt := NA_real_]
  pool <- pool[source != "fill-in"]
  pt_eff <- function(x) { p <- x$pt; d <- unname(fit_pt_default[x$source]); p[is.na(p) | p <= 0] <- d[is.na(p) | p <= 0]; p[is.na(p)] <- cap_pa; p }
  add <- list()
  for (g in groups$group) for (s in seasons) { k <- groups$slots[match(g, groups$group)]; r <- rates[[as.character(s)]]; x <- pool[group == g & season == s]
    if (g %in% fit_pt_hitter_groups) { e <- k * cap_pa - sum(pt_eff(x)); if (e > 1e-6) add[[length(add) + 1L]] <- data.table(mlbam_id = NA_integer_, name = "fill-in (new players at the league rate)", season = s, w = r * e / 600, group = g, source = "fill-in", pt = e)
    } else if (g %in% names(FIT_SLOT_UNITS) && nrow(x) < k) add[[length(add) + 1L]] <- data.table(mlbam_id = NA_integer_, name = "fill-in (new players at the league rate)", season = s,
                                                                                                    w = r * FIT_SLOT_UNITS[[g]], group = g, source = "fill-in", pt = NA_real_)[rep(1L, k - nrow(x))] }
  rbind(pool, data.table::rbindlist(add), fill = TRUE) }

fit_disp_one <- function(x, k, d, cap_pa = 650) {
  if (!nrow(x)) return(0); p <- x$pt; dflt <- unname(fit_pt_default[x$source]); p[is.na(p) | p <= 0] <- dflt[is.na(p) | p <= 0]; p[is.na(p)] <- cap_pa
  x <- data.table::data.table(w = x$w, pt = p)[, rate := w / pt][order(-rate)]; cap <- k * cap_pa
  x[, counted := pmax(0, pmin(pt, cap - (cumsum(pt) - pt)))]
  over <- min(d, max(0, sum(x$counted) + d - cap)); if (over <= 0) return(0)
  x <- x[counted > 0][order(rate)]; x[, disp := pmax(0, pmin(counted, over - (cumsum(counted) - counted)))]
  sum(x$disp * pmax(x$rate, 0)) }

fit_vec_p <- function(pool, inc, g, L, d = NULL, groups = alt_groups, seasons = 2027:2031, cap_pa = 650) {
  if (g %in% fit_pt_hitter_groups) { k <- groups$slots[match(g, groups$group)]; if (is.null(d)) d <- rep(cap_pa, 5)
    v <- vapply(seq_along(seasons), function(i) fit_disp_one(pool[group == g & season == seasons[i]], k, min(cap_pa, d[min(i, length(d))]), cap_pa), 0)
  } else v <- pmax(inc[inc$group == g][order(season)]$incumbent_w, 0)
  if (!length(v)) v <- rep(0, 5); c(v, rep(tail(v, 1), 10))[seq_len(L)] }
