
suppressMessages({ library(shiny); library(bslib); library(data.table); library(DT); library(ggplot2) })
options(DT.fillContainer = FALSE, DT.autoHideNavigation = TRUE)
root <- "."
rp <- function(...) file.path(root, ...)
source(rp("R/data_pull.R")); source(rp("R/payroll_scenarios.R"))
source(rp("R/structure.R")); source(rp("R/structure_l.R")); source(rp("R/risk.R")); source(rp("R/explain.R")); source(rp("R/win_value.R"))
source(rp("R/plausibility.R")); source(rp("R/price_reliever_fix.R")); source(rp("R/alternatives.R")); source(rp("R/fit_core.R")); source(rp("R/fit_core_pt.R"))
source(rp("R/fit_core_p.R"))
local({ f <- rp("data/fill_rate.csv"); x <- if (file.exists(f)) data.table::fread(f) else NULL; FIT_FILL_RATE <<- if (!is.null(x)) setNames(x$fill_rate, x$season) else NULL })
if (!exists("st_extend_paths0")) st_extend_paths0 <- st_extend_paths
st_extend_paths <- function(war, pt, L) { E <- st_extend_paths0(war, pt, L); Y <- ncol(war); if (L > Y) { z <- pt[, Y] <= 0; if (any(z)) E$war[z, (Y + 1):L] <- 0 }; E }
rd <- function(path, ...) if (file.exists(rp(path))) fread(rp(path), encoding = "UTF-8", ...) else NULL
PUBLIC_BUILD <- file.exists("PUBLIC_BUILD") || identical(Sys.getenv("FA_APP_PUBLIC_BUILD"), "1")
FCST_PRED_URL <- "https://www.mlbtraderumors.com/2026/11/2026-27-top-50-free-agents-with-predictions.html"
BETA <- if (PUBLIC_BUILD) div(class = "alert alert-warning py-1 px-2 mb-2 small", "Beta. Numbers may change until the final release.") else NULL
public_mode <- file.exists("PUBLIC_MODE") || identical(Sys.getenv("FA_APP_PUBLIC"), "1") || PUBLIC_BUILD

pa <- rd(if (file.exists(rp("data/roster.csv"))) "data/roster.csv" else "data/none")
pab <- pa[, .(mlbam_id, pos_board = NA_character_, primary_position)]
ledger_file <- if (file.exists(rp("data/payroll_ledger.csv"))) "data/payroll_ledger.csv" else if (file.exists(rp("data/none"))) "data/none" else if (file.exists(rp("data/none"))) "data/none" else "data/none"
ledger_label <- "payroll ledger"
ledger <- rd(ledger_file)
params <- cbt_read_params(rp("data/cbt_thresholds.csv"))
lock <- NULL
preds <- rd(if (file.exists(rp("data/prices.csv"))) "data/prices.csv" else if (file.exists(rp("data/none"))) "data/none" else if (file.exists(rp("data/none"))) "data/none" else if (file.exists(rp("data/none"))) "data/none"
             else if (file.exists(rp("data/none"))) "data/none"
             else if (file.exists(rp("data/none"))) "data/none"
             else if (file.exists(rp("data/none"))) "data/none"
             else if (file.exists(rp("data/none"))) "data/none" else if (file.exists(rp("data/none"))) "data/none"
             else "data/none")
if (is.null(preds)) { cpar <- rd("data/none"); preds <- rf_cap_prices(lock, cpar$payroll_2027, cpar$record_rp_aav_2027)[, price_source := "My model"] }
bounds <- rd("data/role_bounds.csv")
pl_tab <- if (!is.null(bounds)) pl_review_sheet(pl_flags(preds[, .(name = player, mlbam_id, role, branch = branch_shown, model_years, model_aav, aav_1y, aav_2y, aav_3y, aav_4y, aav_5y,
                                                                   aav_6y, aav_7y, aav_8y, aav_9y, aav_10y, estimate_lengths, raise_flag, raise_band, low_confidence)], bounds), "app") else NULL
signed_tab <- local({ fs <- sort(list.files(rp("data/signings"), pattern = "^signings_live.*\\.csv$", full.names = TRUE))
  s0 <- rbindlist(lapply(fs, function(f) read_human_csv(f, colClasses = "character")), fill = TRUE)
  asof <- suppressWarnings(max(c(as.IDate(s0$date_reported), as.IDate(s0$date_official)), na.rm = TRUE))
  x <- unique(s0[in_class == "Y", .(mlbam_id = as.integer(mlbam_id), club, deal_type, years = suppressWarnings(as.integer(years)), aav = suppressWarnings(as.numeric(aav_nominal)))], by = "mlbam_id")
  x[, label := fifelse(deal_type == "minor_league", sprintf("%s (minor league deal)", club), sprintf("%s, %s yr, $%sM a year", club, fcoalesce(as.character(years), "?"), fifelse(is.na(aav), "?", formatC(aav / 1e6, format = "f", digits = 2))))]
  list(tab = x, asof = asof) })
SIGNED_IDS <- signed_tab$tab$mlbam_id; SIGNED_ASOF <- if (is.finite(signed_tab$asof)) format(signed_tab$asof, "%b %d, %Y") else "not available"
live <- NULL

TICKER_ROOT <- if (dir.exists(rp("data/ticker"))) "data/ticker" else "data/none"
ticker_dir <- local({ d <- sort(list.dirs(rp(TICKER_ROOT), recursive = FALSE, full.names = FALSE), decreasing = TRUE)
  d <- d[file.exists(rp(TICKER_ROOT, d, "grading.csv"))]; if (length(d)) d[1] else NA_character_ })
grading <- if (!is.na(ticker_dir)) fread(rp(TICKER_ROOT, ticker_dir, "grading.csv"), encoding = "UTF-8") else NULL
pick_src <- function(target) { for (s in c(if (target == "fa") "steamer", if (target == "fa") "steamer", if (target == "fa") "steamer" else "steamer", if (target == "fa") "steamer", if (target == "fa") "steamer", if (target == "fa") "steamer", "steamer", if (target == "fa") "steamer", "steamer", "steamer", "steamer", "steamer", "steamer")) if (file.exists(rp(sprintf("data/%s_war_paths_%s.csv", target, s)))) return(s); "steamer" }
val_status <- rd(if (file.exists(rp("data/valuation_status.csv"))) "data/valuation_status.csv" else if (file.exists(rp("data/none"))) "data/none" else "data/none")
status_of0 <- function(id) { s <- if (is.null(val_status)) NA_character_ else val_status[mlbam_id == id, status][1]; if (is.na(s)) "not valued: not in the class file" else s }
qo_dec <- if (file.exists(rp("data/qo_decisions.csv"))) read_human_csv(rp("data/qo_decisions.csv"), colClasses = "character") else NULL
QO_DUE <- "QO decision due Nov 17, 3pm CT"
qo_ids <- if (is.null(qo_dec)) integer() else as.integer(qo_dec[!nzchar(response) | is.na(response), mlbam_id])

pick_par <- rd(if (file.exists(rp("data/pick_cost.csv"))) "data/pick_cost.csv" else "data/none")
pick_cost_for <- function(id, moved_ids, scen = "current") {
  if (is.null(pick_par) || !(as.integer(id) %in% qo_ids)) return(0)
  if (scen == "union_2026_05_27") return(0)
  if (scen == "owners_2026_11_03" && isTRUE(preds[mlbam_id == as.integer(id), age_2027][1] - 1 < 30)) return(0)
  if (any(setdiff(as.integer(moved_ids), as.integer(id)) %in% qo_ids)) pick_par$second_qo_cost else pick_par$first_qo_cost }
pick_aav <- function(cost, L) cost / sum((1 + st_p$r)^-(seq_len(min(as.integer(L), 10L)) - 1))
qo_now <- function(x) {
  if (is.null(qo_dec)) return(x$qo_status)
  d <- qo_dec[as.integer(mlbam_id) == x$mlbam_id]
  if (nrow(d)) return(if (nzchar(d$response[1]) && !is.na(d$response[1])) paste0("received a qualifying offer; ", d$response[1]) else paste0("received a qualifying offer; ", QO_DUE))
  if (grepl("^eligible", x$qo_status)) return(sprintf("eligible, but not among the %d players who received a qualifying offer", nrow(qo_dec)))

  if (grepl("^depends on the option", x$qo_status)) return(sprintf("no qualifying offer (not among the %d players who received one on Nov 2)", nrow(qo_dec)))
  x$qo_status }
CATCHER_CAVEAT <-"Catcher ranges are too narrow in testing: 80% ranges held about 70% of catchers' outcomes at years 2-3 (63-71% at years 4-5), and catchers' paths run high. Three fixes were tested and none passed."
path_src <- pick_src("fa")
paths <- rd(sprintf("data/sox_war_paths_%s.csv", pick_src("sox")))
fa_paths <- rd(sprintf("data/fa_war_paths_%s.csv", path_src))
path_badge <- if (grepl("^steamer", path_src)) "Steamer 2027" else "INTERIM: Marcel"
fa27 <- if (!is.null(fa_paths)) fa_paths[year == 1L, .(war27 = sum(p50), lo27 = sum(p10), hi27 = sum(p90)), by = mlbam_id] else NULL
fb_ids <- if (!is.null(fa_paths) && "path_source" %in% names(fa_paths)) unique(fa_paths[grepl("^Marcel", path_source), mlbam_id]) else integer()

LPT_LABEL <- "low playing time, wider range"
lpt_ids <- if (!is.null(fa_paths) && "path_source" %in% names(fa_paths)) unique(fa_paths[grepl("low playing time", path_source), mlbam_id]) else integer()
low20 <- local({ z <- rd("data/placeholder_pitchers.csv"); if (is.null(z)) integer() else as.integer(z$mlbam_id) })
NV_PLACE <- "not valued yet: no real Steamer projection"
status_of <- function(id) { id <- as.integer(id)
  if (id %in% low20) return("not valued: no real Steamer projection (Steamer 2027 gives a 1-IP placeholder, no playing time); he is valued once Steamer gives him playing time")
  if (id %in% lpt_ids) paste0(LPT_LABEL, ": Steamer 2027 projects under 30 IP") else status_of0(id) }
nv_text <- function(id) { id <- as.integer(id); ifelse(id %in% low20, NV_PLACE, ifelse(grepl("import", vapply(id, status_of, "")), "not valued yet: no 2027 projection (new import)", "not valued yet: no 2027 projection")) }

cba_sc <- NULL
CBA_TAG <- "possible rules, not a prediction"
cba_choices <- c(current = "Current rules (in force through Dec 1, 2026)", owners_2026_11_03 = "Owners' latest proposal (Nov 3, 2026)", union_2026_05_27 = "Union's latest proposal (May 27, 2026)")
floor_gaps <- rd("data/payroll_under_floor.csv")
bands <- rd(if (file.exists(rp("data/market_bands.csv"))) "data/market_bands.csv" else if (file.exists(rp("data/none"))) "data/none" else "data/none")
gap_vec <- function(scen, L, mv) { L <- min(as.integer(L), 10L); if (is.null(floor_gaps) || scen == "current") return(rep(0, L))
  g <- floor_gaps[scenario == scen][order(season)]$gap; if (!length(g)) return(rep(0, L)); g <- c(g, rep(tail(g, 1), 10))[seq_len(L)]
  if (!is.null(mv) && nrow(mv)) for (j in seq_len(nrow(mv))) { k <- seq_len(min(as.integer(mv$years[j]), L)); g[k] <- g[k] - mv$aav[j] }
  pmax(0, g) }
disc <- function(L) (1 + st_p$r)^-(seq_len(min(as.integer(L), 10L)) - 1)

REC_AAV <- c(Hitter = 59.2e6, Starter = 52.6e6, Reliever = 24.8e6)

tol_all <- rd("data/spending_limit.csv")
tol_par <- rd("data/none")
tol_windows <- if (is.null(tol_all)) character() else unique(tol_all$window)
tol_tab <- if (is.null(tol_all)) NULL else tol_all[window == tol_windows[1]][order(season)]
DIAL_UP <- 1.08
DIAL_UP_SRC <- "the median push past a club's own prior high (clubs that set a new 10-year payroll high, 2016-2026; Spotrac team payrolls)"
TOL_K <- 10; TOL_RAMP <- 0.9

SOX_CALLS <- c("Fits and a bargain", "Depends on price", "Bargain, but not an upgrade", "Close call", "Fits, but costs more than he's worth to the Sox right now", "Blocked: the Sox already have a better option", "Not valued yet")
SOX_COL <- c("Fits and a bargain" = "#009E73", "Depends on price" = "#CFC79B", "Bargain, but not an upgrade" = "#56B4E9", "Close call" = "#F0E442", "Fits, but costs more than he's worth to the Sox right now" = "#D55E00", "Blocked: the Sox already have a better option" = "#D55E00", "Not valued yet" = "#BBBBBB")
low_price_text <- function(model_aav, fcst_aav, price_source, public) { f <- !is.na(fcst_aav) & !is.na(model_aav) & fcst_aav > 0 & !grepl("^Imports", fcoalesce(price_source, "")) & model_aav < 0.70 * fcst_aav
  ifelse(!f, "", if (public) "My market price is well below public forecasts for this player; treat this call with caution."
         else sprintf("My price is %.0f%% of the public forecast's ($%.1fM vs $%.1fM); this call rests on my price.", 100 * model_aav / fcst_aav, model_aav / 1e6, fcst_aav / 1e6)) }
price_check_text <- function(model_aav, fcst_aav, model_years, fcst_years, price_source, public) {
  ly <- suppressWarnings(as.numeric(model_years)); my <- suppressWarnings(as.numeric(fcst_years))
  ok <- !is.na(fcst_aav) & !is.na(model_aav) & fcst_aav > 0 & !grepl("^Imports", fcoalesce(price_source, ""))
  lo <- ok & model_aav < 0.70 * fcst_aav; long <- ok & !is.na(ly) & !is.na(my) & ly > my + 1; hi <- ok & !lo & (model_aav > 1.30 * fcst_aav | long)
  pct <- sprintf("My price is %.0f%% of the public forecast's ($%.1fM vs $%.1fM%s); this call rests on my price.", 100 * model_aav / fcst_aav, model_aav / 1e6, fcst_aav / 1e6,
                 ifelse(long, sprintf("; %d yr vs %d yr", as.integer(ly), as.integer(my)), ""))
  ifelse(lo, if (public) "My market price is well below public forecasts for this player; treat this call with caution." else pct,
         ifelse(hi, if (public) "My market price is well above public forecasts for this player; treat this call with caution." else pct, "")) }
POSTURE_CHOICES <- c("Going for it" = "going", "Holding (default)" = "holding", "Not going for it" = "not", "None: players under control only" = "none")
POST_RET <- local({ x <- rd("data/posture_retention.csv"); if (is.null(x)) list(w = c(1, 0, 0, 0, 0), p = c(1, 0, 0, 0, 0), ppw = 0) else list(w = c(1, x$r_w), p = c(1, x$r_p), ppw = x$payroll_per_win_2027[1]) })
POSTURE_GAIN <- list(going = c(u75 = 6.710, m = 6.933, h = 6.618), holding = c(u75 = 3.476, m = 4.177, h = 4.070), not = c(u75 = 2.013, m = 3.367, h = 3.721), none = c(u75 = 0, m = 0, h = 0))
OV_FACTS <- list(pay_rank_2026 = 24L, pay_2026 = NA_real_, bat_age = 25.1, bat_age_rank = 29L, pit_age = 28.3, pit_age_rank = 17L,
  lg_slot_war = c(2.78, 2.49, 2.10, 1.15, 0.94), lg_slot_gs = c(31.1, 27.3, 24.0, 20.0, 15.2),
  sal_group = c(hitters = 51.16e6, starters = 30.29e6, relievers = 20.60e6, other = -3.00e6))
CALL_ORDER <- c("Fits and a bargain", "Depends on price", "Bargain, but not an upgrade", "Close call", "Fits, but costs more than he's worth to the Sox right now", "Blocked: the Sox already have a better option", "Not valued yet")
would_replace <- function(who, src, w27) {
  ROOKIE_NAMES <- c("Prelander Berroa", "Drew Thorpe", "David Sandlin", "Tanner Murray", "Hagen Smith", "Tyler Davis", "Tanner McDougal", "Tyler Schweitzer", "Rikuu Nishida")
  debut_2026 <- function(n) { d <- as.character(pa[name == n, mlb_debut_date]); any(grepl("^2026", d)) }
  vapply(seq_along(who), function(i) { w <- who[i]; if (is.na(w)) return(""); if (grepl("^none", w)) return("Would fill an empty spot (replacement level)")
    if (grepl(" + ", w, fixed = TRUE) || grepl("displaced", w, fixed = TRUE)) return(sprintf("Would take playing time from %s: %.2f WAR in 2027 at the position's playing time", w, w27[i]))
    kind <- if (grepl("prospect", fcoalesce(src[i], "")) || w %in% ROOKIE_NAMES) "prospect" else if (grepl("signed", fcoalesce(src[i], ""))) "signed this winter" else if (debut_2026(w)) "rookie in 2026" else "40-man"
    sprintf("Would replace %s (%s, %.2f WAR in 2027)", w, kind, w27[i]) }, "") }
SOX_TXT <- c("Fits and a bargain" = "#FFFFFF", "Depends on price" = "#000000", "Bargain, but not an upgrade" = "#000000", "Close call" = "#000000", "Fits, but costs more than he's worth to the Sox right now" = "#FFFFFF", "Blocked: the Sox already have a better option" = "#FFFFFF", "Not valued yet" = "#000000")
SOX_CALLS <- c(SOX_CALLS, "Signed"); SOX_COL <- c(SOX_COL, "Signed" = "#5F5F5F"); SOX_TXT <- c(SOX_TXT, "Signed" = "#FFFFFF"); CALL_ORDER <- c(CALL_ORDER, "Signed")
skro <- list(png = if (PUBLIC_BUILD) rp("data/skubal_rogers_2027.png") else rp("data/none"),
              rng = rd("data/skubal_rogers_range.csv"))
FLOOR_OWNERS <- 180.2e6
ext_vec <- function(v, L, grow = 0) { v <- as.numeric(v); n <- length(v); if (L <= n) return(v[seq_len(L)]); c(v, tail(v, 1) * (1 + grow)^seq_len(L - n)) }
tol_cost <- function(x, Tt) { a <- TOL_RAMP * Tt; w <- (1 - TOL_RAMP) * Tt; ifelse(x <= a, x, a + (x - a) + (TOL_K - 1) * (pmin(x, Tt) - a)^2 / (2 * w)) }

floor_adj <- function(walk, L, scen, mv) { d <- disc(L); g <- gap_vec(scen, L, mv); V <- walk * sum(d)
  if (is.na(V) || V <= 0 || all(g == 0)) return(walk)
  stats::uniroot(function(A) sum(d * pmax(0, A - g)) - V, c(0, max(g) + V / min(d) + 1))$root }
floor_saving <- function(A, L, scen, mv) { d <- disc(L); sum(d * pmin(A, gap_vec(scen, L, mv))) }
st_p <- st_defaults(rp("data/cbt_rates.csv"))

RP_DEFAULT <- local({ f <- rp("data/reliever_roles.csv"); if (!file.exists(f)) f <- rp("data/none"); if (!file.exists(f)) return(list())
  x <- fread(f)[rp_role %in% c("closer", "setup", "middle")]; stats::setNames(as.list(x$rp_role), as.character(x$mlbam_id)) })
fa_draws <- local({
  f <- rp(sprintf("data/fa_war_paths_draws_%s.rds", path_src))
  if (!file.exists(f)) return(list())
  dr <- readRDS(f); out <- list()
  for (b in dr) for (i in seq_along(b$ids)) {
    k <- paste(b$ids[i], b$side[i]); if (is.null(out[[k]])) out[[k]] <- list(id = b$ids[i], side = b$side[i], W = NULL, P = NULL)
    out[[k]]$W <- cbind(out[[k]]$W, b$draws[i, ]); out[[k]]$P <- cbind(out[[k]]$P, b$pt[i, ])
  }
  res <- list()
  for (z in out) { k <- as.character(z$id)
    if (is.null(res[[k]])) res[[k]] <- list(W = z$W, P = z$P) else { res[[k]]$W <- res[[k]]$W + z$W; if (z$side == "bat") res[[k]]$P <- z$P } }
  lapply(res, function(z) { e <- st_extend_paths(z$W, z$P, 11L); list(W = e$war, P = e$pt) })
})

wv_dir <- if (file.exists(rp("data/win_value/revenue_curve_calibrated.csv"))) "data/win_value" else "data/none"
wv_anchor_txt <- if (TRUE) "averaged over 84-92 wins" else "at 88 wins"

WV_AVG <- "data/win_curve_avg.csv"
wv_avg <- file.exists(rp(WV_AVG))
wv_cal <- rd(if (wv_avg) WV_AVG else file.path(wv_dir, "revenue_curve_calibrated.csv"))
wv_curves <- if (!is.null(wv_cal)) rbind(wv_cal[setting == "base", .(setting = "cal_base", W, R = R_cal, mv = mv_cal)],
                                         wv_cal[setting == "low", .(setting = "cal_low", W, R = R_cal, mv = mv_cal)],
                                         wv_cal[setting == "high", .(setting = "cal_high", W, R = R_cal, mv = mv_cal)],
                                         wv_cal[setting == "base", .(setting = "measured", W, R, mv)]) else NULL

WV_RE <- if (file.exists(rp("data/win_curve_reanchored.csv"))) "data/win_curve_reanchored.csv" else "data/none"
wv_cal_re <- if (wv_avg && file.exists(rp(WV_RE))) rd(WV_RE) else NULL
wv_curves_re <- if (!is.null(wv_cal_re)) rbind(wv_cal_re[setting == "base", .(setting = "cal_base", W, R = R_cal, mv = mv_cal)], wv_cal_re[setting == "low", .(setting = "cal_low", W, R = R_cal, mv = mv_cal)],
                                               wv_cal_re[setting == "high", .(setting = "cal_high", W, R = R_cal, mv = mv_cal)], wv_cal_re[setting == "base", .(setting = "measured", W, R, mv)]) else NULL
wv_path_file <- if (file.exists(rp("data/sox_baseline_wins_2027.csv"))) "data/sox_baseline_wins_2027.csv" else if (file.exists(rp("data/none"))) "data/none" else if (file.exists(rp("data/none"))) "data/none" else if (file.exists(rp("data/none"))) "data/none" else if (file.exists(rp("data/none"))) "data/none" else if (file.exists(rp("data/none"))) "data/none" else "data/none"
wv_base_wins <- rd(wv_path_file)
win_path_detail <- rd(sub("sox_baseline_wins_", "sox_win_path_", wv_path_file))

NEAR <- local({ x <- rd(if (file.exists(rp("data/near_line.csv"))) "data/near_line.csv" else if (file.exists(rp("data/near_line.csv"))) "data/near_line.csv" else "data/near_line.csv"); if (is.null(x)) NULL else x[state == "before any moves", .(mlbam_id, near0 = near_line, hf = (fit_max - fit_min) / 2, hb = (bargain_max - bargain_min) / 2)] })
NEAR_TXT <- "Near the line: in five runs of my model this call came out differently at least once, so a small change in his projection, price or the Sox roster could flip it."
wv_w27 <- round(wv_base_wins$wins[1], 1)
wv_implied <- rd(file.path(wv_dir, "implied_value_per_win.csv"))
wv_labels <- c(cal_base = "Calibrated to the market (default)", cal_low = "Calibrated: lowest market price per win, low revenue inputs",
               cal_high = "Calibrated: highest market price per win, high revenue inputs", measured = "Measured revenue streams only (gate and postseason)",
               placeholder = "Market price per win at every win total")
unmeasured_88 <- if (!is.null(wv_cal)) 1 - wv_cal[setting == "base" & W == 88, mv / mv_cal] else NA_real_
needs_in <- rd("data/needs_inputs.csv")
cal <- list(
  em = rd("data/acc_error_model.csv"),
  arb = rd("data/acc_arbitration.csv"),
  inj_pt = rd("data/acc_injury_pt.csv"),
  inj_60 = rd("data/acc_injury_60.csv"),
  att = rd("data/acc_attendance.csv"),
  eng = rd("data/acc_engine.csv"),
  price = rd("data/acc_prices.csv"),
  y45 = rd("data/acc_engine_y45.csv"))
war27_sox <- if (!is.null(paths)) paths[year == 1L, .(war27 = sum(p50), lo27 = sum(p10), hi27 = sum(p90)), by = mlbam_id] else data.table(mlbam_id = integer(), war27 = numeric(), lo27 = numeric(), hi27 = numeric())
ps_all <- NULL
prole <- rd("data/pitcher_roles.csv")

fit_pool0 <- local({ pl <- rd(sub("sox_baseline_wins_", "sox_win_path_players_", wv_path_file)); pp <- rd(sub("sox_baseline_wins_", "prospects_", wv_path_file))
  if (!is.null(pp) && !all(c("eta", "season", "w") %in% names(pp))) pp <- data.table(mlbam_id = integer(), name = character(), eta = integer(), season = integer(), w = numeric())
  if (is.null(pl) || is.null(pp)) NULL else { fp <- fit_pool(pl, pp, pa, pab, prole); ptt <- rd("data/sox_fit_pt.csv")
    if (!is.null(ptt)) fp <- merge(fp, ptt, by = c("mlbam_id", "season"), all.x = TRUE); fp } })
sox_depth <- alt_sox_depth(pa, war27_sox, prole)
CASTILLO <- 622491L
nz <- function(x) ifelse(abs(x) < 0.05, 0, x)
fmt_rng <- function(a, b, c) sprintf("%.1f (%.1f to %.1f)", nz(a), nz(b), nz(c))
M <- function(x, d = 1) ifelse(is.na(x), "", paste0(ifelse(x < 0, "-$", "$"), formatC(abs(x) / 1e6, format = "f", digits = d), "M"))
pct <- function(e) ifelse(is.na(e), "", sprintf("%+.0f%%", 100 * (exp(e) - 1)))
seasons <- 2027:2031
DTo <- function(id) DTOutput(id, height = "auto")
placeholder <- function(title, text) card(class = "border-warning", card_header(span(class = "badge bg-warning text-dark me-2", "PLACEHOLDER"), title), p(text))
groups <- alt_groups$group
preds[, group := alt_group_of(position)]
import_ids <- preds[grepl("^[Ii]mports", price_source), unique(mlbam_id)]
plat <- if (file.exists(rp("data/platoon.csv"))) rd("data/platoon.csv") else rd("data/none")
preds[, qo_short := vapply(seq_len(.N), function(i) { s <- qo_now(preds[i]); if (grepl("^received", s)) if (grepl("due Nov 17", s)) paste("QO made:", sub("^QO ", "", QO_DUE)) else "QO made" else if (grepl("not among", s)) "no QO" else sub(" \\(.*|;.*", "", s) }, "")]
if (!is.null(needs_in)) preds <- merge(preds, needs_in[, .(mlbam_id, bats, throws, platoon_runs, ip170)], by = "mlbam_id", all.x = TRUE)
preds[, c("fcst_aav", "fcst_years") := NA_real_]

lg_pay_raw <- rd("data/league_payroll.csv")
LG_GROWTH <- 0.0385
lg_pay <- data.table(season = seasons, avg = lg_pay_raw[season == 2026L, avg_payroll_nominal] * (1 + LG_GROWTH)^(seasons - 2026L))

cmp_of <- function(pos) fcase(pos %in% c("LF", "CF", "RF", "OF", "corner OF"), "OF", pos %in% c("C", "1B", "2B", "3B", "SS", "DH", "SP", "RP"), pos, default = NA_character_)
cmp_levels <- c("C", "1B", "2B", "3B", "SS", "OF", "DH", "SP", "RP")
ps26 <- NULL
pos_war26 <- NULL; if (FALSE) rbind(
  ps26[!is.na(pos_fg) & nzchar(pos_fg) & pos_fg != "P", .(mlbam_id, team_fg, cmp = cmp_of(sub("/.*", "", pos_fg)), war = fcoalesce(fwar_bat, 0))],
  ps26[fcoalesce(as.numeric(g_pit), 0) > 0, .(mlbam_id, team_fg, cmp = fifelse(fcoalesce(as.numeric(gs), 0) / as.numeric(g_pit) >= 0.5, "SP", "RP"), war = fcoalesce(fwar_pit, 0))])[!is.na(cmp)]
lg_pos <- rd("data/league_pos_war.csv")
sox_pos26 <- data.table(cmp = cmp_levels, sox26 = NA_real_, rank26 = NA_integer_, n_clubs = 30L)
mean27 <- if (!is.null(paths)) paths[year == 1L, .(mean27 = sum(mean)), by = mlbam_id] else data.table(mlbam_id = integer(), mean27 = numeric())
sox_pos27 <- merge(sox_depth[, .(mlbam_id, cmp = cmp_of(group))], mean27, by = "mlbam_id")[!is.na(cmp), .(sox27 = sum(mean27)), by = cmp]
pos_cmp <- merge(data.table(cmp = cmp_levels), lg_pos, by = "cmp", all.x = TRUE)
pos_cmp <- merge(merge(pos_cmp, sox_pos27, by = "cmp", all.x = TRUE), sox_pos26, by = "cmp", all.x = TRUE)
pos_cmp[is.na(sox27), sox27 := 0][, gap := sox27 - lg_avg][, weak := frank(gap, ties.method = "first") <= 3L]
pos_cmp[, cmp := factor(cmp, levels = cmp_levels)]
team_ranks <- rd("data/team_ranks.csv")

war26 <- data.table(mlbam_id = integer(), fwar26 = numeric(), bwar26 = numeric())
sox_lost <- merge(unique(preds[last_club == "Chicago White Sox"], by = "mlbam_id")[, .(mlbam_id, player, position, age_2027, model_years, model_aav, price_source, option_text, option_pending)],
                  war26, by = "mlbam_id", all.x = TRUE)[, war26 := (fwar26 + bwar26) / 2][, market_total := model_aav * model_years][order(-market_total)]

age_on <- function(b, d = as.Date("2027-07-01")) as.numeric(d - as.Date(b)) / 365.25
avg_age_40 <- 27.670124

levers <- sidebar(
  width = 320, title = "Levers",
  conditionalPanel("input.tabs == 'Player'", h6("Player settings"),
    sliderInput("conviction", "Conviction: my WAR adjustment on the selected player", min = -2, max = 2, value = 0, step = 0.5),
    numericInput("mult", "Player revenue multiplier (selected player)", value = 1, min = 0, max = 3, step = 0.05)),
  conditionalPanel("input.tabs == 'Player' || input.tabs == 'Board'", h6("Valuation settings"),
    sliderInput("risk", "Risk appetite: -1 chase upside, 0 expected value, +1 protect downside", min = -1, max = 1, value = 0, step = 0.1),
    checkboxInput("fit_on", "Count positional fit in Most the Sox should pay (value his wins over the Sox' current option at his position)", FALSE)),
  conditionalPanel("input.tabs == 'Roster and payroll' || input.tabs == 'Board' || input.tabs == 'Overview'", h6("Budget"),
    numericInput("budget", "2027 payroll budget ($M)", value = 150, min = 50, max = 350, step = 5),
    p(class = "small text-muted", "Castillo's 2028 option is set on the Roster and payroll tab (Contract settings).")),
  conditionalPanel("input.tabs == 'Overview' || input.tabs == 'Roster and payroll' || input.tabs == 'Board' || input.tabs == 'Player'",
    numericInput("sox_w27", sprintf("Sox 2027 baseline wins before any move (my projection: %.1f)", wv_w27), value = wv_w27, min = 50, max = 110, step = 0.5),
    p(class = "small text-muted", "Shifts the whole 2027-2031 path by the same amount. The Overview and the Roster and payroll tab show the path.")),
  accordion(open = FALSE, accordion_panel("Model settings (apply to every tab)",
    h6("Payroll settings"),
    radioButtons("cbt", "Luxury-tax scenario (CBA after Dec 1)", c("Current: $244M threshold (2026) carried forward, pending new CBA" = "current", "Growth: $244M +$3.5M a year, pending new CBA" = "growth")),
    radioButtons("tol_window", "Spending history (AP opening-day payrolls)", if (length(tol_windows)) setNames(tol_windows, sprintf("%s peak: %.2fx the league median (%d)", sub(" \\(.*", "", tol_windows),
      vapply(tol_windows, function(w) tol_all[window == w, peak_ratio[1]], 0), vapply(tol_windows, function(w) tol_all[window == w, peak_season[1]], 0L))) else c("none" = "none")),
    sliderInput("appetite", "Ownership appetite (x the Sox' peak payroll vs the league median)", min = 0.80, max = DIAL_UP, value = 1, step = 0.01),
    p(class = "small text-muted", sprintf("1.00 = their prior peak. Top = dial it up: %.2f, %s.", DIAL_UP, DIAL_UP_SRC)),
    p(class = "small text-muted", "Assumptions: each added dollar costs more from 90% of the limit, rising to 10 dollars at the limit."),
    checkboxInput("owners_floor", "Owners' proposal (Nov 3): $258.2M cap and $180.2M floor from 2028.", FALSE),
    uiOutput("cba_note"),
    hr(), h6("Value of a win"),
    radioButtons("wv_version", "Calibration (both averaged over the Sox' range of win totals)", c("Re-anchored (default): calibrated to what clubs pay; a team projected at 84-92 wins values a win at the market price" = "reanchored", "Old anchor: not calibrated to what clubs pay" = "default"), selected = "reanchored"),
    radioButtons("sox_mkt", "Sox market size (x the Sox' value of every win; the anchor is held)", c("Current: 2024-2026 attendance (1.00, default)" = "1",
      "2010-2019 attendance (1.12)" = "1.12145299498913", "2026 attendance (1.24)" = "1.23818980460795", "Chicago population share (1.46)" = "1.45684715127242"), selected = "1"),
    radioButtons("wv", NULL, setNames(names(wv_labels), wv_labels), selected = "cal_base"))),
  uiOutput("wv_note")
)
cap <- function(...) p(class = "small text-muted mb-1", ...)
how_to_read <- card(card_header("How to read this"),
  p(class = "small", "Each bar moves the price from where the market is to the most the Sox should pay. Green bars raise it, red bars lower it, grey bars are not built yet (0)."),
  tags$dl(class = "small mb-0",
    tags$dt("Market price"), tags$dd("What the market is expected to pay him a year at this length (my model, for every player)."),
    tags$dt("My view of his production"), tags$dd("How much more or less he is worth once his own projected WAR, aging and injury risk replace the market's average player at that price."),
    tags$dt("Your conviction"), tags$dd("The change from your own WAR adjustment in the sidebar (0 if you leave it at 0)."),
    tags$dt("Revenue multiplier"), tags$dd("The change from valuing his wins above or below a typical player's (a star draw, say); 0 at a multiplier of 1."),
    tags$dt("Risk"), tags$dd("What your risk appetite does: protecting the downside lowers the price, chasing upside raises it."),
    tags$dt("Contract structure"), tags$dd("What options, opt-outs, deferrals and a signing bonus in the offer builder are worth to the Sox, up or down."),
    tags$dt("What a win is worth to the Sox"), tags$dd("The change from pricing his wins at the Sox's own win level instead of the market rate; a rebuilding club's wins are worth less, so this is usually negative."),
    tags$dt("Fit (park, needs)"), tags$dd("Park fit is not built yet (0); the needs bonus adds what you set on the Needs tab for filling a need, if you turn it on."),
    tags$dt("Draft pick (QO)"), tags$dd("For a player with a qualifying offer: the value of the draft pick and international money the Sox give up to sign him, spread over the contract as AAV (current CBA, pending new CBA). 0 for everyone else."),
    tags$dt("Most the Sox should pay"), tags$dd("The sum of all the bars: the highest AAV at which the offer still pays off for the Sox, given every setting above.")))
ui <- page_sidebar(
  title = "Free Agent Market Simulator: White Sox, 2026-27 (beta)",
  theme = bs_theme(version = 5, bootswatch = "flatly", font_scale = 0.9),
  sidebar = levers, fillable = FALSE,
  tags$head(tags$script(HTML("$(document).on('shiny:inputchanged', function(e) { if (e.name === 'player') window.__offerSel = Date.now(); }); $(document).on('mousedown touchstart keydown', '.irs, #years_num', function(e) { var t = $(e.target); if (t.is('#years_num') || t.closest('.irs').nextAll('#years').length) window.__offerSel = Date.now(); }); $(document).on('input keyup', '#offer_aav', function() { window.__offerTyped = Date.now(); }); $(document).on('shiny:updateinput', function(e) { if (e.target && e.target.id === 'offer_aav' && (window.__offerTyped || 0) > (window.__offerSel || 0)) e.preventDefault(); });"))),
  tags$head(tags$style(".html-widget.datatables { flex: 0 0 auto !important; height: auto !important; } .bslib-card { overflow: visible; } .bslib-card > .card-body { overflow: visible; } table.dataTable td { font-size: 0.85rem; } .ov-big { font-size: 1.6rem; font-weight: 600; margin-bottom: 0; } .ov-weak { color: #c53030; font-weight: 600; }")),
  navset_card_tab(id = "tabs",
    nav_panel("Overview", BETA,
      layout_columns(col_widths = c(3, 3, 3, 3),
        card(card_header("Projected 2027 (win path point, before any move)"), uiOutput("ov_war")),
        card(card_header("Committed for 2027 so far (before any moves)"), uiOutput("ov_pay")),
        card(card_header("2026 ranks (of 30)"), uiOutput("ov_ranks")),
        card(card_header("40-man average age"), uiOutput("ov_age"))),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Win path 2027-2031"), plotOutput("ov_win_plot", height = 280),
             layout_columns(col_widths = c(3, 3, 3, 3), selectInput("posture_2028", "Winter before 2028", POSTURE_CHOICES, "holding"), selectInput("posture_2029", "2029", POSTURE_CHOICES, "holding"),
                            selectInput("posture_2030", "2030", POSTURE_CHOICES, "holding"), selectInput("posture_2031", "2031", POSTURE_CHOICES, "holding")),
             cap("Posture: each future winter adds the typical wins clubs added that winter (free agents and trades, projected WAR; median for clubs projected like the Sox that year): going for it about 7, holding about 4, not going for it about 2-4. 2027 has no addition: it is valued by the moves you log. These wins enter the win level every move is valued at. Each winter's additions carry into later seasons at the historical retention of free-agent additions (about half their wins a year later, then 29%, 16% and 8%), and their payroll (about $9.1M per added win in 2027 dollars; 58% of it still owed a year later, then 38%, 24%, 18%) counts against the spending limit in every later season."),
             cap("Line: projected wins (replacement level 47.7 + mean WAR of players under club control + a fill-in for new players (rookies, call-ups, returnees and imports) that grows each year as today's players age and leave, sized so the 30 clubs together average 80-81 wins in every season; prospects are part of it). Shaded: the 80% range from 180 past team projections (2021-2026, the current engine). The blue line in 2028-2031 is players under control plus that fill-in, before any additions. Green: with the chosen posture's additions each winter. Orange (if shown): the win level after your moves and sidebar shift.")),
        card(card_header("WAR by position against league average (weakest spots in red; Sox bars are 40-man only, without fill-in)"), plotOutput("ov_pos_plot", height = 280),
             cap("Bars: Sox 2027 projected WAR at each position (my engine, mean of the paths, 40-man players, Steamer 2027 input). Dots: 2026 league average per club (FanGraphs WAR of every player at that position / 30; CF and corner OF joined as OF; pitchers SP if half their games were starts). The 40-man projection leaves out fill-in play, so the Sox bars run a little low everywhere; the red bars are the three largest gaps below average."))),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Suggested needs"), DTo("ov_needs"), uiOutput("ov_rotation"),
             cap("The Needs tab's ranking: target WAR for the slot minus the Sox's k-th best 40-man player there (Steamer paths, median). Change targets on the Needs tab.")),
        card(card_header("Projected starters, years of control and 2027 salary"), DTo("ov_starters"),
             cap(sprintf("The top 40-man player(s) per position by 2027 median WAR (5 starters, 3 relievers). Control: seasons 2027-2031 in the payroll ledger not marked free agent (option years included; 5 means through 2031 at least). 2027 salary: ledger cash (%s; arbitration is a projection).", ledger_label)))),
      layout_columns(col_widths = c(5, 7),
        card(card_header("2026 ranks"), DTo("ov_rank_tbl"),
             cap("Runs scored (1 = most), runs allowed (1 = fewest) and run differential: ranks from MLB Stats API, 2026 season.")),
        card(card_header("What the Sox lost this winter"), DTo("ov_lost"), uiOutput("ov_lost_note")))),
    nav_panel("Needs", BETA,
      layout_columns(col_widths = c(7, 5),
        card(card_header("Needs overlay: positions and targets (set by the GM)"),
             p(class = "small text-muted", "Tick the needs to fill and set each target (2027 WAR the slot should reach). The gap is the target minus the Sox's k-th best 40-man player there (Steamer paths). Defaults: 2.0 for a regular, 1.0 for the 5th starter, 0.2 for the 8th reliever."),
             uiOutput("needs_inputs"), DTo("needs_tbl")),
        card(card_header("Handedness and roles"),
             selectInput("hand_bat", "Bat I want (for position needs)", c("Any" = "any", "Left-handed bat" = "L", "Right-handed bat" = "R", "Switch hitter" = "S")),
             selectInput("hand_rp", "Reliever I want", c("Any" = "any", "Left-handed reliever" = "L", "Right-handed reliever" = "R")),
             checkboxInput("role_innings", "A starter who eats innings (Steamer 2027 projection of 170+ IP)", FALSE),
             hr(), h6("Optional fit bonus (off by default; never added to the most the Sox should pay when off)"),
             checkboxInput("bonus_on", "Add a value for filling a need", FALSE),
             numericInput("bonus_pos", "Bonus per year for a position need he fills ($M)", 0, 0, 20, 0.5),
             numericInput("bonus_hand", "Bonus per year for the handedness I want ($M)", 0, 0, 20, 0.5),
             uiOutput("bonus_hint")))),
    nav_panel("Board", BETA,
      layout_columns(col_widths = c(9, 3),
        card(card_header("Free agents: market prices (my own model for every player: the Nov 3 fit with the reliever and record ceilings and the declined-option floor, raised 7.5% for the market level; signed players: their contract)"),
             layout_columns(col_widths = c(3, 3, 3, 3),
               selectInput("f_role", "Role", c("All", "Hitter", "Starter", "Reliever")),
               selectInput("f_tier", "Tier", c("All", "star", "regular", "role player", "depth")),
               checkboxInput("f_pending", "Hide players still under an option", TRUE),
               checkboxInput("f_fits", "Show only players who fit my needs", FALSE)),
             p(class = "small mb-1", strong(paste0("Signings as of ", SIGNED_ASOF, " (the last signings refresh): ")), "signed players show 'Signed' (dark gray) with club, years and AAV; they are left out of the colors and the counts, and stay in the Market ticker."),
             p(class = "small mb-1", strong("Sox call: "), "Green = fits and a bargain (Sox score 1.10x+); Blue = a bargain, but not an upgrade over the Sox' current option (the Board says whom he would replace); Yellow = close call (Sox score 0 or more); Red = fits, but costs more than he's worth to the Sox right now, or blocked: the Sox already have a better option at his position (value over their current option at or below zero; a Sox score below zero is never a close call); Gray = not valued yet. A flag in Price check marks a market price under 70% or over 130% of public forecasts (or a deal more than a year longer). Money columns are in $M and sort as numbers. Order: green, blue, yellow, red, gray, then Sox score. Green at = the price at which he turns green; Green if the Sox reach = the 2027 win total at which he would. Calls update after every move: signing one player can change another's call. Sox score = value over the Sox' current option / market price. Display only: Most the Sox should pay changes only with fit on."),
             checkboxGroupInput("f_color", NULL, choices = SOX_CALLS, selected = SOX_CALLS, inline = TRUE),
             DTo("board"), p(class = "small text-muted", "Players you have signed (Move log) are taken off the Board. The Fits column shows needs set on the Needs tab. A dot in the Changed column marks a call that changed at the last move or undo (hover for why). Sox surplus = the risk-scored surplus of his market offer (length and AAV, plain) on his WAR paths, at the win value chosen in the sidebar. WAR paths: my engine on Steamer 2027 projections (FanGraphs); * Marcel playing time where Steamer gives none (low confidence). Low playing time, wider range: Steamer projects him under 30 IP (the engine's floor), so his paths come from historical veterans projected that low. Not valued: a new import with no Steamer 2027 projection, or a Steamer placeholder (1 IP, no playing time). Sox surplus is after the moves logged (current rules) and includes the spending-limit cost (Ownership appetite in the sidebar).")),
        card(card_header("Budget"), uiOutput("budget_box")))),
    nav_panel("Player", BETA,
      layout_columns(col_widths = c(5, 7),
        card(card_header("Player"), selectizeInput("player", NULL, choices = NULL), uiOutput("player_card"),
             conditionalPanel("output.sel_reliever", selectInput("rp_role", "Reliever role (leverage; default from his 2024-2026 gmLI)", c("As projected (no extra scaling)" = "proj",
               "Closer (x1.33: median gmLI 1.67)" = "closer", "Setup (x1.15: gmLI 1.29)" = "setup", "Middle (x0.95: gmLI 0.90)" = "middle"), selected = "proj"),
               p(class = "small text-muted mt-n2", "Scales his WAR by (1 + the role's historical gmLI) / 2 (relief seasons 2021-2026, 30+ relief games). FanGraphs reliever WAR already includes some leverage, ",
                 "so 'As projected' may already credit part of it; realized fWAR has beaten Steamer by about 0.5 a season for closers (partly because better pitchers are made closers). ",
                 "Default: the role from his gmLI over 2024-2026 (closer 1.6+, setup 1.2-1.6, else middle; no relief gmLI = middle), but only for pitchers who were mostly relievers in 2024-2026 (at least half their games in relief and at least 30 relief games); everyone else starts at As projected. Tested on 527 past signings: relievers' value per dollar paid moved from 0.54 to 0.57 of what clubs paid.")),
             layout_columns(col_widths = c(8, 4),
               sliderInput("years", "Contract length (years)", min = 1, max = 10, value = 3, step = 1),
               numericInput("years_num", "Years (type)", value = 3, min = 1, max = 10, step = 1)),
             uiOutput("slider_price"),
             conditionalPanel("!output.sel_signed", actionButton("sign", "Sign this offer (adds a move)", class = "btn-primary")),
             conditionalPanel("output.sel_signed", p(class = "text-success", strong("Signed: "), "this player is in the Move log. Undo the move there to offer again."))),
        div(card(card_header("Price by length (market prices)"), plotOutput("slider_plot", height = 260)),
            card(card_header(textOutput("range_head", inline = TRUE)), plotOutput("range_plot", height = 250), uiOutput("range_note")),
            card(card_header("WAR paths, 2027-2031 (my engine)"), plotOutput("fa_path_plot", height = 230), uiOutput("fa_path_note")))),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Offer builder"),
             numericInput("offer_aav", "Offer AAV ($M a year; length from the years above)", value = 10, min = 0.5, max = 80, step = 0.25),
             p(class = "small text-muted mt-n2", "Type the AAV; the offer updates half a second after you stop typing. It resets to his market price when you change the player or the length."),
             p(class = "small", "Options and opt-outs change what the deal really costs the Sox. An opt-out costs more (he leaves when he's good); a club option costs less (the Sox can walk when he's not). The card shows the deal as one guaranteed-equivalent AAV."),
             selectInput("opt_kind", "Extra option year", c("None" = "none", "Club option" = "club_option", "Player option" = "player_option",
                                                           "Mutual option" = "mutual_option", "Vesting option" = "vesting_option")),
             layout_columns(col_widths = c(4, 4, 4),
               numericInput("opt_sal", "Option salary (% of AAV)", 100, 0, 200, 5),
               numericInput("opt_buy", "Buyout (% of AAV)", 10, 0, 100, 5),
               numericInput("vest_thr", "Vests at PA / IP in prior season", 550, 0, 800, 10)),
             layout_columns(col_widths = c(4, 4, 4),
               selectInput("opt_out", "Opt-out after season", c("None" = "0", setNames(as.character(1:9), 1:9))),
               numericInput("defer", "Deferred (% of salary, paid 10 yr later)", 0, 0, 80, 5),
               numericInput("bonus", "Signing bonus (% of total, up front)", 0, 0, 50, 5)),
             uiOutput("offer_out")),
        card(card_header("Surplus distribution for this offer"), plotOutput("offer_hist", height = 260))),
      layout_columns(col_widths = c(8, 4),
        card(card_header("Why: the most the Sox should pay, from the market price"), plotOutput("waterfall_plot", height = 400), uiOutput("wf_posture_note"), uiOutput("waterfall_note")),
        how_to_read),
      card(card_header("Alternatives: the need he fills and every other way to fill it"),
           uiOutput("alt_need"),
           layout_columns(col_widths = c(5, 7), div(h6(if (public_mode) "Internal options (Sox 40-man)" else "Internal options (Sox 40-man)"), DTo("alt_internal")),
                          div(h6("Free agent alternatives for the same need (at their market offers)"), DTo("alt_fa"))),
           uiOutput("alt_opp"))),
    nav_panel("Roster and payroll", BETA,
      card(card_header(sprintf("Payroll by season, 2027-2031 (%s plus moves)", ledger_label)), plotOutput("payroll_plot", height = 480)),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Payroll table and roster moves"), DTo("payroll_tbl"),
             hr(), h6("Roster moves (logged in the Move log; Undo there)"), selectizeInput("rm_player", "40-man player", choices = NULL),
             actionButton("rm_release", "Release or DFA (guaranteed salary still owed)", class = "btn-outline-danger btn-sm"), actionButton("rm_trade", "Trade away (salary leaves; return modeled in Release 3)", class = "btn-outline-secondary btn-sm"),
             p(class = "small text-muted mt-1", "Either move takes him off the 40-man, out of the win path (his projected WAR) and out of the positional fit. Release: his guaranteed salary stays on the payroll. Trade: his salary leaves; what comes back is not modeled yet (Release 3).")),
        div(card(card_header("Contract settings"),
                 radioButtons("castillo", "Luis Castillo's 2028 vesting option ($25M)", c("Assume it vests" = "vests", "Assume it does not vest" = "does_not_vest"), inline = TRUE),
                 p(class = "small text-muted", "Castillo's 2028 option ($25M) vests if he reaches the innings condition in 2027 and passes a physical. Choose whether to assume it vests; the payroll, luxury tax and budget use your choice, and the Move log records it with each move.")),
            card(card_header("Roster-rule warnings"), uiOutput("warnings")))),
      card(card_header("Sox win path 2027-2031 (the win level each move is valued at)"), DTo("win_path_tbl"),
           p(class = "small text-muted", "Replacement level (47.7 wins) + the mean WAR of players under club control that season + a fill-in for new players (rookies, call-ups, returnees and imports) that grows each year as today's players age and leave, sized so the 30 clubs together average 80-81 wins in every season; prospects are part of it (2.7 wins in 2027, 7.1 in 2028, 10.2 in 2029, 13.1 in 2030, 16.0 in 2031). Each logged move adds its mean WAR in its contract seasons, less the WAR of the player it pushes out of the lineup or rotation. The 80% range is from 180 past team projections (2021-2026, the current engine).")),
      card(card_header("40-man roster"), DTo("roster"),
           p(class = "small text-muted", "Flags in plain English, from the player assets file (options, outright rights) and the payroll ledger (2027 pay status, Super Two, when the contract or club control ends).")),
      card(card_header("White Sox WAR paths (Steamer 2027)"), DTo("paths_tbl"))),
    nav_panel("Market ticker", BETA,
      card(card_header("Signings so far, and what my model said"), if (!PUBLIC_BUILD) checkboxInput("ticker_details", "Show the details", FALSE), DTo("ticker"),
           uiOutput("ticker_note"))),
    nav_panel("Skubal vs Rogers", BETA,
      card(card_header("How sure is the 2027 projection? Tarik Skubal vs Trevor Rogers (error model and engine ranges, engine paths)"),
           if (PUBLIC_BUILD) cap("A: each one's 2027 WAR range from my error model and from my engine (10th percentile, median, 90th percentile, chance of 0 WAR or less). B: their 2027-2031 paths from my engine (the paths every value in this app uses).") else cap("A: each one's 2027 WAR range from the error model. B: what widened or narrowed each range against a typical starter (Shapley values, top 6). C: their 2027-2031 paths from the engine (the paths every value in this app uses)."),
           DTo("skro_tbl"), imageOutput("skro_png", height = if (PUBLIC_BUILD) "940px" else "1320px"))),
    nav_panel("How accurate is this?", BETA,
      layout_columns(col_widths = c(6, 6),
        card(card_header("Market prices: my model (rolling backtest, rows with a forecast)"),
             cap("How far my model's predicted contracts were from the real ones, on past winters; MAE is the typical miss on a log scale (0.10 is about 10%), so lower is better, and bias above 0 means the real deals came in higher."), DTo("cal_price")),
        card(card_header("WAR path engine (all origins, in-sample)"),
             cap("The share of real seasons that landed inside the engine's 80% range, by group and year ahead; a well-calibrated engine sits near 0.80."), DTo("cal_eng")),
        card(card_header("Error model"),
             cap("How well the spread around each projection matched real outcomes on seasons it never saw; lower CRPS is better, and 80% coverage should be near 0.80."), DTo("cal_em")),
        card(card_header("Arbitration: my model, one year ahead"),
             cap("The typical miss on arbitration salaries for my model, season by season; lower is better."), DTo("cal_arb")),
        card(card_header("Injury model"),
             cap("Scores for the playing-time share and the chance of a 60-day injured-list stint against simpler baselines; lower scores are better."), DTo("cal_inj")),
        card(card_header("Attendance (leave one season out)"),
             cap("The typical miss in predicted attendance when each season is held out in turn, for the model, a spline and a baseline; lower is better."), DTo("cal_att")),
        card(card_header("WAR paths, years 4-5 (backtest from 2018 and 2019)"),
             cap("How often real seasons fell inside the 80% range three to five years out, the average overshoot (prediction minus actual), and the predicted against the actual share of seasons at or below 0 WAR."), DTo("cal_y45"),
             p(class = "small text-muted", "For hitters over 30 the engine runs high at years 4-5: more of them left the majors than it predicts.")),
        card(card_header("Catchers"), cap("A known weakness, stated plainly:"), p(class = "text-warning", CATCHER_CAVEAT)),
        card(card_header("Sources and credits"), cap("Where every number in this app comes from."), tags$ul(class = "small",
             tags$li("Market prices: my model for every player (the Nov 3 fit, with the reliever and record ceilings and the declined-option floor), raised 7.5% for the market level, the amount it ran low on past winters' deals. New imports with no MLB record (Taira, Itoh, Sato) are priced by my model too, with low confidence."),
             tags$li("Projections: Steamer 2027 (FanGraphs) as the input to my WAR path engine; Marcel playing time where Steamer gives none (low confidence). Only my engine's derived values are shown."),
             tags$li("Win value: attendance (Baseball-Reference), revenue per fan (Atlanta Braves Holding Company 10-K, Forbes team pages), postseason gate (MLB postseason share announcements), scaled to the market price per win."),
             tags$li("Payroll and roster: MLB Stats API (rosters, options); contracts from public reports, credited to Cot's Baseball Contracts where it settled a disputed term. League-average payroll and the spending limit: AP opening-day team payrolls, grown 3.85% a year; the 'dial it up' figure from Spotrac team payrolls."),
             tags$li("2026 team ranks: MLB Stats API (ranks only). League-average WAR by position and rotation slot, and league ages: FanGraphs, aggregated over every player (no player values shown). Attendance: Baseball-Reference."),
             tags$li("Market ticker: signings as reported by MLB Trade Rumors and the MLB transactions feed; for players who have signed, MLB Trade Rumors' predicted AAV is shown, credited."),
             tags$li("Arbitration 2027: my arbitration model. Draft-pick values: Edwards (FanGraphs, 2019). Model and app by Henry."))))),
    nav_panel("Move log", BETA,
      card(card_header("Moves this session"), DTo("moves"), div(class = "mt-2", actionButton("undo", "Undo last move"), downloadButton("dl_moves", "Download log (CSV)"))))
  )
)

server <- function(input, output, session) {
  moves <- reactiveVal(data.table(time = character(), player = character(), mlbam_id = integer(), years = integer(), aav = numeric(),
                                  total = numeric(), conviction = numeric(), risk = numeric(), castillo = character(), cbt = character(),
                                  structure = character(), ge_aav = numeric(), win_value = character(), kind = character()))

  sox_shift <- reactive(if (is.null(input$sox_w27) || is.na(input$sox_w27)) 0 else input$sox_w27 - wv_w27)

  mkind <- function(m) if (!nrow(m)) character() else if ("kind" %in% names(m)) fcoalesce(m$kind, "sign") else rep("sign", nrow(m))
  gone_ids <- reactive({ m <- moves(); m$mlbam_id[mkind(m) %in% c("release", "trade")] })
  mv_rows <- function(m) { if (!nrow(m) || is.null(fa_paths)) return(NULL); rbindlist(lapply(seq_len(nrow(m)), function(j) { a <- fa_paths[mlbam_id == m$mlbam_id[j] & season %in% seasons, .(w = sum(mean)), by = season][season < 2027L + m$years[j]]
      g <- alt_group_of(preds[mlbam_id == m$mlbam_id[j], position][1]); if (!nrow(a) || is.na(g)) return(NULL); a[, .(mlbam_id = m$mlbam_id[j], name = m$player[j], group = g, season, w)] })) }
  pt_mean <- function(id) { z <- fa_draws[[as.character(id)]]; if (is.null(z)) NULL else colMeans(z$P[, seq_len(min(5L, ncol(z$P))), drop = FALSE]) }
  move_war <- function(m) { out <- rep(0, length(seasons)); if (!nrow(m)) return(out); k <- mkind(m)
    if (!is.null(paths)) for (j in which(k != "sign")) { a <- paths[mlbam_id == m$mlbam_id[j] & season %in% seasons, .(w = sum(mean)), by = season]; out <- out - (a$w[match(seasons, a$season)] %|% 0) }
    m <- m[k == "sign"]; if (!nrow(m) || is.null(fa_paths)) return(out)
    pool <- if (!is.null(fit_pool0)) fit_pool0[!mlbam_id %in% gone_ids()] else NULL
    for (j in seq_len(nrow(m))) { a <- fa_paths[mlbam_id == m$mlbam_id[j] & season %in% seasons, .(w = sum(mean)), by = season]
      inc <- seasons < 2027L + m$years[j]; aw <- a$w[match(seasons, a$season)] %|% 0; g <- alt_group_of(preds[mlbam_id == m$mlbam_id[j], position][1]); disp <- rep(0, length(seasons))
      if (!is.null(pool) && !is.na(g)) { pf <- fit_pool_fill(pool); disp <- fit_vec_p(pf, fit_incumbents(pf), g, length(seasons), d = pt_mean(m$mlbam_id[j])); pool <- fit_add_moves(pool, mv_rows(m[j])) }
      out <- out + inc * pmax(aw - disp, 0) }
    out }
  posture_gain <- reactive({ base <- wv_base_wins$wins + sox_shift()
    vapply(seq_along(seasons), function(i) { yr <- seasons[i]; if (yr < 2028L) return(0); ps <- input[[paste0("posture_", yr)]]; if (is.null(ps)) ps <- "holding"
      g <- POSTURE_GAIN[[ps]]; if (base[i] < 75) g[["u75"]] else if (base[i] < 86) g[["m"]] else g[["h"]] }, 0) })

  posture_add <- reactive({ g <- posture_gain(); vapply(seq_along(seasons), function(i) sum(g[seq_len(i)] * POST_RET$w[i - seq_len(i) + 1L]), 0) })
  posture_pay <- reactive({ g <- posture_gain(); vapply(seq_along(seasons), function(i) sum(g[seq_len(i)] * POST_RET$ppw * 1.0385^(seasons[seq_len(i)] - 2027L) * POST_RET$p[i - seq_len(i) + 1L]), 0) })
  posture_label <- reactive({ ps <- vapply(2028:2031, function(y) { v <- input[[paste0("posture_", y)]]; if (is.null(v)) "holding" else v }, "")
    nm <- c(going = "going-for-it", holding = "holding", not = "not-going-for-it", none = "no"); if (length(unique(ps)) == 1L) sprintf("with %s additions each winter", nm[[ps[1]]]) else "with the chosen additions each winter" })
  wb <- reactive({ b <- wv_base_wins$wins + sox_shift() + move_war(moves()) + posture_add(); c(b, rep(tail(b, 1), 6)) })
  `%|%` <- function(a, b) ifelse(is.na(a), b, a)
  curve <- reactive({ if (input$wv == "placeholder" || is.null(wv_curves)) return(NULL)
    base <- if (identical(input$wv_version, "reanchored") && !is.null(wv_curves_re)) wv_curves_re else wv_curves; x <- copy(base[setting == input$wv])
    f <- suppressWarnings(as.numeric(input$sox_mkt)); if (length(f) && !is.na(f) && f != 1) x[, `:=`(R = R * f, mv = mv * f)]; x })

  fit_pool_now <- reactive({ if (is.null(fit_pool0)) return(NULL); m <- moves(); fit_pool_fill(fit_add_moves(fit_pool0[!mlbam_id %in% gone_ids()], if (nrow(m)) mv_rows(m) else NULL)) })
  fit_inc <- reactive({ p <- fit_pool_now(); if (is.null(p)) return(NULL); fit_incumbents_pt(p) })
  fit_info <- function(id, L) { inc <- fit_inc(); g <- alt_group_of(preds[mlbam_id == id, position][1])
    if (is.null(inc) || is.na(g)) return(list(v = rep(0, L), who = NA_character_, w27 = NA_real_, group = g, src = NA_character_))
    list(v = fit_vec_p(fit_pool_now(), inc, g, L, d = pt_mean(id)), who = inc[group == g & season == 2027L, incumbent], w27 = max(0, inc[group == g & season == 2027L, incumbent_w]), group = g,
         src = inc[group == g & season == 2027L, incumbent_source]) }
  fitW <- function(W, id) { L <- ncol(W); v <- fit_info(id, L)$v; sweep(W, 2, v) }
  eqW <- function(W, mult = 1) if (is.null(curve())) mult * W else wv_equiv_war(W, wb(), curve(), st_p$vpw, mult)

  LEV <- c(proj = 1, closer = 1.334850, setup = 1.145825, middle = 0.951725)

  rp_map <- reactiveValues(m = RP_DEFAULT)
  output$sel_reliever <- reactive(identical(sel()$role, "Reliever")); outputOptions(output, "sel_reliever", suspendWhenHidden = FALSE)
  observeEvent(input$player, { r <- isolate(rp_map$m[[as.character(input$player)]]); updateSelectInput(session, "rp_role", selected = if (is.null(r)) "proj" else r) }, priority = 5)
  observeEvent(input$rp_role, { id <- as.character(isolate(input$player)); if (!length(id) || !nzchar(id)) return(); cur <- isolate(rp_map$m[[id]])
    if (identical(cur, input$rp_role) || (is.null(cur) && identical(input$rp_role, "proj"))) return()
    m <- isolate(rp_map$m); m[[id]] <- input$rp_role; rp_map$m <- m }, ignoreInit = TRUE)
  drw <- reactive({ m <- rp_map$m; x <- fa_draws; for (k in names(m)) { f <- LEV[[m[[k]]]]; if (!is.null(x[[k]]) && f != 1) x[[k]]$W <- x[[k]]$W * f }; x })
  rp_role_of <- function(id) { r <- rp_map$m[[as.character(id)]]; if (is.null(r)) "proj" else r }
  fa_eq <- reactive(lapply(drw(), function(z) list(W = eqW(z$W), P = z$P)))
  output$wv_note <- renderUI({ if (input$wv == "placeholder") return(p(class = "small text-muted", "Every win valued at the market price per win ($7.0M, growing 3.85% a year), whatever the Sox's win total."))
    cv <- curve(); w27 <- round(wb()[1]); v27 <- cv[W == w27, mv]; v88 <- cv[W == 88, mv]
    if (input$wv == "measured") return(p(class = "small text-muted", sprintf("Measured streams only: a win is worth %s at the Sox's %d-win level in 2027 and %s at 88 wins (gate this season and next, net of revenue sharing, plus the club's postseason gate). Local media, sponsorship and franchise value are left out, so this is well below the market's %s per win.",
      M(v27, 2), w27, M(v88, 2), M(st_p$ppw, 1))))
    if (wv_avg) return(p(class = "small text-muted", sprintf("%s: a win is worth %s to the Sox in 2027 (projected %d wins) and %s to a team projected at 88, each averaged over the range of win totals a team with that projection actually finishes with (80%%: about 13 below to 12 above the projection; 180 team-seasons, 2021-2026). %s",
      wv_labels[[input$wv]], M(v27, 2), w27, M(v88, 2), if (identical(input$wv_version, "default")) "Old anchor: not calibrated to what clubs pay (the averaged curve with the point anchor)." else "Re-anchored on the averaged basis so a team projected at 84-92 wins values a win at the market price; chosen by the revealed-preference check.")))
    p(class = "small text-muted", sprintf("%s: a win is worth %s at the Sox's %d-win level in 2027 and %s at 88 wins, a typical contender's total. It is scaled so a contender's win (%s) equals the market price per win. The measured streams (gate and postseason) give the shape; about %.0f%% of a contender's win value is local media, sponsorship and franchise value (not measured), scaled with them.",
      wv_labels[[input$wv]], M(v27, 2), w27, M(v88, 2), wv_anchor_txt, 100 * unmeasured_88)) })

  output$needs_inputs <- renderUI({ tagList(lapply(seq_len(nrow(alt_groups)), function(i) { g <- alt_groups$group[i]; id <- make.names(paste0("need_", g))
    layout_columns(col_widths = c(5, 7), checkboxInput(paste0(id, "_on"), g, FALSE), numericInput(paste0(id, "_t"), NULL, alt_groups$target_war[i], -1, 8, 0.1)) })) })
  gm_groups <- reactive({ g <- copy(alt_groups)
    g[, `:=`(on = vapply(group, function(x) isTRUE(input[[paste0(make.names(paste0("need_", x)), "_on")]]), TRUE),
             target_war = vapply(seq_len(.N), function(i) { v <- input[[paste0(make.names(paste0("need_", group[i])), "_t")]]; if (is.null(v) || is.na(v)) target_war[i] else v }, 0))]; g })
  sox_needs <- reactive(merge(alt_needs(sox_depth, gm_groups()[, .(group, slots, target_war)]), gm_groups()[, .(group, on)], by = "group")[order(need_rank)])
  output$needs_tbl <- renderDT(datatable(sox_needs()[, .(Rank = need_rank, Need = group, Fill = fifelse(on, "yes", ""), Slots = slots, Target = target_war, `Sox k-th best 2027` = round(kth_war, 2), Gap = round(gap, 2), Now = holders)],
                                         rownames = FALSE, class = "compact", options = list(dom = "t", pageLength = 20, scrollX = TRUE)))
  fits <- reactive({ x <- preds[, .(mlbam_id, group, role, bats, throws, ip170)]; n <- sox_needs()[on == TRUE & gap > 0, group]
    x[, pos_fit := group %in% n]
    x[, hand_fit := fifelse(role == "Reliever", input$hand_rp != "any" & throws %in% input$hand_rp,
                            role == "Hitter" & input$hand_bat != "any" & (bats %in% input$hand_bat | (bats %in% "S" & input$hand_bat %in% c("L", "R"))))]
    x[, inn_fit := isTRUE(input$role_innings) & role == "Starter" & ip170 %in% TRUE]
    x[, fit := pos_fit & (input$hand_bat == "any" | role != "Hitter" | hand_fit) & (input$hand_rp == "any" | role != "Reliever" | hand_fit) | inn_fit |
          (length(n) == 0 & (hand_fit | inn_fit))]
    x[, fit_text := paste0(fifelse(pos_fit, paste0("need: ", group), ""), fifelse(hand_fit, fifelse(role == "Reliever", paste0("; ", throws, "HP"), paste0("; bats ", bats)), ""), fifelse(inn_fit, "; innings", ""))]
    x[, fit_text := sub("^; ", "", fit_text)]
    x[, bonus_yr := if (isTRUE(input$bonus_on)) 1e6 * (pos_fit * input$bonus_pos + fcoalesce(hand_fit, FALSE) * input$bonus_hand) else 0]
    unique(x, by = "mlbam_id") })
  output$bonus_hint <- renderUI({ v <- if (is.null(curve())) st_p$vpw else curve()[W == round(wb()[1]), mv]
    p(class = "small text-muted", sprintf("A data-based starting point for the handedness bonus: a hitter's platoon edge (Steamer 2027 wOBA vs RHP minus vs LHP) x the share of his PA against right-handers (about 0.72) x 600 PA / 1.2 wOBA scale = runs; 10 runs a win; x the value of a win (%s now). E.g. a +0.030 edge is about %s a year. The player card shows each hitter's.",
      M(v, 2), M(0.030 * 0.72 * 600 / 1.2 / 10 * v, 2))) })
  board_rows <- reactive({
    x <- copy(preds)[default_view %in% TRUE | !input$f_pending]
    if (isTRUE(input$f_pending)) x <- x[!(option_pending %in% TRUE)]
    if (input$f_role != "All") x <- x[role == input$f_role]
    if (input$f_tier != "All") x <- x[tier == input$f_tier]
    x <- x[!mlbam_id %in% moves()$mlbam_id]
    x <- merge(x, fits()[, .(mlbam_id, fit, fit_text, bonus_yr)], by = "mlbam_id", all.x = TRUE)
    if (isTRUE(input$f_fits)) x <- x[fit %in% TRUE]
    if (!is.null(input$f_color)) { bc <- board_class(); x <- x[fifelse(mlbam_id %in% SIGNED_IDS, "Signed", fcoalesce(bc$call[match(mlbam_id, bc$mlbam_id)], "Not valued yet")) %in% input$f_color] }
    bc <- board_class()
    x[, `:=`(.call = fifelse(mlbam_id %in% SIGNED_IDS, "Signed", fcoalesce(bc$call[match(mlbam_id, bc$mlbam_id)], "Not valued yet")), .score = bc$fitr[match(mlbam_id, bc$mlbam_id)])]
    x <- x[order(match(.call, CALL_ORDER), -fcoalesce(.score, -Inf))][, c(".call", ".score") := NULL]; x })

  fa_fit <- reactive({ rho <- input$risk; ids <- intersect(names(fa_draws), as.character(preds$mlbam_id)); mv_ids <- moves()$mlbam_id
    rbindlist(lapply(ids, function(k) { id <- as.integer(k); r <- preds[mlbam_id == id][1]; if (is.na(r$model_aav)) return(NULL); L <- min(as.integer(r$model_years), 10L)
      fi <- fit_info(id, L); E <- eqW(sweep(drw()[[k]]$W[, seq_len(L), drop = FALSE], 2, fi$v)); d <- disc(L); gr <- (1 + st_p$g)^(seq_len(L) - 1)
      v <- rk_score(as.vector(E %*% (st_p$vpw * gr * d)), rho) / sum(d) - pick_aav(pick_cost_for(id, mv_ids, "current"), L)
      E0 <- fa_eq()[[k]]$W[, seq_len(L), drop = FALSE]; v0 <- rk_score(as.vector(E0 %*% (st_p$vpw * gr * d)), rho) / sum(d) - pick_aav(pick_cost_for(id, mv_ids, "current"), L)
      data.table(mlbam_id = id, fit_val = v, fit_who = fi$who, fit_w27 = fi$w27, fit_group = fi$group, fit_src = fi$src, fit_E = list(E), most_lim = walk_tol(v0, L, pay_now())$walk, fit_lim = walk_tol(v, L, pay_now())$walk, market = r$model_aav) })) })

  flag_ids <- preds[nzchar(price_check_text(model_aav, fcst_aav, model_years, fcst_years, price_source, FALSE)), unique(mlbam_id)]
  fa_fit_m_fcst <- reactive({ rho <- input$risk; mv_ids <- moves()$mlbam_id; ids <- intersect(names(fa_draws), as.character(flag_ids))
    rbindlist(lapply(ids, function(k) { id <- as.integer(k); r <- preds[mlbam_id == id][1]; L <- suppressWarnings(as.integer(r$fcst_years)); A <- r$fcst_aav
      if (is.na(L) || is.na(A) || A <= 0) return(NULL); L <- min(L, 10L); Wk <- drw()[[k]]$W; if (ncol(Wk) < L) return(NULL)
      fi <- fit_info(id, L); d <- disc(L); gr <- (1 + st_p$g)^(seq_len(L) - 1); pk <- pick_aav(pick_cost_for(id, mv_ids, "current"), L)
      v <- rk_score(as.vector(eqW(sweep(Wk[, seq_len(L), drop = FALSE], 2, fi$v)) %*% (st_p$vpw * gr * d)), rho) / sum(d) - pk
      v0 <- rk_score(as.vector(eqW(Wk[, seq_len(L), drop = FALSE]) %*% (st_p$vpw * gr * d)), rho) / sum(d) - pk
      data.table(mlbam_id = id, most_lim_m = walk_tol(v0, L, pay_now())$walk, fit_lim_m = walk_tol(v, L, pay_now())$walk, market_m = A, years_m = L) })) })
  fa_fit_m <- reactive(data.table())
  call_rule <- function(bargain, fitr, fitv) fcase(is.na(fitr) | is.na(bargain), "Not valued yet", fitr >= 1.10, "Fits and a bargain", bargain >= 1.10 & fitr < 1.00, "Bargain, but not an upgrade",
    fitr < 0, "Blocked: the Sox already have a better option",
    bargain < 0.90 & fitr < 0.90 & fitv <= 0, "Blocked: the Sox already have a better option", bargain < 0.90 & fitr < 0.90, "Fits, but costs more than he's worth to the Sox right now", default = "Close call")

  board_class <- reactive({ ff <- fa_fit(); if (!nrow(ff)) return(data.table(mlbam_id = integer(), call = character(), bargain = numeric(), fitr = numeric(), fitv = numeric(), who = character(), group = character()))
    x <- ff[, .(mlbam_id, bargain = most_lim / market, fitr = fit_lim / market, fitv = fit_lim, who = fit_who, group = fit_group)]
    x[, call := call_rule(bargain, fitr, fitv)][, call_ours := call]
    m <- fa_fit_m(); if (nrow(m)) { x <- merge(x, m, by = "mlbam_id", all.x = TRUE)
      x[, call_fcst := fifelse(is.na(market_m), NA_character_, call_rule(most_lim_m / market_m, fit_lim_m / market_m, fit_lim_m))]
      x[!is.na(call_fcst) & call_fcst != call_ours & call_ours != "Not valued yet", call := "Depends on price"] } else x[, call_fcst := NA_character_]
    x[mlbam_id %in% SIGNED_IDS, call := "Signed"]
    if (!is.null(NEAR)) { x <- merge(x, NEAR, by = "mlbam_id", all.x = TRUE)
      x[, near := fcoalesce(near0, FALSE)]
      if (nrow(moves())) x[!is.na(hf) & !call %in% c("Signed", "Not valued yet", "Depends on price"), near := near | call_rule(bargain - hb, fitr - hf, fitv) != call_ours | call_rule(bargain + hb, fitr + hf, fitv) != call_ours |
                                                                                   call_rule(bargain - hb, fitr + hf, fitv) != call_ours | call_rule(bargain + hb, fitr - hf, fitv) != call_ours]
      x[call == "Signed", near := FALSE] } else x[, near := FALSE]
    x[] })

  green_reach <- reactive({ bc <- board_class(); cv <- curve(); if (is.null(cv) || !nrow(bc)) return(data.table(mlbam_id = integer(), reach = character()))
    ids <- bc[!is.na(fitr) & fitr < 1.10 & fitv > 0 & !mlbam_id %in% SIGNED_IDS, mlbam_id]; base <- wb(); mv_ids <- moves()$mlbam_id; rho <- input$risk; P <- pay_now()
    rbindlist(lapply(ids, function(id) { r <- preds[mlbam_id == id][1]; L <- min(as.integer(r$model_years), 10L); W <- drw()[[as.character(id)]]$W; if (is.null(W)) return(NULL)
      fi <- fit_info(id, L); Wf <- sweep(W[, seq_len(L), drop = FALSE], 2, fi$v); d <- disc(L); gr <- (1 + st_p$g)^(seq_len(L) - 1); pk <- pick_aav(pick_cost_for(id, mv_ids, "current"), L)
      hit <- NA_integer_; for (Wt in c(75L, 80L, 85L, 90L, 95L)) { v <- rk_score(as.vector(wv_equiv_war(Wf, base + (Wt - base[1]), cv, st_p$vpw) %*% (st_p$vpw * gr * d)), rho) / sum(d) - pk
        if (walk_tol(v, L, P)$walk / r$model_aav >= 1.10) { hit <- Wt; break } }
      data.table(mlbam_id = id, reach = if (is.na(hit)) "not within 75-95" else sprintf("about %d wins", hit)) })) })

  cls_rv <- reactiveValues(prev = NULL, curr = NULL, n = NA_integer_, undo = FALSE)
  observe({ cl <- board_class(); n <- nrow(moves())
    isolate({ if (is.na(cls_rv$n) || n == cls_rv$n) { cls_rv$prev <- NULL } else { cls_rv$prev <- cls_rv$curr; cls_rv$undo <- n < cls_rv$n }
      cls_rv$curr <- cl; cls_rv$n <- n }) })
  call_change <- reactive({ cur <- board_class(); pv <- cls_rv$prev; if (is.null(pv) || !nrow(cur)) return(data.table(mlbam_id = integer(), why = character()))
    x <- merge(cur, pv[, .(mlbam_id, call0 = call, who0 = who)], by = "mlbam_id")[call != call0]
    if (!nrow(x)) return(data.table(mlbam_id = integer(), why = character()))
    x[, why := paste0(call0, " -> ", call, ": ", fifelse(fcoalesce(who, "") != fcoalesce(who0, ""), sprintf("the Sox' current option at %s is now %s (was %s)", group, fcoalesce(who, "no one"), fcoalesce(who0, "no one")),
                                                     "the Sox' win level or payroll changed"), if (isTRUE(cls_rv$undo)) " (after the undo)" else " (after the last move)")][, .(mlbam_id, why)] })
  sox_board <- reactive({ rho <- input$risk; eq <- fa_eq()
    if (isTRUE(input$fit_on)) { ff <- fa_fit(); for (k in seq_len(nrow(ff))) eq[[as.character(ff$mlbam_id[k])]]$W <- ff$fit_E[[k]] }
    x <- preds[as.character(mlbam_id) %in% names(eq) & !is.na(model_aav), .(mlbam_id, L = pmin(as.integer(model_years), 10L), A = model_aav)]
    x[, c("score", "p_neg") := { r <- lapply(seq_len(.N), function(i) { s <- rk_plain_surplus(eq[[as.character(mlbam_id[i])]]$W[, seq_len(L[i]), drop = FALSE], A[i], st_p); c(rk_score(s, rho), mean(s < 0)) })
      list(vapply(r, `[`, 0, 1), vapply(r, `[`, 0, 2)) }]

    mv_ids <- moves()$mlbam_id
    x[, pick := vapply(mlbam_id, pick_cost_for, 0, moved_ids = mv_ids, scen = "current")][, score := score - pick]

    tx <- lapply(seq_len(nrow(x)), function(i) tol_extra(x$A[i], x$L[i], pay_now())); x[, `:=`(fsave = 0, tol_over = vapply(tx, `[[`, TRUE, "over"))][, score := score - vapply(tx, `[[`, 0, "extra")]
    unique(x[, .(mlbam_id, score, p_neg, pick, fsave, tol_over)], by = "mlbam_id") })
  output$skro_png <- renderImage({ req(file.exists(skro$png)); list(src = skro$png, contentType = "image/png", width = 990, alt = "Skubal vs Rogers") }, deleteFile = FALSE)
  output$skro_tbl <- renderDT({ req(!is.null(skro$rng)); r <- skro$rng
    datatable(r[, .(Player = player, `10th` = round(p10, 1), Median = round(p50, 1), `90th` = round(p90, 1), `80% width` = round(width80, 1), `P(0 WAR or less)` = sprintf("%.1f%%", 100 * p_le0),
                    `Engine 2027 (10th / median / 90th)` = sprintf("%.1f / %.1f / %.1f", engine_p10, engine_p50, engine_p90))], rownames = FALSE, options = list(dom = "t")) })
  output$board <- renderDT({ b <- board_rows(); sb <- sox_board()[b, on = "mlbam_id"]; z27 <- if (is.null(fa27)) NULL else fa27[b, on = "mlbam_id"]
    d <- b[, .(Player = player, Fits = fit_text, Pos = position, B = bats, T = throws, Age = age_2027, Tier = tier,
               `WAR 2027 (engine)` = if (is.null(z27)) "" else ifelse(is.na(z27$war27), nv_text(mlbam_id),
                                                                    paste0(fmt_rng(z27$war27, z27$lo27, z27$hi27), ifelse(mlbam_id %in% fb_ids, " Marcel*", ifelse(mlbam_id %in% lpt_ids, paste0(" (", LPT_LABEL, ")"), "")))),
               Years = model_years, `AAV ($M)` = round(model_aav / 1e6, 2), `Total ($M)` = round(model_total / 1e6, 1),
               `Sox surplus ($M, risk-scored, after logged moves)` = round(sb$score / 1e6, 2),
               `Sox surplus notes` = ifelse(is.na(sb$score), nv_text(mlbam_id),
                                                    paste0(sprintf("P<0 %.0f%%", 100 * sb$p_neg), ifelse(fcoalesce(sb$pick, 0) > 0, sprintf(", after the %s draft-pick cost", M(sb$pick, 1)), ""), ifelse(sb$tol_over %in% TRUE, ", over ownership's spending limit", ""))),
               `Value over the Sox' current option ($M AAV)` = { ff <- fa_fit()[b, on = "mlbam_id"]; round(ff$fit_val / 1e6, 2) },
               `Would replace` = { ff <- fa_fit()[b, on = "mlbam_id"]; would_replace(ff$fit_who, ff$fit_src, ff$fit_w27) },
               `Platoon: runs per 600 PA vs Sox opponents (vs RHP / vs LHP)` = { pl <- if (is.null(plat)) NULL else plat[b, on = "mlbam_id"]; if (is.null(pl)) "" else ifelse(is.na(pl$runs_weighted_600), "", if (PUBLIC_BUILD) sprintf("%+.1f", pl$runs_weighted_600) else sprintf("%+.1f (%+.1f / %+.1f)", pl$runs_weighted_600, pl$runs_vs_rhp_600, pl$runs_vs_lhp_600)) },
               `Fit bonus / yr ($M)` = ifelse(bonus_yr > 0, round(bonus_yr / 1e6, 2), NA_real_), QO = qo_short, fitflag = as.integer(fit %in% TRUE))]
    bc <- board_class()[b, on = "mlbam_id"]; ch <- call_change()[b, on = "mlbam_id"]
    d[, `:=`(`Sox call` = fifelse(b$mlbam_id %in% SIGNED_IDS, "Signed", fcoalesce(bc$call, "Not valued yet")), Signed = fcoalesce(signed_tab$tab$label[match(b$mlbam_id, signed_tab$tab$mlbam_id)], ""), `Sox score` = round(bc$fitr, 2),
             `Near the line` = ifelse(fcoalesce(bc$near, FALSE), sprintf("<span title=\"%s\" style=\"cursor:help\">&#9680;</span>", htmltools::htmlEscape(NEAR_TXT, attribute = TRUE)), ""),
             Changed = ifelse(is.na(ch$why), "", sprintf("<span title=\"%s\" style=\"cursor:help\">&#9679;</span>", htmltools::htmlEscape(ch$why, attribute = TRUE))),
             `Price check` = price_check_text(b$model_aav, b$fcst_aav, b$model_years, b$fcst_years, b$price_source, PUBLIC_BUILD))]
    gr_ <- green_reach()[b, on = "mlbam_id"]
    d[, `Green at ($M AAV)` := ifelse(!is.na(bc$fitv) & bc$fitv > 0, round(bc$fitv / 1.10 / 1e6, 2), NA_real_)]
    d[, `Green if the Sox reach (2027 wins)` := fcase(fcoalesce(bc$call, "") == "Fits and a bargain", "already green", !is.na(bc$fitr) & bc$fitr >= 1.10, "green at our price (the call depends on price)", is.na(bc$fitv), "not valued", bc$fitv <= 0, "not within 75-95 (not an upgrade over the Sox' option)",
                                                    !is.na(gr_$reach), gr_$reach, default = "not within 75-95")]
    d[`Sox call` == "Signed", `:=`(`Green at ($M AAV)` = NA_real_, `Green if the Sox reach (2027 wins)` = "signed")]
    setcolorder(d, c("Player", "Sox call", "Near the line", "Signed", "Sox score", "Would replace", "Changed", "Price check", "Green at ($M AAV)", "Green if the Sox reach (2027 wins)"))
    if (PUBLIC_BUILD) setnames(d, "Platoon: runs per 600 PA vs Sox opponents (vs RHP / vs LHP)", "Platoon: runs per 600 PA vs Sox opponents")
    datatable(d, rownames = FALSE, selection = "single", escape = setdiff(names(d), c("Changed", "Near the line")), class = "compact stripe hover",
              options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", columnDefs = list(list(visible = FALSE, targets = which(names(d) == "fitflag") - 1)))) |>
      formatString("Sox score", suffix = "x") |>
      formatCurrency(c("AAV ($M)", "Total ($M)", "Green at ($M AAV)", "Sox surplus ($M, risk-scored, after logged moves)", "Value over the Sox' current option ($M AAV)", "Fit bonus / yr ($M)"), currency = "$", digits = 1) |>
      formatStyle("Sox call", backgroundColor = styleEqual(names(SOX_COL), unname(SOX_COL)), color = styleEqual(names(SOX_TXT), unname(SOX_TXT)), fontWeight = "bold", whiteSpace = "nowrap") |>
      formatStyle("Price check", whiteSpace = "nowrap", color = "#8A5A00") })
  observeEvent(input$board_rows_selected, { id <- board_rows()$mlbam_id[input$board_rows_selected]; updateSelectizeInput(session, "player", selected = id); nav_select("tabs", "Player") })
  observe(updateSelectizeInput(session, "player", choices = setNames(preds$mlbam_id, preds$player), server = TRUE))
  sel <- reactive({ req(input$player); preds[mlbam_id == as.integer(input$player)][1] })

  output$sel_signed <- reactive({ id <- input$player; !is.null(id) && nzchar(id) && as.integer(id) %in% moves()$mlbam_id })
  outputOptions(output, "sel_signed", suspendWhenHidden = FALSE)
  output$player_card <- renderUI({ x <- sel(); f <- fits()[mlbam_id == x$mlbam_id]; v <- if (is.null(curve())) st_p$vpw else curve()[W == round(wb()[1]), mv]
    tagList(
    h4(x$player), if (x$mlbam_id %in% SIGNED_IDS) p(class = "text-secondary", strong("Signed: "), signed_tab$tab[mlbam_id == x$mlbam_id, label], sprintf(" (signings as of %s)", SIGNED_ASOF)),
    p(x$position, " | ", x$role, " | bats ", x$bats, if (nzchar(x$throws) && !is.na(x$throws)) paste0(" / throws ", x$throws), " | age ", x$age_2027, " in 2027 | last club ", x$last_club),
    p(strong("Market price: "), x$model_years, " years, ", M(x$model_aav), " a year (", M(x$model_total, 0), " total). ", span(class = "text-muted", paste0("Source: ", x$price_source, "."))),
    if (!is.na(x$fcst_aav) && grepl("^Imports", x$price_source)) p(class = "small text-muted", "Imports: public forecasts prediction, my model has no MLB history to use (credited). My model alone would say ", M(x$model_aav_capped), " a year."),
    if (!PUBLIC_BUILD && !is.na(x$fcst_aav) && !grepl("^Imports", x$price_source) && !grepl("^MLB Trade Rumors' AAV", x$price_source)) p(class = "small text-muted", "For reference only (not in the price): MLB Trade Rumors predicts ", x$fcst_years, " yr, ", M(x$fcst_aav), " (credited)."),
    { ff <- fa_fit()[mlbam_id == x$mlbam_id]; if (nrow(ff) && !is.na(ff$fit_who)) p(class = "small", strong("Positional fit: "), paste0(would_replace(ff$fit_who, ff$fit_src, ff$fit_w27), ".")) },
    { bc <- board_class()[mlbam_id == x$mlbam_id]; if (nrow(bc) && isTRUE(bc$near)) p(class = "small", style = "background:#EEF2FF;padding:4px 6px;border-left:4px solid #56B4E9", strong(paste0("Sox call: ", bc$call, ", near the line. ")), sub("^Near the line: ", "", NEAR_TXT)) },
    { lp <- price_check_text(x$model_aav, x$fcst_aav, x$model_years, x$fcst_years, x$price_source, PUBLIC_BUILD); if (nzchar(lp)) tagList(p(class = "small", style = "background:#FFF3CD;padding:4px 6px;border-left:4px solid #E69F00", strong("Price check: "), lp),
      { bc <- board_class()[mlbam_id == x$mlbam_id]; if (nrow(bc) && "call_fcst" %in% names(bc) && !is.na(bc$call_fcst)) p(class = "small", strong("Sox call by price: "),
        sprintf("at our price, %s; at %s, %s%s.", bc$call_ours, if (PUBLIC_BUILD) "the public forecast's price" else "the public forecast's price", bc$call_fcst,
                if (identical(bc$call, "Depends on price")) " (the Board shows 'Depends on price')" else " (the same call)")) }) },
    if ("price_note" %in% names(x) && !is.na(x$price_note) && nzchar(x$price_note)) p(class = "small text-info", strong("Price note: "), x$price_note),
    if (x$mlbam_id %in% qo_ids && !is.null(pick_par)) p(class = "small text-danger", strong(paste0(QO_DUE, ".")), " ",
      sprintf("If he turns it down, signing him costs the Sox their second-highest 2027 draft pick (about pick %d, worth %s in 2027 dollars: Edwards, FanGraphs 2019, re-based on the 2019 and 2026 slot values) and %s of international bonus pool money: %s in all (%s). It is subtracted from the most the Sox should pay.",
              as.integer(pick_par$slot), M(pick_par$pick_value, 1), M(pick_par$intl, 1), M(pick_par$first_qo_cost, 1), pick_par$label)),
    p(strong("WAR 2027 (my engine on Steamer 2027, median and 80% range): "), if (!is.null(fa27) && nrow(fa27[mlbam_id == x$mlbam_id])) with(fa27[mlbam_id == x$mlbam_id], fmt_rng(war27, lo27, hi27)) else "not valued"),
    { pl <- if (is.null(plat)) NULL else plat[mlbam_id == x$mlbam_id]; if (!is.null(pl) && nrow(pl)) p(class = "small", strong("Platoon value: "), if (PUBLIC_BUILD) sprintf("%+.1f runs per 600 PA against the Sox' expected opponents (%.0f%% right-handed pitching, weighted by the schedule).", pl$runs_weighted_600, 100 * pl$rhp_share) else sprintf("%+.1f runs per 600 PA against the Sox' expected opponents (%.0f%% right-handed pitching: Steamer 2027 innings of the other 29 clubs, weighted by the schedule); %+.1f vs RHP, %+.1f vs LHP; %+.1f against his own projected mix.", pl$runs_weighted_600, 100 * pl$rhp_share, pl$runs_vs_rhp_600, pl$runs_vs_lhp_600, pl$platoon_adj_600)) },
    if (x$role == "Hitter" && !is.na(x$platoon_runs)) p(class = "small", strong("Platoon: "),
      if (public_mode) sprintf("handedness bonus starting point %s a year (from his projected platoon edge).", M(x$platoon_runs * v, 2))
      else sprintf("Steamer 2027 wOBA %.3f vs RHP, %.3f vs LHP (edge %+.3f); handedness bonus starting point %s a year.", x$woba_vr, x$woba_vl, x$platoon_woba, M(x$platoon_runs * v, 2))),
    if (!public_mode && x$role != "Hitter" && !is.na(x$pit_split_vrhb)) p(class = "small", strong("Splits allowed (Steamer 2027): "), sprintf("wOBA %.3f vs LHB, %.3f vs RHB.", x$pit_split_vlhb, x$pit_split_vrhb)),
    if (nrow(f) && nzchar(f$fit_text)) p(class = "text-success", strong("Fits: "), f$fit_text),
    p(strong("QO: "), qo_now(x), if (nzchar(x$option_text)) tagList(br(), strong("Option: "), x$option_text)),
    card_labels(x)) })

  card_labels <- function(x) {
    st <- status_of(x$mlbam_id); g <- if (!is.null(pl_tab)) pl_tab[mlbam_id == x$mlbam_id] else NULL
    items <- list(
      if (grepl("^low confidence", st)) "WAR paths: Marcel playing time, because Steamer's November projection gives him too little (low confidence).",
      if (grepl(LPT_LABEL, st, fixed = TRUE)) paste0("WAR paths: ", LPT_LABEL, ". Steamer 2027 projects him under the engine's 30-IP floor, so his range comes from what similar veterans projected that low actually did (80% ranges held 81-88% out of sample; most threw no MLB inning)."),
      if (grepl("^not valued", st)) paste0("Not valued: ", sub("^not valued: ", "", st), ". The market price stands; Sox value, surplus and the most the Sox should pay are left blank."),
      if (isTRUE(x$low_confidence)) "Market price: low confidence (new import or thin comparables).",
      if (nzchar(x$estimate_lengths)) sprintf("Slider lengths beyond comparable deals (estimates): %s years.", x$estimate_lengths),
      if (grepl("^C$|Catcher", x$position)) CATCHER_CAVEAT,
      if (!is.null(g) && nrow(g)) paste0("Plausibility check: ", g$flags, ". ", g$why),
      "Years 4-5 of the WAR paths are backtested from 2018-2019 (coverage 0.78 and 0.82); for hitters over 30 they run high.",
      if (input$wv == "placeholder") "Win value: market price per win (no Sox-specific value).",
      "Park fit: not built yet (shown as 0 in the waterfall).")
    items <- Filter(Negate(is.null), items)
    div(class = "border rounded p-2 mt-2 small", strong("Labels for this player"), tags$ul(class = "mb-0", lapply(items, tags$li))) }
  price <- reactive({ x <- sel(); k <- input$years; list(aav = x[[paste0("aav_", k, "y")]], total = x[[paste0("total_", k, "y")]]) })
  output$slider_price <- renderUI({ p(strong(input$years, "-year price: "), M(price()$aav), " a year, ", M(price()$total, 0), " total",
    if (grepl(paste0("\\b", input$years, "\\b"), sel()$estimate_lengths)) span(class = "text-warning", " (estimate: beyond comparable deals)")) })
  output$slider_plot <- renderPlot({ x <- sel(); d <- data.table(years = 1:10, aav = unlist(x[, paste0("aav_", 1:10, "y"), with = FALSE]) / 1e6)
    ggplot(d, aes(years, aav)) + geom_line() + geom_point(aes(colour = years == input$years), size = 3, show.legend = FALSE) +
      scale_x_continuous(breaks = 1:10) + labs(x = "Years", y = "AAV ($M, 2027)") + theme_minimal(base_size = 13) })
  msg_plot <- function(txt) ggplot() + annotate("text", x = 0, y = 0, label = txt, size = 4.5, colour = "grey40") + theme_void()
  output$fa_path_plot <- renderPlot({ x <- if (is.null(fa_paths)) data.table() else fa_paths[mlbam_id == sel()$mlbam_id]
    if (!nrow(x)) return(msg_plot("No WAR paths for this player (not valued)"))
    x <- x[, .(p10 = sum(p10), p50 = sum(p50), p90 = sum(p90)), by = season]
    ggplot(x, aes(season, p50)) + geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2) + geom_line() + geom_point() + geom_hline(yintercept = 0, linetype = 3) +
      labs(x = NULL, y = "WAR (median, 80% range)") + theme_minimal(base_size = 13) })
  output$fa_path_note <- renderUI({ st <- status_of(sel()$mlbam_id)
    if (grepl("^not valued", st)) return(p(class = "text-muted", "Not valued: ", sub("^not valued: ", "", st), ". Steamer's November projection (and Marcel's) gives him less than the engine's playing-time floor (150 PA / 30 IP), or he is a new import Steamer does not project."))
    if (grepl(LPT_LABEL, st, fixed = TRUE)) return(p(class = "small text-muted", paste0(LPT_LABEL, ": Steamer 2027 projects him under 30 IP (below the engine's floor). His paths are Steamer plus the outcomes of historical veterans projected that low (2016-2026; 80% ranges held 81-88% out of sample), so most of his paths are 0 WAR (no MLB innings).")))
    p(class = "small text-muted", if (grepl("^low confidence", st)) "My engine on Marcel playing time (low confidence: Steamer's November projection gives him too little). " else "My engine on Steamer 2027 projections (FanGraphs). ",
      "Dropout term, pitcher playing-time fix, hitter shift, pitcher level calibration, hitter and starter range scales. Years 4-5 backtested from 2018-2019.") })

  years_box <- debounce(reactive(input$years_num), 500)
  observeEvent(years_box(), { v <- years_box(); if (is.null(v) || is.na(v)) return()
    v <- min(10L, max(1L, as.integer(round(v)))); if (!identical(v, as.integer(input$years))) updateSliderInput(session, "years", value = v) }, ignoreInit = TRUE)
  observeEvent(input$years, { if (!isTRUE(as.integer(input$years) == input$years_num)) updateNumericInput(session, "years_num", value = input$years) }, ignoreInit = TRUE)
  aav_in <- debounce(reactive({ v <- input$offer_aav; if (is.null(v) || is.na(v) || v < 0.5 || v > 80) NULL else v }), 500)
  push_aav <- function(v) updateNumericInput(session, "offer_aav", value = v)

  rng <- reactive({ x <- sel(); L <- as.integer(input$years); A <- x[[paste0("aav_", L, "y")]]
    b <- if (is.null(bands)) NULL else bands[mlbam_id == x$mlbam_id & branch_shown == x$branch_shown]
    has_b <- !is.null(b) && nrow(b) == 1 && !is.na(b[[paste0("r10_", L, "y")]])
    rec <- REC_AAV[[if (x$role %in% names(REC_AAV)) x$role else "Hitter"]]
    lo <- if (has_b) min(A * b[[paste0("r10_", L, "y")]], rec) else NA_real_; hi <- if (has_b) min(A * b[[paste0("r90_", L, "y")]], rec) else NA_real_
    valued <- !is.null(fa_draws[[as.character(x$mlbam_id)]])
    most <- ceil <- NA_real_
    if (valued) { o <- offer(); most <- o$walk
      ceil <- walk_tol(stats::quantile(o$r$paths$value, 0.8, names = FALSE) / sum(disc(L)) + bonus_sel() - pick_sel(), L, pay_now())$walk }
    list(L = L, A = A, lo = lo, hi = hi, easy = lo, most = most, ceil = ceil, valued = valued, b = if (has_b) b else NULL, rec = rec) })
  output$range_head <- renderText(sprintf("Price range at %d year%s: the market band and three Sox prices", rng()$L, if (rng()$L > 1) "s" else ""))
  output$range_plot <- renderPlot({ r <- rng(); if (is.na(r$lo)) return(msg_plot("No market band for this player"))
    mk <- data.table(what = c("Easy yes", "Market price", if (nrow(moves())) "Most the Sox should pay\n(after logged moves)" else "Most the Sox should pay", "Absolute ceiling"), v = c(r$easy, r$A, r$most, r$ceil) / 1e6,
                     col = c("#2f855a", "grey30", "#2b6cb0", "#c05621"), y = c(1.32, 0.68, 1.32, 0.68))
    if (!r$valued) mk <- mk[what %in% c("Market price")]
    mk <- mk[!is.na(v)][order(v)]; mk[, y := c(1.62, 0.38, 1.36, 0.64)[seq_len(.N)]]
    ggplot() + annotate("rect", xmin = r$lo / 1e6, xmax = r$hi / 1e6, ymin = 0.85, ymax = 1.15, fill = "#cbd5e0") +
      annotate("text", x = r$hi / 1e6, y = 1, hjust = 1.03, label = sprintf("market: 80%% of deals $%.1fM-$%.1fM", r$lo / 1e6, r$hi / 1e6), size = 3.4) +
      geom_segment(data = mk, aes(x = v, xend = v, y = 0.8, yend = 1.2, colour = col), linewidth = 1.3) +
      geom_label(data = mk, aes(x = v, y = y, label = sprintf("%s\n$%.1fM", what, v), colour = col), size = 3.3, label.size = 0.2) +
      scale_colour_identity() + scale_y_continuous(limits = c(0.2, 1.8), breaks = NULL) +
      scale_x_continuous(labels = function(z) paste0("$", z, "M"), expand = expansion(mult = c(0.3, 0.08))) +
      labs(x = "AAV ($M, 2027)", y = NULL) + theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank()) })
  output$range_note <- renderUI({ r <- rng(); x <- sel(); b <- r$b
    line80 <- if (!is.null(b)) { y1 <- as.integer(b$years_p10); y2 <- as.integer(b$years_p90)
      Lm <- suppressWarnings(as.integer(sub("\\+", "", x$most_likely_years))); if (is.na(Lm)) Lm <- as.integer(x$model_years); Lm <- min(max(Lm, 1L), 10L)
      Am <- x[[paste0("aav_", Lm, "y")]]
      sprintf("I'm 80%% confident he signs for %s years, and at %d year%s for %s-%s a year.", if (y1 == y2) y1 else paste0(y1, "-", y2), Lm, if (Lm > 1) "s" else "",
              M(min(Am * b[[paste0("r10_", Lm, "y")]], r$rec), 0), M(min(Am * b[[paste0("r90_", Lm, "y")]], r$rec), 0)) } else NULL
    o <- if (r$valued) offer() else NULL
    tagList(if (!is.null(line80)) p(strong(line80)),
      if (r$valued && !is.na(r$easy) && r$easy > r$most) p(class = "text-danger small", "Even his low-end market price is more than he's worth to the Sox's current roster."),
      if (!r$valued) p(class = "small text-muted", "Not valued: only the market band is shown."),
      if (!is.null(o) && isTRUE(o$tol$at_limit)) p(class = "text-danger", strong("The Sox are at ownership's spending limit.")),
      if (!is.null(o) && !isTRUE(o$tol$at_limit) && isTRUE(o$tol$lowered)) p(class = "text-danger", sprintf("His wins are worth %s to the Sox, but they're near ownership's spending limit, so the most they should pay drops to %s.", M(o$walk_base, 1), M(o$walk, 1))),
      if (!is.null(o) && !is.null(o$walk_before)) p(class = "small", sprintf("Most the Sox should pay: %s after the moves logged; %s before any moves.", M(o$walk, 1), M(o$walk_before, 1))),
      if (!is.null(o) && !is.null(o$walk_floor_first)) p(class = "small", sprintf("Owners' proposal, if signed first (possible rules, not a prediction): %s (floor money at zero cost up to the lesser of his salary and each season's gap).", M(o$walk_floor_first, 1))),
      if (length(floor_beats_tol())) p(class = "small text-warning", sprintf("Under the owners' proposal the $180.2M floor is above ownership's spending limit in %s, so the floor wins: the Sox must spend to it.", paste(floor_beats_tol(), collapse = ", "))),
      p(class = "small text-muted", "Band: my model's 10th to 90th percentile of AAV at this length (the Nov 3 fit, as ratios to its median, so the band is centred on the market price), top capped at the role's record AAV (hitters $59.2M, starters $52.6M, relievers $24.8M). Easy yes: the band's 10th percentile, a price worth taking whatever the roster or position. Most the Sox should pay: as in the offer builder. Absolute ceiling: the 80th percentile of his value to the Sox at this length (paths, Sox win value), less any draft-pick cost.",
        sprintf(" Spending limit: ownership appetite %.2f; the cost of each added dollar ramps up from 90%% of the limit (an assumption).", appetite_val()))) })
  output$cba_note <- renderUI({ s <- if (isTRUE(input$owners_floor)) "owners_2026_11_03" else "current"; g <- if (is.null(floor_gaps)) NULL else floor_gaps[scenario == s & season >= 2028]
    txt <- switch(s, current = "Off: current rules (no cap or floor; QO draft-pick cost as now; $244M tax threshold carried forward).",
      owners_2026_11_03 = "Cap $258.2M and floor $180.2M from 2028 (tax payroll; 90% allowed in a season, full floor over three years); 2027 under current tax rules; QO only for players 30+; minimum $900K ($1M with 2+ years). AP, Nov 4, 2026.",
      union_2026_05_27 = "Tax threshold $300M; a 'competitive integrity tax' on clubs under about $150M (penalty not reported; treated as a floor); QO eliminated; minimum $1.5M. Fox News / USA Today, May 27, 2026.")
    p(class = "small text-muted", strong(paste0(toupper(substr(CBA_TAG, 1, 1)), substring(CBA_TAG, 2), ". ")), txt,
      if (!is.null(g) && nrow(g) && any(g$gap > 0)) sprintf(" Sox payroll under the floor, 2028-31: %s (before your moves); that salary costs the Sox 0.", paste(sprintf("%d %s", g$season, M(g$gap, 0)), collapse = ", ")) else "",
      " Each player card shows his value if signed first under the floor; the Board and the headline stay on current rules. Market prices are not changed.") })

  observeEvent(input$player, { x <- sel(); L <- suppressWarnings(as.integer(x$model_years)); if (!length(L) || is.na(L)) return(); L <- min(10L, max(1L, L))
    pa_ <- x[[paste0("aav_", L, "y")]]; if (length(pa_) && !is.na(pa_)) push_aav(round(pa_ / 1e6, 2)) })
  observeEvent(input$years, { pa_ <- price()$aav; if (length(pa_) && !is.na(pa_)) push_aav(round(pa_ / 1e6, 2)) }, ignoreInit = TRUE)
  observeEvent(input$opt_kind, { if (identical(sel()$role, "Starter")) updateNumericInput(session, "vest_thr", value = 150) else if (identical(sel()$role, "Reliever")) updateNumericInput(session, "vest_thr", value = 50) })
  paths_sel <- reactive({ z <- drw()[[as.character(sel()$mlbam_id)]]; req(!is.null(z))
    W <- z$W; if (input$conviction != 0) W <- W + input$conviction * (z$P > 0); list(W = W, P = z$P, E = eqW(if (isTRUE(input$fit_on)) fitW(W, sel()$mlbam_id) else W, input$mult)) })
  spec <- reactive(list(L = input$years, option = input$opt_kind, opt_salary_share = input$opt_sal / 100, opt_buyout_share = input$opt_buy / 100,
                        vest_threshold = input$vest_thr, opt_out_after = as.integer(input$opt_out), defer_share = input$defer / 100, defer_lag = 10L,
                        bonus_share = input$bonus / 100))
  bonus_sel <- reactive({ f <- fits()[mlbam_id == sel()$mlbam_id]; if (nrow(f)) f$bonus_yr else 0 })

  pick_cost_sel <- reactive(pick_cost_for(sel()$mlbam_id, moves()$mlbam_id, "current"))

  tol_tab_r <- reactive({ if (is.null(tol_all)) return(NULL); w <- if (is.null(input$tol_window) || !input$tol_window %in% tol_windows) tol_windows[1] else input$tol_window
    tol_all[window == w][order(season)] })
  pay_adj <- reactive({ t <- tol_tab_r(); if (is.null(t)) rep(0, 5) else t$sox_pay_adjust })
  pay_now <- reactive({ x <- led()[, .(cash = sum(cash_nominal, na.rm = TRUE)), by = season][order(season)]; x$cash - pay_adj() + posture_pay() })
  pay_before <- reactive({ p0 <- pay_now(); m <- moves(); if (nrow(m)) for (j in which(mkind(m) == "sign")) { k <- seq_len(min(as.integer(m$years[j]), 5L)); p0[k] <- p0[k] - m$aav[j] }
    tr <- m$mlbam_id[mkind(m) == "trade"]; if (length(tr)) { a <- ledger[mlbam_id %in% tr, .(c = sum(cash_nominal, na.rm = TRUE)), by = season]; p0 <- p0 + (a$c[match(seasons, a$season)] %|% 0) }; p0 })
  tol_vec <- function(L) { L <- min(as.integer(L), 10L); tol_tab <- tol_tab_r(); t0 <- if (is.null(tol_tab)) rep(Inf, L) else ext_vec(appetite_val() * tol_tab$tolerance, L, 0.0385)
    if (isTRUE(input$owners_floor)) t0 <- pmax(t0, c(0, rep(FLOOR_OWNERS, 10))[seq_len(L)]); t0 }
  appetite_val <- reactive(if (is.null(input$appetite)) 1 else input$appetite)
  floor_beats_tol <- reactive({ tol_tab <- tol_tab_r(); if (!isTRUE(input$owners_floor) || is.null(tol_tab)) return(integer()); s <- 2028:2031; s[FLOOR_OWNERS > appetite_val() * tol_tab$tolerance[match(s, tol_tab$season)]] })
  tol_extra <- function(A, L, pay) { L <- min(as.integer(L), 10L); d <- disc(L); Tt <- tol_vec(L); P <- ext_vec(pay, L)
    list(over = any(P + A > Tt), extra = sum(d * (tol_cost(P + A, Tt) - tol_cost(P, Tt) - A))) }
  walk_tol <- function(walk, L, pay) { L <- min(as.integer(L), 10L); d <- disc(L); Tt <- tol_vec(L); P <- ext_vec(pay, L)
    Amax <- min(Tt - P); if (Amax <= 0) return(list(walk = 0, at_limit = TRUE, lowered = walk > 0))
    V <- walk * sum(d); if (V <= 0) return(list(walk = walk, at_limit = FALSE, lowered = FALSE))
    cost <- function(A) sum(d * (tol_cost(P + A, Tt) - tol_cost(P, Tt)))
    if (cost(Amax) <= V) return(list(walk = Amax, at_limit = FALSE, lowered = Amax < walk - 1))
    A <- stats::uniroot(function(A) cost(A) - V, c(0, Amax))$root; list(walk = A, at_limit = FALSE, lowered = A < walk - 1) }
  wb0 <- reactive({ b <- wv_base_wins$wins + sox_shift() + posture_add(); c(b, rep(tail(b, 1), 6)) })
  eqW0 <- function(W, mult = 1) if (is.null(curve())) mult * W else wv_equiv_war(W, wb0(), curve(), st_p$vpw, mult)
  pick_sel <- reactive(pick_aav(pick_cost_sel(), input$years))

  observeEvent(input$player, { L <- suppressWarnings(as.integer(sel()$model_years)); if (length(L) && !is.na(L)) updateSliderInput(session, "years", value = min(10L, max(1L, L))) }, priority = 10)
  offer <- reactive({ ps <- paths_sel(); req(!is.null(aav_in())); A <- aav_in() * 1e6; req(A > 0)
    r <- wv_st_price(ex_make_contract(spec())(A), ps$W, ps$E, ps$P, st_with_market_phi(st_p))
    up <- 4 * max(A, price()$aav) + 5e7
    wb_ <- wv_walkaway(ex_make_contract(spec()), ps$W, ps$E, ps$P, st_with_market_phi(st_p), rho = input$risk, plain = ex_is_plain(spec()), upper = up) + bonus_sel() - pick_sel()
    t1 <- walk_tol(wb_, input$years, pay_now())
    out <- list(r = r, risk = rk_summary(r$paths$surplus, input$risk), A = A, walk_base = wb_, walk = t1$walk, tol = t1, n_moves = nrow(moves()))
    need0 <- nrow(moves()) > 0 || isTRUE(input$owners_floor)
    if (need0) { W0 <- ps$W; E0 <- eqW0(W0, input$mult)
      w0 <- wv_walkaway(ex_make_contract(spec()), W0, E0, ps$P, st_with_market_phi(st_p), rho = input$risk,
      plain = ex_is_plain(spec()), upper = up) + bonus_sel()
      pk0 <- pick_aav(pick_cost_for(sel()$mlbam_id, integer(), "current"), input$years)
      if (nrow(moves())) out$walk_before <- walk_tol(w0 - pk0, input$years, pay_before())$walk
      if (isTRUE(input$owners_floor)) { pko <- pick_aav(pick_cost_for(sel()$mlbam_id, integer(), "owners_2026_11_03"), input$years)
        out$walk_floor_first <- walk_tol(floor_adj(w0 - pko, input$years, "owners_2026_11_03", NULL), input$years, pay_before())$walk } }
    out })
  output$offer_out <- renderUI({
    if (is.null(fa_draws[[as.character(sel()$mlbam_id)]])) return(p(class = "text-muted", "Not valued: this player has no WAR paths, so the builder shows no Sox value for an offer. His market price above still stands."))
    o <- offer(); s <- o$r$summary; k <- o$risk; mk <- price()$aav
    ex <- if (length(o$r$exercise)) paste(sprintf("%s %.0f%%", gsub("_", " ", names(o$r$exercise)), 100 * o$r$exercise), collapse = "; ") else "none"
    tagList(
      tags$table(class = "table table-sm mb-1",
        tags$tr(tags$td("Guaranteed-equivalent AAV of this offer (Sox value basis)"), tags$td(strong(M(s$ge_aav, 2)))),
        tags$tr(tags$td(sprintf("Market AAV at %d years", input$years)), tags$td(M(mk, 2))),
        tags$tr(tags$td(if (o$n_moves > 0) "Most the Sox should pay (AAV, risk-scored, this structure), after the moves logged" else "Most the Sox should pay (AAV, risk-scored, this structure); no moves logged, so this is before any moves"), tags$td(strong(M(o$walk, 2)), if (bonus_sel() > 0) sprintf(" (incl. fit bonus %s)", M(bonus_sel(), 1)),
                                                                                             if (pick_sel() > 0) sprintf(" (after the draft-pick cost, %s a year)", M(pick_sel(), 2)))),
        if (!is.null(o$walk_before)) tags$tr(tags$td("Most the Sox should pay, before any moves"), tags$td(M(o$walk_before, 2))),
        { fv <- fa_fit()[mlbam_id == sel()$mlbam_id]; if (nrow(fv)) tags$tr(tags$td(sprintf("Value over the Sox' current option (he would replace %s, 2027 mean WAR %.1f; plain contract at his market length)", fcoalesce(fv$fit_who, "no one"), fcoalesce(fv$fit_w27, 0))),
          tags$td(strong(M(fv$fit_val, 2)), if (isTRUE(input$fit_on)) " (fit on: counted in Most above)" else " (shown only; turn fit on in the sidebar to count it)")) },
        if (!is.null(o$walk_floor_first)) tags$tr(tags$td("Under the owners' proposal, if signed first ($180.2M floor from 2028; possible rules, not a prediction)"), tags$td(M(o$walk_floor_first, 2))),
        if (pick_cost_sel() > 0) tags$tr(tags$td("Draft-pick cost of a QO player (at signing; current CBA, pending new CBA)"),
                                         tags$td(M(pick_cost_sel(), 1), sprintf(" = %s a year over %d years; not in the surplus rows below", M(pick_sel(), 2), as.integer(input$years)))),
        tags$tr(tags$td("Expected Sox value / expected cost (PV, 2027 $)"), tags$td(M(s$e_value, 1), " / ", M(s$e_cost, 1))),
        tags$tr(tags$td("Surplus: expected (bust 10th, hit 90th)"), tags$td(M(k$e_surplus, 1), " (", M(k$q10, 1), ", ", M(k$q90, 1), ")")),
        tags$tr(tags$td(sprintf("Risk-scored surplus at risk appetite %+.1f", input$risk)), tags$td(strong(M(k$score, 1)), sprintf("   P(surplus < 0) %.0f%%", 100 * k$p_negative))),
        tags$tr(tags$td("Clauses exercised (share of paths)"), tags$td(ex))),
      p(class = "small text-muted", sprintf("Priced on %d simulated WAR paths%s. Win value: %s. Player revenue multiplier %.2f.", nrow(paths_sel()$W),
                                            if (sel()$mlbam_id %in% fb_ids) " (Marcel playing time, low confidence)" else if (sel()$mlbam_id %in% lpt_ids) paste0(" (", LPT_LABEL, ")") else "", wv_labels[[input$wv]], input$mult))) })
  output$offer_hist <- renderPlot({ if (is.null(fa_draws[[as.character(sel()$mlbam_id)]])) return(msg_plot("Not valued (no WAR paths)")); o <- offer(); d <- data.table(s = o$r$paths$surplus / 1e6)
    ggplot(d, aes(s)) + geom_histogram(bins = 40, fill = "#9fb3c8") + geom_vline(xintercept = 0, colour = "grey40") +
      geom_vline(xintercept = o$risk$e_surplus / 1e6, linetype = 2) + geom_vline(xintercept = o$risk$score / 1e6, colour = "#c05621", linewidth = 1) +
      labs(x = "Surplus over the contract ($M, PV)", y = "paths", caption = "Dashed: expected surplus. Orange: risk-scored (certainty-equivalent) surplus.") + theme_minimal(base_size = 13) })

  wf <- reactive({ req(!is.null(fa_draws[[as.character(sel()$mlbam_id)]])); z <- drw()[[as.character(sel()$mlbam_id)]]
    w <- ex_walkaway_waterfall(z$W, z$P, market_aav = price()$aav, spec = spec(), conviction = input$conviction, mult = input$mult, rho = input$risk, p = st_with_market_phi(st_p))
    end0 <- w$contribution[w$kind == "end"]; fin <- offer()$walk_base; fl_c <- offer()$walk - offer()$walk_base
    pk <- pick_sel()
    wv_c <- if (input$wv == "placeholder") -pk else fin - bonus_sel() + pk - end0
    i_wv <- which(w$kind == "placeholder")[1]; i_nb <- which(w$kind == "placeholder")[3]
    w$contribution[i_wv] <- if (input$wv == "placeholder") 0 else wv_c; w$kind[i_wv] <- if (input$wv == "placeholder") "placeholder" else "lever"
    w$contribution[i_nb] <- bonus_sel(); if (bonus_sel() != 0) w$kind[i_nb] <- "lever"

    k <- which(w$kind == "end")
    w <- rbind(w[seq_len(k - 1), ], data.frame(step = "spending limit", contribution = fl_c, kind = if (abs(fl_c) > 1) "lever" else "nofloor", cumulative = 0),
               data.frame(step = "draft pick (QO)", contribution = -pk, kind = if (pk > 0) "lever" else "noqo", cumulative = 0), w[k, ])
    k <- k + 2L
    if (input$wv == "placeholder") wv_c <- 0
    w$contribution[k] <- end0 + wv_c + bonus_sel() - pk + fl_c
    w$cumulative <- c(w$contribution[1], w$contribution[1] + cumsum(w$contribution[2:(k - 1)]), w$contribution[k])
    attr(w, "check") <- abs(sum(w$contribution[1:(k - 1)]) - w$contribution[k]); w }) |>
    bindEvent(input$player, input$years, input$opt_kind, input$opt_sal, input$opt_buy, input$vest_thr, input$opt_out, input$defer, input$bonus, input$conviction, input$mult, input$risk, input$wv, input$sox_w27, moves(), input$bonus_on, input$bonus_pos, input$bonus_hand, aav_in(), rp_map$m, input$appetite, input$owners_floor)
  output$wf_posture_note <- renderUI(p(class = "small", strong("Win path used: "), sprintf("the Sox' win path %s (2028-2031; set on the Overview). 2027 is valued at the Sox' projection plus the moves you log.", posture_label())))
  output$waterfall_plot <- renderPlot({ if (is.null(fa_draws[[as.character(sel()$mlbam_id)]])) return(msg_plot("Not valued (no WAR paths), so no 'most the Sox should pay'"))
    w <- as.data.table(wf())
    w[, `:=`(i = .I, lab = fifelse(kind %in% c("start", "end"), sprintf("$%.1fM", contribution / 1e6), sprintf("%+.1f", contribution / 1e6)))]
    w[, end := cumulative]; w[, start := fifelse(kind %in% c("start", "end"), 0, cumulative - contribution)]
    w[, fillk := fcase(kind %in% c("start", "end"), "level", kind %in% c("placeholder", "noqo", "nofloor"), "placeholder", contribution >= 0, "up", default = "down")]
    w[, lab := fcase(kind == "placeholder", "0*", kind == "noqo", "0 (no QO)", kind == "nofloor", "0 (under\nthe limit)", default = lab)]

    short <- c("Market\nprice", "My view of\nhis production", "Your\nconviction", "Revenue\nmultiplier", "Risk", "Contract\nstructure", "What a win\nis worth\nto the Sox", "Fit:\npark*", "Fit:\nneeds", "Spending\nlimit", "Draft pick\n(QO)", "Most the\nSox should\npay")
    ggplot(w, aes(xmin = i - 0.4, xmax = i + 0.4, ymin = start / 1e6, ymax = end / 1e6, fill = fillk)) + geom_rect() +
      geom_text(aes(x = i, y = pmax(start, end) / 1e6, label = lab), vjust = -0.4, size = 4.2) +
      scale_fill_manual(values = c(level = "#2b6cb0", up = "#2f855a", down = "#c53030", placeholder = "#cbd5e0"), guide = "none") +
      scale_x_continuous(breaks = w$i, labels = short) + scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
      labs(x = NULL, y = "AAV ($M)") + theme_minimal(base_size = 14) + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10.5)) })
  output$waterfall_note <- renderUI(p(class = "small text-muted", paste0("The first six bars are exact Shapley values (all 32 on/off combinations) with every win at the market price. The 'what a win is worth to the Sox' bar then moves the price to the value of his wins at the Sox's win level under the sidebar's win value (",
    tolower(wv_labels[[input$wv]]), "), the needs bonus (if on) is added, and for a player with a qualifying offer the draft-pick cost (the Sox's second-highest 2027 pick, valued by Edwards' 2019 study re-based on 2019 and 2026 slot values, plus $500K of international pool money, current CBA, pending new CBA) is taken off last, spread over the contract; the bars add up to the most the Sox should pay. * Park fit is not built yet (0).")))

  fa_vals <- reactive(alt_fa_values(preds[, .(mlbam_id, player, position, role, age_2027, model_years, model_aav)], fa_eq(), st_p, input$risk))
  alt_grp <- reactive(alt_group_of(sel()$position))
  output$alt_need <- renderUI({ g <- alt_grp(); n <- sox_needs()[group == g]
    if (!nrow(n)) return(p("No need group for this position."))
    tagList(p(strong("Need he fills: "), g, sprintf(" (Sox need rank %d of %d%s). ", n$need_rank, nrow(sox_needs()), if (isTRUE(n$on)) "; a need you set" else ""),
              sprintf("The Sox' k-th best at %s projects %.1f WAR in 2027 against your target of %.1f: gap %+.1f. ", g, n$kth_war, n$target_war, n$gap),
              span(class = "text-muted", "Now: ", n$holders, "."))) })
  output$alt_internal <- renderDT({ x <- alt_internal(sox_depth, pa, alt_grp(), 0L)
    pos_match <- switch(alt_grp(), `corner OF` = c("LF", "RF", "OF"), CF = c("CF", "OF"), SP = c("SP", "MIRP", "SIRP", "P"), RP = c("SIRP", "MIRP", "P"), `2B` = c("2B", "IF", "SS"), `3B` = c("3B", "IF"), SS = c("SS", "IF"), alt_grp())
    pr <- NULL; if (FALSE) pab[!is.na(fv) & !(on_40man %in% TRUE) & pos_board %in% pos_match][order(-fv_num, eta)][seq_len(min(.N, 5)),
            .(Player = name, Status = "prospect", Level = level_board, `2027 WAR (80%)` = "", ETA = if (public_mode) "" else as.character(eta), Grade = if (public_mode) "" else paste0("FV ", fv, " (", risk, " risk)"))] else NULL
    datatable(rbind(x, pr), rownames = FALSE, class = "compact", options = list(dom = "t", pageLength = 20, scrollX = TRUE)) })
  output$alt_fa <- renderDT({ req(sel(), alt_grp()); v <- alt_fa_alternatives(fa_vals(), alt_grp(), sel()$mlbam_id, 5L); me <- fa_vals()[mlbam_id == sel()$mlbam_id]
    x <- rbind(if (nrow(me)) cbind(me, who = "this player"), if (nrow(v)) cbind(v, who = "alternative"), fill = TRUE)
    if (!nrow(x)) return(datatable(data.table(Note = "No valued free agents at this need")))
    datatable(x[, .(` ` = who, Player = player, Pos = position, Age = age_2027, `Market offer` = sprintf("%d yr, %s", as.integer(model_years), M(model_aav)),
                    `Sox value (PV)` = M(e_value), `Cost (PV)` = M(cost), `Risk-scored surplus` = M(score), `Surplus per $` = sprintf("%+.2f", per_dollar))], rownames = FALSE, class = "compact", options = list(dom = "t", scrollX = TRUE)) })
  output$alt_opp <- renderUI({ x <- sel(); me <- fa_vals()[mlbam_id == x$mlbam_id]; req(nrow(me))
    o <- alt_opportunity(fa_vals(), sox_needs(), alt_grp(), me$model_aav, me$score)
    if (is.null(o$best)) return(p(strong("Opportunity cost: "), sprintf("no valued free agent at %s (the next-biggest need) costs %s a year or less.", o$need$group, M(me$model_aav))))
    p(strong("Opportunity cost: "), sprintf("the same money at the Sox' next-biggest need (%s) buys %s, %d yr at %s, risk-scored surplus %s. ", o$need$group, o$best$player, as.integer(o$best$model_years), M(o$best$model_aav), M(o$best$score)),
      if (o$gain > 0) span(class = "text-danger", sprintf("That is %s more than %s's %s.", M(o$gain), x$player, M(me$score))) else span(class = "text-success", sprintf("%s's %s is %s more.", x$player, M(me$score), M(-o$gain)))) })

  observeEvent(input$sign, { x <- sel(); if (x$mlbam_id %in% moves()$mlbam_id) return()
    A <- isolate(input$offer_aav) * 1e6; ge <- tryCatch(offer()$r$summary$ge_aav, error = function(e) NA_real_)
    moves(rbind(moves(), data.table(time = format(Sys.time(), "%H:%M:%S"), player = x$player, mlbam_id = x$mlbam_id, years = input$years, aav = A, total = A * input$years,
                                    conviction = input$conviction, risk = input$risk, castillo = input$castillo, cbt = input$cbt,
                                    structure = paste(c(if (input$opt_kind != "none") input$opt_kind, if (input$opt_out != "0") paste("opt-out after", input$opt_out),
                                                        if (input$defer > 0) paste0(input$defer, "% deferred"), if (input$bonus > 0) paste0(input$bonus, "% bonus")), collapse = "; "),
                                    ge_aav = ge, win_value = input$wv), fill = TRUE))
    showNotification(paste("Move logged:", x$player, input$years, "years at", M(A)), type = "message") })
  observeEvent(input$undo, { m <- moves(); if (nrow(m)) moves(m[-nrow(m)]) })
  observe({ r <- pa[on_40man %in% TRUE & !mlbam_id %in% gone_ids()]; updateSelectizeInput(session, "rm_player", choices = setNames(r$mlbam_id, r$name), server = TRUE) })
  add_roster_move <- function(kind) { id <- suppressWarnings(as.integer(input$rm_player)); if (!length(id) || is.na(id) || id %in% moves()$mlbam_id) return()
    nm <- pa[mlbam_id == id, name][1]; owed <- ledger[mlbam_id == id & season %in% seasons, sum(cash_nominal, na.rm = TRUE)]
    moves(rbind(moves(), data.table(time = format(Sys.time(), "%H:%M:%S"), player = nm, mlbam_id = id, years = 0L, aav = 0, total = 0, kind = kind,
      structure = if (kind == "release") sprintf("Release or DFA: %s guaranteed 2027-2031 still owed", M(owed)) else sprintf("Trade away: %s of 2027-2031 salary leaves (return modeled in Release 3)", M(owed))), fill = TRUE)) }
  observeEvent(input$rm_release, add_roster_move("release")); observeEvent(input$rm_trade, add_roster_move("trade"))
  output$moves <- renderDT(datatable(moves()[, .(Time = time, Player = player, Years = years, AAV = M(aav), Total = M(total, 0), Structure = structure, `GE AAV` = M(ge_aav, 2), `Win value` = win_value, Conviction = conviction, Risk = risk,
                                                 `Castillo 2028` = fifelse(castillo == "vests", "assumed to vest", "assumed not to vest"), CBT = cbt)], rownames = FALSE, options = list(dom = "t", pageLength = 50)))
  output$dl_moves <- downloadHandler(function() paste0("move_log_", Sys.Date(), ".csv"), function(f) fwrite(moves(), f))
  led <- reactive({ l <- copy(ledger)

    if (identical(input$castillo, "does_not_vest")) l <- l[!(mlbam_id %in% CASTILLO & season == 2028L & status == "vesting_option")]
    m <- moves(); tr <- m$mlbam_id[mkind(m) == "trade"]; if (length(tr)) l <- l[!(mlbam_id %in% tr)]
    m <- m[mkind(m) == "sign"]
    if (nrow(m)) l <- rbind(l, m[, .(season = seasons[seq_len(min(years, 5L))], name = player, status = "signed (move)", cash_nominal = aav, tax_aav_nominal = aav), by = .(mlbam_id, years, aav)][, .(mlbam_id, season, name, status, cash_nominal, tax_aav_nominal)], fill = TRUE)
    l })
  cbt <- reactive(cbt_summary(led(), cbt_scenario(params, scenario = input$cbt), params))
  output$payroll_plot <- renderPlot({ l <- led(); tol_tab <- tol_tab_r(); pp_ <- posture_pay(); if (any(pp_ > 0)) l <- rbind(l, data.table(season = seasons, status = "posture", cash_nominal = pp_), fill = TRUE)

    l[, cat := factor(fcase(status == "posture", "Posture additions (assumed)", status == "signed (move)", "New signings", grepl("^arb", status), "Projected arbitration", grepl("pre", status), "Pre-arbitration",
                            status %in% c("vesting_option", "club_option"), "Options (vesting, club)", default = "Guaranteed contracts"),
                      levels = c("Guaranteed contracts", "Options (vesting, club)", "Projected arbitration", "Pre-arbitration", "New signings", "Posture additions (assumed)"))]
    d <- l[, .(cash = sum(cash_nominal, na.rm = TRUE) / 1e6), by = .(season, cat)]; th <- cbt()[, .(season, base = base_threshold / 1e6)]
    lg <- lg_pay[, .(x = as.integer(factor(season, levels = seasons)), avg = avg / 1e6)]
    ggplot(d, aes(factor(season, levels = seasons), cash, fill = cat)) + geom_col() + geom_point(data = th, aes(factor(season, levels = seasons), base), inherit.aes = FALSE, shape = 95, size = 14) +
      geom_line(data = lg, aes(x, avg), inherit.aes = FALSE, linetype = 2, linewidth = 0.8, colour = "grey25") +
      (if (!is.null(tol_tab)) geom_line(data = tol_tab[, .(x = as.integer(factor(season, levels = seasons)), tl = appetite_val() * tolerance / 1e6)], aes(x, tl), inherit.aes = FALSE, linewidth = 1, colour = "#c05621")) +
      (if (!is.null(tol_tab)) annotate("text", x = 1, y = appetite_val() * tol_tab$tolerance[1] / 1e6, label = sprintf("Ownership spending limit (appetite %.2f): $%.0fM", appetite_val(), appetite_val() * tol_tab$tolerance[1] / 1e6), vjust = -0.6, hjust = 0.1, size = 3.6, colour = "#c05621")) +
      annotate("text", x = 1, y = lg$avg[1], label = sprintf("League average payroll (projected): $%.0fM", lg$avg[1]), vjust = 1.6, hjust = 0.1, size = 3.6, colour = "grey25") +
      (if (!is.null(tol_tab)) geom_line(data = tol_tab[, .(x = as.integer(factor(season, levels = seasons)), md = league_median / 1e6)], aes(x, md), inherit.aes = FALSE, linetype = 3, linewidth = 0.8, colour = "#c05621")) +
      (if (!is.null(tol_tab)) geom_point(data = data.table(x = seq_along(seasons), p = pay_now() / 1e6), aes(x, p), inherit.aes = FALSE, shape = 4, size = 4, stroke = 1.3, colour = "black")) +
      (if (!is.null(tol_tab)) { rm_ <- data.table(x = seq_along(seasons), tot = d[, .(t = sum(cash)), by = season][match(seasons, season), t], lim = appetite_val() * tol_tab$tolerance[match(seasons, tol_tab$season)] / 1e6, pl = pay_now() / 1e6)
        geom_label(data = rm_, aes(x, pmax(tot, lim, na.rm = TRUE), label = sprintf("$%.0fM room", lim - pl)), inherit.aes = FALSE, vjust = -0.3, size = 5, fontface = "bold", linewidth = 0, fill = "white", colour = "#2f855a") }) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "$M (nominal)", fill = NULL, caption = "Bars: the current 40-man only (no future call-ups or signings unless logged). After 2027 the Sox have no guaranteed contracts;\nprojected arbitration = players reaching 3 years of service (2+ for Super Two), each assumed tendered, at the arbitration model's salary.\nBar marks: luxury-tax base threshold (the 2026 $244M carried forward,\nor grown per the sidebar; pending new CBA). Dashed: projected league-average\npayroll (2026 AP average grown 3.85% a year). Orange line: ownership spending limit\n(the Sox' peak payroll vs the AP opening-day league median, x appetite; dotted orange: that median, grown 3.85% a year).\nX: Sox payroll on the limit's basis (opening-day roster and injured lists; 2027 leaves out the minor leaguers' $200K salaries).") + theme_minimal(base_size = 13) + theme(plot.caption = element_text(hjust = 0)) })
  output$payroll_tbl <- renderDT({ c <- cbt(); l <- led()[, .(cash = sum(cash_nominal, na.rm = TRUE)), by = season]
    x <- merge(merge(l, c[, .(season, tax_payroll_40man, base_threshold, room_under_base, tax_owed)], by = "season"), lg_pay, by = "season")
    tol_tab <- tol_tab_r(); x[, pay_lim := cash - pay_adj()[match(season, seasons)] + posture_pay()[match(season, seasons)]]
    if (!is.null(tol_tab)) x <- merge(x, tol_tab[, .(season, limit = appetite_val() * tolerance, lg_median = league_median)], by = "season", all.x = TRUE) else x[, `:=`(limit = NA_real_, lg_median = NA_real_)]
    thr_note <- if (identical(input$cbt, "growth")) " ($244M grown $3.5M a year; pending new CBA)" else " (carried forward, pending new CBA)"
    datatable(x[, .(Season = season, `Cash payroll` = M(cash), `League average (projected)` = M(avg), `League median (AP opening day, projected)` = M(lg_median), `Payroll on the limit's basis` = M(pay_lim), `Spending limit` = M(limit), `Headroom to the limit` = M(limit - pay_lim), `Tax payroll` = M(tax_payroll_40man), Threshold = paste0(M(base_threshold, 0), thr_note), `Room under` = M(room_under_base), `Tax owed` = M(tax_owed))], rownames = FALSE, options = list(dom = "t")) })
  output$budget_box <- renderUI({ c27 <- led()[season == 2027L, sum(cash_nominal, na.rm = TRUE)]; left <- input$budget * 1e6 - c27
    tagList(p("2027 cash payroll: ", strong(M(c27))), p("Budget: ", M(input$budget * 1e6, 0)), p(class = if (left < 0) "text-danger" else "text-success", "Room: ", strong(M(left))),
            p(class = "small text-muted", sprintf("Sox win level used for the win value: %.1f in 2027 (my projection %.1f%s%s).", wb()[1], wv_w27,
              if (sox_shift() != 0) sprintf(", GM shift %+.1f", sox_shift()) else "", if (nrow(moves())) sprintf(", plus %d move(s)", nrow(moves())) else ""))) })
  output$win_path_tbl <- renderDT({ mv <- move_war(moves())
    x <- data.table(Season = seasons, `Under control + fill-in` = round(wv_base_wins$wins_no_prospects, 1), `My projection (the fill-in includes prospects)` = round(wv_base_wins$wins, 1),
                    `80% range` = sprintf("%.0f-%.0f", wv_base_wins$wins_p10, wv_base_wins$wins_p90), `GM shift` = round(sox_shift(), 1), `Posture additions` = round(posture_add(), 1), `Moves` = round(mv, 1),
                    `Win level used` = round(wb()[seq_along(seasons)], 1))
    datatable(x, rownames = FALSE, options = list(dom = "t")) })
  roster40 <- reactive(pa[on_40man %in% TRUE & !mlbam_id %in% gone_ids()])

  ledger_flags <- reactive({ l <- led()[!is.na(mlbam_id) & status != "signed (move)"]
    if (!nrow(l)) return(data.table(mlbam_id = integer(), lflag = character()))
    l[, {
      st27 <- status[season == 2027L][1]; ctrl <- sort(season[status != "free_agent"]); last <- if (length(ctrl)) max(ctrl) else NA_integer_
      f <- c(if (!is.na(st27) && grepl("^arb", st27)) "Arbitration-eligible: salary set by arbitration or agreement",
             if (!is.na(st27) && st27 == "pre_arb") "Pre-arbitration: near the league minimum",
             if (!is.na(st27) && st27 == "guaranteed") "Guaranteed contract in 2027",
             if (any(super_two_on_path %in% TRUE)) "Super Two: reaches arbitration a year early (four arbitration years)",
             if (any(status == "vesting_option")) sprintf("Vesting option in %s", paste(season[status == "vesting_option"], collapse = ", ")),
             if (any(status == "mutual_option")) sprintf("Mutual option in %s", paste(season[status == "mutual_option"], collapse = ", ")),
             if (!is.na(last) && last == 2027L) "Contract ends after 2027 (free agent next winter)",
             if (!is.na(last) && last > 2027L && last < 2031L) sprintf("Under club control through %d", last))
      .(lflag = paste(f, collapse = "; ")) }, by = mlbam_id] })
  output$roster <- renderDT({ r <- merge(roster40(), ledger_flags(), by = "mlbam_id", all.x = TRUE)
    r[, plain := mapply(function(oyl, fo, cr, lf) paste(c(
        if (oyl %in% 0L) "Out of minor league options: must pass waivers to be sent down",
        if (fo %in% c("granted_per_sources")) "Fourth option year granted",
        if (fo %in% "possible") "May qualify for a fourth option year",
        if (cr %in% TRUE) "Can refuse an outright assignment and elect free agency",
        if (!is.na(lf) && nzchar(lf)) lf,
        if (is.na(lf)) "Not in the payroll ledger"), collapse = "; "), option_years_left, fourth_option, can_refuse_assignment, lflag)]
    d <- r[order(primary_position, name), .(Player = name, Pos = primary_position, Age = age_2027, B = bats, T = throws, Level = level,
                                           `IL` = il_status, `Options left` = option_years_left, Flags = plain, `Data checks` = flags)]
    if (public_mode) d[, `Data checks` := NULL]
    datatable(d, rownames = FALSE, class = "compact stripe", options = list(pageLength = 15, scrollX = TRUE)) })
  output$warnings <- renderUI({ n40 <- nrow(roster40()) + sum(mkind(moves()) == "sign"); w <- list()
    w[[1]] <- div(class = if (n40 > 40) "alert alert-danger" else "alert alert-secondary", sprintf("40-man roster: %d of 40%s", n40, if (nrow(moves())) sprintf(" (incl. %d signed)", nrow(moves())) else ""))
    oo <- roster40()[option_years_left %in% 0L]; if (nrow(oo)) w[[2]] <- div(class = "alert alert-warning", strong("Out of minor league options"), " (must pass waivers to be sent down): ", paste(oo$name, collapse = ", "))
    r5 <- pa[!(on_40man %in% TRUE) & rule5_eligible_2026 %in% TRUE & org_status != "not_in_org"]
    if (nrow(r5)) w[[3]] <- div(class = "alert alert-warning", strong(sprintf("Rule 5 eligible this winter, not on the 40-man (%d)", nrow(r5))), ": must be added to the 40-man or can be taken in the Rule 5 draft. ", paste(head(r5$name, 25), collapse = ", "), if (nrow(r5) > 25) " ...")
    cr <- roster40()[can_refuse_assignment %in% TRUE]; if (nrow(cr)) w[[4]] <- div(class = "alert alert-info", strong("Can refuse an outright assignment"), " (may elect free agency instead): ", paste(cr$name, collapse = ", "))
    tagList(w) })
  output$paths_tbl <- renderDT({ if (is.null(paths)) return(NULL); x <- copy(paths)[, rng := fmt_rng(p50, p10, p90)]
    datatable(dcast(x, name + side ~ season, value.var = "rng", fun.aggregate = function(v) v[1], fill = "")[order(name)], rownames = FALSE, class = "compact stripe", options = list(pageLength = 10, scrollX = TRUE)) })

  output$ticker <- renderDT({
    if (is.null(grading) || !nrow(grading)) return(datatable(data.table(Note = "No signings logged yet")))
    nm <- function(x) suppressWarnings(as.numeric(x)); mm <- function(x) ifelse(is.na(x), "", paste0("$", sub("\\.?0+$", "", formatC(x / 1e6, format = "f", digits = 2)), "M"))
    g <- copy(grading)[graded == "Y"]; if (!nrow(g)) return(datatable(data.table(Note = "No signing from the class I predicted can be compared yet")))
    yrs <- ifelse(nm(g$years) == 1, "1 year", paste(g$years, "years"))
    off <- function(e) { p <- 100 * (exp(e) - 1); ifelse(is.na(p), "", ifelse(p >= 0, sprintf("%.0f%% above my prediction", p), sprintf("%.0f%% below my prediction", -p))) }
    said <- ifelse(g$mlbam_id %in% import_ids, "MLB Trade Rumors' prediction (credited; my model has no MLB history for imports) said ", "my model said ")
    d <- data.table(Player = g$player, Club = g$club, Reported = as.character(fcoalesce(as.IDate(g$date_reported), as.IDate(g$date_official))),
                    `What happened` = sprintf("Signed for %s a year for %s; %s%s a year: %s.", mm(nm(g$aav_nominal)), yrs, said, mm(nm(g$model_aav)), off(nm(g$err_log_aav))))
    if (PUBLIC_BUILD && "fcst_aav" %in% names(g)) d[, `MLB Trade Rumors' predicted AAV (credited)` := mm(nm(g$fcst_aav))]
    if (!PUBLIC_BUILD && isTRUE(input$ticker_details)) {
      pr <- function(y, a, t) ifelse(is.na(a), "none", paste0(y, " yr, ", M(a), " a year (", M(t), ")"))
      d[, `:=`(`My prediction` = pr(g$model_years, nm(g$model_aav), nm(g$model_total)), `My error (AAV / total)` = paste(pct(nm(g$err_log_aav)), "/", pct(nm(g$err_log_total))),
               `FCST prediction` = pr(g$fcst_years, nm(g$fcst_aav), nm(g$fcst_total)),
               `FCST error (AAV / total)` = fifelse(!is.na(nm(g$err_log_aav_fcst)), paste(pct(nm(g$err_log_aav_fcst)), "/", pct(nm(g$err_log_total_fcst))), ""), Notes = g$notes)] }
    datatable(d, rownames = FALSE, class = "compact stripe", options = list(dom = "ftp", pageLength = 25, scrollX = TRUE)) })
  output$ticker_note <- renderUI({
    if (is.null(grading) || !nrow(grading)) return(NULL)
    u <- grading[graded != "Y"]
    why <- fifelse(u$in_class != "Y", "not in the free-agent class I predicted", fifelse(u$deal_type == "minor_league", "a minor league deal",
           fifelse(grepl("before the Nov 3", u$grade_note), "signed before my Nov 3 predictions were published", fifelse(u$terms_status != "public", "terms not public", "not compared yet"))))
    grp <- split(u$player, why); txt <- paste(vapply(names(grp), function(k) paste0(paste(grp[[k]], collapse = ", "), " (", k, ")"), ""), collapse = "; ")
    tagList(if (nrow(u)) p(class = "small", strong("Not compared with a prediction: "), paste0(txt, ".")),
            p(class = "small text-muted", sprintf("The percent is how far the real AAV landed from my prediction. Signings as reported by MLB Trade Rumors and the MLB transactions feed (credited). Updated %s.",
                                                  if (is.na(ticker_dir)) "never" else sub("[a-z]+$", "", ticker_dir))),
            if (PUBLIC_BUILD) p(class = "small", tags$a(href = FCST_PRED_URL, target = "_blank", "MLB Trade Rumors' 2026-27 predictions"), " (credited).")) })

  output$ov_war <- renderUI({ b <- wv_base_wins[1]; off <- b$wins - b$war
    tagList(p(class = "ov-big", sprintf("%.1f wins", b$wins)), p(class = "mb-1", strong("The 2027 point of the win path"), sprintf(": players under club control plus the fill-in for new players (%.1f wins of it in 2027; it grows each year and includes prospects).", win_path_detail$fill_F[1])), p(class = "small mb-1", sprintf("80%% range %.0f to %.0f wins", b$wins_p10, b$wins_p90)),
            p(class = "mb-1", sprintf("%.1f WAR (80%% range %.1f to %.1f)", b$war, b$wins_p10 - off, b$wins_p90 - off)),
            p(class = "small text-muted mb-0", "Before any move. WAR = players under club control; wins add replacement level and the fill-in for new players. The WAR range is the wins range less those."),
            if (sox_shift() != 0 || nrow(moves())) p(class = "small text-warning mb-0", sprintf("With your shift and moves: %.1f wins in 2027.", wb()[1]))) })
  output$ov_pay <- renderUI({ c27 <- led()[season == 2027L, sum(cash_nominal, na.rm = TRUE)]; a <- lg_pay[season == 2027L, avg]
    tagList(p(class = "ov-big", M(c27)), p(class = "mb-1", sprintf("League average (projected): %s", M(a))),
            p(class = if (c27 > a) "text-danger mb-1" else "text-success mb-1", sprintf("%s %s the average (%.0f%% of it)", M(abs(c27 - a)), if (c27 > a) "above" else "below", 100 * c27 / a)),
            p(class = "mb-1", sprintf("2026 payroll rank: %d of 30 (team payrolls, 2026 season).", OV_FACTS$pay_rank_2026)),
            p(class = "mb-1", sprintf("By group, 2027 committed: hitters %s, starters %s, relievers %s%s.", M(OV_FACTS$sal_group[["hitters"]]), M(OV_FACTS$sal_group[["starters"]]), M(OV_FACTS$sal_group[["relievers"]]),
              if (OV_FACTS$sal_group[["other"]] != 0) sprintf(", other money lines %s", M(OV_FACTS$sal_group[["other"]])) else "")),
            p(class = "small text-muted mb-0", sprintf("Sox: 2027 cash in the %s plus your moves (the big number is before any moves only when none are logged). League: the 2026 average club payroll grown 3.85%% a year.", ledger_label))) })
  output$ov_ranks <- renderUI({ if (is.null(team_ranks)) return(p(class = "text-muted", "Team ranks not available."))
    s <- team_ranks[team_id == 145L]
    tagList(p(class = "mb-1", strong(sprintf("Runs scored: %d", s$rank_runs_scored))),
            p(class = "mb-1", strong(sprintf("Runs allowed: %d", s$rank_runs_allowed)), " (1 = fewest)"),
            p(class = "mb-1", sprintf("Run differential: rank %d", s$rank_run_diff)),
            p(class = "small text-muted mb-0", "MLB Stats API, 2026 season (ranks only).")) })
  output$ov_age <- renderUI(tagList(p(class = "ov-big", sprintf("%.1f", avg_age_40)),
    p(class = "small text-muted mb-1", sprintf("Average age of the %d players on the 40-man roster on July 1, 2027 (roster as of Nov 5; moves not included).", 39L)),
    p(class = "mb-0", sprintf("League rank, 2026 season (1 = oldest): hitters %.1f, %d of 30; pitchers %.1f, %d of 30. FanGraphs player-season ages, weighted by PA and IP, single-club players.",
      OV_FACTS$bat_age, OV_FACTS$bat_age_rank, OV_FACTS$pit_age, OV_FACTS$pit_age_rank))))
  output$ov_win_plot <- renderPlot({ d <- copy(wv_base_wins)[, used := wb()[seq_along(seasons)]][, post := wins + sox_shift() + posture_add()]
    g <- ggplot(d, aes(season)) + geom_ribbon(aes(ymin = wins_p10, ymax = wins_p90), fill = "#2b6cb0", alpha = 0.15) +
      geom_line(aes(y = wins), colour = "#2b6cb0", linewidth = 1) + geom_point(aes(y = wins), colour = "#2b6cb0", size = 2.5) +
      geom_line(aes(y = wins_no_prospects), colour = "#2b6cb0", linetype = 2) + geom_hline(yintercept = 81, linetype = 3, colour = "grey50") +
      annotate("text", x = 2031, y = 81, label = ".500", vjust = -0.4, hjust = 1, size = 3.5, colour = "grey40")
    if (any(posture_add() != 0)) g <- g + geom_line(data = d[season >= 2027L], aes(y = post), colour = "#2f855a", linewidth = 1) +
      annotate("text", x = 2027.05, y = max(d$wins_p90, na.rm = TRUE), label = paste("Green:", posture_label()), vjust = 1, hjust = 0, size = 3.3, colour = "#2f855a")
    g <- g + annotate("text", x = 2027.05, y = min(d$wins_p10, na.rm = TRUE), label = "Blue, 2028-2031: players under control plus the\ngrowing fill-in, before any additions", vjust = 0, hjust = 0, size = 3.2, colour = "#2b6cb0")
    if (sox_shift() != 0 || nrow(moves())) g <- g + geom_line(aes(y = used), colour = "#c05621", linewidth = 1)
    g + scale_x_continuous(breaks = seasons) + labs(x = NULL, y = "Wins") + theme_minimal(base_size = 13) })
  output$ov_pos_plot <- renderPlot({ d <- copy(pos_cmp)
    ggplot(d, aes(cmp)) + geom_col(aes(y = sox27, fill = weak), width = 0.65) + geom_point(aes(y = lg_avg), shape = 23, size = 3.5, fill = "white", colour = "black") +
      geom_text(aes(y = pmax(sox27, lg_avg), label = sprintf("%+.1f", gap)), vjust = -0.7, size = 3.6, colour = ifelse(d$weak, "#c53030", "grey30")) +
      scale_fill_manual(values = c(`TRUE` = "#c53030", `FALSE` = "#9fb3c8"), guide = "none") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      labs(x = NULL, y = "WAR", subtitle = "Bars: Sox 2027 projection, 40-man only, without fill-in.\nDiamonds: 2026 league average per club. Labels: Sox minus average.") + theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(size = 10.5), plot.margin = margin(6, 14, 6, 6), axis.text.x = element_text(size = 10.5)) })
  output$ov_rotation <- renderUI({ inc <- fit_inc(); if (is.null(inc)) return(NULL)
    sp <- fit_pool0[group == "SP" & season == 2027L][order(-w)][seq_len(min(.N, 5))]; if (!nrow(sp)) return(NULL)
    m <- moves(); k <- if ("kind" %in% names(m)) fcoalesce(m$kind, "sign") else rep("sign", nrow(m)); gone <- m$mlbam_id[k != "sign"]; sp <- fit_pool0[group == "SP" & season == 2027L & !mlbam_id %in% gone][order(-w)]
    add <- if (nrow(m)) { a <- rbindlist(lapply(which(k == "sign"), function(j) { g <- alt_group_of(preds[mlbam_id == m$mlbam_id[j], position][1]); if (!identical(g, "SP") || is.null(fa_paths)) return(NULL)
      data.table(name = m$player[j], w = fa_paths[mlbam_id == m$mlbam_id[j] & season == 2027L, sum(mean)]) })); a } else NULL
    sp <- rbind(sp[, .(name, w)], add, fill = TRUE)[order(-w)][seq_len(min(.N, 5))]
    d <- data.table(Slot = seq_len(5), Sox = c(sprintf("%s (%.1f)", sp$name, sp$w), rep("none", 5 - nrow(sp))), `League average (2026)` = sprintf("%.1f", OV_FACTS$lg_slot_war))
    gap <- c(sp$w, rep(0, 5 - nrow(sp))) - OV_FACTS$lg_slot_war
    typ <- c("front-line", "front-line", "mid-rotation", "back-end", "back-end and depth")
    worst <- order(gap)[1:2]
    words <- sprintf("Rotation: the biggest gaps to the league-average slot are slot %d (%s, %+.1f WAR) and slot %d (%s, %+.1f). %s", worst[1], typ[worst[1]], gap[worst[1]], worst[2], typ[worst[2]], gap[worst[2]],
      if (min(gap[1:2]) < -0.5) "The Sox lack a front-line starter: their best options project like a league-average No. 3-4." else "The top of the rotation is near league average; the need is depth.")
    pc <- copy(pos_cmp)[!grepl("SP", cmp)][order(gap)][1:3]
    other <- paste(sprintf("%s: %.1f WAR projected vs %.1f league average", pc$cmp, pc$sox27, pc$lg_avg), collapse = "; ")
    tagList(h6(class = "mt-2", "Rotation slots vs the league average slot"), tags$table(class = "table table-sm small mb-1", tags$thead(tags$tr(lapply(names(d), tags$th))), tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(lapply(unlist(d[i]), function(v) tags$td(as.character(v))))))), p(class = "small", words),
      p(class = "small", strong("Other positions, largest gaps: "), other, ". Sox: 2027 engine WAR (40-man, moves included); league: 2026 FanGraphs WAR by slot (club starters ranked by starts, single-club pitchers) and by position.")) })
  output$ov_needs <- renderDT({ n <- sox_needs()[gap > 0][order(need_rank)][seq_len(min(.N, 5))]
    datatable(n[, .(Rank = need_rank, Need = group, `Target (2027 WAR)` = target_war, `Sox k-th best` = round(kth_war, 1), Gap = round(gap, 1), Now = holders)],
              rownames = FALSE, class = "compact", options = list(dom = "t", scrollX = TRUE)) })
  output$ov_starters <- renderDT({ l <- led()
    ctrl <- l[status != "free_agent" & season %in% seasons, .(control = uniqueN(season)), by = mlbam_id]
    c27 <- l[season == 2027L, .(cash27 = sum(cash_nominal, na.rm = TRUE), st27 = status[1]), by = mlbam_id]
    k <- data.table(group = alt_groups$group, k = pmin(alt_groups$slots, c(rep(5L, 9), 3L)))
    s <- merge(sox_depth[!is.na(war27)], k, by = "group")[order(group, -war27)][, .SD[seq_len(min(.N, k[1]))], by = group]
    s <- merge(merge(s, ctrl, by = "mlbam_id", all.x = TRUE), c27, by = "mlbam_id", all.x = TRUE)
    s[, group := factor(group, levels = alt_groups$group)]
    datatable(s[order(group, -war27), .(Position = as.character(group), Player = name, Age = age_2027, `2027 WAR (80%)` = fmt_rng(war27, lo27, hi27),
                                        `Years of control` = fifelse(is.na(control), "not in ledger", as.character(control)),
                                        `2027 salary` = fifelse(is.na(cash27), "", paste0(M(cash27, 2), fifelse(grepl("^arb", st27), " (arb. projection)", ""))))],
              rownames = FALSE, class = "compact", options = list(dom = "t", pageLength = 30, scrollX = TRUE)) })
  output$ov_rank_tbl <- renderDT({ s <- if (is.null(team_ranks)) NULL else team_ranks[team_id == 145L]
    r1 <- if (!is.null(team_ranks)) data.table(Measure = c("Runs scored", "Runs allowed", "Run differential"), `Rank (of 30)` = c(s$rank_runs_scored, s$rank_runs_allowed, s$rank_run_diff)) else NULL
    datatable(r1, rownames = FALSE, class = "compact", options = list(dom = "t", pageLength = 20)) })
  output$ov_lost <- renderDT({ x <- copy(sox_lost)
    if (!nrow(x)) return(datatable(data.table(Note = "No free agents whose last club is the White Sox")))
    d <- x[, .(Player = player, Pos = position, Age = age_2027,
               `Market price` = paste0(model_years, " yr, ", M(model_aav), " a year"), `Market total` = M(market_total),
               Note = fifelse(option_pending %in% TRUE, "option still pending", ""))]
    if (!public_mode) d[, `fWAR / bWAR (2026)` := sprintf("%.1f / %.1f", x$fwar26, x$bwar26)]
    p27 <- if (is.null(fa_paths)) rep(NA_real_, nrow(x)) else vapply(x$mlbam_id, function(id) { v <- fa_paths[mlbam_id == id & season == 2027L, sum(mean)]; if (length(v) && v != 0) v else NA_real_ }, 0)
    d[, `2027 projection (WAR, my engine)` := ifelse(is.na(p27), "not valued", sprintf("%.1f", p27))]
    tot <- data.table(Player = "Total", Pos = "", Age = NA_integer_, `Market price` = sprintf("%s a year (sum of AAVs)", M(sum(x$model_aav))),
                      `Market total` = M(sum(x$market_total)), Note = "")
    datatable(rbind(d, tot, fill = TRUE), rownames = FALSE, class = "compact", options = list(dom = "t", pageLength = 30, scrollX = TRUE)) })
  output$ov_lost_note <- renderUI(cap("Clubs projected like the Sox (75-85 wins) typically add about 5 wins and lose about 4 in a winter (free agency and trades; median projected WAR, 140 club-winters 2015-16 to 2025-26). Free agents whose last 2026 club is the White Sox (the market prices file; the class file has no club column). Market price: my model, years x AAV. 2027 projection: my engine."))
  tb <- function(x) if (is.null(x)) datatable(data.table(Note = "not available")) else datatable(x, rownames = FALSE, class = "compact", options = list(dom = "t", scrollX = TRUE))
  r3 <- function(x) round(x, 3)
  output$cal_price <- renderDT(tb(if (!is.null(cal$price)) cal$price[, .(source, outcome, n, MAE = r3(mae), bias = r3(bias))]))
  output$cal_eng <- renderDT(tb(if (!is.null(cal$eng)) dcast(cal$eng[group %in% c("all", "hitters", "pitchers", "C", "SP")], group + year ~ model, value.var = "cov80")))
  output$cal_em <- renderDT(tb(if (!is.null(cal$em)) cal$em[input == "steamer", .(model, group, n, CRPS = r3(crps), `80% cov` = r3(cover80))]))
  output$cal_arb <- renderDT(tb(if (!is.null(cal$arb)) cal$arb[, .(season, n, `My model (MAE)` = r3(ours_mae))]))
  output$cal_inj <- renderDT(tb(if (!is.null(cal$inj_pt)) rbind(cal$inj_pt[group == "all", .(output = "PT share", model, n, score = round(rps, 4))], cal$inj_60[, .(output = "P(60-day)", model, n, score = round(logloss, 4))])))
  output$cal_y45 <- renderDT(tb(if (!is.null(cal$y45)) cal$y45[grepl("with production", model) & group %in% c("all", "hitters", "pitchers", "bat 30+", "C") & year >= 3,
    .(group = sub("bat 30\\+", "hitters 30+", group), year, n, `80% cov` = r3(cov80), `bias (pred - actual)` = round(bias, 2), `P(WAR<=0) pred` = r3(pred_le0), `actual` = r3(act_le0))]))
  output$cal_att <- renderDT(tb(if (!is.null(cal$att)) cal$att[season == "all", .(rows = n, model = r3(rmse_main), spline = r3(rmse_spline), baseline = r3(rmse_base))]))
}
shinyApp(ui, server)
