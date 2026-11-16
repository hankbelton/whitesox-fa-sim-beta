
cbt_read_params <- function(path = "data/cbt_thresholds.csv") {
  x <- read_human_csv(path)
  x[, `:=`(season = as.integer(season), value = as.numeric(value))]
  x[]
}

cbt_scenario <- function(params, seasons = 2027:2031, scenario = c("current", "growth"), growth_per_year = 3.5e6,
                         threshold_override = NULL) {
  scenario <- match.arg(scenario)
  p <- data.table::as.data.table(params)
  base26 <- p[param == "base_threshold" & season == 2026L, value]
  step <- p[param == "surcharge_step", value]
  base <- if (!is.null(threshold_override)) rep(threshold_override, length(seasons)) else
    if (scenario == "current") rep(base26, length(seasons)) else base26 + growth_per_year * (seasons - 2026L)
  data.table::data.table(season = seasons, scenario = if (is.null(threshold_override)) scenario else "override",
                         base_threshold = base, tier1 = base + step, tier2 = base + 2 * step, tier3 = base + 3 * step)
}

cbt_tax <- function(cbt_payroll, scen_row, params, consecutive = 1L) {
  p <- data.table::as.data.table(params)
  r <- function(k) p[param == k, value]
  base_rate <- c(r("base_rate_1"), r("base_rate_2"), r("base_rate_3"))[min(max(consecutive, 1L), 3L)]
  s2 <- if (consecutive <= 1L) r("surcharge_2_first") else r("surcharge_2_repeat")
  over <- function(t) pmax(0, cbt_payroll - t)
  over(scen_row$base_threshold) * base_rate +
    (over(scen_row$tier1) - over(scen_row$tier2)) * r("surcharge_1") +
    (over(scen_row$tier2) - over(scen_row$tier3)) * s2 +
    over(scen_row$tier3) * r("surcharge_3")
}

cbt_summary <- function(ledger, scen, params, consecutive_start = 1L, other_cbt_nominal = 0) {
  l <- data.table::as.data.table(ledger)
  tp <- l[, .(tax_payroll_40man = sum(tax_aav_nominal, na.rm = TRUE)), by = season]
  x <- merge(tp, scen, by = "season")
  x[, cbt_payroll := tax_payroll_40man + other_cbt_nominal]
  x[, room_under_base := base_threshold - cbt_payroll]
  cons <- consecutive_start; tax <- numeric(nrow(x))
  for (i in seq_len(nrow(x))) {
    if (x$cbt_payroll[i] > x$base_threshold[i]) { tax[i] <- cbt_tax(x$cbt_payroll[i], x[i], params, cons); cons <- cons + 1L }
    else cons <- 1L
  }
  x[, `:=`(tax_owed = tax, other_cbt_note = if (other_cbt_nominal == 0) "benefits and other CBT charges not counted (setting)" else "benefits and other CBT charges counted")]
  x[]
}

payroll_arb_level_from_history <- function(ledger, history) {
  l <- data.table::copy(data.table::as.data.table(ledger))
  h <- data.table::as.data.table(history)
  l <- merge(l, h, by = "mlbam_id", all.x = TRUE)
  l[is.na(n_arb_before), n_arb_before := 0L]
  data.table::setorder(l, mlbam_id, season)
  l[, arb_index := cumsum(grepl("^arb", status)), by = mlbam_id]
  l[grepl("^arb", status), status_history := paste0("arb", n_arb_before + arb_index)]
  l[, level_corrected := grepl("^arb", status) & status_history != status]
  l[grepl("^arb", status), status := status_history]
  l[!is.na(super_two_before) & super_two_before == TRUE, super_two_on_path := TRUE]
  l[, c("arb_index", "status_history", "n_arb_before", "super_two_before") := NULL]
  l[]
}

payroll_arb_source <- function(model, fcst, first_season = 2027L) {
  m <- data.table::copy(data.table::as.data.table(model))
  b <- data.table::as.data.table(fcst)[, .(mlbam_id, fcst_nominal, fcst_url = url)]
  m <- merge(m, b, by = "mlbam_id", all.x = TRUE)
  m[, model_nominal := salary_nominal]
  m[season == first_season & !is.na(fcst_nominal), `:=`(salary_nominal = fcst_nominal,
      basis = paste0("the public forecast's 2027 arbitration projection (Matt Swartz's model, credited; ", fcst_url, "); our model ",
                     format(model_nominal, big.mark = ",", scientific = FALSE), " as the check"))]
  m[season != first_season, fcst_nominal := NA_real_]
  m[, fcst_url := NULL]
  m[]
}
