
fit_add_moves <- function(pool, moves_tab) {
  if (is.null(moves_tab) || !nrow(moves_tab)) return(pool)
  rbind(pool, data.table::as.data.table(moves_tab)[, .(mlbam_id, name, season, w, group, source = "signed (move)")], fill = TRUE)
}
fit_pt_hitter_groups <- c("C", "1B", "2B", "3B", "SS", "CF", "corner OF", "DH")
fit_pt_default <- c(`40-man` = 650, `prospect due 2027` = 400, `signed (move)` = 650)

if (!exists("fit_incumbents_kth")) fit_incumbents_kth <- fit_incumbents
fit_incumbents_pt <- function(pool, groups = alt_groups, seasons = 2027:2031, cap_pa = 650) {
  base <- fit_incumbents_kth(pool, groups, seasons)
  pool <- data.table::copy(data.table::as.data.table(pool))
  if (!"pt" %in% names(pool)) pool[, pt := NA_real_]
  pool[is.na(pt) | pt <= 0, pt := unname(fit_pt_default[source])]; pool[is.na(pt), pt := cap_pa]
  for (g in intersect(fit_pt_hitter_groups, groups$group)) for (s in seasons) {
    k <- groups$slots[match(g, groups$group)]; cap <- k * cap_pa
    x <- pool[group == g & season == s][, rate := w / pt][order(-rate)]
    if (!nrow(x)) { base[group == g & season == s, `:=`(incumbent = "none (replacement level)", incumbent_w = 0, incumbent_source = "none")]; next }
    x[, counted := pmax(0, pmin(pt, cap - (cumsum(pt) - pt)))]
    over <- min(cap_pa, max(0, sum(x$counted) + cap_pa - cap))
    if (over <= 0) { base[group == g & season == s, `:=`(incumbent = "none (replacement level)", incumbent_w = 0, incumbent_source = "none")]; next }
    x <- x[counted > 0][order(rate)]; x[, disp := pmax(0, pmin(counted, over - (cumsum(counted) - counted)))]; d <- x[disp > 0]
    val <- sum(d$disp * pmax(d$rate, 0))
    lab <- if (nrow(d) == 1L && d$disp >= d$pt - 1e-6) d$name else
      paste(sprintf("%s (%.2f WAR in %.0f PA%s)", d$name, d$w, d$pt, ifelse(d$disp < d$pt - 1e-6, sprintf(", %.0f of them displaced", d$disp), "")), collapse = " + ")
    src <- if (nrow(d) == 1L) d$source else "shared position"
    base[group == g & season == s, `:=`(incumbent = lab, incumbent_w = val, incumbent_source = src)] }
  base[]
}
