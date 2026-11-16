
alt_groups <- data.table::data.table(
  group = c("C", "1B", "2B", "3B", "SS", "CF", "corner OF", "DH", "SP", "RP"),
  slots = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 5L, 8L),
  target_war = c(2, 2, 2, 2, 2, 2, 2, 2, 1, 0.2))
alt_group_of <- function(pos) data.table::fcase(pos %in% c("LF", "RF", "OF"), "corner OF", pos %in% c("SP", "RP", "C", "1B", "2B", "3B", "SS", "CF", "DH"), pos,
                                                 pos %in% "Starter", "SP", pos %in% "Reliever", "RP", default = NA_character_)

alt_pitcher_role <- function(ps, ids) {
  x <- data.table::as.data.table(ps)[mlbam_id %in% ids & season >= 2024 & data.table::fcoalesce(as.numeric(g_pit), 0) > 0][order(mlbam_id, -season)][, .SD[1], by = mlbam_id]
  x[, .(mlbam_id, role = data.table::fifelse(data.table::fcoalesce(as.numeric(gs), 0) / as.numeric(g_pit) >= 0.5, "SP", "RP"))]
}

alt_eta <- function(level) data.table::fcase(level == "AAA", "2027", level == "AA", "2028", level == "A+", "2029", level == "A", "2030", level %in% c("CPX", "DSL"), "2031+", default = "")

alt_sox_depth <- function(pa, war27, prole) {
  x <- data.table::as.data.table(pa)[on_40man %in% TRUE, .(mlbam_id, name, pos = primary_position, age_2027, level, il_status)]
  x[data.table::as.data.table(prole), on = "mlbam_id", prole := i.role]
  x[pos == "P", pos := data.table::fcoalesce(prole, "RP")]
  x[, group := alt_group_of(pos)]
  x <- merge(x, data.table::as.data.table(war27), by = "mlbam_id", all.x = TRUE)
  x[, prole := NULL][]
}

alt_needs <- function(depth, groups = alt_groups) {
  d <- data.table::as.data.table(depth)
  out <- groups[, {
    w <- sort(d[group == .BY$group & !is.na(war27), war27], decreasing = TRUE)
    kth <- if (length(w) >= slots) w[slots] else 0
    .(slots, target_war, kth_war = kth, holders = paste(head(d[group == .BY$group & !is.na(war27)][order(-war27), sprintf("%s %.1f", name, war27)], slots), collapse = ", "))
  }, by = group]
  out[, gap := target_war - kth_war][order(-gap)][, need_rank := .I][]
}

alt_internal <- function(depth, pa, grp, n_prosp = 4L) {
  mlb <- data.table::as.data.table(depth)[group == grp][order(-data.table::fcoalesce(war27, -99)),
           .(Player = name, Status = "40-man", Level = level, `2027 WAR (80%)` = data.table::fifelse(is.na(war27), "no path", sprintf("%.1f (%.1f to %.1f)", war27, lo27, hi27)),
             ETA = "now", Grade = "")]
  pos_match <- switch(grp, `corner OF` = c("LF", "RF", "OF"), CF = c("CF", "OF"), SP = "P", RP = "P", `2B` = c("2B", "IF"), `3B` = c("3B", "IF"), SS = c("SS", "IF"), grp)
  lv <- c("AAA", "AA", "A+", "A", "CPX", "DSL")
  pr <- data.table::as.data.table(pa)[!(on_40man %in% TRUE) & org_status != "not_in_org" & primary_position %in% pos_match & level %in% lv]
  pr <- pr[order(match(level, lv), age_2027)][seq_len(min(.N, n_prosp)),
           .(Player = name, Status = "prospect", Level = level, `2027 WAR (80%)` = "", ETA = paste0(alt_eta(level), "*"), Grade = "pending*")]
  rbind(mlb, pr)
}

alt_fa_values <- function(market, draws, p, rho = 0) {
  m <- data.table::as.data.table(market)[as.character(mlbam_id) %in% names(draws) & !is.na(model_aav)]
  m <- unique(m, by = "mlbam_id")
  r <- lapply(seq_len(nrow(m)), function(i) {
    L <- min(as.integer(m$model_years[i]), 10L); A <- m$model_aav[i]; W <- draws[[as.character(m$mlbam_id[i])]]$W[, seq_len(L), drop = FALSE]
    d <- (1 + p$r)^-(seq_len(L) - 1); gr <- (1 + p$g)^(seq_len(L) - 1)
    v <- as.vector(W %*% (p$vpw * gr * d)); cst <- A * sum(d); s <- v - cst
    c(e_value = mean(v), cost = cst, score = rk_score(s, rho))
  })
  m[, c("e_value", "cost", "score") := data.table::transpose(lapply(r, unname))]
  m[, per_dollar := score / cost][]
}

alt_fa_alternatives <- function(vals, grp, exclude_id, k = 5L) {
  v <- data.table::as.data.table(vals)[alt_group_of(position) == grp & mlbam_id != exclude_id]
  v[order(-score)][seq_len(min(.N, k))]
}

alt_opportunity <- function(vals, needs, grp, aav, own_score) {
  nx <- data.table::as.data.table(needs)[group != grp][1]
  v <- data.table::as.data.table(vals)[alt_group_of(position) == nx$group & model_aav <= aav * 1.0001][order(-score)]
  best <- if (nrow(v)) v[1] else NULL
  list(need = nx, best = best, gain = if (is.null(best)) NA_real_ else best$score - own_score)
}
