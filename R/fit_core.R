
fit_pool <- function(players, prospects, pa, pab, prole) {
  pl <- data.table::as.data.table(players)[under_control %in% TRUE, .(w = sum(mean)), by = .(mlbam_id, name, season)]
  gp <- data.table::as.data.table(pa)[, .(mlbam_id, pos = primary_position)]
  gp[data.table::as.data.table(prole), on = "mlbam_id", prole := i.role][pos %in% c("P", "TWP"), pos := data.table::fcoalesce(prole, "RP")]
  pl <- merge(pl, gp[, .(mlbam_id, group = alt_group_of(pos))], by = "mlbam_id", all.x = TRUE)
  pr <- data.table::as.data.table(prospects)[eta == 2027]
  pr <- merge(pr, unique(data.table::as.data.table(pab)[, .(mlbam_id, pos = data.table::fcoalesce(pos_board, primary_position))], by = "mlbam_id"), by = "mlbam_id", all.x = TRUE)
  pr[pos %in% c("P", "SP", "RP", "RHP", "LHP", "SIRP", "MIRP"), pos := "SP"]
  pr <- pr[, .(mlbam_id, name, season, w, group = alt_group_of(pos), source = "prospect due 2027")]
  rbind(pl[, .(mlbam_id, name, season, w, group, source = "40-man")], pr)[!is.na(group)]
}
fit_add_moves <- function(pool, moves_tab) {
  if (is.null(moves_tab) || !nrow(moves_tab)) return(pool)
  rbind(pool, data.table::as.data.table(moves_tab)[, .(mlbam_id, name, season, w, group, source = "signed (move)")])
}
fit_incumbents <- function(pool, groups = alt_groups, seasons = 2027:2031) {
  g <- data.table::CJ(group = groups$group, season = seasons)
  g[, c("incumbent", "incumbent_w", "incumbent_source") := {
    k <- groups$slots[match(.BY$group, groups$group)]; x <- pool[group == .BY$group & season == .BY$season][order(-w)]
    if (nrow(x) >= k) list(x$name[k], x$w[k], x$source[k]) else list("none (replacement level)", 0, "none") }, by = .(group, season)]
  g[]
}

fit_inc_vec <- function(inc, grp, L) { v <- inc[inc$group == grp][order(season)]$incumbent_w; if (!length(v)) v <- rep(0, 5); v <- pmax(v, 0); c(v, rep(tail(v, 1), 10))[seq_len(L)] }
