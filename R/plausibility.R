
pl_role_bounds <- function(rows) {
  d <- data.table::as.data.table(rows)[!is.na(pv_aav_2027) & pv_aav_2027 > 0 & role %in% c("H", "SP", "RP")]
  d[order(-pv_aav_2027), .(n = .N, record_aav = pv_aav_2027[1], record_holder = sprintf("%s (%s)", name[1], winter[1]),
                           p99_aav = unname(stats::quantile(pv_aav_2027, 0.99, type = 7))), by = role][order(match(role, c("H", "SP", "RP")))]
}

pl_role_code <- function(r) data.table::fcase(r %in% c("H", "Hitter"), "H", r %in% c("SP", "Starter"), "SP", r %in% c("RP", "Reliever"), "RP", default = NA_character_)

pl_flags <- function(pred, bounds) {
  p <- data.table::copy(data.table::as.data.table(pred))
  if (!"branch" %in% names(p)) p[, branch := "single"]
  if (!"slider_estimate_lengths" %in% names(p) && "estimate_lengths" %in% names(p)) p[, slider_estimate_lengths := estimate_lengths]
  p[, role_code := pl_role_code(role)]
  p <- merge(p, data.table::as.data.table(bounds)[, .(role_code = role, record_aav, record_holder, p99_aav)], by = "role_code", all.x = TRUE)
  est_len <- function(s, L) vapply(seq_along(s), function(i) {
    x <- s[i]; if (is.na(x) || !nzchar(x)) return(FALSE)
    rng <- unlist(lapply(strsplit(x, ",")[[1]], function(t) { a <- as.integer(strsplit(trimws(t), "-")[[1]]); if (length(a) == 2) a[1]:a[2] else a }))
    L[i] %in% rng }, logical(1))
  out <- list(); add <- function(i, flag, why) if (length(i)) out[[length(out) + 1]] <<- data.table::data.table(row = i, flag = flag, why = why)
  i <- which(p$model_aav > p$record_aav)
  add(i, "above role record", sprintf("model AAV %s above the %s record %s, %s", pl_m(p$model_aav[i]), p$role_code[i], pl_m(p$record_aav[i]), p$record_holder[i]))
  i <- which(p$model_aav > p$p99_aav & !(p$model_aav > p$record_aav))
  add(i, "above role 99th percentile", sprintf("model AAV %s above the %s 99th percentile %s", pl_m(p$model_aav[i]), p$role_code[i], pl_m(p$p99_aav[i])))
  sl <- intersect(sprintf("aav_%dy", 1:10), names(p))
  if (length(sl)) {
    S <- as.matrix(p[, sl, with = FALSE]); S[is.na(S)] <- -Inf
    ok_len <- matrix(TRUE, nrow(S), ncol(S))
    if ("slider_estimate_lengths" %in% names(p)) for (L in seq_len(ncol(S))) ok_len[, L] <- !est_len(p$slider_estimate_lengths, rep(L, nrow(S)))
    hit <- (S > p$record_aav) & ok_len
    i <- which(rowSums(hit) > 0 & !(p$model_aav > p$record_aav))
    add(i, "slider above role record", vapply(i, function(r) { L <- which(hit[r, ]); sprintf("slider AAV above the %s record %s at %s yr (max %s), lengths within comparable deals",
      p$role_code[r], pl_m(p$record_aav[r]), pl_lengths(L), pl_m(max(S[r, L]))) }, ""))
  }
  if ("model_length_estimate" %in% names(p)) { i <- which(p$model_length_estimate %in% c(TRUE, "TRUE"))
    add(i, "headline length is an estimate", sprintf("model length %s yr is beyond comparable deals", p$model_years[i])) }
  if ("raise_flag" %in% names(p)) { i <- which(p$raise_flag %in% c("above band", "below band"))
    add(i, paste("raise", p$raise_flag[i]), sprintf("AAV over previous salary is %s the 10th-90th percentile band%s", sub(" band", "", p$raise_flag[i]),
                                                     if ("raise_band" %in% names(p)) paste0(" (", p$raise_band[i], ")") else "")) }
  if ("low_confidence" %in% names(p)) { i <- which(p$low_confidence %in% c(TRUE, "TRUE"))
    add(i, "low confidence", "low confidence: new import or thin comparables") }
  if (!length(out)) return(data.table::data.table(name = character(), mlbam_id = integer(), role = character(), branch = character(), flag = character(), why = character(), model_aav = numeric()))
  f <- data.table::rbindlist(out)
  cbind(p[f$row, .(name, mlbam_id, role = role_code, branch, model_years = if ("model_years" %in% names(p)) model_years else NA, model_aav)], f[, .(flag, why)])
}

pl_review_sheet <- function(flags, label) {
  f <- data.table::as.data.table(flags)
  if (!nrow(f)) return(data.table::data.table(prediction_set = character(), name = character(), mlbam_id = integer(), role = character(), flags = character(),
                                              why = character(), max_model_aav = numeric(), note = character()))
  pri <- c("above role record", "slider above role record", "above role 99th percentile", "headline length is an estimate", "raise above band", "raise below band", "low confidence")
  f[, pr := match(flag, pri)]
  s <- f[order(pr), .(role = role[1], flags = paste(unique(flag), collapse = "; "),
                      why = paste(unique(sprintf("%s: %s", branch, why)), collapse = " | "), max_model_aav = max(model_aav, na.rm = TRUE), top = min(pr)), by = .(name, mlbam_id)]
  s[order(top, -max_model_aav)][, .(prediction_set = label, name, mlbam_id, role, flags, why, max_model_aav = round(max_model_aav), note = "")]
}

pl_require_notes <- function(sheet) {
  s <- data.table::as.data.table(sheet)
  miss <- s[is.na(note) | !nzchar(trimws(note))]
  if (nrow(miss)) stop(sprintf("plausibility check: %d of %d flagged player(s) have no note (%s). Fill the note column of the review sheet before writing.",
                               nrow(miss), nrow(s), paste(utils::head(miss$name, 8), collapse = ", ")), call. = FALSE)
  TRUE
}

pl_write_release <- function(pred, path, bounds, sheet_path, label, writer = data.table::fwrite) {
  sheet <- pl_review_sheet(pl_flags(pred, bounds), label)
  if (file.exists(sheet_path)) {
    old <- data.table::as.data.table(read_human_csv(sheet_path, colClasses = "character"))
    sheet[, mlbam_chr := as.character(mlbam_id)]
    sheet[old, on = .(mlbam_chr = mlbam_id), note := data.table::fcoalesce(i.note, "")]
    sheet[, mlbam_chr := NULL]
  }
  write_human_csv(sheet, sheet_path)
  pl_require_notes(sheet)
  writer(pred, path)
  invisible(path)
}

pl_m <- function(x) sprintf("$%.1fM", x / 1e6)
pl_lengths <- function(L) { if (!length(L)) return("")
  g <- split(L, cumsum(c(1, diff(L) != 1))); paste(vapply(g, function(v) if (length(v) > 1) sprintf("%d-%d", min(v), max(v)) else as.character(v), ""), collapse = ",") }
