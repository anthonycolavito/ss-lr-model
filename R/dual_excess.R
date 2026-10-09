# dual_excess.R
#
# Dual-entitlement excess inside the Supplement's retired-worker averages (DECISIONS.md DX-01).
#
# The Supplement's average monthly benefit of retired workers (5.A1.1, 5.A3a, 5.B4) is the combined
# benefit: for a retired worker also entitled as a spouse or widow(er), it includes the excess of
# the auxiliary benefit over the worker benefit (5.G3: 8.18 million dually entitled, average excess
# $784 in December 2025; women's average benefit $1,872 is above their average PIA, $1,792). OCACT's
# retired-worker matrices exclude the excess, which is projected separately.
#
# excess_by_age() returns, by sex and single age 62-100, the excess per retired worker and its share
# of the published average benefit, December 2025:
#   women 65+     5.A15 by age band: dually entitled wives and widows x combined benefit x the excess
#                 share of the combined benefit for that type (5.G3: wives 0.324, widows 0.476)
#   women 62-64   5.A14 (62+) minus 5.A15 (65+), at the 65-69 band's benefits
#   men           5.G3 husband and widower totals spread over ages like women's wives and widows
# Excess per retired worker interpolated linearly between band averages (placed at each band's mean
# age); totals scaled to 5.G3.

excess_by_age <- function(f5a = "data-raw/supplement/2026/5a.xlsx", f5g = "data-raw/supplement/2026/5g.xlsx") {
  num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
  rd <- function(f, s) { m <- as.matrix(readxl::read_excel(f, sheet = s, col_names = FALSE, col_types = "text", .name_repair = "minimal")); m[is.na(m)] <- ""; m }
  lab <- function(m) trimws(gsub("\\s+", " ", paste(m[, 1], m[, 2], m[, 3], m[, 4])))
  g3 <- rd(f5g, "5.G3"); l3 <- lab(g3)
  row3 <- function(p) which(grepl(p, l3))[1]
  g <- function(p) num(g3[row3(p), 5:8])            # number, combined, worker part, excess
  wives <- g("Wives of"); widows <- g("^Widows$"); husb <- g("Husbands of"); widr <- g("^Widowers$")
  sh_w <- wives[4] / wives[2]; sh_wd <- widows[4] / widows[2]
  # women 65+ by band (5.A15)
  a15 <- rd(f5a, "5.A15"); l15 <- lab(a15)
  rows <- which(grepl("Wife's benefit|Survivor's ben", l15))
  avg_row <- which(apply(a15, 1, function(r) any(grepl("Average monthl", r))))[1]
  # first two rows = "entitled as worker" (dually entitled) numbers; rows 3-4 their averages after the "Average" header
  nums <- rows[rows < avg_row][1:2]
  avgs <- rows[rows > avg_row][1:2]
  bands <- tibble::tibble(lo = c(65, 70, 75, 80, 85, 90), hi = c(69, 74, 79, 84, 89, 100))
  bands$n_w <- num(a15[nums[1], 6:11]); bands$n_wd <- num(a15[nums[2], 6:11])
  bands$c_w <- num(a15[avgs[1], 6:11]); bands$c_wd <- num(a15[avgs[2], 6:11])
  # women 62-64: 5.A14's 2025 row (thousands) minus 65+
  a14 <- rd(f5a, "5.A14"); r25 <- which(trimws(a14[, 1]) == "2025")[1]
  n62_w <- num(a14[r25, 7]) * 1e3 - sum(bands$n_w); n62_wd <- num(a14[r25, 8]) * 1e3 - sum(bands$n_wd)
  bands <- dplyr::bind_rows(tibble::tibble(lo = 62, hi = 64, n_w = max(n62_w, 0), n_wd = max(n62_wd, 0), c_w = bands$c_w[1], c_wd = bands$c_wd[1]), bands)
  bands <- dplyr::mutate(bands, exc_w = n_w * c_w * sh_w, exc_wd = n_wd * c_wd * sh_wd)
  # men: totals spread like women's
  bands <- dplyr::mutate(bands, exc_h = husb[1] * husb[4] * n_w / sum(n_w), exc_wr = widr[1] * widr[4] * n_wd / sum(n_wd))
  ages <- tidyr::uncount(dplyr::mutate(bands, k = hi - lo + 1), k, .id = "i")
  ages <- dplyr::mutate(ages, age = lo + i - 1L, F = exc_w + exc_wd, M = exc_h + exc_wr)   # band totals
  ex <- tidyr::pivot_longer(dplyr::select(ages, lo, age, F, M), c(F, M), names_to = "sex", values_to = "band_total")
  # per retired worker and share of the published average (5.A1.1, single ages to 89, 90-94, 95-99, 100+)
  a11 <- rd(f5a, "5.A1.1"); l11 <- ifelse(grepl("^[0-9]+$", trimws(a11[, 2])), trimws(a11[, 2]), trimws(a11[, 1]))
  lo11 <- suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", gsub("[^0-9a-z ]", "-", l11))))
  ok <- !is.na(lo11) & lo11 >= 62 & (grepl("^[0-9]+$", l11) | lo11 >= 90)
  pub <- dplyr::bind_rows(tibble::tibble(sex = "M", lo = lo11[ok], n = num(a11[ok, 6]), mba = num(a11[ok, 7])),
                          tibble::tibble(sex = "F", lo = lo11[ok], n = num(a11[ok, 8]), mba = num(a11[ok, 9])))
  pub <- dplyr::distinct(pub, sex, lo, .keep_all = TRUE)
  pub <- dplyr::mutate(pub, hi = dplyr::case_when(lo < 90 ~ lo, lo == 90 ~ 94L, lo == 95 ~ 99L, TRUE ~ 100L))
  pa <- tidyr::uncount(dplyr::mutate(pub, k = hi - lo + 1), k, .id = "i")
  pa <- dplyr::mutate(pa, age = lo + i - 1L, n = n / (hi - lo + 1))
  out <- dplyr::inner_join(ex, dplyr::select(pa, sex, age, n, mba), by = c("sex", "age"))
  # excess per retired worker: each band's average placed at its count-weighted mean age and
  # interpolated linearly between bands (flat beyond the end bands); totals scaled to 5.G3
  tot <- c(F = wives[1] * wives[4] + widows[1] * widows[4], M = husb[1] * husb[4] + widr[1] * widr[4])
  bm <- dplyr::summarise(dplyr::group_by(out, sex, lo), x = sum(age * n) / sum(n), y = first(band_total) / sum(n), .groups = "drop")
  out <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(out, sex), excess = {
    b <- bm[bm$sex == first(sex), ]; approx(b$x, b$y, xout = age, rule = 2)$y }))
  out <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(out, sex), excess = excess * tot[sex] / sum(excess * n)))
  out <- dplyr::mutate(out, excess_total = excess * n, share = excess / mba)
  attr(out, "totals") <- c(wives = wives[1] * wives[4], widows = widows[1] * widows[4], husbands = husb[1] * husb[4], widowers = widr[1] * widr[4])
  out
}
