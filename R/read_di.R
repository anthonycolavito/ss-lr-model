# Readers for disability (Phase 2) source documents.

source("R/read_studies.R")   # pdf_lines()

#' Actuarial Note 2026.6, Tables C (men) and D (women): survival and disability
#' status of 1,000,000 insured workers attaining age 20 in 2026, ages 20-67,
#' 2026 TR intermediate assumptions.
#'
#' Columns, for the year from age x to x+1: living at the beginning of the year
#' (total, active, disabled, recovered); deaths (total, active, disabled,
#' recovered); newly disabled (total, from active, from recovered); recoveries.
#' Active = insured, never entitled; recovered = previously entitled, not now.
read_an_illustration <- function(path = "docs/reference/AN2026-6_death_disability_probabilities.pdf") {
  L <- pdf_lines(path)
  one <- function(title, sex) {
    i0 <- grep(title, L, fixed = TRUE)[1]
    rows <- L[(i0 + 1):(i0 + 70)]
    rows <- rows[grepl("^\\s*[0-9]{2}\\s+[0-9,]+", rows)]
    vals <- lapply(strsplit(trimws(rows), "\\s+"), function(v) {
      v[v %in% c("—", "-", "—")] <- "0"
      as.numeric(gsub(",", "", v))
    })
    vals <- vals[!duplicated(sapply(vals, `[`, 1))]
    full <- vals[lengths(vals) == 21]
    last <- vals[lengths(vals) == 5]
    m <- do.call(rbind, full)
    out <- data.frame(age = m[, 1], total = m[, 2], active = m[, 3], disabled = m[, 4],
                      recovered = m[, 5], deaths = m[, 6], deaths_active = m[, 8],
                      deaths_disabled = m[, 10], deaths_recovered = m[, 12],
                      new_disabled = m[, 14], new_from_active = m[, 16],
                      new_from_recovered = m[, 18], recoveries = m[, 20])
    if (length(last)) {
      l <- last[[1]]
      out <- rbind(out, data.frame(age = l[1], total = l[2], active = l[3], disabled = l[4],
                                   recovered = l[5], deaths = NA, deaths_active = NA,
                                   deaths_disabled = NA, deaths_recovered = NA,
                                   new_disabled = NA, new_from_active = NA,
                                   new_from_recovered = NA, recoveries = NA))
    }
    out$sex <- factor(sex, levels = c("M", "F"))
    out$year <- 2006 + out$age
    out
  }
  rbind(one("Table C: Illustrations", "M"), one("Table D: Illustrations", "F"))
}

#' Actuarial Study No. 130 select-and-ultimate tables (2016-20 experience):
#' probability of death (7A men, 7B women) or recovery (14A, 14B) in a
#' multiple-decrement setting, by age at entitlement (16-65) and completed
#' duration 0-9, plus the ultimate column (duration 10+, indexed by attained
#' age = entitlement age + 10). Returns long data: entl_age, duration (0-10,
#' 10 = ultimate), attained_age, q. Missing cells (recovery past NRA) are NA.
read_as130_su <- function(table_id, path = "docs/reference/AS130_DI_disabled_worker_experience_2026.pdf") {
  L <- pdf_lines(path)
  i0 <- grep(paste0("Table ", table_id, ".—"), L, fixed = TRUE)
  i0 <- i0[length(i0)]                         # the table itself, not the list of tables
  block <- L[i0:(i0 + 75)]
  block <- block[seq_len(grep("^\\s*Notes:", block)[1] - 1)]
  rows <- block[grepl("^\\s*[0-9]{2}\\s+(0\\.|—)", block)]
  out <- do.call(rbind, lapply(strsplit(trimws(rows), "\\s+"), function(v) {
    v[v %in% c("—", "-")] <- NA
    v <- suppressWarnings(as.numeric(v))
    stopifnot(length(v) == 13)
    data.frame(entl_age = v[1], duration = 0:10, q = v[2:12], attained_age = v[1] + 0:10)
  }))
  stopifnot(all(sort(unique(out$entl_age)) == 16:65))
  out
}

#' Actuarial Study No. 130, Table 7C: probability of death, attained ages 76+.
read_as130_7c <- function(path = "docs/reference/AS130_DI_disabled_worker_experience_2026.pdf") {
  L <- pdf_lines(path)
  i0 <- grep("Table 7C.—", L, fixed = TRUE); i0 <- i0[length(i0)]
  block <- L[i0:(i0 + 60)]
  block <- block[seq_len(grep("^\\s*Notes:", block)[1] - 1)]
  rows <- block[grepl("^\\s*[0-9]{2,3}\\s+0\\.", block)]
  m <- do.call(rbind, lapply(strsplit(trimws(rows), "\\s+"), as.numeric))
  rbind(data.frame(attained_age = m[, 1], sex = "M", q = m[, 2]),
        data.frame(attained_age = m[, 1], sex = "F", q = m[, 3]))
}

#' Actuarial Study No. 130 historical tables by year (2001-24) and sex:
#'   3  awards by age group at award
#'   4  incidence rates per 1,000 exposed (award basis) by age group, plus
#'      gross and age-adjusted totals
#'   5  terminations (death, recovery, other), conversions, and rates per
#'      1,000 beneficiaries
#'   6  in current-payment status by age group at end of year
#' `cols` names the numeric columns after the year. Returns year, sex, cols.
read_as130_hist <- function(table_id, cols,
                            path = "docs/reference/AS130_DI_disabled_worker_experience_2026.pdf") {
  L <- pdf_lines(path)
  i0 <- grep(paste0("Table ", table_id, ".—"), L, fixed = TRUE); i0 <- i0[length(i0)]
  block <- L[i0:(i0 + 90)]
  stop_at <- grep("^\\s*(Notes?:|Table [0-9])", block)
  stop_at <- stop_at[stop_at > 1]
  if (length(stop_at)) block <- block[seq_len(stop_at[1] - 1)]
  sex <- NA; out <- list()
  for (ln in block) {
    t <- trimws(ln)
    if (t %in% c("Men", "Women", "Total")) { sex <- t; next }
    if (!is.na(sex) && grepl("^[0-9]{4}\\s", t)) {
      v <- strsplit(t, "\\s+")[[1]]
      v <- as.numeric(gsub(",", "", v))
      if (length(v) != length(cols) + 1) stop("Table ", table_id, ": unexpected row: ", t)
      out[[length(out) + 1]] <- setNames(as.list(c(v[1], v[-1])), c("year", cols))
      out[[length(out)]]$sex <- sex
    }
  }
  d <- do.call(rbind, lapply(out, as.data.frame))
  d$sex <- factor(c(Men = "M", Women = "F", Total = "T")[d$sex], levels = c("M", "F", "T"))
  d
}

# ---- Annual Statistical Supplement, 2026 (December 2025 data) -------------------

supp26 <- function(file) file.path("data-raw/supplement/2026", file)
read_text_sheet <- function(path, sheet) {
  d <- as.data.frame(readxl::read_excel(path, sheet, col_names = FALSE, col_types = "text",
                                        .name_repair = "minimal"))
  d[] <- lapply(d, function(x) trimws(gsub("[\u2013\u2014]", "-", gsub("[[:space:]\u00a0]+", " ", x))))
  d
}

#' Table 5.A1.2: disabled-worker beneficiaries in current-payment status by
#' single age (20-66) and sex, December 2025, with average monthly benefit (mba).
#' "Under 20" is returned as age 19.
read_supp_5a12 <- function(path = supp26("5a.xlsx")) {
  d <- read_text_sheet(path, "5.A1.2")
  single <- !is.na(d[[2]]) & grepl("^[0-9]+$", d[[2]])
  u20 <- which(trimws(d[[1]]) == "Under 20")
  rows <- c(u20, which(single))
  age <- c(19L, as.integer(d[[2]][single]))
  out <- rbind(data.frame(age = age, sex = "M", number = as.numeric(d[[6]][rows]), mba = as.numeric(d[[7]][rows])),
               data.frame(age = age, sex = "F", number = as.numeric(d[[8]][rows]), mba = as.numeric(d[[9]][rows])))
  out$sex <- factor(out$sex, levels = c("M", "F"))
  tot <- d[!is.na(d[[3]]) & d[[3]] == "Total", ]
  stopifnot(abs(sum(out$number[out$sex == "M"]) - as.numeric(tot[[6]])) < 1,
            abs(sum(out$number[out$sex == "F"]) - as.numeric(tot[[8]])) < 1)
  out
}

#' Table 5.D1: disabled-worker beneficiaries by year of entitlement and sex,
#' December of the edition's data year. "Before YYYY" is returned as year
#' YYYY - 1 with before = TRUE; mba = average monthly benefit. Works for the
#' 2013-2016, 2025 and 2026 editions.
read_supp_5d1 <- function(path = supp26("5d.xlsx")) {
  d <- read_text_sheet(path, "5.D1")
  yr <- trimws(d[[1]])
  keep <- !is.na(yr) & grepl("^([0-9]{4}|Before [0-9]{4})$", yr)
  out <- rbind(data.frame(ent_year = yr[keep], sex = "M", number = as.numeric(d[[7]][keep]), mba = as.numeric(d[[10]][keep])),
               data.frame(ent_year = yr[keep], sex = "F", number = as.numeric(d[[11]][keep]), mba = as.numeric(d[[14]][keep])))
  out$before <- grepl("^Before", out$ent_year)
  out$ent_year <- ifelse(out$before, as.integer(sub("Before ", "", out$ent_year)) - 1L,
                         suppressWarnings(as.integer(out$ent_year)))
  out$sex <- factor(out$sex, levels = c("M", "F"))
  tot <- d[!is.na(d[[2]]) & d[[2]] == "Total", ]
  stopifnot(abs(sum(out$number[out$sex == "M"]) - as.numeric(tot[[7]])) < 1,
            abs(sum(out$number[out$sex == "F"]) - as.numeric(tot[[11]])) < 1)
  out
}

#' Table 6.A4: disabled-worker awards in 2025 by age at award and sex. Single
#' ages 50-66; younger ages in groups (Under 25, 25-29, ..., 45-49), returned
#' with lo/hi bounds.
read_supp_6a4_di <- function(path = supp26("6a.xlsx")) {
  d <- read_text_sheet(path, "6.A4")
  i0 <- which(trimws(d[[5]]) == "Disabled workers")
  d <- d[(i0 + 1):nrow(d), ]
  tot <- d[!is.na(d[[4]]) & trimws(d[[4]]) == "Total", ][1, ]
  grp <- trimws(d[[1]]); single <- !is.na(d[[2]]) & grepl("^[0-9]+$", d[[2]])
  g_keep <- grp %in% c("Under 25", "25-29", "30-34", "35-39", "40-44", "45-49")
  lo <- c(`Under 25` = 15, `25-29` = 25, `30-34` = 30, `35-39` = 35, `40-44` = 40, `45-49` = 45)
  a <- data.frame(lo = c(lo[grp[g_keep]], as.integer(d[[2]][single])),
                  hi = c(lo[grp[g_keep]] + c(9, 4, 4, 4, 4, 4)[match(grp[g_keep], names(lo))],
                         as.integer(d[[2]][single])),
                  men = as.numeric(c(d[[7]][g_keep], d[[7]][single])),
                  women = as.numeric(c(d[[9]][g_keep], d[[9]][single])))
  stopifnot(abs(sum(a$men) - as.numeric(tot[[7]])) < 1, abs(sum(a$women) - as.numeric(tot[[9]])) < 1)
  rbind(data.frame(lo = a$lo, hi = a$hi, sex = "M", awards = a$men),
        data.frame(lo = a$lo, hi = a$hi, sex = "F", awards = a$women)) |>
    transform(sex = factor(sex, levels = c("M", "F")))
}

#' Table 6.F2: disabled-worker terminations in 2025 by reason.
read_supp_6f2_di <- function(path = supp26("6f.xlsx")) {
  d <- read_text_sheet(path, "6.F2")
  lab <- trimws(paste(ifelse(is.na(d[[1]]), "", d[[1]]), ifelse(is.na(d[[2]]), "", d[[2]])))
  v <- suppressWarnings(as.numeric(d[[6]]))
  data.frame(reason = lab, number = v)[!is.na(v), ]
}

#' Table 6.C2: disabled-worker awards by year (annual from 1980), sex, and age
#' group at award (percent distribution times number). Groups: under 30,
#' 30-39, 40-44, 45-49, 50-54, 55-59, 60-61, 62-64, 65-FRA.
read_supp_6c2 <- function(path = supp26("6c.xlsx")) {
  d <- read_text_sheet(path, "6.C2")
  sec <- rep(NA_character_, nrow(d))
  cur <- NA
  for (i in seq_len(nrow(d))) {
    if (!is.na(d[[3]][i]) && d[[3]][i] %in% c("Men", "Women")) cur <- d[[3]][i]
    sec[i] <- cur
  }
  keep <- !is.na(sec) & !is.na(d[[1]]) & grepl("^[0-9]{4}$", d[[1]])
  grp <- c("u30", "30_39", "40_44", "45_49", "50_54", "55_59", "60_61", "62_64", "65_fra")
  out <- data.frame(year = as.integer(d[[1]][keep]), sex = c(Men = "M", Women = "F")[sec[keep]],
                    number = as.numeric(d[[3]][keep]))
  pct <- sapply(6:14, function(j) suppressWarnings(as.numeric(d[[j]][keep])))
  pct[is.na(pct)] <- 0
  colnames(pct) <- grp
  out <- cbind(out, pct)
  out <- tidyr::pivot_longer(out, tidyr::all_of(grp), names_to = "group", values_to = "pct")
  out$awards <- out$number * out$pct / 100
  out$sex <- factor(out$sex, levels = c("M", "F"))
  out
}

#' Table 5.D1 from a PDF edition (the 2012 Supplement, December 2011 data):
#' single-year rows only. Returns ent_year, sex, number, mba, before (FALSE).
read_supp_5d1_pdf <- function(path) {
  L <- pdf_lines(path)
  i0 <- grep("Single-year data", L)[1]
  rows <- L[(i0 + 1):length(L)]
  rows <- rows[grepl("^\\s*[0-9]{4}\\s+[0-9,]+", rows)]
  v <- lapply(strsplit(trimws(rows), "\\s+"), function(x) as.numeric(gsub(",", "", x)))
  v <- v[lengths(v) == 13]
  m <- do.call(rbind, v)
  out <- rbind(data.frame(ent_year = as.integer(m[, 1]), sex = "M", number = m[, 6], mba = m[, 9]),
               data.frame(ent_year = as.integer(m[, 1]), sex = "F", number = m[, 10], mba = m[, 13]))
  out$before <- FALSE
  out$sex <- factor(out$sex, levels = c("M", "F"))
  stopifnot(all(abs(m[, 2] - m[, 6] - m[, 10]) <= 1))
  out
}
