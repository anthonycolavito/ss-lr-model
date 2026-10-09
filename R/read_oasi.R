# Readers for OASI (Phase 3) source tables.

source("R/read_di.R")   # read_text_sheet(), supp26()


# Rows of single ages plus the age groups above the last single age.
# label_single, label_group: columns holding single ages and group labels.
parse_age_rows <- function(d, col_single, col_group) {
  single <- !is.na(d[[col_single]]) & grepl("^[0-9]+$", d[[col_single]])
  top <- max(as.integer(d[[col_single]][single]))
  g <- d[[col_group]]
  lo <- suppressWarnings(as.integer(sub("^([0-9]+)(-[0-9]+| or older)$", "\\1", g)))
  hi <- ifelse(grepl("or older", g), 120L, suppressWarnings(as.integer(sub("^[0-9]+-([0-9]+)$", "\\1", g))))
  grp <- !is.na(g) & grepl("^[0-9]+(-[0-9]+| or older)$", g) & !is.na(lo) & lo > top
  rows <- which(single | grp)
  data.frame(row = rows,
             age = ifelse(single[rows], as.integer(d[[col_single]][rows]), lo[rows]),
             age_hi = ifelse(single[rows], as.integer(d[[col_single]][rows]), hi[rows]))
}

supp_data_year <- function(d) as.integer(sub(".*December\\s*([0-9]{4}).*", "\\1", d[[1]][2]))

#' Table 5.A1.1: retired workers in current pay by single age and sex,
#' December of the edition's data year. Single ages 62 to the last single age

#' their lower bound with open = TRUE.
read_supp_5a11 <- function(path) {
  d <- read_text_sheet(path, "5.A1.1")
  yr <- supp_data_year(d)
  r <- parse_age_rows(d, 2, 1)
  out <- rbind(data.frame(year = yr, age = r$age, age_hi = r$age_hi, sex = "M", number = as.numeric(d[[6]][r$row])),
               data.frame(year = yr, age = r$age, age_hi = r$age_hi, sex = "F", number = as.numeric(d[[8]][r$row])))
  tot <- d[!is.na(d[[3]]) & d[[3]] == "Total", ][1, ]
  stopifnot(abs(sum(out$number[out$sex == "M"]) - as.numeric(tot[[6]])) < 2,
            abs(sum(out$number[out$sex == "F"]) - as.numeric(tot[[8]])) < 2)
  out$sex <- factor(out$sex, levels = c("M", "F"))
  out
}

#' Table 5.A1.6: nondisabled widow(er) beneficiaries by single age (60+) and
#' sex of the beneficiary, December of the data year ("By age" block only).
read_supp_5a16 <- function(path) {
  d <- read_text_sheet(path, "5.A1.6")
  yr <- supp_data_year(d)
  i_end <- which(!is.na(d[[1]]) & grepl("^By marital", d[[1]]))
  if (!length(i_end)) i_end <- nrow(d) + 1
  blk <- d[seq_len(i_end[1] - 1), ]
  r <- parse_age_rows(blk, 3, 2)
  out <- rbind(data.frame(year = yr, age = r$age, age_hi = r$age_hi, sex = "M", number = suppressWarnings(as.numeric(blk[[7]][r$row]))),
               data.frame(year = yr, age = r$age, age_hi = r$age_hi, sex = "F", number = suppressWarnings(as.numeric(blk[[9]][r$row]))))
  tot <- d[!is.na(d[[4]]) & d[[4]] == "Total", ][1, ]
  # Suppressed cells "(X)": split the group's both-sexes count by each sex's
  # residual against its published total.
  miss <- is.na(out$number)
  if (any(miss)) {
    all_n <- suppressWarnings(as.numeric(blk[[5]][r$row]))
    gaps <- c(M = as.numeric(tot[[7]]) - sum(out$number[out$sex == "M"], na.rm = TRUE),
              F = as.numeric(tot[[9]]) - sum(out$number[out$sex == "F"], na.rm = TRUE))
    k <- which(is.na(out$number[out$sex == "M"]))
    out$number[out$sex == "M"][k] <- all_n[k] * gaps["M"] / sum(gaps)
    out$number[out$sex == "F"][k] <- all_n[k] * gaps["F"] / sum(gaps)
  }
  stopifnot(abs(sum(out$number[out$sex == "M"]) - as.numeric(tot[[7]])) < 2,
            abs(sum(out$number[out$sex == "F"]) - as.numeric(tot[[9]])) < 2)
  out$sex <- factor(out$sex, levels = c("M", "F"))
  out
}

oasi_vintage_files <- function() {
  c(sprintf("data-raw/supplement/5a_vintages/5a_%d.xlsx", 2016:2024),
    "data-raw/supplement/supplement25_all.xlsx", supp26("5a.xlsx"))
}
