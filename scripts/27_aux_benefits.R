# 27_aux_benefits.R
#
# Average benefits of auxiliary and survivor beneficiaries, December 2025-2100 (methodology 4.3,
# "Annualizing Benefits": avgben = linkage x the relevant average PIA or MBA of the account holder;
# DECISIONS.md AX-01 to AX-03).
#
# OCACT's linkages (qlink26.xlsx) aren't published. Here each linkage is the December 2025 average
# benefit of the category (Supplement 5.A1, 5.A1.3, 5.A1.5-5.A1.8) over the December 2025 average of
# the reference amount, held constant:
#   OASI dependents and survivors      average PIA of retired workers of the account holder's sex
#                                      (scripts/26; wives, widows, mothers: men's; husbands, widowers,
#                                      fathers: women's; children and parents: men's, AX-02)
#   DI dependents                      average benefit of disabled workers (scripts/25), spouses by the
#                                      account holder's sex, children both sexes
# The amounts are benefits paid to the category: dually entitled spouses and widow(er)s are retired
# workers in these tables, and their excess is projected separately (scripts/28).
#
# Input:  data/rw_benefits.rds (26), di_benefits.rds (25), oasi_auxiliaries.rds (15), aged_widows.rds (12),
#         di_auxiliaries.rds (11); Supplement 2026 5.A1, 5.A1.3, 5.A1.5, 5.A1.6, 5.A1.7
# Output: data/aux_benefits.rds

suppressMessages({library(dplyr); library(tidyr)})
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
f5a <- "data-raw/supplement/2026/5a.xlsx"
rd <- function(s) { m <- as.matrix(readxl::read_excel(f5a, sheet = s, col_names = FALSE, col_types = "text", .name_repair = "minimal")); m[is.na(m)] <- ""; m }
lab <- function(m) trimws(gsub("\\s+", " ", apply(m[, 1:4], 1, paste, collapse = " ")))

# ---- December 2025 averages by category ------------------------------------------------------------------------
a1 <- rd("5.A1"); l1 <- lab(a1)
row1 <- function(p, k = 1) which(grepl(p, l1))[k]
v1 <- function(i) c(n = num(a1[i, 6]), all = num(a1[i, 7]))
pub <- list()
# children: 5.A1 rows under retired workers (1st), deceased (2nd), disabled (3rd)
for (nm in c("Under age 18", "Students aged", "Disabled adult")) {
  key <- c("Under age 18" = "minor", "Students aged" = "student", "Disabled adult" = "dac")[[nm]]
  ii <- which(grepl(nm, l1))      # first is the all-children total
  pub[[paste0("rw_", key)]] <- v1(ii[2])[["all"]]; pub[[paste0("dw_", key)]] <- v1(ii[3])[["all"]]; pub[[paste0("di_", key)]] <- v1(ii[4])[["all"]]
}
pub$parent <- v1(row1("Parents of deceased"))[["all"]]
# spouses (5.A1.3): blocks all / retired-worker spouses / disabled-worker spouses
a13 <- rd("5.A1.3"); l13 <- lab(a13)
care <- which(grepl("Care of children", l13)); nond <- which(grepl("Nondivorced", l13)); divd <- which(grepl("^Divorced", l13))
sp <- function(k, col_n, col_m) {
  nc <- num(a13[care[k], col_n]); mc <- num(a13[care[k], col_m]); nn <- num(a13[nond[k], col_n]); mn <- num(a13[nond[k], col_m])
  c(young = mc, aged_mar = (nn * mn - nc * mc) / (nn - nc), aged_div = num(a13[divd[k], col_m]))
}
w_rw <- sp(2, 8, 9); h_rw <- sp(2, 10, 11); w_di <- sp(3, 8, 9); h_di <- sp(3, 10, 11)
pub$young_F <- w_rw[["young"]]; pub$young_M <- h_rw[["young"]]; pub$aged_mar_F <- w_rw[["aged_mar"]]; pub$aged_mar_M <- h_rw[["aged_mar"]]
pub$aged_div_F <- w_rw[["aged_div"]]; pub$aged_div_M <- h_rw[["aged_div"]]
pub$di_young_F <- w_di[["young"]]; pub$di_young_M <- h_di[["young"]]; pub$di_aged_F <- w_di[["aged_mar"]]; pub$di_aged_M <- h_di[["aged_mar"]]
pub$di_div_F <- w_di[["aged_div"]]; pub$di_div_M <- h_di[["aged_div"]]
# widowed mothers and fathers (5.A1.5), nondisabled (5.A1.6) and disabled (5.A1.7) widow(er)s: totals and marital status
wtab <- function(s) { m <- rd(s); l <- lab(m); t <- which(grepl("Total", apply(m[, 1:4], 1, paste, collapse = " ")))[1]
  nd <- which(grepl("Nondivorced", l))[1]; dv <- which(grepl("^Divorced", l))[1]
  list(M = num(m[t, 8]), F = num(m[t, 10]), M_wid = num(m[nd, 8]), F_wid = num(m[nd, 10]), M_div = num(m[dv, 8]), F_div = num(m[dv, 10])) }
mo <- wtab("5.A1.5"); wd <- wtab("5.A1.6"); dw <- wtab("5.A1.7")
pub$mother_F <- mo$F; pub$mother_M <- mo$M
pub$widow_wid_F <- wd$F_wid; pub$widow_div_F <- wd$F_div; pub$widow_wid_M <- wd$M_wid; pub$widow_div_M <- wd$M_div
pub$dis_widow_F <- dw$F; pub$dis_widow_M <- dw$M

# ---- Reference amounts and linkages ----------------------------------------------------------------------------
rwt <- readRDS("data/rw_benefits.rds")$totals |> select(year, sex, pia)
dit <- readRDS("data/di_benefits.rds")$totals |> select(year, sex, mba, cp)
ref <- bind_rows(rwt |> transmute(year, ref = paste0("rw_pia_", sex), v = pia),
                 dit |> transmute(year, ref = paste0("di_", sex), v = mba),
                 dit |> group_by(year) |> summarise(v = sum(cp * mba) / sum(cp)) |> mutate(ref = "di_all"))
cats <- tribble(
  ~cat,          ~fund, ~ref,        ~src,
  "rw_minor",    "OASI", "rw_pia_M", "rw_minor",   "rw_student", "OASI", "rw_pia_M", "rw_student", "rw_dac", "OASI", "rw_pia_M", "rw_dac",
  "dw_minor",    "OASI", "rw_pia_M", "dw_minor",   "dw_student", "OASI", "rw_pia_M", "dw_student", "dw_dac", "OASI", "rw_pia_M", "dw_dac",
  "aged_mar_F",  "OASI", "rw_pia_M", "aged_mar_F", "aged_mar_M", "OASI", "rw_pia_F", "aged_mar_M",
  "aged_div_F",  "OASI", "rw_pia_M", "aged_div_F", "aged_div_M", "OASI", "rw_pia_F", "aged_div_M",
  "young_F",     "OASI", "rw_pia_M", "young_F",    "young_M",    "OASI", "rw_pia_F", "young_M",
  "mother_F",    "OASI", "rw_pia_M", "mother_F",   "mother_M",   "OASI", "rw_pia_F", "mother_M",
  "parent",      "OASI", "rw_pia_M", "parent",
  "widow_wid_F", "OASI", "rw_pia_M", "widow_wid_F", "widow_wid_M", "OASI", "rw_pia_F", "widow_wid_M",
  "widow_div_F", "OASI", "rw_pia_M", "widow_div_F", "widow_div_M", "OASI", "rw_pia_F", "widow_div_M",
  "dis_widow_F", "OASI", "rw_pia_M", "dis_widow_F", "dis_widow_M", "OASI", "rw_pia_F", "dis_widow_M",
  "di_minor",    "DI",   "di_all",   "di_minor",   "di_student", "DI",   "di_all",   "di_student", "di_dac", "DI", "di_all", "di_dac",
  "di_young_F",  "DI",   "di_M",     "di_young_F", "di_young_M", "DI",   "di_F",     "di_young_M",
  "di_aged_F",   "DI",   "di_M",     "di_aged_F",  "di_aged_M",  "DI",   "di_F",     "di_aged_M",
  "di_div_F",    "DI",   "di_M",     "di_div_F",   "di_div_M",   "DI",   "di_F",     "di_div_M")
cats$avg25 <- unlist(pub[cats$src])
cats <- cats |> left_join(ref |> filter(year == 2025) |> select(ref, ref25 = v), by = "ref") |> mutate(linkage = avg25 / ref25)
cat("Linkages (December 2025 average / reference):\n")
print(as.data.frame(cats |> transmute(cat, ref, avg25 = round(avg25, 2), ref25 = round(ref25, 2), linkage = round(linkage, 4))))

# ---- Counts by category and year -------------------------------------------------------------------------------
oa <- readRDS("data/oasi_auxiliaries.rds")$aux |> transmute(year, cat = category, n)
aw <- readRDS("data/aged_widows.rds")
wid <- aw$aged |> group_by(year, sex) |> summarise(wid = sum(wid), div = sum(div), .groups = "drop") |>
  pivot_longer(c(wid, div), names_to = "m", values_to = "n") |> transmute(year, cat = paste0("widow_", m, "_", sex), n)
dwid <- aw$disabled |> group_by(year, sex) |> summarise(n = sum(disabled), .groups = "drop") |> transmute(year, cat = paste0("dis_widow_", sex), n)
da <- readRDS("data/di_auxiliaries.rds")$aux |>
  mutate(cat = recode(category, minor = "di_minor", student = "di_student", dac = "di_dac", young_F = "di_young_F", young_M = "di_young_M",
                      aged_F = "di_aged_F", aged_M = "di_aged_M", div_F = "di_div_F", div_M = "di_div_M")) |> transmute(year, cat, n)
cnt <- bind_rows(oa, wid, dwid, da) |> filter(year >= 2025, year <= 2100)
miss <- setdiff(cats$cat, unique(cnt$cat)); if (length(miss)) stop("No counts for: ", paste(miss, collapse = ", "))

out <- cnt |> inner_join(cats |> select(cat, fund, ref, linkage), by = "cat") |> inner_join(ref |> rename(refv = v), by = c("year", "ref")) |>
  mutate(avgben = linkage * refv, monthly = n * avgben)
chk <- out |> filter(year == 2025) |> group_by(fund) |> summarise(model = sum(monthly) / 1e3)
cat("\nDecember 2025 monthly benefits of auxiliaries and survivors ($ thousands, by construction):\n"); print(chk)
cat("Supplement 5.A1 for comparison: OASI dependents and survivors",
    round((num(a1[row1("Spouses of retired"), 6]) * num(a1[row1("Spouses of retired"), 7]) + num(a1[row1("Children of retired"), 6]) * num(a1[row1("Children of retired"), 7]) +
           num(a1[row1("Survivor benefits"), 6]) * num(a1[row1("Survivor benefits"), 7])) / 1e3),
    "; DI dependents", round((num(a1[row1("Spouses of disabled"), 6]) * num(a1[row1("Spouses of disabled"), 7]) + num(a1[row1("Children of disabled"), 6]) * num(a1[row1("Children of disabled"), 7])) / 1e3), "\n")

tot <- out |> group_by(year, fund) |> summarise(n = sum(n), monthly = sum(monthly), .groups = "drop")
cat("\nAuxiliary and survivor beneficiaries (millions) and December monthly benefits ($ billions):\n")
print(as.data.frame(tot |> filter(year %in% c(2025, 2030, 2035, 2050, 2075, 2100)) |> mutate(n = round(n / 1e6, 2), monthly = round(monthly / 1e9, 2)) |>
  pivot_wider(names_from = fund, values_from = c(n, monthly))))

saveRDS(list(by_category = out, totals = tot, linkages = cats), "data/aux_benefits.rds")
cat("Saved data/aux_benefits.rds\n")
