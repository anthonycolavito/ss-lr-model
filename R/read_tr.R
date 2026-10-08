# Helpers for reading Trustees Report tables from the OCACT Excel workbooks.

#' Read a single-year TR table, keeping historical and intermediate rows only.
#'
#' The single-year workbook stacks each table in blocks headed "Historical
#' data:", "Intermediate:", "Low-cost:" and "High-cost:", so every projected
#' year appears three times. This returns the historical block plus the
#' intermediate (alternative II) block, one row per year.
#'
#' @param sheet  Sheet name, e.g. "V.A3".
#' @param cols   Names for the columns after the year column, in order.
#' @param path   Workbook path.
#' @return A data frame with `year` (integer), `section`, and the named columns
#'   as numbers. Footnote letters on years (e.g. "2025c") are dropped.
read_tr_single_year <- function(sheet, cols,
                                path = "data-raw/tr2026/SingleYearTRTables_TR2026.xlsx") {
  x <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                          col_types = "text", .name_repair = "minimal")
  x <- x[, seq_len(length(cols) + 1)]
  names(x) <- c("label", cols)

  # Carry each block's header ("Historical data:", "Intermediate:", ...) down
  # to the year rows beneath it.
  header <- ifelse(grepl(":\\s*$", x$label), sub(":\\s*$", "", x$label), NA)
  section <- header
  for (i in seq_along(section)[-1]) if (is.na(section[i])) section[i] <- section[i - 1]

  year <- suppressWarnings(as.integer(sub("[a-z]+$", "", x$label)))
  keep <- !is.na(year) & section %in% c("Historical data", "Intermediate")

  out <- data.frame(year = year[keep], section = section[keep])
  for (cn in cols) out[[cn]] <- as.numeric(x[[cn]][keep])
  stopifnot(!anyDuplicated(out$year))
  out
}
