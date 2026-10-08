# Helpers for reading Trustees Report tables from the OCACT Excel workbooks.

#' Turn TR table text into numbers.
#'
#' TR cells carry footnote letters ("g176,100", "e$1,085"), dollar signs,
#' commas and thin spaces. Strip those and convert; anything left that isn't a
#' number (e.g. "d" for "no provision in law") becomes NA.
tr_number <- function(x) {
  x <- gsub("[^0-9.\\-]", "", x)
  suppressWarnings(as.numeric(x))
}

#' Read one table from a sheet of the full TR workbook (TRTables_TR2026.xlsx).
#'
#' Each sheet of that workbook holds several tables stacked vertically, e.g.
#' sheet "V_C" holds V.C1 through V.C7. This finds the rows belonging to one
#' table, carries the block headers ("Historical data:", "Intermediate:", ...)
#' down, and keeps historical + intermediate rows.
#'
#' @param sheet     Sheet name, e.g. "V_C".
#' @param table_id  Table number, e.g. "V.C1".
#' @param year_col  Column (1-based) holding the calendar year.
#' @param cols      Named integer vector: output name = column number.
#' @param path      Workbook path.
#' @return Data frame with `year`, `section`, and the named columns as numbers.
read_tr_block <- function(sheet, table_id, year_col, cols,
                          path = "data-raw/tr2026/TRTables_TR2026.xlsx") {
  x <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                          col_types = "text", .name_repair = "minimal")
  starts <- grep("^Table [IVX]+\\.[A-Z][0-9]+", x[[1]])
  first <- grep(paste0("^Table ", gsub(".", "\\.", table_id, fixed = TRUE), "\\b"), x[[1]])[1]
  stopifnot(!is.na(first))
  last <- c(starts[starts > first], nrow(x) + 1)[1] - 1
  x <- x[first:last, ]

  label <- x[[1]]
  header <- ifelse(!is.na(label) & grepl(":\\s*$", label), sub(":\\s*$", "", label), NA)
  section <- header
  for (i in seq_along(section)[-1]) if (is.na(section[i])) section[i] <- section[i - 1]

  year <- suppressWarnings(as.integer(sub("[a-z]+$", "", x[[year_col]])))
  keep <- !is.na(year) & year > 1900 & section %in% c("Historical data", "Intermediate")

  out <- data.frame(year = year[keep], section = section[keep])
  for (nm in names(cols)) out[[nm]] <- tr_number(x[[cols[[nm]]]][keep])
  stopifnot(!anyDuplicated(out$year))
  out
}

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
  for (cn in cols) out[[cn]] <- tr_number(x[[cn]][keep])
  stopifnot(!anyDuplicated(out$year))
  out
}
