# Helpers for reading Annual Statistical Supplement tables.

#' Read a Supplement table laid out as years down, age groups across.
#'
#' Tables 4.B5, 4.B6 and 4.C2 share a layout: column 1 is the year, column 3
#' is the all-ages total, and columns 4 onward are age groups. Rows are split
#' into blocks by a label in column 3 ("All workers", "Men", "Women"; or for
#' 4.C2 "Fully insured ...", "Total", "Male", "Female", ...). This returns one
#' row per block x year x age group.
#'
#' @param sheet   Sheet name in the workbook, e.g. "4.B5".
#' @param groups  Names for the age-group columns, left to right.
#' @param top     Regular expression for top-level headings (e.g. "^(Fully|Insured)"
#'   in 4.C2). A top-level heading starts a new block stack; any other heading
#'   is a sub-heading beneath the current top-level one.
#' @param path    Workbook path.
#' @return Data frame: block (top heading / sub-heading, or just the heading),
#'   year, group, value. Suppressed or empty cells are NA.
read_supp_year_by_age <- function(sheet, groups, top = NULL,
                                  path = "data-raw/supplement/supplement25_all.xlsx") {
  x <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                          col_types = "text", .name_repair = "minimal")
  year <- suppressWarnings(as.integer(sub("\\s.*$", "", x[[1]])))
  label <- x[[3]]
  is_label <- is.na(year) & !is.na(label) & is.na(suppressWarnings(as.numeric(label)))

  # Headings: with `top` given, a top-level heading resets the stack and any
  # other heading sits beneath it; without `top`, each heading stands alone.
  top_heading <- NULL; sub_heading <- NULL; block <- rep(NA_character_, nrow(x))
  for (i in seq_len(nrow(x))) {
    if (is_label[i]) {
      lab <- trimws(gsub("[[:space:]\u00a0]+", " ", label[i]))
      if (grepl("^Total, all", lab)) next
      if (!is.null(top) && grepl(top, lab)) {
        top_heading <- sub("\\s+[a-z]$", "", lab); sub_heading <- NULL
      } else {
        sub_heading <- lab
      }
    } else if (!is.na(year[i])) {
      block[i] <- paste(c(top_heading, sub_heading), collapse = " / ")
    }
  }

  keep <- !is.na(year) & !is.na(block)
  cols <- 3 + seq_along(groups)
  vals <- as.data.frame(lapply(x[keep, cols], function(z) suppressWarnings(as.numeric(gsub("[^0-9.]", "", z)))))
  names(vals) <- groups
  out <- cbind(data.frame(block = block[keep], year = year[keep]), vals)
  out <- tidyr::pivot_longer(out, -c(block, year), names_to = "group", values_to = "value")
  out$group <- factor(out$group, levels = groups)
  out
}
