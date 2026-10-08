# Helpers for reading tables out of OCACT actuarial studies (PDFs in
# docs/reference/). Uses pdftotext (poppler) in layout mode, so each table row
# comes out as one line of text.

pdf_lines <- function(path) {
  stopifnot(nzchar(Sys.which("pdftotext")))
  out <- system2("pdftotext", c("-layout", shQuote(path), "-"), stdout = TRUE)
  out
}

#' Employment-to-population ratios by age group, Actuarial Study No. 127
#' (2022 Trustees Report, intermediate). Table 1 is men, Table 2 women; both
#' run 1981-2021 (historical) and 2022 onward (projected).
#'
#' @return Data frame: year, sex, group (16_17, 18_19, 20_24, ...), ratio
#'   (percent of the civilian noninstitutional population employed).
read_as127_employment <- function(path = "docs/reference/AS127_employment_projections_2022TR.pdf") {
  lines <- pdf_lines(path)
  groups <- c("16_17", "18_19", "20_24", "25_29", "30_34", "35_39", "40_44",
              "45_49", "50_54", "55_59", "60_64", "65_69", "70plus")
  start_m <- grep("Table 1\\. Ratio of Employment", lines)[1]
  start_f <- grep("Table 2\\. Ratio of Employment", lines)[1]
  end_f   <- grep("Table 3\\. Ratio of Employment", lines)[1]

  parse_block <- function(from, to, sex) {
    block <- lines[from:(to - 1)]
    rows <- grep("^\\s*(19|20)[0-9]{2}\\s+[0-9]", block, value = TRUE)
    rows <- strsplit(trimws(rows), "\\s+")
    rows <- rows[lengths(rows) >= 1 + length(groups)]
    d <- do.call(rbind, lapply(rows, function(r) {
      vals <- suppressWarnings(as.numeric(r[2:(1 + length(groups))]))
      data.frame(year = as.integer(r[1]), group = groups, ratio = vals)
    }))
    d$sex <- sex
    d[!duplicated(d[c("year", "group")]), ]
  }
  out <- rbind(parse_block(start_m, start_f, "M"), parse_block(start_f, end_f, "F"))
  out$sex <- factor(out$sex, levels = c("M", "F"))
  out
}
