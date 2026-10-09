# extract_bepuf_earnings.R
#
# Keeps the BEPUF 2020 earnings records of recent new worker beneficiaries:
# retired workers entitled 2016-2020 (one in four, chosen by ID) and all
# disabled workers entitled 2016-2020. Reads the earnings zip as a stream, so
# nothing is unzipped to disk. About 15-30 minutes; prints progress.
#
# Usage: put this file, BEPUF-2020-benefits.csv and BEPUF-2020-earnings-CSV.zip
# in one folder, set that folder as R's working directory, then
#   source("extract_bepuf_earnings.R")
# Output: bepuf_awardee_earnings.csv.gz (send this back).

if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)

zipfile <- "BEPUF-2020-earnings-CSV.zip"
stopifnot(file.exists(zipfile), file.exists("BEPUF-2020-benefits.csv"))

# 1. Who to keep
b <- fread("BEPUF-2020-benefits.csv", select = c(1, 2, 4, 5, 13), showProgress = FALSE)
setnames(b, c("ID", "BY", "BT", "IP", "ACE"))
b[, ent := BY + ACE]
keep <- b[BT == "A" & IP %in% c("R", "D") & ent %in% 2016:2020 & (IP == "D" | ID %% 4 == 0), ID]
cat("Records to keep:", length(keep), "\n")

# 2. Stream the earnings file and filter by ID
inner <- unzip(zipfile, list = TRUE)
print(inner)
csvname <- inner$Name[grepl("\\.csv$", inner$Name, ignore.case = TRUE)][1]
con <- unz(zipfile, csvname); open(con, "rt")
header <- readLines(con, n = 1)
header <- sub("^﻿", "", header)
cat("Columns:", header, "\n")
cols <- trimws(strsplit(header, ",")[[1]])
idcol <- if ("ID" %in% toupper(cols)) which(toupper(cols) == "ID")[1] else 1L
out <- gzfile("bepuf_awardee_earnings.csv.gz", "w")
writeLines(paste(cols, collapse = ","), out)
n_in <- 0; n_out <- 0; t0 <- Sys.time()
repeat {
  lines <- readLines(con, n = 1e6)
  if (!length(lines)) break
  n_in <- n_in + length(lines)
  d <- fread(text = lines, header = FALSE, colClasses = "character", showProgress = FALSE)
  d <- d[as.numeric(trimws(d[[idcol]])) %in% keep]
  if (nrow(d)) { writeLines(do.call(paste, c(d, sep = ",")), out); n_out <- n_out + nrow(d) }
  cat(format(Sys.time(), "%H:%M:%S"), " read ", format(n_in, big.mark = ","), " rows, kept ",
      format(n_out, big.mark = ","), "\n", sep = "")
}
close(con); close(out)
cat("Done in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "minutes. Send bepuf_awardee_earnings.csv.gz\n")
