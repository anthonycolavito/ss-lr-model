# run_all.R
#
# Rebuilds everything in data/ and outputs/ from data-raw/, in order. Each
# script runs in its own R process, so a script only sees what earlier scripts
# saved. Run from the repository root:
#
#   Rscript scripts/run_all.R            # everything (about 25 minutes on 2 cores)
#   Rscript scripts/run_all.R 11         # resume from step "11"
#
# Insured status (07) and the DI rolls (09, 10) depend on each other: 09 needs
# 07's disability-insured rates, and 07 adds back people on the DI rolls 4+
# years (DINADD) from 09 and 10. On a fresh build the first 07 runs without
# DINADD (bootstrap), then the loop runs once more (DECISIONS.md P-10).
#
# 06 (insured simulation) is the long step: about 21 minutes on 2 cores (3 to
# fit SRCH, the rest the full run). It uses every core it finds.

steps <- c(
  "01" = "01_import_population.R",
  "02" = "02_import_mortality.R",
  "03" = "03_program_parameters.R",
  "04" = "04_insured_inputs.R",
  "05" = "05_net_immigration.R",
  "06" = "06_insured_simulation.R",
  "08" = "08_di_inputs.R",
  "07a" = "07_insured_calibrate.R",   # bootstrap on a fresh build (no DI stock yet)
  "09a" = "09_di_start_stock.R",
  "10a" = "10_di_projection.R",
  "07" = "07_insured_calibrate.R",    # with DINADD from the projected rolls
  "09" = "09_di_start_stock.R",
  "10" = "10_di_projection.R",
  "11" = "11_di_auxiliaries.R",
  "12" = "12_aged_widows.R",
  "13" = "13_retired_workers.R",
  "14" = "14_rw_entitlement_age.R",
  "15" = "15_oasi_auxiliaries.R"
)

args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args)) args[1] else names(steps)[1]
if (!from %in% names(steps)) stop("Unknown step '", from, "'. Steps: ", paste(names(steps), collapse = ", "))
todo <- steps[match(from, names(steps)):length(steps)]

dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)
t_all <- Sys.time()
for (k in names(todo)) {
  log <- file.path("outputs/logs", paste0(k, "_", sub("\\.R$", ".log", todo[[k]])))
  t0 <- Sys.time()
  message(format(t0, "%H:%M:%S"), "  step ", k, ": ", todo[[k]], " (log: ", log, ")")
  status <- system2("Rscript", file.path("scripts", todo[[k]]), stdout = log, stderr = log)
  mins <- round(as.numeric(Sys.time() - t0, units = "mins"), 1)
  if (status != 0) {
    message("FAILED at step ", k, " after ", mins, " min. Last lines of the log:")
    message(paste(tail(readLines(log), 25), collapse = "\n"))
    quit(status = 1)
  }
  message("           done in ", mins, " min")
}
message("All steps done in ", round(as.numeric(Sys.time() - t_all, units = "mins"), 1), " min")
