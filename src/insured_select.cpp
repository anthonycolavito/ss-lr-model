// OCACT's selection of non-covered workers (methodology section 3.1.c,
// "Simulation process - assigning QC to records").
//
// For one cohort, sex and year, choose which records are NOT covered workers
// this year. The number to choose is set by the covered-worker rate. Choices
// favor records that have gone several consecutive years without a quarter of
// coverage, which is what makes gaps in work cluster in time:
//
//   1. Pick a random starting record.
//   2. Walk forward from it (wrapping around) through records not yet chosen
//      this year, until one has at least SLCT consecutive prior years with no
//      QCs. Choose it.
//   3. If SRCH records are examined without a match, choose the examined
//      record closest to the criterion (the most consecutive zero years;
//      the first one found if tied).
//   4. Repeat until the target number is chosen.
//
// zero_run[i] is record i's count of consecutive prior years with no QCs.
// Returns a logical vector: TRUE = not a covered worker this year.

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector select_noncovered(IntegerVector zero_run, int n_target, int slct, int srch) {
  int n = zero_run.size();
  LogicalVector chosen(n, false);
  if (n_target <= 0) return chosen;
  if (n_target >= n) { std::fill(chosen.begin(), chosen.end(), true); return chosen; }
  if (srch < 1) srch = 1;

  int n_chosen = 0;
  while (n_chosen < n_target) {
    int i = (int) std::floor(R::runif(0.0, 1.0) * n);
    if (i >= n) i = n - 1;
    int examined = 0, best = -1, best_run = -1;
    int pick = -1;
    for (int step = 0; step < n; ++step) {
      int k = (i + step) % n;
      if (chosen[k]) continue;
      ++examined;
      if (zero_run[k] >= slct) { pick = k; break; }
      if (zero_run[k] > best_run) { best_run = zero_run[k]; best = k; }
      if (examined >= srch) { pick = best; break; }
    }
    if (pick < 0) pick = best;          // fewer unchosen records than SRCH
    chosen[pick] = true;
    ++n_chosen;
  }
  return chosen;
}
