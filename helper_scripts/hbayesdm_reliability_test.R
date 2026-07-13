# Fits every hBayesDM task model (using the package's bundled data = "example")
# with a small number of iterations/chains, then checks that
# gbtoolbox::reliability() runs on the resulting fit without erroring.
#
# This is a structural smoke test (does reliability.hBayesDM() work against
# every model's $par_vals/$all_ind_pars shape?), not a check of inference
# quality - niter/nwarmup are deliberately tiny. Run manually/interactively;
# expect this to take a while, since each model is compiled via cmdstanr on
# first use.
#
# Note: the "_single" models (e.g. dd_hyperbolic_single) fit only one subject
# and are not hierarchical, so par_vals has no subject dimension for them.
# reliability() is expected to find zero individual-level parameters (or
# error) for these - that's correct behaviour, not a bug.

devtools::load_all(".")
library(hBayesDM)

model_names = c(
  "alt_delta", "alt_gamma",
  "bandit2arm_delta",
  "bandit4arm2_kalman_filter", "bandit4arm_2par_lapse", "bandit4arm_4par",
  "bandit4arm_lapse", "bandit4arm_lapse_decay", "bandit4arm_singleA_lapse",
  "banditNarm_2par_lapse", "banditNarm_4par", "banditNarm_delta",
  "banditNarm_kalman_filter", "banditNarm_lapse", "banditNarm_lapse_decay",
  "banditNarm_singleA_lapse",
  "bart_ewmv", "bart_par4",
  "cgt_cm",
  "choiceRT_ddm", "choiceRT_ddm_single", "choiceRT_lba", "choiceRT_lba_single",
  "cra_exp", "cra_linear",
  "dbdm_prob_weight",
  "dd_cs", "dd_cs_single", "dd_exp", "dd_hyperbolic", "dd_hyperbolic_single",
  "gng_m1", "gng_m2", "gng_m3", "gng_m4",
  "hgf_ibrb", "hgf_ibrb_single",
  "igt_orl", "igt_pvl_decay", "igt_pvl_delta", "igt_vpp",
  "peer_ocu",
  "prl_ewa", "prl_fictitious", "prl_fictitious_multipleB", "prl_fictitious_rp",
  "prl_fictitious_rp_woa", "prl_fictitious_woa", "prl_rp", "prl_rp_multipleB",
  "pstRT_ddm", "pstRT_rlddm1", "pstRT_rlddm6",
  "pst_Q", "pst_gainloss_Q",
  "ra_noLA", "ra_noRA", "ra_prospect",
  "rdt_happiness",
  "task2AFC_sdt",
  "ts_par4", "ts_par6", "ts_par7",
  "ug_bayes", "ug_delta",
  "wcs_sql"
)

ncore = 2

results = data.frame(
  model     = model_names,
  fit_ok    = NA,
  fit_error = NA_character_,
  rel_ok    = NA,
  rel_error = NA_character_,
  n_params  = NA_integer_,
  stringsAsFactors = FALSE
)

for (i in 25:length(model_names)) {

  model_name = model_names[i]
  cat(sprintf("\n[%d/%d] %s\n", i, length(model_names), model_name))

  model_fun = tryCatch(getExportedValue("hBayesDM", model_name), error = function(e) NULL)
  if (is.null(model_fun)) {
    results$fit_error[i] = "function not found in hBayesDM namespace"
    next
  }

  fit = tryCatch(
    model_fun(
      data    = "example",
      niter   = 200,
      nwarmup = 100,
      nchain  = 2,
      ncore   = ncore,
      inits   = "random"
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    results$fit_error[i] = conditionMessage(fit)
    next
  }
  results$fit_ok[i] = TRUE

  rel = tryCatch(reliability(fit), error = function(e) e)

  if (inherits(rel, "error")) {
    results$rel_error[i] = conditionMessage(rel)
    next
  }

  results$rel_ok[i]   = TRUE
  results$n_params[i] = length(rel)
}

# Summary ---------------------------------------------------------------

cat("\n\n==== Summary ====\n")
print(results)

cat(sprintf(
  "\nFit succeeded:            %d / %d\nreliability() succeeded:  %d / %d\n",
  sum(results$fit_ok, na.rm = TRUE), nrow(results),
  sum(results$rel_ok, na.rm = TRUE), nrow(results)
))

failures = results[is.na(results$rel_ok), ]
if (nrow(failures) > 0) {
  cat("\nModels where reliability() did not succeed:\n")
  print(failures[, c("model", "fit_error", "rel_error")])
}

# 
# > failures = results[is.na(results$rel_ok), ]
# > if (nrow(failures) > 0) {
#   +   cat("\nModels where reliability() did not succeed:\n")
#   +   print(failures[, c("model", "fit_error", "rel_error")])
#   + }
# 
# Models where reliability() did not succeed:
#   model                                fit_error                          rel_error
# 21  choiceRT_ddm_single                                     <NA>               argument of length 0
# 22         choiceRT_lba function not found in hBayesDM namespace                               <NA>
#   23  choiceRT_lba_single function not found in hBayesDM namespace                               <NA>
#   28         dd_cs_single                                     <NA>               argument of length 0
# 31 dd_hyperbolic_single                                     <NA>               argument of length 0
# 36             hgf_ibrb                                     <NA> dim(X) must have a positive length
# 37      hgf_ibrb_single                                     <NA>               argument of length 0

# Most of the models above are for single individuals (_single) and can't be used to estimate reliability

# Need to investigate why reliability doesn't work for hgf_ibrb