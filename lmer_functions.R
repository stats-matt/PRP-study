# this code was copied from https://joshua-nugent.github.io/allFit/ - it selects an optimizer in lmer when convergence becomes an issue

library(parallel)


# control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# control = glmerControl(optimizer = "optimx", optCtrl = list(method = 'L-BFGS-B'))
# control = glmerControl(optimizer = "optimx", optCtrl = list(method = 'nlminb'))

lmer_best <- function(model) {
  diff_optims <-
    allFit(
      model,
      maxfun = 1e5,
      parallel = 'multicore',
      ncpus = detectCores()
    )
  is.OK <- sapply(diff_optims, is, "merMod")
  diff_optims.OK <- diff_optims[is.OK]
  lapply(diff_optims.OK, function(x)
    x@optinfo$conv$lme4$messages)
  
  convergence_results <-
    lapply(diff_optims.OK, function(x)
      x@optinfo$conv$lme4$messages)
  working_indices <- sapply(convergence_results, is.null)
  
  if (sum(working_indices) == 0) {
    print("No algorithms from allFit converged.")
    print("You may still be able to use the results, but proceed with extreme caution.")
    first_fit <- NULL
  } else {
    first_fit <- diff_optims[working_indices][[1]]
  }
  first_fit
}