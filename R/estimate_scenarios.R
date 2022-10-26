# devtools::install_github("icra/ediblecity")

library(ediblecity)
library(tidyverse)
library(stars)
library(parallel)

set.seed(1121)


#define the function to estimate indicators for each scenario
estimate_scenario <- function(pVacant){

  scenario <- suppressWarnings(set_scenario(
    city_example,
    pGardens = 0,
    pVacant = pVacant,
    vacant_from = "Streets",
    pRooftop = 0,
    pCommercial = 0,
    area = "flat_area"))

  results <- list()

  results$UHI <- ediblecity::UHI(scenario, SVF = SVF, verbose = F)
  results$runoff <- ediblecity::runoff_prev(scenario)
  results$green_distance <- ediblecity::green_distance(scenario, percent_out = F)
  results$no2_seq <- ediblecity::no2_seq(scenario)
  results$volunteers <- ediblecity::edible_volunteers(scenario, verbose=T)
  results$green_cap <- ediblecity::green_capita(scenario,
                                                private = T,
                                                neighbourhoods = neighbourhoods,
                                                name_col = "name",
                                                inh_col = "inhabitants",
                                                verbose = T)
  results$jobs <- ediblecity::edible_jobs(scenario, verbose = T)
  results$food <- ediblecity::food_production(scenario, verbose = T)


  return(results)

}

#define a function to return confidence intervals
interval_ <- function(x){
  quantile(x, c(0.05, 0.5, 0.95), names=F)
}

#define scenarios
scenarios <- tibble(
  names = c("Current", "Future"),
  pVacant = c(0, 100),
)

n_sim <- 100


start_time <- Sys.time()

#iterate scenarios
for (i in 1:nrow(scenarios)){

  #set parallelizing
  cl <- makeCluster(detectCores()-1)
  clusterEvalQ(cl,library(ediblecity))
  clusterSetRNGStream(cl)
  clusterExport(cl, c("estimate_scenario", "scenarios", "n_sim", "i"))
  #n simulations of one scenario n_sim times
  results <- parSapply(cl, 1:n_sim, function(u) estimate_scenario(
    scenarios$pVacant[i]
    )
  )
  stopCluster(cl)
  #handling results
  # saveRDS(results, paste0("scenarios/",scenarios$name[i],"_results.RDS"))

  # results_df <- tibble(.rows = 3)
  #
  # #estimate confidence intervals for each indicator
  # results_df$UHI <- interval_(unlist(lapply(results[1,], function(x) x[[3]])))
  # results_df$runoff <- interval_(unlist(lapply(results[2,], function(x) x[[1]])))
  # results_df$green_d <- interval_(unlist(results[3,]))
  # results_df$no2_seq <- interval_(unlist(results[4,]))
  # results_df$volunteers <- interval_(unlist(results[5,]))
  # results_df$green_cap <- interval_(unlist(results[6,]))
  # results_df$jobs <- interval_(unlist(results[7,]))
  # results_df$food <- interval_(unlist(results[8,]))

  assign(scenarios$names[i], results)

}

stop_time <- Sys.time()
stop_time - start_time

all_dfs <- list("Current" = Current,
                "Future" = Future)
saveRDS(all_dfs, "data/all_dfs.RDS")
