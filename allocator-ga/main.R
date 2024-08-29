# Packages and functions --------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("repopulating.R")
source("resizing.R")
source("utils.R")

`%>%` <- magrittr::`%>%`


# fgga --------------------------------------------------------------------
fgga <- function(preferences, n_pop, n_sizes, obj, runs_total, runs_pop,
                 elitism = 0.25*n_pop, n_parents = 2, max_parents = n_parents,
                 prob = "inverse", prob.args = NULL){
  n_obs <- nrow(preferences)
  n_opt <- ncol(preferences)

  sizes_base <- deal_sizes_base(n_obs, n_opt)
  
  population <- fgga_populate(sizes_base, preferences, n_sizes = n_sizes, n_pop = n_pop,
                              n_opt = n_opt, n_obs = n_obs)
  sizes_mutated <- population$sizes
  
  cat("== Initial sizes ==\n")
  write.table(format(rbind(1:n_opt, sizes_mutated)),
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  cat("==\n")
  
  for(i in 1:runs_total){
    for(j in 1:runs_pop){
      population <- fgga_pop_crossover(population, sizes_mutated, preferences,
                                       n_pop = n_pop, n_opt = n_opt, n_obs = n_obs, #n_sizes = n_sizes
                                       elitism = elitism, n_parents = n_parents, max_parents = max_parents)
    }
    sizes_crossoved <- fgga_sizes_crossover(population, sizes_base, n_opt = n_opt, n_pop = n_pop)
    sizes_mutated <- fgga_sizes_mutation(sizes_crossoved, prob, prob.args, n_sizes = n_sizes, n_opt = n_opt)
    
    progress_bar(i, population$obj, sizes_crossoved, n_opt)
  }
  
  progress_bar(i, population$obj, sizes_crossoved, n_opt)
  return(population$pop[,which.min(population$obj)] )
}


# user --------------------------------------------------------------------
set.seed(1)

n_opt <- 14; n_obs = 61; preferences <- t(replicate(n_obs, sample(1:n_opt, n_opt)))
obj <- function(allocation, preferences, n_obs = nrow(preferences)){
  sum(preferences[n_obs*(allocation-1) + 1:n_obs])
}

result <- fgga(preferences, n_pop <- 40, n_sizes = 15, obj = obj,
               runs_total = 3, runs_pop = 15, prob = FALSE,
               elitism = 0.25*n_pop, n_parents = 10, max_parents = 3)
