fgga_populate <- function(sizes_base, preferences, n_sizes, n_pop, suggestions = matrix(NA, n_opt, 0),
                          n_opt = ncol(preferences), n_obs = nrow(preferences)){
  
  population <- list(sizes = cbind(suggestions, replicate(n_sizes - ncol(suggestions), sample(sizes_base))),
                     pop = matrix(nrow = n_obs, ncol = n_pop*n_sizes),
                     obj = double(n_pop*n_sizes))
  
  for(s in 1:n_sizes){
    for(i in 1:n_pop){
      population$pop[,i + (s-1)*n_pop] <- sample(rep(1:n_opt, times = population$sizes[,s]))
      population$obj[i + (s-1)*n_pop] <- obj(population$pop[,i + (s-1)*n_pop], preferences, n_obs)
    }
  }
  
  return(population)
}


fgga_pop_crossover <- function(population, sizes_chosen, preferences, n_pop, n_sizes = ncol(sizes_chosen),
                               n_opt = ncol(preferences), n_obs = nrow(preferences),
                               elitism = 0.25*n_pop, n_parents = 2, max_parents = n_parents){
  children <- list(sizes = sizes_chosen,
                   pop = matrix(nrow = n_obs, ncol = n_pop*n_sizes),
                   obj = double(n_pop*n_sizes))
  
  for(s in 1:n_sizes){
    index_size <- ((s-1)*n_pop + 1):(s*n_pop)
    index_elitism <- which(population$obj[index_size] <= sort(population$obj[index_size], decreasing = TRUE)[elitism])
    parents <- list(pop = population$pop[,index_size][,index_elitism], obj = population$obj[index_size][index_elitism])
    for(i in 1:n_pop){
      index_chosen <- sample(1:ncol(parents$pop), n_parents)
      parents_options <- cbind(obs = rep(1:n_obs, n_parents),
                               option = as.double(parents$pop[,index_chosen]),
                               obj = parents$obj[index_chosen])
      
      child <- select_recursion(parents_options, k = n_obs, max = max_parents,
                                sizes = rep(1:n_opt, times = sizes_chosen[,s]))
      
      child[is.na(child)] <- resample(rep(1:n_opt, times = sizes_chosen[,s] - tabulate_all(child, n_opt)))
      children$pop[,i + (s-1)*n_pop] <- child
      children$obj[i + (s-1)*n_pop] <- obj(child, preferences, n_obs)
    }
  }
  
  return(children)
}
