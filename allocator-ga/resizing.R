fgga_sizes_crossover <- function(population, sizes_base,
                                 n_opt = length(sizes_base), n_pop = ncol(population$pop)/ncol(population$sizes)){
  sizes_unique <- unique(sizes_base)
  sizes_length <- length(sizes_unique)

  sizes_options <- matrix(nrow = n_opt*sizes_length, ncol = 3) %>%
    `colnames<-`(c("obs", "option", "obj"))
  
  for(opt in 1:n_opt){
    for(i in 1:sizes_length){
      index <- vapply(n_pop * (which(population$sizes[opt,] == sizes_unique[i]) - 1),
                      function(x) 1:n_pop + x, double(n_pop))
      sizes_options[(opt-1)*3 + i,] <- c(opt, sizes_unique[i], remean(population$obj[index]))
    }
  }

  select_recursion(sizes_options, k = n_opt, sizes = sizes_base)
}


fgga_sizes_mutation <- function(sizes_crossover, n_sizes,
                                prob = "inverse", prob_args = NULL,
                                n_opt = length(sizes_chosen)){
  if(isFALSE(prob)){
    return(as.matrix(sizes_crossover))
  } else if(prob == "inverse"){
    if(is.null(prob_args)) prob_args <- list(a = 0.3, b = 3)
    prob <- c(1, (function(x) prob_args$a/(x-1)^prob_args$b)(3:n_opt))
  }
  
  sizes_chosen <- matrix(nrow = n_opt, ncol = n_sizes)
  sizes_chosen[,1] <- sizes_crossover
  
  for(s in 2:n_sizes){
    index <- sample(1:n_opt, sample(2:n_opt, 1, prob = prob))
    
    sizes_chosen[,s] <- sizes_crossover
    sizes_chosen[index,s] <- sample(sizes_crossover[index])
  }
  
  return(sizes_chosen)
}


