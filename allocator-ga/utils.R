tabulate_all <- function(x, k){
  c(tabulate(x), rep(0, k - max(x, na.rm = TRUE)))
} 


resample <- function(x, ...){
  x[sample.int(length(x), ...)]
} 

remean <- function(x, sub = Inf, ...){
  if(length(x) == 0) sub else mean(x, ...)
}

deal_sizes_base <- function(n_obs, n_opt, init_size = 5){
  rest <- n_obs %% init_size
  
  rep(init_size, n_obs %/% init_size) %>%
    {if(rest == 4){
      c(., 4)
    } else {
      . + rep(1:0, times = c(rest, n_obs %/% init_size - rest))
    }} %>%
    c(rep(0, n_opt - length(.)))
}


select_recursion <- function(x, k, sizes, max = nrow(x)/k){
  check_groups <- function(x, ind, result, sizes){
    x[ind, "option"] %>% {sum(result == ., na.rm = TRUE) < sum(sizes == .)}
  }
  
  recursion <- function(i, x, index, result, sizes){
    if(check_groups(x, index[i], result, sizes)){
      x[index[i], "option"]
    } else if(i+1 <= max){
      recursion(i+1, x, index, result, sizes)
    } else {
      NA
    }
  }
  
  x <- x[order(x[,"obj"]),] 
  result <- rep(NA, k)
  i <- 1
  
  for(obs in unique(x[,"obs"])){
    index <- which(x[,"obs"] == obs)
    result[obs] <- recursion(i, x, index, result, sizes)
  }
  
  return(result)
}

progress_bar <- function(iter, obj, sizes, n_opt = length(sizes)){
  cat(paste0("== Iteration: ", iter,
             " == mean: ", round(mean(obj), 2),
             ", min: ", min(obj), ", size:\n"))
  write.table(format(rbind(1:n_opt, sizes)),
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  
}
