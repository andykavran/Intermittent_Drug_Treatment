get_bootstrap_line = function(data_subset, x_var, y_var, boot_reps){
  require(rlang)
  require(dplyr)
  index = 1
  fifty_ci = vector("list", 1)
  confidence_interval = vector("list", 1)
  val_range = matrix(0, nrow = boot_reps, ncol = 200) 
  by_conc = data_subset %>% group_by(!!x_var) # the !! unquotes conc_var
  for(jj in 1:boot_reps){
    boot_data = sample_frac(by_conc, 1, replace = TRUE)
    fit_line = loess(boot_data$number~log10(boot_data[, x_var]), span = 0.5)
    start = min(log10(boot_data[, x_var]))
    end = max(log10(boot_data[, x_var]))
    x = seq(start, end, length.out = 200)
    y = predict(fit_line, x)
    val_range[jj, ] = y
  }
  bootstrap_lines = as.data.frame(val_range)
  colnames(bootstrap_lines) = x
  bootstrap_lines = bind_cols(repli = 1:boot_reps, bootstrap_lines)
  gathered_bootstrap = bootstrap_lines %>% gather(key = !!x_var, value = !!y_var, -repli, convert = TRUE)
  return(gathered_bootstrap)
  
}

