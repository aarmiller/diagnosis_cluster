

# Function to extract model parameters -----------------------------------------
extract_parameters <- function(data, var_name = "n_visits",method, week_effect = F){
  # fit model
  fit <- delaySim::find_change_point(data,
                                     var_name=var_name,
                                     week_period = week_effect,
                                     method = method)

  # pull out parameter values
  params <- tibble(cp = -fit$change_point$period) %>%
    bind_cols(tibble(temp = names(fit$fit$coefficients),
                     vals = fit$fit$coefficients) %>%
                arrange(temp) %>%
                mutate(name = ifelse(str_detect(temp, "week"),
                                     temp, paste0("b", row_number()-1))) %>%
                select(-temp) %>%
                pivot_wider(names_from = name, values_from = vals))


  # pull out predicted values
  # preds <- fit$pred %>%
  #   as_tibble() %>%
  #   select(period,obs=Y,fitted=pred)


  return(params)
}
