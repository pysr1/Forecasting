


# This function converts a weekly tibble into a ts object
convert = function(df){
  min_ = min(df$ds)
  max_ = max(df$ds)
  Dates = tibble(ds = seq(min_, max_, "week")) 
  df = df %>% 
    mutate(Year = year(ds), Week = week(ds)) %>% 
    right_join(Dates, by = "ds")
  starty = df$Year[1]
  startw = df$Week[1]
  TS = df %>% select(ds,y) %>% 
    tk_ts(start = c(starty , startw), frequency = 52) 
  return(TS)
}

# this function plots a forcast evaluation
forecast_eval = function(forecast, model_name = "", eval_set = test){
  acc = accuracy(forecast, eval_set)
  acc = round(acc[6], 2) 
  model_name = model_name
  forecast %>% 
    autoplot() + 
    autolayer(test, series = "Test set")+
    theme_classic()+
    labs(title = paste(model_name, " model of by ", acc, " on average across test set", sep = ""))
}