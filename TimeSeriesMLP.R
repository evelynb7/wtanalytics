########################################################################### 
# TimeSeriesMLP.R
# Time series experiment for MLP.
###########################################################################
library(caret)
library(mlbench)
library(lubridate)
library(keras)
source("HelperFunctions.R")

# Load weather data
weather_data <- readRDS(file = "weather_data.rds")
#power_curve <- readRDS(file = "power_curve.rds")


### Time Series test bed ###

ts_testperiod <- weather_data %>% #filter(filename == "Aug2018") %>%
  select(filename, ts, wind.speed.MS, temp.rotor.bearing.C, hub.temp.C,
         humidity, pressure, apparentTemperature, active.power.kW) %>%
  mutate_if(is.character, as.numeric)

# Get the difference in power

leadlag <-  1 # hours for prediction time horizon
train_period <- 7 # days for training window
test_period <- 1 # days for showing next-day prediction (24 hours)

# Get hourly difference
ts_testperiod <- ts_testperiod %>%
  mutate(power_lead = lead(active.power.kW,
                           default = active.power.kW[1],
                           n = leadlag)) %>% # n is # of hours ahead
  mutate(power_diff = active.power.kW - power_lead) %>% 
  mutate_at(c(3:8), funs(c(scale(.))))

# Getting the train months (7 days of data) and
# Substituting actual power values with lead values from testing set
testdate <- ymd("2018-08-31") #ymd("2018-08-31")
testdate <- testdate - hours(leadlag)
idx_testdate <- which(ts_testperiod$ts == testdate)

startdate <- testdate - days(train_period)
idx_startdate <- which(ts_testperiod$ts == startdate)

enddate <- testdate + days(test_period) + hours(leadlag)
idx_enddate <- which(ts_testperiod$ts == enddate)

ts_training <- ts_testperiod %>% 
  slice(idx_startdate:(idx_testdate - 1)) %>%
  select(-filename)

ts_training_time <- select(ts_training, ts)
train_power_diff <- select(ts_training, power_diff)

numcols <- dim(ts_training)[2]-3
ts_training <- ts_training %>% select(2: numcols)
ts_testing <- ts_testperiod %>% 
  slice(idx_testdate:idx_enddate) %>%
  select(-filename)
ts_testing_time <- select(ts_testing, ts)
test_power_diff <- select(ts_testing, power_diff)

ts_testing <- ts_testing %>% select(2:numcols) 

## Scaling
train_pred <- as.matrix(ts_training)
train_lab <- as.matrix(train_power_diff)
test_pred <- as.matrix(ts_testing)
test_lab <- as.matrix(test_power_diff)

##

K <- backend()
K$clear_session()

use_session_with_seed(123)

##### START code attribution #####
##### (Falbel et al., 2019) #####
mlp_epochs <- 500

# For showing training progress
mlp_print_dot_callback <- callback_lambda(
  on_epoch_end = function(mlp_epochs, logs){
    if(mlp_epochs %% 80 == 0) cat("\n")
    cat(".")
  }
)

# Check if epoch has any progress, if not stop training - prevent overfitting
mlp_early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

mlp_model <- fn_build_keras_model(ip_shape = dim(train_pred)[2], 
                                  act = "elu", opt = optimizer_rmsprop(lr = 0.01))

# Fit model and store training stats
start.time <- Sys.time()

mlp_history <- mlp_model %>% fit(
  train_pred, train_lab,
  epochs = mlp_epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(mlp_early_stop, mlp_print_dot_callback)
)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(mlp_history, metrics = c("mean_absolute_error"), smooth = FALSE) +
  coord_cartesian(xlim = c(0,210))
# TRAIN SET EVAL
c(loss, MAE_mlp, RMSE_mlp, R2_mlp, MAPE_mlp) %<-% (mlp_model %>% 
                                                     evaluate(train_pred, train_lab, verbose = 0))

# Accuracy (mean error)
#(RMSE_mlp/mean(train_lab))*100

### THIS IS FOR THE TEST SET
c(loss, test_MAE_mlp, test_RMSE_mlp, test_R2_k, test_MAPE_mlp) %<-% (mlp_model %>% 
                                                                       evaluate(test_pred, test_lab, verbose = 0))

mlp_predictions <- mlp_model %>% predict(test_pred)
##### END code attribution #####
# Accuracy (mean error)
#(test_RMSE_mlp/mean(test_lab))*100

mlp_vis <- ts_testing <- ts_testperiod %>% 
  slice(idx_testdate:idx_enddate) %>% 
  mutate(predictions = as.vector(mlp_predictions)) %>%
  mutate(predicted_power = predictions + power_lead) %>%
  mutate(predicted_power=lag(predicted_power, leadlag)) %>%
  na.omit()

# Power accuracy
power_MAE <- mean(abs(mlp_vis$predicted_power - mlp_vis$active.power.kW))
power_MAE

power_RMSE <- sqrt(mean((mlp_vis$predicted_power - mlp_vis$active.power.kW)^2))
power_RMSE
(power_RMSE/mean(mlp_vis$active.power.kW))*100

# Mean daily actual power
mean(mlp_vis$active.power.kW)

# Mean daily predicted power
mean(mlp_vis$predicted_power)

# Plot actual vs predicted
fn_plot_act_vs_pred(data = mlp_vis, actual = "active.power.kW", predicted = "predicted_power", subtitle = "Aug 2018 (1 hour)", alpha = 1)

# Plot actual vs predicted time series
ggplot() +
  geom_line(data = mlp_vis, aes(x = ts, y = active.power.kW, colour = "active.power.kW")) +
  geom_line(data = mlp_vis, aes(x = ts, y = predicted_power, colour = "predicted_power")) + # keras
  scale_y_continuous(breaks = seq(0, 4000, 500)) +
  labs(title = "Actual vs predicted power values time series", 
       subtitle = "Aug 2018 (1 hour)",
       x = "time (hourly)", y = "wind power (kW)") +
  theme_bw(base_family = "serif") +
  scale_colour_manual("Wind power",values=c("black", "#F8766D"),
                      labels = c("Actual", "Predicted"))

