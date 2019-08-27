########################################################################### 
# LSTMXVal.R
# Cross validation and tuning for LSTM (stateless model)
###########################################################################
library(caret)
library(mlbench)
library(lubridate)
library(keras)
source("HelperFunctions.R")

# Load weather data
weather_data <- readRDS(file = "weather_data.rds")

### Time Series test bed ###

ts_testperiod <- weather_data %>%
  select(filename, ts, wind.speed.MS, temp.rotor.bearing.C, hub.temp.C,
         humidity, pressure, apparentTemperature, active.power.kW) %>%
  mutate_if(is.character, as.numeric)

# Get the difference in power

leadlag <-  1 # hours
train_period <- 7 # days
test_period <- 1 # days
val_period <- 1 # days

# Get hourly difference
ts_testperiod <- ts_testperiod %>%
  mutate(power_lead = lead(active.power.kW,
                           default = active.power.kW[1],
                           n = leadlag)) %>% # n is no. of hours ahead
  mutate(power_diff = active.power.kW - power_lead) %>% 
  mutate_at(c(3:8), funs(c(scale(.))))

# Getting the train months (7 days of data) and
# Substituting actual power values with lead values from testing set
testdate <- ymd("2018-01-01") #ymd("2018-08-31")
testdate <- testdate - hours(leadlag)
idx_testdate <- which(ts_testperiod$ts == testdate)

startdate <- testdate - days(train_period)
idx_startdate <- which(ts_testperiod$ts == startdate)

enddate <- testdate + days(test_period) + hours(leadlag)
idx_enddate <- which(ts_testperiod$ts == enddate)

valdate <- testdate - days(val_period) + hours(leadlag)
idx_valdate <- which(ts_testperiod$ts == valdate)

ts_training <- ts_testperiod %>% 
  slice(idx_startdate:(idx_valdate - 1)) %>% 
  select(-filename)

ts_training_time <- select(ts_training, ts)
train_power_diff <- select(ts_training, power_diff)

numcols <- dim(ts_training)[2]-3
ts_training <- ts_training %>% select(2: numcols)

ts_validation <- ts_testperiod %>%
  slice(idx_valdate:(idx_testdate -1)) %>%
  select(-filename)

ts_validation_time <- select(ts_validation, ts)
val_power_diff <- select(ts_validation, power_diff)
ts_validation <- ts_validation %>% select(2: numcols)


ts_testing <- ts_testperiod %>% 
  slice(idx_testdate:idx_enddate) %>%
  select(-filename)
ts_testing_time <- select(ts_testing, ts)
test_power_diff <- select(ts_testing, power_diff)

ts_testing <- ts_testing %>% select(2:numcols) 

### Scaling

train_pred <- as.matrix(ts_training)
train_lab <- as.matrix(train_power_diff)
val_pred<- as.matrix(ts_validation)
val_lab <- as.matrix(val_power_diff)
test_pred <- as.matrix(ts_testing)
test_lab <- as.matrix(test_power_diff)

###
##### START code attribution #####
##### (Dancho & Keydana, 2018; Falbel et al., 2019; Wanjohi, 2018) #####

# Reshape the input to 3-dim
dim(train_pred) <- c(nrow(train_pred), 1 , ncol(train_pred))
dim(val_pred) <- c(nrow(val_pred), 1, ncol(val_pred))

# specify required arguments
X_shape2 = dim(train_pred)[2]
X_shape3 = dim(train_pred)[3]
batch_size = 1 
units = 1  

K <- backend()
K$clear_session()

use_session_with_seed(123)

tsmodel <- keras_model_sequential() %>%
  layer_lstm(units = 32, 
             batch_input_shape = c(batch_size, X_shape2, X_shape3), 
             #activation = "elu",
             return_sequences = TRUE#, 
             #stateful= TRUE
  ) %>%
  # layer_lstm(units = 64, #comment
  #            return_sequences = FALSE, #comment
  #            stateful= TRUE) %>% #comment
  layer_dense(units = 1)

tsmodel %>% compile(
  loss = "mean_squared_error",
  #optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  optimizer = optimizer_rmsprop(),  
  metrics = c("mean_absolute_error")
)

# summary(tsmodel)
tsEpochs = 100  


callbacks <- list(
  callback_early_stopping(monitor = "val_loss", patience = 20)
)

# Stateless model – enables saving of history and epoch analysis
start.time <- Sys.time()

history <-   tsmodel %>% fit(train_pred, train_lab,
                             validation_data = list(val_pred, val_lab),
                             callbacks = callbacks,
                             epochs=tsEpochs,
                             batch_size=batch_size,
                             verbose = 0)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(history, metrics = c("mean_absolute_error"),
     smooth = FALSE) +
  coord_cartesian(xlim = c(0,100))


dim(test_pred) <- c(nrow(test_pred), 1, ncol(test_pred))

predictions <- tsmodel %>% predict(test_pred, batch_size=batch_size)

c(loss, MAE_ts) %<-% (tsmodel %>% 
                        evaluate(test_pred, test_lab, verbose = 0,batch_size = batch_size))

##### END code attribution #####

# MAE
MAE_ts

# RMSE
test_RMSE_lstm <- sqrt(mean((predictions - as.vector(test_lab))^2))
test_RMSE_lstm

# Accuracy (mean error)
# (test_RMSE_lstm/mean(abs(test_lab)))*100


ts_vis <- ts_testing <- ts_testperiod %>% 
  slice(idx_testdate:idx_enddate) %>% 
  mutate(predictions = as.vector(predictions)) %>%
  mutate(predicted_power = predictions + power_lead) %>%
  mutate(predicted_power=lag(predicted_power, leadlag)) %>%
  na.omit()

# Power accuracy
power_MAE <- mean(abs(ts_vis$predicted_power - ts_vis$active.power.kW))
power_MAE

power_RMSE <- sqrt(mean((ts_vis$predicted_power - ts_vis$active.power.kW)^2))
power_RMSE

(power_RMSE/mean(ts_vis$active.power.kW))*100 # Mean error

# Mean daily actual power
mean(ts_vis$active.power.kW)

# Mean daily predicted power
mean(ts_vis$predicted_power)


# Plot actual vs predicted
fn_plot_act_vs_pred(data = ts_vis, actual = "active.power.kW", predicted = "predicted_power", subtitle = "Jan 2018 (24 hours)", alpha = 1)

# Plot actual vs predicted time series
ggplot() +
  geom_line(data = ts_vis, aes(x = ts, y = active.power.kW, colour = "active.power.kW")) +
  geom_line(data = ts_vis, aes(x = ts, y = predicted_power, colour = "predicted_power")) + # keras
  scale_y_continuous(breaks = seq(0, 4000, 500)) +
  labs(title = "Actual vs predicted power values time series", 
       subtitle = "Jan 2018 (24 hours)",
       x = "time (hourly)", y = "wind power (kW)") +
  theme_bw(base_family = "serif") +
  scale_colour_manual("Wind power", values=c("black", "#F8766D"),
                      labels = c("Actual", "Predicted"))

