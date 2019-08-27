########################################################################### 
# TimeSeriesLSTM.R
# Time series experiment for LSTM. Network designed based on cross validation and evaluation in LSTMXVal.R. Includes stationarity test.
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

# Get hourly difference
ts_testperiod <- ts_testperiod %>%
  mutate(power_lead = lead(active.power.kW,
                           default = active.power.kW[1],
                           n = leadlag)) %>% # n is no. of hours ahead
  mutate(power_diff = active.power.kW - power_lead) %>% 
  mutate_at(c(3:8), funs(c(scale(.))))

# Getting the train months (7 days of data) and
# Substituting actual power values with lead values from testing set
testdate <- ymd("2018-08-31") 
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

### Scaling
train_pred <- as.matrix(ts_training)
train_lab <- as.matrix(train_power_diff)
test_pred <- as.matrix(ts_testing)
test_lab <- as.matrix(test_power_diff)

##### START code attribution #####
##### (Falbel et al., 2019; Wanjohi, 2018) #####

# Reshape the input to 3-dim
dim(train_pred) <- c(nrow(train_pred), 1 , ncol(train_pred))

# specify required arguments
X_shape2 = dim(train_pred)[2]
X_shape3 = dim(train_pred)[3]
batch_size = 1 # must be a common factor of both the train and test samples
units = 1      # can adjust this, in model tuning phase

K <- backend()
K$clear_session()

use_session_with_seed(123)

tsmodel <- keras_model_sequential() %>%
  layer_lstm(units = 32, 
             batch_input_shape = c(batch_size, X_shape2, X_shape3), 
             stateful= TRUE) %>%
  layer_dense(units = 1)

tsmodel %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_rmsprop(),  
  metrics = c("mean_absolute_error")
)

# summary(tsmodel)

tsEpochs = 50  

# 16.1 secs
start.time <- Sys.time()
for(i in 1:tsEpochs ){
  tsmodel %>% fit(train_pred, train_lab, 
                  epochs=1, batch_size=batch_size, verbose=0, shuffle=FALSE)
  tsmodel %>% reset_states()
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

dim(test_pred) <- c(nrow(test_pred), 1, ncol(test_pred))

predictions <- tsmodel %>% predict(test_pred, batch_size=batch_size)

c(loss, MAE_ts) %<-% (tsmodel %>% 
                        evaluate(test_pred, test_lab, verbose = 0,batch_size = batch_size))

##### END code attribution #####

MAE_ts
test_RMSE_lstm <- sqrt(mean((predictions - test_lab)^2))
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
(power_RMSE/mean(ts_vis$active.power.kW))*100 # (mean error)


# Mean daily actual power
mean(ts_vis$active.power.kW)

# Mean daily predicted power
mean(ts_vis$predicted_power)

# Plot actual vs predicted
fn_plot_act_vs_pred(data = ts_vis, actual = "active.power.kW", predicted = "predicted_power", subtitle = "Aug 2018 (1 hour)", alpha = 1)

# Plot actual vs predicted time series
ggplot() +
  geom_line(data = ts_vis, aes(x = ts, y = active.power.kW, colour = "active.power.kW")) +
  geom_line(data = ts_vis, aes(x = ts, y = predicted_power, colour = "predicted_power")) + # keras
  scale_y_continuous(breaks = seq(0, 4000, 500)) +
  labs(title = "Actual vs predicted power values time series", 
       subtitle = "Aug 2018 (1 hour)",
       x = "time (hourly)", y = "wind power (kW)") +
  theme_bw(base_family = "serif") +
  scale_colour_manual("Wind power",
                      values=c("black", "#F8766D"),
                      labels = c("Actual", "Predicted"))
##### ARIMA #####

library(forecast)

arima_training <- ts_testperiod %>% 
  slice(idx_startdate:(idx_testdate)) %>% 
  select(-filename)

arima_testing <- ts_testperiod %>% 
  slice((idx_testdate + leadlag):idx_enddate) %>%
  select(-filename)

##### START code attribution #####
##### (Rob J Hyndman & Athanasopoulos, 2018) #####

arima.model <- auto.arima(arima_training$active.power.kW)
#summary(arima.model)
checkresiduals(arima.model)
autoplot(forecast(arima.model))

arima.predictions <- forecast(arima.model, h = length(arima_testing$active.power.kW))

##### END code attribution #####
# MAE
MAE(pred = arima.predictions$mean, obs = arima_testing$active.power.kW)

# RMSE
a.RMSE <- RMSE(obs = arima_testing$active.power.kW, pred = arima.predictions$mean)
a.RMSE

# Mean error
(a.RMSE/mean(arima_testing$active.power.kW))*100

# Mean daily actual power
mean(arima_testing$active.power.kW)

# Mean daily predicted power
mean(arima.predictions$mean)

a.vis <- cbind(arima_testing, as.data.frame(as.numeric(arima.predictions$mean)))
colnames(a.vis)[ncol(a.vis)] <- "predicted_power"

# Plot actual vs predicted
fn_plot_act_vs_pred(data = a.vis, 
                    actual = "active.power.kW", predicted = "predicted_power", 
                    subtitle = "Aug 2018", alpha = 1)

# Plot actual vs predicted time series
ggplot() +
  geom_line(data = a.vis, aes(x = ts, y = active.power.kW, colour = "active.power.kW")) +
  geom_line(data = a.vis, aes(x = ts, y = predicted_power, colour = "predicted_power")) + 
  scale_y_continuous(breaks = seq(0, 4000, 500)) +
  labs(title = "Actual vs predicted power values time series", subtitle = "Aug 2018", x = "time (hourly)", y = "wind power (kW)") +
  theme_bw(base_family = "serif") +
  scale_colour_manual("Wind power", values=c("black", "#F8766D"),
                      labels = c("Actual", "Predicted"))

##### START STATIONARITY VISUALISATION ####################################

stationarity <- ts_testperiod %>% filter(filename == "Aug2018")

##### START code attribution #####
##### (Robinson, 2018) #####

acf_result1 <- acf(stationarity$active.power.kW,lag.max = length(stationarity$active.power.kW), plot = FALSE)
acf_result1 <- broom::tidy(acf_result1)
acf_result2 <- acf(stationarity$power_diff,lag.max = length(stationarity$power_diff), plot = FALSE)
acf_result2 <- broom::tidy(acf_result2)
conf.int <- qnorm((1 + 0.95)/2)/sqrt(nrow(stationarity))

ggplot(acf_result1, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = c(-conf.int, conf.int), colour = "dodgerblue", linetype = "dashed") + labs(title = "Non-stationary data", subtitle = "Aug 2018") +
  theme_bw(base_family = "serif")

ggplot(acf_result2, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = c(-conf.int, conf.int), colour = "dodgerblue", linetype = "dashed") + labs(title = "Stationary data (differenced)", subtitle = "Aug 2018") +
  theme_bw(base_family = "serif")

##### END code attribution #####

##### END STATIONARITY VISUALISATION ######################################
