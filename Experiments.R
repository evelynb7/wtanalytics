########################################################################### 
# Experiments.R
# Experiments for RF, SVM and ANN.
###########################################################################
library(caret)
library(mlbench)
library(lubridate)
source("HelperFunctions.R")

# Load weather data
weather_data <- readRDS(file = "weather_data.rds")
power_curve <- readRDS(file = "power_curve.rds")

# Test months are as follows:
# Spring - April
# Summer - July
# Autumn - October
# Winter - January

#testmonth <- weather_data %>% filter(filename == "Aug2018")
testmonth <- weather_data %>% filter(filename %in% c("Jan2018", "Apr2017", "Jul2017", "Oct2018"))

p.mat <- corrplot::cor.mtest(testmonth %>% select(-c(filename, year, month, ts)))$p
png(file="testmonth.png", res=300, width=4500, height=4500)
corrplot::corrplot(cor(testmonth %>% select(-c(filename, year, month, ts)), method = "spearman"), method = "color",
                   type = "upper", number.cex = .7,
                   addCoef.col = "black",
                   tl.col = "black", tl.srt = 90, tl.cex = .7,
                   p.mat = p.mat, sig.level = 0.05, #insig = "blank",
                   diag = FALSE)
dev.off()

rfe_Control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_Results <- rfe(testmonth[,c(3:26, 28:38)], testmonth[,27], rfeControl = rfe_Control)
# predictors(rfe_Results)

# Based on correlation map, select certain features and rfe_Results
testmonth <- testmonth %>%
  select(ts, wind.speed.MS, temp.rotor.bearing.C, hub.temp.C,
         day, humidity, pressure, apparentTemperature, active.power.kW)

# Extract timestamp
ts <- testmonth$ts
# Extract target feature
wind_power <- testmonth$active.power.kW
# Extract predictors and scale
predictors <- testmonth %>% select(-active.power.kW, -ts) %>%
  mutate_if(is.numeric, scale)

# Reconstruct original data frame with scaled predictors
scaled_data <- cbind(ts, predictors, wind_power)

set.seed(123)
idx <- scaled_data$wind_power %>% createDataPartition(p = 0.7, list = FALSE)
training <- scaled_data[idx,]
training_ts <- select(training, ts)
training <- select(training, -ts)
testing <- scaled_data[-idx,]
testing_ts <- select(testing, ts)
testing <- select(testing, -ts)

# Basic parameter tuning
control <- trainControl(method="repeatedcv", number=10, repeats=3)

##### SVM #################################################################

start.time <- Sys.time()

set.seed(123)
# To determine original sigma:
# sigma_svm <- mean(kernlab::sigest(wind_power ~., data = training))
# sigma_svm = 0.1129935/0.129/0.13
tune.grid.svm <- expand.grid(sigma = 0.2, C = 10)
fit.svm <- caret::train(wind_power ~ . , 
                        data = training, 
                        method = "svmRadial", 
                        trControl = control,
                        tuneGrid = tune.grid.svm,
                        importance = T)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

##### RANDOM FOREST #######################################################

start.time <- Sys.time()

set.seed(123)
tune.grid.rf <- expand.grid(mtry = 4)
fit.rf <- caret::train(wind_power ~ ., 
                       data = training, 
                       method = "rf", 
                       trControl = control, 
                       tuneGrid = tune.grid.rf,
                       importance = T)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

##### EVALUATE SVM AND RF MODELS ##########################################

results <- resamples(list(SVM = fit.svm, RF = fit.rf))

results_save <- summary(results)

idx_svm <- which(fit.svm$results$sigma == fit.svm$bestTune$sigma & fit.svm$results$C == fit.svm$bestTune$C)
idx_rf <- which(fit.rf$results$mtry == fit.rf$bestTune$mtry)

train_MAE_rf <- fit.rf$results$MAE[idx_rf]
train_MAE_svm <- fit.svm$results$MAE[idx_svm]
train_RMSE_rf <- fit.rf$results$RMSE[idx_rf]
train_RMSE_svm <- fit.svm$results$RMSE[idx_svm]
train_R2_rf <- fit.rf$results$Rsquared[idx_rf]
train_R2_svm <- fit.svm$results$Rsquared[idx_svm]

# RF accuracy = 11.5%
(train_RMSE_rf/mean(training$wind_power))*100

# SVM accuracy = 12.2%
(train_RMSE_svm/mean(training$wind_power))*100

plot(varImp(fit.svm, scale = FALSE), main = "Support Vector Machine")
plot(varImp(fit.rf, scale = FALSE), main = "Random Forest")


# Predictions with RF
predictions_rf <-  predict(fit.rf, testing)

# Predictions with SVM
predictions_svm <- predict(fit.svm, testing)

RMSE_rf <- sqrt(mean((predictions_rf - testing$wind_power)^2))
MAE_rf <- mean(abs(predictions_rf - testing$wind_power))
MAPE_rf <- mean(abs((testing$wind_power - predictions_rf)/testing$wind_power) * 100) # mean error

RMSE_svm <- sqrt(mean((predictions_svm - testing$wind_power)^2))
MAE_svm <- mean(abs(predictions_svm - testing$wind_power))
MAPE_svm <- mean(abs((testing$wind_power - predictions_svm)/testing$wind_power) * 100) # mean error


# RF accuracy = 10.9%
(RMSE_rf/mean(testing$wind_power))*100

# SVM accuracy = 10.8%
(RMSE_svm/mean(testing$wind_power))*100

# Visualise actual vs predicted
vis_rf <- testing %>% select(wind_power) %>%
  mutate(predicted = predictions_rf)
fn_plot_act_vs_pred(data = vis_rf, 
                    actual = "wind_power", 
                    predicted = "predicted", 
                    title = "RF actual vs. predicted wind power (kW)")

vis_svm <- testing %>% select(wind_power) %>%
  mutate(predicted = predictions_svm)
fn_plot_act_vs_pred(data = vis_svm, 
                    actual = "wind_power", 
                    predicted = "predicted", 
                    title = "SVM actual vs. predicted wind power (kW)")

##### KERAS – MLP ANN #####################################################

##### START code attribution #####
##### (Falbel et al., 2019) #####

library(keras)

K <- backend()
K$clear_session()
use_session_with_seed(123)

klabels <- select(training, wind_power) %>% as.matrix()
ktraining <- select(training, -wind_power) %>% as.matrix()

kTestLabels <- select(testing, wind_power) %>% as.matrix()
kTesting <- select(testing, -wind_power) %>% as.matrix()


model <- fn_build_keras_model(ip_shape = dim(ktraining)[2], 
                              act = "relu",
                              opt = optimizer_rmsprop())#Update act and opt # for hyperparameter tuning


# For showing training progress
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs){
    if(epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

# Check if epoch has any progress, if not stop training - prevent overfitting
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

epochs <- 500

# Fit model and store training stats
start.time <- Sys.time()

history <- model %>% fit(
  ktraining,
  klabels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(history, metrics = c("mean_absolute_error"), 
     smooth = FALSE) +
  coord_cartesian(xlim = c(0,210))

# TRAIN SET EVAL
c(loss, MAE_k, RMSE_k, R2_k, MAPE_k) %<-% (model %>% 
                                             evaluate(ktraining, klabels, verbose = 0))

# Accuracy (mean error)
(RMSE_k/mean(klabels))*100

### THIS IS FOR TEST SET
# In c(x, y), these can be called whatever you want
c(loss, test_MAE_k, test_RMSE_k, test_R2_k, test_MAPE_k) %<-% (model %>% 
                                                                 evaluate(kTesting, kTestLabels, verbose = 0))


prediction_keras <- model %>% predict(kTesting)

##### END code attribution #####

# Accuracy (mean error)
(test_RMSE_k/mean(kTestLabels))*100

# Visualise actual vs predicted
vis_ann <- cbind(kTestLabels, prediction_keras)
vis_ann <- as.data.frame(vis_ann)
fn_plot_act_vs_pred(data = vis_ann,
                    actual = "wind_power",
                    predicted = "V2", 
                    title = "ANN actual vs. predicted wind power (kW)")

###########################################################################


### Visualise all 3 models ###

models_vis_unscaled <- cbind(testmonth[-idx,], 
                             as.data.frame(prediction_keras),
                             as.data.frame(predictions_rf),
                             as.data.frame(predictions_svm))

fn_visualise_pred_power_curve(data = models_vis_unscaled %>%
                                filter(month(models_vis_unscaled$ts) == 1), 
                              subtitle = "Jan 2018",
                              xfeat = "wind.speed.MS",
                              y_act = "active.power.kW",
                              y_nn = "V1",
                              y_rf = "predictions_rf",
                              y_svm = "predictions_svm")

fn_visualise_pred_time_series(data = models_vis_unscaled %>% 
                                filter(month(models_vis_unscaled$ts) == 10), 
                              subtitle = "Oct 2018",
                              xfeat = "ts",
                              y_act = "active.power.kW",
                              y_nn = "V1",
                              y_rf = "predictions_rf",
                              y_svm = "predictions_svm")

fn_plot_act_vs_pred(models_vis_unscaled, actual = "active.power.kW", predicted = "V1")
fn_plot_act_vs_pred(models_vis_unscaled, actual = "active.power.kW", predicted = "predictions_rf")
fn_plot_act_vs_pred(models_vis_unscaled, actual = "active.power.kW", predicted = "predictions_svm")


##### GRID SEARCH TUNING FOR SVM AND RF ###################################

##### SVM tuning
set.seed(123)
svm_grid <- expand.grid(sigma = c(0.0001, 0.001, 0.01, 0.2),
                        C = c(0.1, 1, 10, 100, 1000))

start.time <- Sys.time()
fit.svm1 <- caret::train(wind_power ~ ., 
                         data = training, 
                         method = "svmRadial", 
                         trControl = control, 
                         tuneGrid = svm_grid,
                         importance = T)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

##### Random Forest tuning

set.seed(123)
rf_grid <- expand.grid(mtry = c(1:7))
fit.rf1 <- caret::train(wind_power ~ .,
                        data = training,
                        method = "rf",
                        trControl = control,
                        tuneGrid = rf_grid,
                        importance = T)

##########################################################################


##### Evaluation for selected months (set in filename == "MMMYYYY") ######

evalMonth <- weather_data %>% filter(filename == "Jan2018") %>%
  select(ts, wind.speed.MS, temp.rotor.bearing.C, hub.temp.C,
         day, humidity, pressure, apparentTemperature, active.power.kW)
scale_evalMonth <- fn_scale_data(evalMonth)
nn_evalMonth <- select(scale_evalMonth, 2:8) %>% as.matrix()
lab_evalMonth <- select(scale_evalMonth, 9) %>% as.matrix()

pred_rf <-  predict(fit.rf, scale_evalMonth)
pred_svm <- predict(fit.svm, scale_evalMonth)
pred_nn <- model %>% predict(nn_evalMonth)

evalMonth <- cbind(evalMonth, pred_nn, pred_rf, pred_svm)


### Evaluation metrics ###

eval_RMSE_rf <- sqrt(mean((pred_rf - scale_evalMonth$wind_power)^2))
eval_MAE_rf <- mean(abs(pred_rf - scale_evalMonth$wind_power))
eval_MAPE_rf <- mean(abs((scale_evalMonth$wind_power - pred_rf)/scale_evalMonth$wind_power) * 100) # mean error

eval_RMSE_svm <- sqrt(mean((pred_svm - scale_evalMonth$wind_power)^2))
eval_MAE_svm <- mean(abs(pred_svm - scale_evalMonth$wind_power))
eval_MAPE_svm <- mean(abs((scale_evalMonth$wind_power - pred_svm)/scale_evalMonth$wind_power) * 100) # mean error


c(loss, eval_MAE_k, eval_RMSE_k, eval_R2_k, eval_MAPE_k) %<-% (model %>% 
                                                                 evaluate(nn_evalMonth, lab_evalMonth, verbose = 0))


##### VISUALISE PREDICTIONS FOR ANN, RF and SVM ###########################

fn_visualise_pred_power_curve(data = evalMonth, subtitle = "Aug 2018",
                              xfeat = "wind.speed.MS",
                              y_act = "active.power.kW",
                              y_nn = "pred_nn",
                              y_rf = "pred_rf",
                              y_svm = "pred_svm")

fn_visualise_pred_time_series(data = evalMonth, subtitle = "Aug 2018",
                              xfeat = "ts",
                              y_act = "active.power.kW",
                              y_nn = "pred_nn",
                              y_rf = "pred_rf",
                              y_svm = "pred_svm")

###########################################################################
