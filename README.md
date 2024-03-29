# Wind Turbine Analytics
## Forecasting wind power generation

<b>Background.</b> Wind power generation is one of the fastest growing sources of clean energy that is both sustainable and cost effective. Wind turbines produce vast amounts of sensor data that are generally used to optimise operations and energy output. These data have gained the added potential of informing predictive analytics in areas such as wind power forecasting. The stochastic characteristics of wind energy, however, present challenges to the stability and consistency of these predictions. This study investigates several wind power prediction models in both static and temporal domains.
<p>
<b>Aims.</b> The main purpose of this study is to develop short-term wind power forecasting models from historical SCADA and weather data using machine learning techniques. Relevant data science frameworks and methods are utilised in the process of achieving the objectives defined for this research. These include a survey of literature on the development and use of similar models in industry, evaluation of appropriate algorithms and performance assessment of the models developed for this study.
<p>
<b>Methods.</b> Wind turbine data was provided by <i>Visualwind</i> in the form of SCADA measurements and was further enriched with weather records obtained via the <i>Dark Sky</i> API. Appropriate cleaning and pre-processing strategies were applied to the data and feature engineering carried out to convert the data to stationary time series. Three types of regression models were developed: Random Forest (RF), Support Vector Machine (SVM) and Artificial Neural Network (ANN). The ANN model was further examined with respect to network types: Multi-Layer Perceptron (MLP) versus Long Short-Term Memory (LSTM). Additional benchmarking for LSTM was provided by a basic statistical Autoregressive Integrated Moving Average (ARIMA) model. Metrics such as R2, MAE, RMSE and mean error were used to measure model performance.
<p>
<b>Results.</b> The LSTM model exhibited the best performance overall. Model forecasting experiments were carried out for time steps between 1 and 48 hours with error margins as low as 0.8% for the former. As expected, the error margins grew with increasing time steps; however, this effect was greatly reduced for the LSTM model. Additionally, a combination of SCADA and weather model input features were observed to enhance model performance compared to purely weather features.
<p>
<b>Conclusions.</b> Despite the stochasticity of weather-related features, LSTM networks have been proven to be adept at interpreting underlying seasonal patterns and interactions. They are therefore capable of  providing more accurate wind power predictions compared to other static models. Moreover, businesses in the wind turbine sector can capitalise on these gains from neural network models through accessible open source frameworks such as <i>Keras</i> and <i>TensorFlow</i>.

# Files
## Data pre-processing and cleaning
<b><code>HelperFunctions.R</code></b> Helper functions for processing multiple datasets at a time.
<p>
<b><code>DataProc.R</code></b> Data pre-processing for SCADA and <i>Dark Sky</i>.

## Experiments with standard regression
<b><code>Experiments.R</code></b> Experiments for Random Forest (RF), Support Vector Machine (SVM) and Artificial Neural Network (ANN).

## Time series forecasting
<b><code>LSTMXVal.R</code></b> Cross validation and tuning for LSTM (stateless model).
<p>
<b><code>TimeSeriesMLP.R</code></b> Time series experiment for MLP.
<p>
<b><code>TimeSeriesLSTM.R</code></b> Time series experiment for LSTM. Network designed based on cross validation and evaluation in <code>LSTMXVal.R</code>. Includes stationarity test.
