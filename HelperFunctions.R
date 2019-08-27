########################################################################### 
# HelperFunctions.R
# Helper Functions for processing multiple datasets at a time.
###########################################################################
library(tidyverse)
library(lubridate)
library(darksky)


# Unzip files based on provided filename input
# Inputs :
# zipfiles	= Vector of csv filenames
# output_dir = Direct to unzip file to

fn_unzip <- function(zipfiles, output_dir){
  file_count <- length(zipfiles)
  for (i in 1:file_count) {
    unzip(zipfiles[i], exdir = output_dir)
  }
}


# Read CSV and resturns as a list of 24 objects
# Inputs:
# filenames = Character vector of .csv file names
# path = Directory where .csv files are located
# lastrows = Default number of last rows in csv with meta info that need to be filtered out

fn_read_csv <- function(filenames, path, lastrows = 209){
  file_count <- length(filenames)
  csv_list <- list()
  for (i in 1:file_count) {
    name <- substring(filenames[i], 19, 25)
    filepath <- paste(path, filenames[i], sep = "/")
    file <- read.csv(file = filepath, header = TRUE, sep = ";",
                     stringsAsFactors = FALSE, strip.white = TRUE)
    #file <- file %>% mutate_if(is.character, as.numeric())
    row_total <- nrow(file)
    # customising last rows = 245 for aug, sept, oct, dec 2017
    if (i %in% c(15, 17, 19, 23)) {
      remove_rows = 245
    } else{
      remove_rows = lastrows
    }
    keep_rows <- row_total - remove_rows
    #print(paste("name: ",name, "rows: ", row_total, "remove rows: ", remove_rows, "keep rows: ", keep_rows, sep = " "))
    #file <- file[1:keep_rows, ]
    csv_list[[name]] <- file[1:keep_rows, ]
  }
  return(csv_list)
}


# Read darksky CSV and resturns as a list of 24 objects
# Inputs:
# filenames = Character vector of .csv file names
# path = Directory where .csv files are located

fn_read_darksky_csv <- function(filenames, path){
  file_count <- length(filenames)
  csv_list <- list()
  for (i in 1:file_count) {
    name <- substring(filenames[i], 1, 7)
    filepath <- paste(path, filenames[i], sep = "/")
    file <- read.csv(file = filepath, header = TRUE,
                     stringsAsFactors = FALSE, strip.white = TRUE)
    csv_list[[name]] <- file
  }
  return(csv_list)
}

# Return column indices in groups of 3 for each WT from CSV 
# [1 - 67], [68 - 134], [135 - 201]
# Inputs:
# colnums = Total number of columns in CSV (202 including timestamp)
# turbnum = Known 3 turbines; default set to 3

fn_three_groups <- function(colnums, turbnum = 3){
  # Total columns from csv - 1 for timestamp which is common to all 3 WTs
  total_vars <- (colnums - 1)
  # Deriving the value "67"
  per_turbine <- total_vars/turbnum
  # Get total number of WT features (columns) = 201
  var_idx <- 1:total_vars
  # Split column index into list of 3 sets [1 - 67], [68 - 134], [135 - 201]
  split(var_idx, ceiling(seq_along(var_idx)/per_turbine))
}


# Cleans column names for CSV data (excluding timestamp)
# Ensure only features 2:68 are sent for cleaning
fn_clean_colnames <- function(){
  clean_colnames <- c("blade.angle", "curr.axis1.A", "curr.axis2.A", "curr.axis3.A", "hub.temp.C", "temp.motor.a1.C", "temp.motor.a2.C", "temp.motor.a3.C", "temp.base.box.C", "temp.transf.cell.C", "power.cable.util", "gen.speed.conv.RPM","conv.torque.Nm", "pos.act.work.kWh", "neg.act.work.kWh", "pos.react.work.kvarh", "neg.react.work.kvarh", "status.power.curve", "tower.oscil.y.mG", "tower.oscil.x.mG", "drive.train.osc.z.mG", "active.power.kW", "reactive.power.kvar", "apparent.power.kVa", "cos.phi", "setpoint.torque.Nm", "metalscan.particl", "grid.operation.time.s", "cpu.temp", "temp.gearb.bear.3.C", "temp.gearb.bear.4.C", "temp.trafo.L2.660V.C", "temp.trafo.L2.950V.C", "temp.transf.iron.core.C", "temp.gen.bearing1.C", "Temp.gen.bearing2.C",
                      "temp.stator.wind.C", "gearbox.speed.RPM", "actual.volt.L1.N.V", "actual.volt.L2.N.V", "actual.volt.L3.N.V", "actual.curr.I1.A", "actual.curr.I2.A", "actual.curr.I3.A", "temp.gearb.bear1.C", "temp.gearb.bear2.C", "temp.gearb.inlet.C", "temp.gearb.oil.sump.C",
                      "press.gearb.inlet.BAR", "nacelle.position", "nacelle.temp.C", "wind.speed1.MS", "wind.speed2.MS", "wind.speed.MS", "abs.wind.dir", "vane.position", "outd.temp.nacelle.C", "grid.freq.Hz", "grid.volt.V", "actual.p.setpoint.kW", "light.redux", "temp.top.box.C", "rotor.speed.RPM", "temp.rotor.bearing.C", "curr.A", "wind.speed.ice.det.MS", "torque.Nm")
  
}



# Returns combined WT01, WT02, WT03 in a data frame (rbind)
# Inputs: 
# wt_obj_wtime  = WT data from csv (including timestamp)
# group_list    = List of 3 groups of indices [1-67], [68 - 134], [135 - 201] (for 67 repeating features excluding common timestamp)
# num_groups    = Known 3 groups; default set to 3
# datafile      = Month and Year e.g. "Jul2018"

fn_3_WT_objs_wtime <- function(wt_obj_wtime, group_list, num_groups = 3, datafile){
  # Extract timestamp from 1st column and make POSIX (lubridate::dmy_hm)
  timestamp <- dmy_hm(wt_obj_wtime[,1])
  # Create object without timestamp
  no_time <- wt_obj_wtime %>% select(-1)
  # Remove columns where ALL data is NA (for problematic aug, sept, oct, dec 2017)
  if(dim(no_time)[2] > 201)
  {
    #print("first")
    no_time <- no_time %>% select_if(~!all(is.na(.)))
  }
  # Each WT will be appended to a list
  df_list <- list()
  for (i in 1:num_groups) {
    # Name for each WT list element e.g. "WT01", "WT02", "WT03"
    name <- paste("WT0", i, sep = "")
    # Create WT names nrow times (for new column of WT code names)
    turbine <- rep(name, nrow(wt_obj_wtime))
    # Extract each turbine by turn (looping 3 times)
    wt_xtime <- no_time[,group_list[[i]]]
    
    # Clean column names
    colnames(wt_xtime) <- fn_clean_colnames()
    # Create df with turbine code name "WT01", "WT02", "WT03"), timestamp, and the rest
    wt_xtime <- cbind(turbine, timestamp, wt_xtime)
    # Some num cols are char with "," as thousands marker; remove "," then convert to num
    wt_xtime <- wt_xtime %>% mutate_if(is.character, function(x) gsub(pattern = ",", "", x)) %>%
      mutate_if(is.character, as.numeric) %>% arrange(timestamp)
    # Turbine is factor, convert to char
    wt_xtime[,1] <- as.character(wt_xtime[,1])
    # Append WT01, WT02, WT03 to list in turn
    df_list[[name]] <- wt_xtime
  }
  # Create rbind dataframe with WT01, WT02, WT03 as long data
  wt_df <- plyr::ldply(df_list, data.frame, .id = "turbine")
  filename <- rep(datafile, nrow(wt_df))
  wt_df <- cbind(filename, wt_df)
  return(wt_df)
}


# Plots power curve data for WT01, WT02 and WT03
# Inputs: 
# data      = Data frame of combined Jan 2017 - Dec 2018 data (filter beforehand if visualising subset)
# subtitle  = Define appropriate subtitle (default "")
# fontfam   = "san", "serif" (default) or "mono"

fn_plot_power_curve <- function(data, subtitle = "", fontfam = "serif"){
  ggplot() + 
    geom_point(data = data, 
               aes(wind.speed.MS, active.power.kW, colour = turbine), shape = 1) +
    geom_line(data = power_curve, aes(wind_speed, power), color = "black") +
    scale_y_continuous(breaks = seq(0, 4000, 500)) +
    labs(title = "Power curves for 3 wind turbines overlaid", subtitle = subtitle,
         x = "Wind speed (m/s)", y = "Wind power (kW)") +
    theme_bw(base_family = fontfam) +
    facet_wrap(filename ~ ., ncol = 2, scales = "free")
}

# Plots time series data for WT01, WT02 and WT03
# Inputs: 
# data      = Data frame of combined Jan 2017 - Dec 2018 data (filter beforehand if visualising subset)
# subtitle  = Define appropriate subtitle (default "")
# fontfam   = "san", "serif" (default) or "mono"

fn_plot_time_series <- function(data, subtitle = "", fontfam = "serif", columns = 2){
  ggplot() +
    geom_line(data = data, 
              aes(x = timestamp, y = active.power.kW, group = turbine, colour = turbine)) +
    scale_y_continuous(breaks = seq(0, 4000, 500)) +
    labs(title = "10-min average daily power output for 3 wind turbines", subtitle = subtitle,
         x = "Time", y = "Wind power (kW)") +
    theme_bw(base_family = fontfam) +
    facet_wrap(filename ~ ., ncol = columns, scales = "free")
}

# Returns a wide dataframe of 24 months and average percentage missing vals for each
# Inputs: 
# obj_names = Vector of object names (char) as created in # # n_3_WT_objs_wtime (e.g. "Jan2017", "Jan2018", "Feb2017", "Feb2018" etc.)
# precision = results to decimal places (default = 3)

fn_eval_missing_vals <- function(obj_names, precision = 3){
  missing_df <- data.frame()
  for(i in 1: length(obj_names)){
    data <- get(obj_names[i])        # convert string to object
    month <- as.data.frame(obj_names[i])
    temp_df <- as.data.frame(data %>%
                               summarise_all(funs(round(100*mean(is.na(.)), precision))))
    temp_df <- cbind(month, temp_df)
    temp_df <- temp_df %>% select(-c(2,3)) %>% rename(month = 1) # removed turbine and timestamp cols
    missing_df <- rbind(missing_df, temp_df)
  }
  #return(t(missing_df))
  return(setNames(data.frame(t(missing_df[,-1])), missing_df[,1]))
}


# Visualises missing map for selected month
# Inputs: 
# monthdata = Dataframe with cols 1:3 filename (e.g. Jul2018), turbine (e.g. WT01, WT02, WT03), timestamp + 67 vars
# turbine   = Either WT01 (default), WT02 or WT03 to filter as example (since missing vals are common across all 3 turbines)
# title     = Customise title (default = "Missingness Map")

fn_missmap <- function(monthdata, filt_turb = c("WT01", "WT02", "WT03"), title = "Missingness Map"){
  Amelia::missmap(monthdata %>% filter(turbine %in% filt_turb) %>% select(-c(1,2,3)), # remove cols filename, turbine and timestamp as these are known non-NA cols
                  col = c("grey", "dodgerblue"), margins = c(5,3),
                  y.labels = NULL, y.at = NULL, x.cex = 0.5, main = title)
  
}

# Returns dataframe of missing rows for each WT (cols) by months (rows)
# Inputs: 
# month_names = Vector of month names (e.g. Jul2018)

fn_missing_rows <- function(month_names){
  missingrows_df <- data.frame()
  for(i in 1:length(month_names))
  {
    temp_df <- as.data.frame(t(get(month_names[i]) %>% 
                                 group_by(turbine) %>% 
                                 summarise(sum(is.na(active.power.kW)))))
    month_df <- as.data.frame(month_names[i])
    temp_df <- cbind(month_df, temp_df)
    missingrows_df <- rbind(missingrows_df, temp_df[2,])
  }
  rownames(missingrows_df) <- NULL
  colnames(missingrows_df) <- c("month", "WT01", "WT02", "WT03")
  return(missingrows_df)
}


# Carries out imputation of NA values
# Inputs: 
# impute_obj  = Data frame of month with missing values (e.g. Jul2018)
# supply_obj  = Date frame of month where values will be taken to impute into impute_obj (e.g. Jul2017)

fn_impute_NAs <- function(impute_obj, supply_obj){
  
  # Subset rows with missing values and separate timestamp for merging later
  na_df <- impute_obj %>% group_by(turbine) %>% filter_all(any_vars(is.na(.))) %>%
    separate(timestamp, into = c("year", "month", "day", "hours", "min", "sec"))
  
  # Create object of data containing values to be imputed and separate timestamp
  # for merging later
  right_df <- supply_obj %>%
    separate(timestamp, into = c("year", "month", "day", "hours", "min", "sec"))
  
  # Left join and cleanup
  na_df <- left_join(na_df, right_df, by = c("turbine", "month", "day", "hours", "min")) %>%
    select_if(~!all(is.na(.))) %>%
    select(-c("filename.y", "year.y", "sec.y")) %>%
    unite(timestamp, year.x, month, day, sep = "-") %>%
    unite(hours, hours, min, sec.x, sep = ":") %>%
    unite(timestamp, timestamp, hours, sep = " ") %>%
    mutate(timestamp = ymd_hms(timestamp))
  
  temp_df <- impute_obj
  colnames(na_df) <- colnames(temp_df)
  temp_df <- rbind(temp_df, as.data.frame(na_df)) %>%
    filter(complete.cases(.)) %>% arrange(turbine, timestamp)
  
  temp_df$filename <- paste(temp_df$filename, "imputed", sep = "_")
  
  return(temp_df)
  rm(temp_df)
}


# Scaling for predictors
# Inputs:
# data  = Dataset of predictors

fn_scale_data <- function(data){
  
  scaledf <- data
  
  # Extract timestamp
  ts <- scaledf$ts
  # Extract target feature
  wind_power <- scaledf$active.power.kW
  # Extract predictors for scaling
  predictors <- scaledf %>% select(-active.power.kW, -ts) %>%
    mutate_if(is.numeric, scale)
  
  # Reconstruct original data frame with scaled predictors
  scaledf <- cbind(ts, predictors, wind_power)
}


# Returns a dataframe of unique list of column names that have zero variance for each turbine (WT01, WT02, WT03) across all months (2017 and 2018)
# !!TAKES TIME TO COMPLETE PROCESS FOR 24 MONTHS!! - but only needs to run once
# Inputs: 
# obj_names = Vector of object names (char) as created in fn_3_WT_objs_wtime (e.g. "Jan2017", "Jan2018", "Feb2017", "Feb2018" etc.)
# turbnum   = Number of turbines (default = 3) 
# colidx    = References the column id for "turbine"

fn_get_zero_var <- function(obj_names, turbnum = 3, colidx = 1){
  # Create data frame to append results (rbind)
  nzv_df <- data.frame()
  # Loop through object names
  for (i in 1:length(obj_names)) {
    obj <- get(obj_names[i])        # convert string to object
    col_name <- colnames(obj[colidx])# script to get "turbine" col name (1st col)
    unique_turb <- unique(obj[,colidx])# vector of turbines "WT01", "WT02", "WT03"
    # Loop through each turbine WT01, WT02, WT03
    for(j in 1:turbnum){
      turb_data <- obj %>% filter(get(col_name) == unique_turb[j]) # filter rows of respective WT
      near_zero <- nearZeroVar(turb_data, saveMetrics = TRUE)
      near_zero <- near_zero %>% rownames_to_column() %>% 
        filter(zeroVar) %>% select(rowname)
      nzv_df <- rbind(nzv_df, near_zero)
    }
    
  }
  return(unique(nzv_df))
}

# Returns a 69-var df with zero-var columns removed identified from fn_get_zero_var
# Inputs: 
# obj_names     = Vector of object names (char) as created in fn_3_WT_objs_wtime (e.g. "Jan2017", "Jan2018", "Feb2017", "Feb2018" etc.)
# remove_cols   = Single-column df containing col names to be removed 

fn_remove_0_var_col <- function(obj_names, remove_cols){
  for (i in 1:length(obj_names)) {
    results <- get(obj_names[i]) %>% select(-c(as.character(remove_cols[3:5,1]))) # removing rows 3-5: temp.gearb.bear.3.C, temp.gearb.bear.4.C, and light.redux, 
  }
  return(results)
}

# Carries out averaging operation to convert 10-min data to hourly
# Inputs: 
# data10mins  = Data frame of 10-min data

fn_mean_aggregate <- function(data10min){
  
  avdata_df <- data10min
  avdata_df <- avdata_df %>%
    select(-filename) %>%
    separate(timestamp, into = c("year", "month", "date", "hour", "min", "sec")) %>%
    select(-c(min, sec)) %>%
    group_by(turbine, year, month, date, hour) %>% summarise_all(funs(mean)) %>%
    group_by(year, month, date, hour) %>% summarise_all(funs(max)) %>%
    mutate_at(6:ncol(avdata_df), round, 3) %>%
    unite(timestamp, year, month, date, sep = "-") %>%
    unite(timestamp, timestamp, hour, sep = " ") %>%
    mutate(timestamp = ymd_hm(paste(timestamp, ":00", sep = ""))) %>%
    select(-turbine)
  
  return(avdata_df)
}

# Plots power curve data for WT01, WT02 and WT03 and the average of the # 3
# Inputs: 
# data      = Original Data frame of combined Jan 2017 - Dec 2018 data  # filter beforehand if visualising subset)
# avdata    = Data frame of averaged values
# title     = Define appropriate title (default )
# subtitle  = Define appropriate subtitle (default "Power curves for 3  # wind turbines overlaid")
# fontfam   = "san", "serif" (default) or "mono"

fn_plot_ave_power_curve <- function(data, avdata, title = "Power curves for 3 wind turbines overlaid", subtitle = "", fontfam = "serif", columns = 2){
  ggplot() + 
    geom_point(data = data, 
               aes(wind.speed.MS, active.power.kW, colour = turbine), shape = 1) +
    geom_line(data = power_curve, aes(wind_speed, power), color = "black") +
    geom_point(data = avdata, 
               aes(wind.speed.MS, active.power.kW, colour = "Mean"), shape = 1) +
    scale_y_continuous(breaks = seq(0, 4000, 500)) +
    labs(title = title, subtitle = subtitle,
         x = "Wind speed (m/s)", y = "Wind power (kW)") +
    theme_bw(base_family = fontfam) +
    facet_wrap(filename ~ ., ncol = columns, scales = "free") +
    scale_color_manual(values=c( "black", "#F8766D", "#00BA28", "#619CFF"))
}


# Plots time series data for WT01, WT02 and WT03 and the average of the 3
# Inputs: 
# data      = Original Data frame of combined Jan 2017 - Dec 2018 data (filter beforehand if visualising subset)
# avdata    = Data frame of averaged values
# title     = Define appropriate title (default "10-min average daily   # power output for 3 wind turbines")
# subtitle  = Define appropriate subtitle (default "")
# fontfam   = "san", "serif" (default) or "mono"

fn_plot_ave_time_series <- function(data, avdata, title = "10-min average daily power output for 3 wind turbines", subtitle = "", fontfam = "serif", columns = 2){
  ggplot() +
    geom_line(data = data, 
              aes(x = timestamp, y = active.power.kW, group = turbine, colour = turbine)) +
    geom_line(data = avdata, 
              aes(x = timestamp, y = active.power.kW, colour = "Mean")) +
    scale_y_continuous(breaks = seq(0, 4000, 500)) +
    labs(title = title, subtitle = subtitle,
         x = "Time", y = "Wind power (kW)") +
    theme_bw(base_family = fontfam) +
    facet_wrap(filename ~ ., ncol = columns, scales = "free") +
    scale_color_manual(values=c( "black", "#F8766D", "#00BA28", "#619CFF"))
}


# Returns a correlation table tested against a specific feature with    # coef and p-values
# Inputs:
# obj         = Dataset object to test
# feat        = Selected feature to test against (e.g. "wind.speed.MS")
# cor_thresh  = Default 0.75 and filters out any correlations below this  # value
# pval        = Default p = 0.05
# colidx      = The first column to be tested (4 = blade.angle), ignoring 1,2,3 (filename, turbine, timestamp)

fn_get_correlation <- function(obj, feat, cor_thresh = 0.75, pval = 0.05, colidx = 4){
  start <- colnames(obj[colidx]) #colnames(obj[2])
  print(start)
  end <- colnames(obj[ncol(obj)])
  print(end)
  print(feat)
  
  # Code snippet from https://stackoverflow.com/questions/48041504/calculate-pairwise-correlation-in-r-using-dplyrmutate
  corrs <- obj %>%
    gather(x_var, x_val, feat) %>%
    gather(y_var, y_val, start:end) %>%
    group_by(x_var, y_var) %>%
    summarise(cor_coef = cor.test(x_val, y_val, method = "spearman", exact = FALSE)$estimate,
              p_val = cor.test(x_val, y_val)$p.value) %>%
    arrange(desc(abs(cor_coef))) %>% # added 7/19 to order by absolute value
    filter(p_val < pval & abs(cor_coef) >= cor_thresh) # remove features that are significant and highly correlated
  
  return(corrs)
}

# Set wind farm decimal degree coordinates 
lat <- " " # Set latitude between and inclusive of quotes as “deg min sec”
lon <- " " # Set longitude between and inclusive of quotes as “deg min sec”

# Convert to decimal degrees as required by darksky API
lat_dec <- measurements::conv_unit(lat, "deg_min_sec", "dec_deg")
lon_dec <- measurements::conv_unit(lon, "deg_min_sec", "dec_deg")

# Downloads Dark Sky historical data via API
# Inputs:
# from        = From date
# to          = To date
# by          = "day" default granularity
# lat         = Latitude in decimal degrees
# lon         = Longitude in decimal degrees
# units       = "si" default
# filename    = Name file should be saved as in string (e.g. "01-2017.csv")
# dir         = Folder to save csv in

fn_dark_sky <- function(from, to, by = "day", lat = lat_dec, lon = lon_dec, units = "si", filename, dir = "darksky"){
  time_seq <- seq(ymd(from), ymd(to), by = by)
  month_data <- time_seq %>% map(~get_forecast_for(lat, lon, units = units, .x)) %>%
    map_df("hourly")
  if(dir.exists(dir)){
    write.csv(month_data, paste(dir, filename, sep = "/"))
  }
  else{
    dir.create(paste(getwd(), dir, sep = "/"))
    write.csv(month_data, paste(dir, filename, sep = "/"))
  }
}
# Plots feature relationships (e.g. to check windSpeed vs windGust) to analyse imputation work
# Inputs:
# data        = Dataset object to test
# xfeat       = Selected x-axis feature (e.g. "windSpeed")
# yfeat       = Selected y-axis feature (e.g. "windGust")
# title       = Title of plot

fn_plot_feat_relationship <- function(data, xfeat, yfeat, title = "Feature relationship"){
  ggplot(data = data) +
    geom_point(aes_string(y = yfeat, x = xfeat),
               shape = 1, alpha = 0.3, size = 2, color = "dodgerblue") +
    geom_smooth(aes_string(y = yfeat, x = xfeat)) +
    labs(title = title) +
    theme_bw(base_family = "serif")
}

# Plots actual vs predicted features after imputation work
# Inputs:
# data        = Dataset object to test
# actual      = Actual feature (e.g. "windGust")
# predicted   = Predicted values feature (e.g. "windGust_preds")
# title       = Title of plot (default = "Actual vs. predicted values")
# subtitle    = Subtitle of plot (default = "")

fn_plot_act_vs_pred <- function(data, actual, predicted, 
                                title = "Actual vs. predicted values", subtitle = "",
                                alpha = 0.3){
  ggplot(data = data) +
    geom_point(aes_string(x = actual, y = predicted), 
               shape = 1, alpha = alpha, size = 2, color = "dodgerblue") +
    geom_abline(colour = "black", linetype = 2) +
    labs(title = title, subtitle = subtitle,
         x = paste(actual, "actual", sep = " "), 
         y = paste(actual, "predicted", sep = " ")) +
    theme_bw(base_family = "serif")
}

# Bins predictions into 10 buckets and mean aggregates the values (to be used for plotting)
# Inputs:
# data  = Dataset with actual and predicted values

fn_aggregate_predictions <- function(data){
  temp_df <- data
  colnum <- ncol(temp_df)
  
  ##### START code attribution #####
  ##### (Sengupta, Kumar, & Upadhyay, 2013) #####
  temp_df$bucket <- with(temp_df, cut(temp_df[,colnum], 
                                      breaks = quantile(temp_df[,colnum], probs = seq(0,1,0.1)),
                                      include.lowest = TRUE,
                                      labels = c(1:10)))
  
  temp_df <- aggregate(temp_df[, c(colnames(temp_df)[1], colnames(temp_df)[colnum])],
                       by = list(temp_df$bucket), FUN = mean)
  
  ##### END code attribution #####
  
  return(temp_df)
}


# Plots binned averages
# Inputs:
# data        = Binned dataset object (derived from fn_aggregate_predictions)
# title       = Title of plot

fn_plot_aggregate_predictions <- function(data, title = "Aggregated actual vs. predicted values"){
  
  temp_df <- data
  actual <- colnames(temp_df)[2]
  prediction <- colnames(temp_df)[3]
  ggplot(data = temp_df, aes_string(x = actual, y = prediction)) +
    geom_point(alpha = 1, size = 2, colour = "dodgerblue") +
    geom_abline(colour = "black", lty = 2) +
    labs(title = title, x = paste(actual, "actual", sep = " "), 
         y = paste(actual, "predictions", sep = " ")) +
    theme_bw(base_family = "serif")
}

# Builds keras model
# Inputs:
# ip_shape    = dim(training matrix)[2] = number of predictor/input features
# units1      = Number of nodes in input layer (default = 64)
# units2      = Number of nodes in hidden layer (default = 64)
# op_units    = Number of nodes in output layer (default = 1)
# act         = Activation function (default = "relu", others defined here: https://keras.io/activations/)
# Code attribution for sections within this function: (Falbel, Allaire, & Chollet, 2019)

fn_build_keras_model <- function(ip_shape = 7,
                                 units1 = 64, units2 = 64, op_units = 1, 
                                 act = "relu", opt){
  model <- keras_model_sequential() %>%
    layer_dense(units = units1, activation = act, 
                input_shape = ip_shape) %>%
    layer_dense(units = units2, activation = act) %>%
    layer_dense(units = op_units)
  
  # RMSE custom function
  kRMSE <- custom_metric("rmse_pred", function(y_true, y_pred){
    k_sqrt(k_mean(k_pow((y_pred - y_true), 2)))
  })
  
  # R Squared custom function
  kR2 <- custom_metric("r2_pred", function(y_true, y_pred){
    res <- k_sum(k_square(y_true - y_pred))
    tot <- k_sum(k_square(y_true - k_mean(y_true)))
    return(1 - res/(tot + k_epsilon()))
  })
  
  # MAPE custom function
  kMAPE <- custom_metric("mape_pred", function(y_true, y_pred){
    k_mean(k_abs((y_true - y_pred)/y_true) * 100)
    
  })
  
  model %>% compile(
    loss =  "mse", # want to minimise this
    optimizer = opt,
    metrics = list("mean_absolute_error", kRMSE, kR2, kMAPE)
  )
  
  model
}


# Plots actual vs predicted power values vs wind speed
# Inputs:
# data        = Dataset object
# subtitle    = Subtitle (blank default)
# xfeat       = Selected x-axis feature (e.g. "wind.speed.MS")
# y_act       = Actual power values (e.g. "active.power.kW")
# y_nn        = NN predicted values
# y_rf        = RF predicted values
# y_svm       = SVM predicted values

fn_visualise_pred_power_curve <- function(data, subtitle = "", xfeat, 
                                          y_act, y_nn, y_rf, y_svm){
  ggplot(data) +
    geom_point(aes_string(x = xfeat, y = y_act, colour = "y_act")) +
    geom_point(aes_string(x = xfeat, y = y_nn, colour = "y_nn"), shape = 1) + # keras
    geom_point(aes_string(x = xfeat, y = y_rf, colour = "y_rf"), shape = 1) + # rf
    geom_point(aes_string(x = xfeat, y = y_svm, colour = "y_svm"), shape = 1) + # svm
    scale_y_continuous(breaks = seq(0, 4000, 500)) +
    labs(title = "Actual vs Predicted Power Values", 
         subtitle = subtitle,
         x = "wind speed (m/s)", y = "wind power (kW)") +
    theme_bw(base_family = "serif") +
    scale_colour_manual("Power values",
                        values=c("black", "#F8766D", "#00BA28", "#619CFF"),
                        labels = c("Actual", "ANN", "RF", "SVM"))  
  
}


# Plots actual vs predicted power values vs time
# Inputs:
# data        = Dataset object
# subtitle    = Subtitle (blank default)
# xfeat       = Selected x-axis feature (e.g. "ts")
# y_act       = Actual power values (e.g. "active.power.kW")
# y_nn        = NN predicted values
# y_rf        = RF predicted values
# y_svm       = SVM predicted values

fn_visualise_pred_time_series <- function(data, subtitle = "", xfeat, 
                                          y_act, y_nn, y_rf, y_svm){
  ggplot(data) +
    geom_line(aes_string(x = xfeat, y = y_act, colour = "y_act"), size = 1) +
    geom_line(aes_string(x = xfeat, y = y_nn, colour = "y_nn")) + # keras
    geom_line(aes_string(x = xfeat, y = y_rf, colour = "y_rf")) + # rf
    geom_line(aes_string(x = xfeat, y = y_svm, colour = "y_svm")) + # svm
    scale_y_continuous(breaks = seq(0, 4000, 500)) +
    labs(title = "Actual vs Predicted Power Values", 
         subtitle = subtitle,
         x = "time", y = "wind power (kW)") +
    theme_bw(base_family = "serif") +
    scale_colour_manual("Power values",
                        values=c("black", "#F8766D", "#00BA28", "#619CFF"),
                        labels = c("Actual", "ANN", "RF", "SVM")) 
}

