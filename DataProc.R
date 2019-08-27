########################################################################### 
# DataProc.R
# Data pre-processing for SCADA and Dark Sky.
###########################################################################
library(caret)
source("HelperFunctions.R")


## Power curve data and visualisation
wind_speed <- c(4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,
                9,9.5,10,10.5,11,11.5,12,12.5,13,13.5,
                14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,
                19,19.5,20,20.5,21,21.5,22,22.5,23,23.5,
                24,24.5,25)
power_data <- c(75,170,275,380,527,827,1126,1412,1698,1968,
                2237,2476,2715,2911,3106,3200,3285,3360,3400,3400,
                3400,3400,3400,3400,3400,3400,3400,3400,3400,3400,
                3400,3400,3400,3400,3400,3400,3400,3400,3400,3400,
                3400,3400,3400)
power_curve <- data.frame("wind_speed" = wind_speed, "power" = power_data)
power_curve <- power_curve %>% mutate(lo20 = power_data*0.8, hi20 = power_data*1.2)
saveRDS(power_curve, file = "power_curve.rds")

g <- ggplot(power_curve, aes(wind_speed, power)) +
  theme_classic(base_family = "serif") +
  theme(plot.title = element_text(colour = "dodgerblue4")) +
  theme(axis.title.x = element_text(colour = "dodgerblue4")) +
  theme(axis.title.y = element_text(colour = "dodgerblue4")) +
  geom_point(color = "lightblue") + geom_line(color = "dodgerblue") +
  scale_x_continuous(breaks = seq(4, 25, 1)) +
  scale_y_continuous(breaks = seq(0, 3500, 500)) +
  annotate("text", x = c(5, 11, 23, 7), y = c(1000, 1500, 1500, 3300), 
           label = c("cut-in speed", "rated speed", "cut-out speed", "rated power"), 
           size = 3, color = "black") +
  geom_segment(aes(x = 4, y = 900, xend = 4, yend = 100), size = 0.5, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_segment(aes(x = 13, y = 3400, xend = 13, yend = 0), size = 0.5, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_segment(aes(x = 13, y = 3400, xend = 4, yend = 3400), size = 0.5, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_segment(aes(x = 25, y = 3400, xend = 25, yend = 0), size = 0.5, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  ggtitle("Model X Power Curve") + xlab("Wind speed (m/s)") + ylab("Wind power (kW)")

# Plot power curve
g


## Unzip and load data in R
working_dir <- getwd()
folder_name <- "data"
zip_files <- list.files(pattern = "\\.zip$")
zip_output_dir <- paste(working_dir, folder_name, sep = "/")
fn_unzip(zip_files, zip_output_dir)
csv_names <- list.files(zip_output_dir)
csv_names <- csv_names[-c(16, 19, 22, 27)] # remove old 2017 data for Aug, Sept, Oct, Dec 2017
monthly_data <- fn_read_csv(csv_names, zip_output_dir)
monthly_data_count <- length(monthly_data)
monthly_data_names <- names(monthly_data)


# Function to get 3 groups of indices
three_groups <- fn_three_groups(dim(monthly_data[[1]])[2])


## Create SCADA R objects
# Create objects for each month
objs <- character()
for(i in 1:monthly_data_count){
  # Create object name
  monyear <- as.character(zoo::as.yearmon(monthly_data_names[i], "%m-%Y"))
  monyear <- sub(" ", "", monyear)
  # Send to function for transforming, renaming and combining 3 WTs as long data
  # Then create as object
  assign(monyear, fn_3_WT_objs_wtime(monthly_data[[i]], three_groups, datafile = monyear))
  # list of object names for bulk removal rm(list = objs)
  objs <- append(objs, monyear) 
}

# Get all 68 columns and arrange in 4x17 matrix for appendix
scada_cols <- colnames(Jan2017[-c(1,2)]) # excluding filename and turbine in the first and second column
colmatrix <- matrix(scada_cols, ncol = 4)
write.csv(colmatrix, "scada_cols.csv")

# Faceted visualisation for all 24 months
months_combined <- do.call("rbind", c(mget(objs), make.row.names = FALSE))


## Power curve visualisation
# Plot sample months (for inline visualisation)
fn_plot_power_curve(data = months_combined %>% 
                      filter(str_detect(filename, c("Mar", "Jul", "Sep", "Dec"))),
                    subtitle = "March, July, September, December 2017 & 2018")

# Plot Jan - Jun data (for Appendix) but using plot with averages overlaid instead
fn_plot_power_curve(data = months_combined %>%
                      filter(str_detect(filename, c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))), 
                    subtitle = "Jan - Jun 2017/2018")

# Plot Jul - Dec data (for Appendix) but using plot with averages overlaid instead
fn_plot_power_curve(data = months_combined %>%
                      filter(str_detect(filename, c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))), 
                    subtitle = "Jul - Dec 2017/2018")


## Time series visualisation
# Plot sample months (for inline visualisation)
fn_plot_time_series(data = months_combined %>% 
                      filter(str_detect(filename, c("Mar", "Jul", "Sep", "Dec"))),
                    subtitle = "March, July, September, December 2017 & 2018")

# Plot Jan - Jun data (for Appendix) but using plot with averages overlaid instead
fn_plot_time_series(data = months_combined %>%
                      filter(str_detect(filename, c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))), 
                    subtitle = "Jan - Jun 2017/2018")

# Plot Jul - Dec data (for Appendix) but using plot with averages overlaid instead
fn_plot_time_series(data = months_combined %>%
                      filter(str_detect(filename, c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))), 
                    subtitle = "Jul - Dec 2017/2018")


## Analyse missing values
missing_vals <- fn_eval_missing_vals(obj_names = objs)

# Affected months with percentage missing values
missing_vals <- missing_vals %>%
  select(which(!colSums(., na.rm = TRUE) %in% 0)) %>%
  summarise_all(mean) %>% round(., 2)
write.csv(missing_vals, "miss_vals.csv")

# Visualise for Jul2018 as this has highest percentage of missing data
#Amelia::missmap(Jul2018 %>% filter(turbine %in% c("WT01")) %>% select(-c(1,2,3)), # remove cols filename, turbine and timestamp as these are known non-NA cols
#                col = c("grey", "dodgerblue"), margins = c(5,3),
#                y.labels = NULL, y.at = NULL, x.cex = 0.5, main = "Missingness map July 2018")
fn_missmap(monthdata = Jul2018, title = "Missingness map Jul 2018")
fn_missmap(monthdata = Jan2018, title = "Missingness map Jan 2018")
fn_missmap(monthdata = Feb2018, title = "Missingness map Feb 2018")
fn_missmap(monthdata = Mar2018, title = "Missingness map Mar 2018")
fn_missmap(monthdata = Apr2018, title = "Missingness map Apr 2018")
fn_missmap(monthdata = May2017, title = "Missingness map May 2017")
fn_missmap(monthdata = May2018, title = "Missingness map May 2018")
fn_missmap(monthdata = Jun2018, title = "Missingness map Jun 2018")
fn_missmap(monthdata = Sep2017, title = "Missingness map Sep 2017")
fn_missmap(monthdata = Sep2018, title = "Missingness map Sep 2018")
fn_missmap(monthdata = Oct2017, title = "Missingness map Oct 2017")
fn_missmap(monthdata = Oct2018, title = "Missingness map Oct 2018")
fn_missmap(monthdata = Nov2018, title = "Missingness map Nov 2018")

# Get number of missing rows per affected month (rows) by WT (cols)
missing_rows <- fn_missing_rows(colnames(missing_vals))

## Imputation
Jan2018i <- fn_impute_NAs(Jan2018, Jan2017)
Feb2018i <- fn_impute_NAs(Feb2018, Feb2017)
Mar2018i <- fn_impute_NAs(Mar2018, Mar2017)
Apr2018i <- fn_impute_NAs(Apr2018, Apr2017)
May2017i <- fn_impute_NAs(May2017, May2018)
May2018i <- fn_impute_NAs(May2018, May2017)
Jun2018i <- fn_impute_NAs(Jun2018, Jun2017)
Jul2018i <- fn_impute_NAs(Jul2018, Jul2017)
Sep2017i <- fn_impute_NAs(Sep2017, Sep2018)
Sep2018i <- fn_impute_NAs(Sep2018, Sep2017)
Oct2017i <- fn_impute_NAs(Oct2017, Oct2018)
Oct2018i <- fn_impute_NAs(Oct2018, Oct2017)
Nov2018i <- fn_impute_NAs(Nov2018, Nov2017)

# Create vector of imputed object names
objs_imputed <- c("Jan2018i", "Feb2018i", "Mar2018i", "Apr2018i", "May2017i", "May2018i", 
                  "Jun2018i", "Jul2018i", "Sep2017i", "Sep2018i", "Oct2017i", "Oct2018i", "Nov2018i")

# Combining original and imputed July 2018 datasets for visual comparison
impute_jul_df <- rbind(Jul2018, Jul2018i)

# Combining first 4 pairs of imputed months for visualisation
impute_comparison1_df <- rbind(Jan2018, Jan2018i,
                               Feb2018, Feb2018i,
                               Mar2018, Mar2018i,
                               Apr2018, Apr2018i)



# Combining second 4 pairs of imputed months for visualisation
impute_comparison2_df <- rbind(May2017, May2017i,
                               May2018, May2018i,
                               Jun2018, Jun2018i,
                               Jul2018, Jul2018i)

# Combining third 5 pairs of imputed months for visualisation
impute_comparison3_df <- rbind(Sep2017, Sep2017i,
                               Sep2018, Sep2018i,
                               Oct2017, Oct2017i,
                               Oct2018, Oct2018i,
                               Nov2018, Nov2018i)

# Visualise July 2018 imputation comparison
fn_plot_time_series(impute_jul_df, subtitle = "July 2018 imputation comparison", columns = 1)

# Visualise first 4 pairs imputation comparison
fn_plot_time_series(impute_comparison1_df, subtitle = "Imputation comparison Jan - Apr")

# Visualise second 4 pairs imputation comparison
fn_plot_time_series(impute_comparison2_df, subtitle = "Imputation comparison May - Jul")

# Visualise third 5 pairs imputation comparison
fn_plot_time_series(impute_comparison3_df, subtitle = "Imputation comparison Sep - Nov")

# Create vector names of objects (may skip this and used "objs" if imputed datasets are directly manipulated in orginal sets i.e. without "i" suffix)
objs_complete_cases <- c("Jan2017", "Jan2018i", "Feb2017", "Feb2018i", "Mar2017", "Mar2018i", "Apr2017", "Apr2018i", "May2017i", "May2018i", "Jun2017", "Jun2018i", "Jul2017", "Jul2018i", "Aug2017", "Aug2018", "Sep2017i", "Sep2018i", "Oct2017i", "Oct2018i", "Nov2017", "Nov2018i", "Dec2017", "Dec2018")

# Check if all have been imputed, i.e. no missing values
for(i in 1:length(objs_complete_cases)){
  print(paste(objs_complete_cases[i], anyNA(get(objs_complete_cases[i])), sep = ": "))
}

# Find the zero variance columns from all 24 months colidx corresponds to "turbine" column id (note: long processing time)
zero_var_cols <- fn_get_zero_var(obj_names = objs_complete_cases, colidx = 2)

# Remove the zero var cols
for(i in 1:monthly_data_count){
  assign(objs_complete_cases[i], fn_remove_0_var_col(objs_complete_cases[i], zero_var_cols))
}

## Averaging operation
objs_hourly <- character()# Create averaged objects for each month
for(i in 1:length(objs_complete_cases)){
  # Create filename
  fname <- substr(objs_complete_cases[i], start = 1, stop = 7)
  # Create object name
  objname <- paste(fname, "av", sep = "")
  # Get data frame
  data10min <- get(objs_complete_cases[i])
  # Send to function for averaging, then create as object
  assign(objname, fn_mean_aggregate(data10min = data10min)) # to check differences in mean/max and mean/mean operations, change in this function
  temp_df <- get(objname)
  firstcol <- data.frame("filename" = rep(fname, nrow(temp_df)))
  temp_df <- cbind(firstcol, temp_df)
  assign(objname, temp_df)
  rm(temp_df)
  # list of object names for bulk removal rm(list = objs_hourly)
  objs_hourly <- append(objs_hourly, objname) 
}
# Remove "_imputed" suffix from filename in imputed objects (to help with visual overlay of orignal/mean)
for (i in 1:length(objs_imputed)) {
  fname <- substr(objs_imputed[i], start = 1, stop = 7)
  temp_obj <- get(objs_imputed[i])
  temp_obj$filename <- fname
  assign(objs_imputed[i], temp_obj)
  rm(temp_obj)
}

orset <- rbind(Jan2017, Jan2018i, Feb2017, Feb2018i, Mar2017, Mar2018i,
               Apr2017, Apr2018i, May2017i, May2018i, Jun2017, Jun2018i,
               Jul2017, Jul2018i, Aug2017, Aug2018, Sep2017i, Sep2018i,
               Oct2017i, Oct2018i, Nov2017, Nov2018i, Dec2017, Dec2018)

avset <- rbind(Jan2017av, Jan2018av, Feb2017av, Feb2018av, Mar2017av, Mar2018av, Apr2017av, Apr2018av, May2017av, May2018av, Jun2017av, Jun2018av, Jul2017av, Jul2018av, Aug2017av, Aug2018av, Sep2017av, Sep2018av, Oct2017av, Oct2018av, Nov2017av, Nov2018av, Dec2017av, Dec2018av)

# Visualise power curves with average overlaid

# Plot Dec 2018 sample (for inline visualisation)
fn_plot_ave_power_curve(data = orset %>% 
                          filter(str_detect(filename, c("Dec2018"))),
                        avdata = avset %>% 
                          filter(str_detect(filename, c("Dec2018"))),
                        title = "Power curves for 3 wind turbines and hourly enhanced mean overlaid",
                        subtitle = "December 2018")

# Plot Jan - Jun data (for Appendix)
fn_plot_ave_power_curve(data = orset %>%
                          filter(str_detect(filename, c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))), 
                        avdata = avset %>%
                          filter(str_detect(filename, c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))),
                        title = "Power curves for 3 wind turbines and hourly enhanced mean overlaid",
                        subtitle = "Jan - Jun 2017/2018")

# Plot Jul - Dec data (for Appendix)
fn_plot_ave_power_curve(data = orset %>%
                          filter(str_detect(filename, c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))), 
                        avdata = avset %>%
                          filter(str_detect(filename, c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))),
                        title = "Power curves for 3 wind turbines and hourly enhanced mean overlaid",
                        subtitle = "Jul - Dec 2017/2018")

# Visualise time series with average overlaid

# Plot sample months (for inline visualisation)
fn_plot_ave_time_series(data = orset %>% 
                          filter(str_detect(filename, c("Dec2018"))),
                        avdata = avset %>% 
                          filter(str_detect(filename, c("Dec2018"))),
                        title = "10-min average daily power output for 3 wind turbines \n\ and hourly enhanced mean overlaid", subtitle = "December 2018")
# Plot Jan - Jun data (for Appendix)
fn_plot_ave_time_series(data = orset %>%
                          filter(str_detect(filename, c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))), 
                        avdata = avset %>%
                          filter(str_detect(filename, c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))),
                        title = "10-min average daily power output for 3 wind turbines and hourly enhanced mean overlaid",
                        subtitle = "Jan - Jun 2017/2018")

# Plot Jul - Dec data (for Appendix)
fn_plot_ave_time_series(data = orset %>%
                          filter(str_detect(filename, c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))), 
                        avdata = avset %>%
                          filter(str_detect(filename, c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))),
                        title = "10-min average daily power output for 3 wind turbines and hourly enhanced mean overlaid",
                        subtitle = "Jul - Dec 2017/2018")


# Get variables that are over 0.7 correlated with wind.Speed.MS
speed_cor <- fn_get_correlation(obj = avset, 
                                feat = "wind.speed.MS", 
                                cor_thresh = 0.7, 
                                colidx = 3) # start from blade.angle

speed_cor <- speed_cor %>% filter(y_var != "active.power.kW")
remove_vars <- as.vector(speed_cor$y_var) # get the variables to be removed
write.csv(speed_cor, "scada_windspeed_cor.csv")


avset <- avset %>% select(-remove_vars)

# Keep features significantly correlated with active.power.kW
power_cor <- fn_get_correlation(obj = avset, 
                                feat = "active.power.kW", 
                                cor_thresh = 0, 
                                colidx = 3) # start from blade.angle

keep_vars <- as.vector(power_cor$y_var) # get the variables to be kept

avset <- avset %>% select(filename, timestamp, keep_vars, active.power.kW)

## Remove highly correlated predictors (excluding wind.Speed.MS)
corr_data <- avset %>% select(-c(filename, timestamp, wind.speed.MS, active.power.kW))
corr_matrix <- cor(corr_data, method = "spearman")
highly_correlated <- findCorrelation(corr_matrix, cutoff = 0.75)
remove_vars <- colnames(corr_data)[highly_correlated]
##

avset <- avset %>% select(-remove_vars)

corr_speed <- avset %>% select(-c(filename, timestamp))

# Wind Speed correlation visualisation
p.mat <- corrplot::cor.mtest(corr_speed)$p
corrplot::corrplot(cor(corr_speed, method = "spearman"), method = "color",
                   type = "upper", number.cex = .3,
                   addCoef.col = "black",
                   tl.col = "black", tl.srt = 90, tl.cex = .7,
                   p.mat = p.mat, sig.level = 0.05, #insig = "blank",
                   diag = FALSE)

## Dark Sky data processing

# 2017
fn_dark_sky(from = "2017-01-01", to = "2017-01-31", filename = "01-2017.csv") # [18,744] NA Pressure
fn_dark_sky(from = "2017-02-01", to = "2017-02-28", filename = "02-2017.csv") # [18,672]
fn_dark_sky(from = "2017-03-01", to = "2017-03-31", filename = "03-2017.csv") # [17,743]
fn_dark_sky(from = "2017-04-01", to = "2017-04-30", filename = "04-2017.csv") # [17,720]
fn_dark_sky(from = "2017-05-01", to = "2017-05-31", filename = "05-2017.csv") # [17,744]
fn_dark_sky(from = "2017-06-01", to = "2017-06-30", filename = "06-2017.csv") # [17,720]
fn_dark_sky(from = "2017-07-01", to = "2017-07-31", filename = "07-2017.csv") # [17,744]
fn_dark_sky(from = "2017-08-01", to = "2017-08-31", filename = "08-2017.csv") # [17,744]
fn_dark_sky(from = "2017-09-01", to = "2017-09-30", filename = "09-2017.csv") # [17,720]
fn_dark_sky(from = "2017-10-01", to = "2017-10-31", filename = "10-2017.csv") # [17,745] dupe
fn_dark_sky(from = "2017-11-01", to = "2017-11-30", filename = "11-2017.csv") # [18,720]
fn_dark_sky(from = "2017-12-01", to = "2017-12-31", filename = "12-2017.csv") # [18,744]

# 2018
fn_dark_sky(from = "2018-01-01", to = "2018-01-31", filename = "01-2018.csv") # [18,744]
fn_dark_sky(from = "2018-02-01", to = "2018-02-28", filename = "02-2018.csv") # [18,672]
fn_dark_sky(from = "2018-03-01", to = "2018-03-31", filename = "03-2018.csv") # [18,743]
fn_dark_sky(from = "2018-04-01", to = "2018-04-30", filename = "04-2018.csv") # [17,720]
fn_dark_sky(from = "2018-05-01", to = "2018-05-31", filename = "05-2018.csv") # [17,744]
fn_dark_sky(from = "2018-06-01", to = "2018-06-30", filename = "06-2018.csv") # [14,711] missing pressure
fn_dark_sky(from = "2018-07-01", to = "2018-07-31", filename = "07-2018.csv") # [19,744] NA pressure
fn_dark_sky(from = "2018-08-01", to = "2018-08-31", filename = "08-2018.csv") # [18,744]
fn_dark_sky(from = "2018-09-01", to = "2018-09-30", filename = "09-2018.csv") # [18,720]
fn_dark_sky(from = "2018-10-01", to = "2018-10-31", filename = "10-2018.csv") # [19,745] dupe
fn_dark_sky(from = "2018-11-01", to = "2018-11-30", filename = "11-2018.csv") # [19,720]
fn_dark_sky(from = "2018-12-01", to = "2018-12-31", filename = "12-2018.csv") # [19,744]

####

# Reading 24 dark sky csv file into a list of 24
folder_name <- "darksky"
darksky_files <- paste(working_dir, folder_name, sep = "/")
csv_names <- list.files(darksky_files)
monthly_ds_data <- fn_read_darksky_csv(csv_names, darksky_files)
monthly_ds_data_count <- length(monthly_ds_data)
monthly_ds_data_names <- names(monthly_ds_data)


# Operation for creating 24 dark sky R objects (e.g. Jul2018ds, Aug2018ds, Sep2018ds)
# WARNINGS ARE EXPECTED

objs_ds <- character()
cols_df <- data.frame()
dim_df <- data.frame()
for(i in 1:monthly_ds_data_count){
  # Create object name
  monyear <- as.character(zoo::as.yearmon(monthly_ds_data_names[i], "%m-%Y"))
  monyear <- sub(" ", "", monyear)
  monyear_df <- data.frame("month" = monyear) # create month name for first column when analysing column names
  filename_col <- data.frame("month" = rep(monyear, nrow(monthly_ds_data[[i]]))) # create filnemae column (e.g. "Jan2017")
  monyear <- paste(monyear, "ds", sep = "")
  # Then create as object
  temp_df <- monthly_ds_data[[i]]
  temp_df <- temp_df %>% select(-1) # Remove 1st column X (id 1 - n)
  # Get dims of each month
  rowcol <- cbind(monyear_df, data.frame("rows" = nrow(temp_df), "columns" = ncol(temp_df)))
  dim_df <- rbind(dim_df, rowcol)
  # Get colnames of each month
  cols_df <- bind_rows(cols_df, cbind(monyear_df, t(as.data.frame(colnames(temp_df)))))
  temp_df[,1] <- ymd_hms(temp_df[,1]) # Time becomes 1st column now
  temp_df <- cbind(filename_col, temp_df) # Put filename in first column followed by Time etc
  temp_df <- temp_df %>% select(., -matches("precipt|precipa|ozone"), 
                                -c("summary", "icon", "cloudCover", "uvIndex")) %>%
    mutate_if(is.factor, as.character)
  assign(monyear, temp_df)
  # Create data frame of all 24 months column names for analysis
  rm(temp_df, rowcol)
  # list of object names for bulk removal rm(list = objs_ds)
  objs_ds <- append(objs_ds, monyear) 
}

# Get Jun 2017 and 2018 to make equal rows

setA <- Jun2017ds %>%
  separate(time, into = c("year", "month", "day", "hours", "min", "sec"))
setB <- Jun2018ds %>%
  separate(time, into = c("year", "month", "day", "hours", "min", "sec"))

# Find the June 2018 missing dates in June 2017
setJoin <- left_join(setA, setB, by = c("month", "day", "hours", "min")) %>%
  unite(time, year.x, month, day, sep = "-") %>%
  unite(hours, hours, min, sec.x, sep = ":") %>%
  unite(time, time, hours, sep = " ") %>%
  mutate(time = ymd_hms(time)) %>%
  filter(is.na(year.y)) %>%
  select(time)

Jun2017_subset <- left_join(setJoin, Jun2017ds, by = "time") %>%
  mutate(time = ymd_hms(format(time, "2018-%m-%d %H:%M:%S"))) %>%
  mutate(month = "Jun2018")

Jun2018ds <- bind_rows(Jun2018ds, Jun2017_subset)


# Get full column names (19 in total)
# WARNING IS EXPECTED because 19 not cleanly divisible by 5 cols
ds_cols <- matrix(unique(as.vector(unname(as.matrix(cols_df[,-1] %>% na.omit())))), ncol = 5)
write.csv(ds_cols, "darksky_colnamelist.csv")
# Missingness analysis of darksky for 24 months
combined_ds <- bind_rows(mget(objs_ds))
Amelia::missmap(combined_ds, y.labels = FALSE, y.at = NULL, 
                col = c("grey", "dodgerblue"),
                main = "Dark Sky Missingness Map", x.cex = .6)

# Find percentage missing in combined dark sky
perc_missing_ds <- as.data.frame(combined_ds %>% 
                                   summarise_all(funs(round(100*mean(is.na(.)), 3))))
write.csv(t(perc_missing_ds), "darksky_perc_miss.csv")

# Impute precipProbability and precipIntensity with 0
combined_ds <- combined_ds %>% 
  separate(time, into = c("year", "month", "day", "hours", "min", "sec")) %>%
  mutate_at(vars(precipProbability, precipIntensity), ~replace_na(., 0))

# Impute windSpeed, windBearing and visibility with median
combined_ds <- combined_ds %>%
  mutate(windSpeed = replace(windSpeed, is.na(windSpeed), median(windSpeed, na.rm = TRUE))) %>%
  mutate(windBearing = replace(windBearing, is.na(windBearing), median(windBearing, na.rm = TRUE))) %>%
  mutate(visibility = replace(visibility, is.na(visibility), median(visibility, na.rm = TRUE)))

# Create correlation data frame, removing min and sec which are 0 for all obs
corr_ds <- combined_ds %>% select(-c(min, sec)) %>% 
  mutate_if(is.character, as.numeric) # because timestamp is separated into chracters

# Dark Sky correlation visualisation
p.mat <- corrplot::cor.mtest(corr_ds %>% na.omit())$p
corrplot::corrplot(cor(corr_ds %>% na.omit()), method = "color",
                   type = "upper", number.cex = .7,
                   addCoef.col = "black",
                   tl.col = "black", tl.srt = 90, tl.cex = .7,
                   p.mat = p.mat, sig.level = 0.05, #insig = "blank",
                   diag = FALSE)

# Get descending order of features significantly correlated with pressure
corr_order <- corr_ds %>%
  gather(x_var, x_val, "pressure") %>%
  gather(y_var, y_val, precipIntensity:visibility) %>%
  group_by(x_var, y_var) %>%
  summarise(cor_coef = cor.test(x_val, y_val)$estimate,
            p_val = cor.test(x_val, y_val)$p.value) %>%
  arrange(desc(abs(cor_coef))) %>% # added 7/19 to order by absolute value
  filter(p_val < 0.05 & abs(cor_coef) >= 0)


fn_plot_feat_relationship(data = combined_ds, xfeat = "windSpeed", yfeat = "windGust")

# Remove rows with windGust = NA from dataset
windGustdata <- combined_ds %>% select(windGust, windSpeed) %>% 
  filter(!is.na(windGust))

# Get training and testing datasets
set.seed(123)
inTrain <- windGustdata$windGust %>% createDataPartition(p = 0.7, list = FALSE)
myTraining <- windGustdata[inTrain,]
myTesting <- windGustdata[-inTrain,]

windGustModel <- lm(windGust ~ windSpeed, data = myTraining)
windGust_preds <- predict(windGustModel, newdata = myTesting)
windGustResults <- cbind(myTesting, windGust_preds)

# RMSE
windGustRMSE <- sqrt(mean(windGustModel$residuals^2))

# % prediction accuracy = 39.9%
(windGustRMSE/mean(myTraining$windGust))*100


fn_plot_act_vs_pred(data = windGustResults, actual = "windGust", "windGust_preds")


windGustResults <- fn_aggregate_predictions(data = windGustResults)

fn_plot_aggregate_predictions(data = windGustResults)


# Impute windGust values
combined_ds <- combined_ds %>% ungroup() %>%
  mutate(windGust_preds = round(predict(windGustModel, .),2)) %>%
  mutate(windGust = ifelse(is.na(windGust), windGust_preds, windGust)) %>%
  select(-windGust_preds)


# Now to predict for pressure

fn_plot_feat_relationship(data = combined_ds, xfeat = "precipProbability", yfeat = "pressure")
fn_plot_feat_relationship(data = combined_ds, xfeat = "windGust", yfeat = "pressure")
fn_plot_feat_relationship(data = combined_ds, xfeat = "humidity", yfeat = "pressure")
fn_plot_feat_relationship(data = combined_ds, xfeat = "apparentTemperature", yfeat = "pressure")

pressure_data <- combined_ds %>% filter(!is.na(pressure)) %>%
  mutate_if(is.character, as.numeric) %>% select(-c(min, sec))

pressure_vals <- pressure_data$pressure
pressure_data <- pressure_data %>% select(-pressure) %>% mutate_if(is.numeric, scale)
pressure_data <- cbind(pressure_vals, pressure_data)


# Get training and testing datasets
set.seed(123)
inTrain <- pressure_data$pressure_vals %>% createDataPartition(p = 0.7, list = FALSE)
myTraining <- pressure_data[inTrain,]
myTesting <- pressure_data[-inTrain,]

train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)

pressureModel <- train(pressure_vals ~ ., data = myTraining, method = "knn", trControl = train_control, importance = T)

pressure_preds <- predict(pressureModel, newdata = myTesting)
pressureResults <- cbind(myTesting, pressure_preds)

# Find results from best tune
idx_best <- which(pressureModel$results$k == pressureModel$bestTune)
# RMSE
pressureRMSE <- pressureModel$results$RMSE[idx_best]

# % prediction accuracy = 0.56%
(pressureRMSE/mean(myTraining$pressure_vals))*100

fn_plot_act_vs_pred(data = pressureResults, actual = "pressure_vals", predicted = "pressure_preds")

pressureResults <- fn_aggregate_predictions(data = pressureResults)

fn_plot_aggregate_predictions(data = pressureResults)

scaled_ds <- combined_ds %>% select(-pressure) %>%
  mutate_if(is.character, as.numeric) %>% select(-c(min, sec)) %>%
  mutate_if(is.numeric, scale)  

new_pressure_pred <- round(predict(pressureModel, newdata = scaled_ds), 2)


for(i in 1:nrow(combined_ds)){
  if(is.na(combined_ds$pressure[i])){
    combined_ds$pressure[i] <- new_pressure_pred[i]
  }else{
    combined_ds$pressure[i] <- combined_ds$pressure[i]
  }
}

combined_ds <- combined_ds %>% 
  unite(timestamp, year, month, day, sep = "-") %>%
  unite(hours, hours, min, sec, sep = ":") %>%
  unite(timestamp, timestamp, hours, sep = " ") %>%
  mutate(timestamp = ymd_hms(timestamp))

# Check if any NA
#anyNA(combined_ds) = FALSE


##### COMBINE SCADA AND DARK SKY ########################################

testA <- avset
testA %>% group_by(timestamp) %>% 
  summarise(n = n()>1) %>% 
  filter(n == TRUE) %>% nrow(.)
testA <- testA[order(testA[,'timestamp'],-testA[,'active.power.kW']),] # keep the higher power
testA <- testA[!duplicated(testA$timestamp),]

testB <- combined_ds
testB %>% group_by(timestamp) %>% 
  summarise(n = n()>1) %>% 
  filter(n == TRUE) %>% nrow(.)
testB <- testB[!duplicated(testB$timestamp),]


weather_data <- left_join(testA, testB, by = "timestamp")
missing_hours <- weather_data[!complete.cases(weather_data), ][,2] # find hours with missing values

# There are 3 hours with missing values: 26 Mar 2017 1AM; 25 Mar 2018 1AM; 1 Jan 2018 12AM
# 1st handling - get DS values from 26 Mar 2018 1AM to impute into 26 Mar 2017 1AM
mar26_1st <- weather_data %>%
  filter(timestamp == missing_hours[1]) %>%
  select(1:24)
mar26_2nd <- weather_data %>% 
  filter(timestamp == missing_hours[1] %m+% years(1)) %>%
  select(25:ncol(.))
mar26 <- cbind(mar26_1st, mar26_2nd)


# 2st handling - get DS values from 25 Mar 2017 1AM to impute into 25 Mar 2018 1AM
mar25_1st <- weather_data %>%
  filter(timestamp == missing_hours[2]) %>%
  select(1:24)
mar25_2nd <- weather_data %>% 
  filter(timestamp == missing_hours[2] %m-% years(1)) %>%
  select(25:ncol(.))
mar25 <- cbind(mar25_1st, mar25_2nd)

# Finally, rbind 1st and 2nd handling to weather_data, 
# remove last row of 1 Jan 2019, and remove NA

weather_data <- weather_data %>% bind_rows(., mar26, mar25) %>%
  filter(timestamp != missing_hours[3]) %>% drop_na()

weather_data <- weather_data %>% mutate(ts = timestamp) %>%
  separate(timestamp, into = c("year", "month", "day", "hour", "min", "sec")) %>%
  mutate_if(is.character, as.numeric) %>%
  select(-c(min, sec))

# Save the final dataset to file
saveRDS(weather_data, file = "weather_data.rds")

