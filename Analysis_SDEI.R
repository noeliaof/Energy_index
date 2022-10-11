setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./config/ConfigureEnv.R")
ConfigureEnv();

#################
# Set up
#################

## load data
load("data/enerH_upIC.Rda")
enerH_upIC <- enerH_upIC%>%dplyr::filter(country!="Montenegro") # Remove Montenegro (there was no IC avaible)


abbrevs <- get_abbrevs(unique(enerH_upIC$country)) # country abbreviations for plotting
n_year <- length(unique(lubridate::year(enerH_upIC$date))) # number of years of data
nvars <- c("PWS", "res_load_WS")


## plot installed capacities (Figure 9)
installed_capacities <- enerH_upIC[!duplicated(enerH_upIC$country), c("country", "IC17_wind", "IC17_solar")] # installed capacities
plot_ics(installed_capacities, names=abbrevs)


## extract data at relevant time scales

# raw data (at hourly, daily, and weekly scales)
raw_1h <- calculate_energy_index(enerH_upIC, method = "none", scale = NULL, nvars)
raw_1d <- calculate_energy_index(enerH_upIC, method = "none", scale = 24, nvars)
raw_1w <- calculate_energy_index(enerH_upIC, method = "none", scale = 7*24, nvars)

# standardised indices (at hourly, daily, and weekly scales)
sdei_1h <- calculate_energy_index(enerH_upIC, method = "empirical", scale = NULL, nvars)
sdei_1d <- calculate_energy_index(enerH_upIC, method = "empirical", scale = 24, nvars)
sdei_1w <- calculate_energy_index(enerH_upIC, method = "empirical", scale = 7*24, nvars)


#################
# Analyse index
#################

### time series

country_str <- "Norway"
variable_str <- "res_load_WS"
year_vec <- seq(2010, 2019)


## example (Figure 1)

plot_example(sdei_1d)


## raw values (figures 2 and 3)

ylabb <- if(variable_str == "res_load_WS"){"RL (GW)"}else if(variable_str == "PWS"){"REP (GW)"}else{""}
plot_time_series(raw_1h, raw_1d, raw_1w, country = country_str, vari = variable_str, year_vec, ylabb)


## index values (figures 2 and 3)

ylabb <- if(variable_str == "res_load_WS"){"SRLI"}else if(variable_str == "PWS"){"SREPI"}else{""}
plot_time_series(sdei_1h, sdei_1d, sdei_1w, country_str, variable_str, year_vec, ylabb)


### distributions (figure 4)

plot_distributions(variable_str)


#################
# Energy droughts
#################

## Define a Renewable Energy Production Drought as:
# Moderate: -1.5 < SREPI <= -1
# Severe: -2 < SREPI <= -1.5
# Extreme: SREPI < -2

## Define a Renewable Energy Supply Drought as:
# Moderate: 1 <= SRLI < 1.5
# Severe: 1.5 <= SRLI < 2
# Extreme: SRLI > 2


variable_str <- "PWS"
title_name <- if(variable_str == "res_load_WS"){"Supply"}else if(variable_str == "PWS"){"Production"}else{""}
threshvals <- c(1, 1.5, 2)*(2*sign(variable_str!="PWS") - 1) # get threshold values to use when defining droughts


### energy drought characteristics

## get data frame of characteristics
sdei_1d_pivot <- lapply(sdei_1d$SDEI, function(x) {data.frame(pivot_wider(x, names_from = type, values_from = SDEI))})
sdei_1d_ev <- lapply(sdei_1d_pivot, def_energy_drought, variable_str, threshvals, higher = variable_str!="PWS")
sdei_1d_ev <- lapply(sdei_1d_ev, function(x){
  month <- lubridate::month(x$date)
  x$seas <- "W"
  x$seas[month %in% seq(4, 9)] <- "S"
  return(x)
}) # add season


## frequency (Figure 6)

num_ev <- sapply(sdei_1d_ev, function(x) sum(x$dur != 0)/n_year) # number of droughts per year in each country

season_ev <- sapply(sdei_1d_ev, function(x) sum(x$occ[x$seas == "W"])/sum(x$occ)) # proportion in each season


plot_drought_freq(num_ev, season_ev, title_name, abbrevs)


## duration (Figure 7)

dur_ev <- lapply(sdei_1d_ev, function(x) summary(x$dur[x$dur > 0])) # duration of droughts in each country

c_lims <- if(variable_str == "res_load_WS"){seq(0, 120, 15)}else if(variable_str == "PWS"){seq(0, 56, 8)} # colour bar breaks


plot_drought_dur(dur_ev, title_name, c_lims, countries = unique(enerH_upIC$country))


## magnitude (Figure 8)

mag_ev <- lapply(sdei_1d_ev, function(x) summary(x$mag[x$mag > 0])) # magnitude of droughts in each country

c_lims <- if(variable_str == "res_load_WS"){seq(0, 120, 20)}else if(variable_str == "PWS"){seq(0, 56, 8)} # colour bar breaks

plot_drought_mag(mag_ev, title_name, c_lims, countries = unique(enerH_upIC$country))


### correlation analysis (Figure 5)

plot_correlation(sdei_1d_pivot, nvars, countries = unique(enerH_upIC$country))


#######################################
# Analysis of parametric distributions:
#######################################

dist_list <- c("norm", "tnorm", "lnorm", "logis", "tlogis", "llogis", "exp", "gamma", "weibull") # vector of distributions

index_list_all <- list("hourly" = raw_1h$SDEI, "daily" = raw_1d$SDEI, "weekly" = raw_1w$SDEI) # list of data frames of raw values

# data frame containing distributions with the lowest AIC value for each country, variable, and timescale (Table 2)
aic_df <- parametric_analysis(var_vec = c("PWS", "res_load_WS"), time_vec = c("hourly", "daily", "weekly"),
                              country_vec = unique(enerH_upIC$country), dist_vec = dist_list,
                              index_list_all)
