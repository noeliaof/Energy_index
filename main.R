################################################################################
############################### Configuration ##################################
################################################################################

source("./config/ConfigureEnv.R")
#devtools::install_github("noeliaof/SEI")
library(SEI)

################################################################################
################################## Set up ######################################
################################################################################

## load data
load("data/enerH_upIC.Rda") 
enerH_upIC <- subset(enerH_upIC, country != "Montenegro") # remove Montenegro (no IC available)

abbrevs <- get_abbrevs(unique(enerH_upIC$country)) # country abbreviations for plotting

evars <- c("PWS", "res_load_WS") # energy variables

n_year <- length(unique(year(enerH_upIC$date))) # number of years of data

countries <- unique(enerH_upIC$country) # countries
n_cnt <- length(countries) # number of countries


################################################################################
########################### Exploratory analysis ###############################
################################################################################


## plot installed capacities
inst_cap <- enerH_upIC[!duplicated(enerH_upIC$country), c("country", "IC17_wind", "IC17_solar")]
inst_cap <- mutate(inst_cap, 
                   Total = IC17_wind + IC17_solar,
                   wind_prop = IC17_wind/(IC17_wind + IC17_solar),
                   solar_prop = IC17_solar/(IC17_wind + IC17_solar))

plot_ics(inst_cap, names = abbrevs) # bar plot
plot_ics(inst_cap, names = abbrevs, ratio = T) # ratio of installed wind and solar capacities


## visualise annual behaviour of demand, wind production, and solar production

cnts <- c("Germany", "Norway", "Spain")
plot_annual_mean(enerH_upIC, "WD", cnts, title = "Energy demand (GWh)", ylims = c(200, 1600))
plot_annual_mean(enerH_upIC, "pw_wind", cnts, title = "Wind production (GWh)", ylims = c(0, 600))
plot_annual_mean(enerH_upIC, "pw_solar", cnts, title = "Solar production (GWh)", ylims = c(0, 600))


################################################################################
###################### Calculate standardised indices ##########################
################################################################################

## raw data (at hourly, daily, and weekly scales)
raw_1h <- calculate_std_index(enerH_upIC, evars, countries, dist = "none")
raw_1d <- calculate_std_index(enerH_upIC, evars, countries, dist = "none", rescale = "days")
raw_1w <- calculate_std_index(enerH_upIC, evars, countries, dist = "none", rescale = "weeks")

## standardised indices (at hourly, daily, and weekly scales)
sei_1h <- calculate_std_index(enerH_upIC, evars, countries, dist = "empirical")
sei_1d <- calculate_std_index(enerH_upIC, evars, countries, dist = "empirical", rescale = "days")
sei_1w <- calculate_std_index(enerH_upIC, evars, countries, dist = "empirical", rescale = "weeks")


## plot example
plot_example(sei_1d) # without lag
plot_example(sei_1d, lag = T) # with lag


################################################################################
############################# Analyse indices ##################################
################################################################################

cnt <- "Spain" # select country
evar <- "res_load_WS" # select energy variable
year_vec <- seq(2010, 2019) # select range of years


# raw values
labb <- if (evar == "res_load_WS") {"RL (GWh)"} else if (evar == "PWS") {"REP (GWh)"} else {""}

year_ind <- year(index(raw_1h[[cnt]][, evar])) %in% year_vec
plot_sei(raw_1h[[cnt]][year_ind, evar], lab = labb, title = "Hourly") # hourly time series

year_ind <- year(index(raw_1d[[cnt]][, evar])) %in% year_vec
plot_sei(raw_1d[[cnt]][year_ind, evar], lab = labb, title = "Daily") # daily time series

year_ind <- year(index(raw_1w[[cnt]][, evar])) %in% year_vec
plot_sei(raw_1w[[cnt]][year_ind, evar], lab = labb, title = "Weekly") # weekly time series

plot_sei(raw_1d[[cnt]][, evar], lab = labb, type = "hist") # daily histogram


# standardised index values
labb <- if (evar == "res_load_WS") {"SRLI"} else if (evar == "PWS") {"SREPI"} else {""}

year_ind <- year(index(sei_1h[[cnt]][, evar])) %in% year_vec
plot_sei(sei_1h[[cnt]][year_ind, evar], lab = labb, title = "Hourly", ylims = c(-4, 4)) # hourly time series

year_ind <- year(index(sei_1d[[cnt]][, evar])) %in% year_vec
plot_sei(sei_1d[[cnt]][year_ind, evar], lab = labb, title = "Daily", ylims = c(-4, 4)) # daily time series

year_ind <- year(index(sei_1w[[cnt]][, evar])) %in% year_vec
plot_sei(sei_1w[[cnt]][year_ind, evar], lab = labb, title = "Weekly", ylims = c(-4, 4)) # weekly time series

plot_sei(sei_1d[[cnt]][, evar], lab = labb, type = "hist") # daily histogram


################################################################################
############################# Energy droughts ##################################
################################################################################

## Define a Renewable Energy Production Drought as:
# Moderate: -1.64 < SREPI <= -1.28
# Severe: -1.96 < SREPI <= -1.64
# Extreme: SREPI < -1.96

## Define a Renewable Energy Supply Drought as:
# Moderate: 1.28 <= SRLI < 1.64
# Severe: 1.64 <= SRLI < 1.96
# Extreme: SRLI > 1.96


evar <- "PWS"
labb <- if (evar == "res_load_WS") {"Supply"} else if (evar == "PWS") {"Production"} else {""}
threshvals <- qnorm(c(0.9, 0.95, 0.975)) # 1.28, 1.64, 1.96
if (evar == "PWS") threshvals <- -threshvals

# energy drought characteristics
drought_df <- lapply(countries, function(z) get_drought(sei_1d[[z]][, evar], threshvals, higher = (evar != "PWS")))
names(drought_df) <- countries

summer <- month(index(sei_1d[[1]])) %in% 4:9


# drought frequency
num_ev <- sapply(drought_df, function(z) sum(z$dur != 0)/n_year)
summer_ev <- sapply(drought_df, function(z) sum(z$occ[summer])/sum(z$occ)) # proportion in summer
plot_drought_freq(num_ev, summer_ev, names = abbrevs, title = labb)


# drought duration
max_dur <- 25
dur_ev <- sapply(drought_df, function(z) sapply(1:max_dur, function(i) mean(z$dur[z$dur > 0] >= i))) 

cnt <- "Germany"
plot_exc_prob(dur_ev[, countries == cnt], title = labb, xlab = "Duration (days)")


# drought magnitude
max_mag <- 30
mag_ev <- sapply(drought_df, function(z) 1 - ecdf(z$mag[z$mag > 0])(seq(1, max_mag, 0.1)))

cnt <- "Germany"
plot_exc_prob(mag_ev[, countries == cnt], t = seq(1, max_mag, 0.1), title = labb, xlab = "Magnitude")


# correlation between SREPI and SRLI
plot_corr_map(sei_1d, evars, countries = countries)


################################################################################
###################### Change energy mix configuration #########################
################################################################################

ws_ratio_vec <- seq(0, 1, 0.1) # proportion of installed capacity that is wind capacity
threshvals <- qnorm(c(0.9, 0.95, 0.975))

# calculate drought characteristics for different proportions of wind capacity
sei_1d_mix <- change_energy_mix(enerH_upIC, evars, countries[1:10], threshvals, ws_ratio_vec)

# visualise indices for different configurations
cnt <- "Portugal"
evar <- "PWS"

dx <- sei_1d_mix[[11]]$ind[[cnt]][, evar]
year_ind <- year(index(dx)) == 2019
plot_sei(dx[year_ind], lab = "SREPI", ylims = c(-4, 4))

dx <- sei_1d_mix[[1]]$ind[[cnt]][, evar]
plot_sei(dx[year_ind], lab = "SREPI", ylims = c(-4, 4))

dx <- sei_1d_mix[[which(ws_ratio_vec == 0.5)]]$ind[[cnt]][, evar]
plot_sei(dx[year_ind], lab = "SREPI", ylims = c(-4, 4))


# proportion of days in drought state
stat_mat <- sapply(seq_along(ws_ratio_vec), function(i) sei_1d_mix[[i]]$PWS_drought[, "num_ev"])
plot_drought_char_mix(stat_mat, abbrevs[1:10], inst_cap$wind_prop[1:10])


# average magnitude of droughts
stat_mat <- sapply(seq_along(ws_ratio_vec), function(i) sei_1d_mix[[i]]$PWS_drought[, "mag_ev"])
stat_mat[is.nan(stat_mat)] <- stat_mat[is.na(stat_mat)] <- 0 # when no droughts occur, set the average magnitude to zero
plot_drought_char_mix(stat_mat, abbrevs[1:10], inst_cap$wind_prop[1:10], clims = c(0, 10), cbreaks = seq(0, 10))


################################################################################
##################### Change energy storage capabilities #######################
################################################################################

# Denmark is the only country with negative residual loads
for (cnt in countries) print(c(cnt, range(raw_1d[[cnt]]$res_load_WS)))

X_ref <- fortify.zoo(raw_1d[["Denmark"]])
X_ref <- mutate(X_ref, ED = PWS + res_load_WS)

storage_lens <- c(1, 7, 30, 90) # days energy can be stored for
threshvals <- qnorm(c(0.9, 0.95, 0.975))

sei_1d_store <- change_storage(X_ref, threshvals, storage_lens)
names(sei_1d_store) <- storage_lens


## visualise indices in different configurations

# no storage
dx <- sei_1d[["Denmark"]]$res_load_WS
year_ind <- year(index(dx)) == 2019
plot_sei(dx[year_ind], ylims = c(-4, 4))

# one day
dx <- as.xts(sei_1d_store[["1"]]$RL_drought$x,
             sei_1d_store[["1"]]$RL_drought$Index)
plot_sei(dx[year_ind], ylims = c(-4, 4))

# seven days
dx <- as.xts(sei_1d_store[["7"]]$RL_drought$x,
             sei_1d_store[["7"]]$RL_drought$Index)
plot_sei(dx[year_ind], ylims = c(-4, 4))

# thirty days
dx <- as.xts(sei_1d_store[["30"]]$RL_drought$x,
             sei_1d_store[["30"]]$RL_drought$Index)
plot_sei(dx[year_ind], ylims = c(-4, 4))

# ninety days
dx <- as.xts(sei_1d_store[["90"]]$RL_drought$x,
             sei_1d_store[["90"]]$RL_drought$Index)
plot_sei(dx[year_ind], ylims = c(-4, 4))


## difference between indices with and without storage

# one day
dx <- as.xts(sei_1d_store[["1"]]$RL_drought$x, 
             sei_1d_store[["1"]]$RL_drought$Index) - sei_1d[["Denmark"]]$res_load_WS
plot_sei(dx[year_ind], ylims = c(-1.5, 0.5))

# seven days
dx <- as.xts(sei_1d_store[["7"]]$RL_drought$x, 
             sei_1d_store[["7"]]$RL_drought$Index) - sei_1d[["Denmark"]]$res_load_WS
plot_sei(dx[year_ind], ylims = c(-1.5, 0.5))


################################################################################
#################### Analysis of parametric distributions ######################
################################################################################

dists <- c("norm", "tnorm", "lnorm", "logis", "tlogis", "llogis", "exp", "gamma", "weibull") # vector of distributions

raw_list <- list("hourly" = raw_1h, "daily" = raw_1d, "weekly" = raw_1w) # list of raw values

# data frame containing distributions with the lowest AIC value for each country, variable, and timescale
aic_df <- parametric_analysis(evars, times = c("hourly", "daily", "weekly"), countries, dists, raw_list)

