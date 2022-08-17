setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./config/ConfigureEnv.R")
ConfigureEnv();

## load data
# general data:energy and meteorology (including HW and CD)
#load("data/enerDaily_upIC.Rda")
load("data/enerH_upIC.Rda")


# Remove Montenegro (there was no IC avaible)
#enerD_upIC <- enerD_upIC%>%dplyr::filter(country!="Montenegro")
enerH_upIC <- enerH_upIC%>%dplyr::filter(country!="Montenegro")

# Select the variables for the index
nvars <- c("PWS", "WD", "res_load_WS") 
# In case we want to use Capacity factors
nvars_cf <- c("cf_wind","cf_solar")

n_year <- length(unique(lubridate::year(enerH_upIC$date)))

## country abbreviations for plotting
abbrevs <- get_abbrevs(unique(enerH_upIC$country))


## installed capacities
installed_capacities <- enerH_upIC[!duplicated(enerH_upIC$country), c("country", "IC17_wind", "IC17_solar")]

plot_ics(installed_capacities, names=abbrevs)


## extract data at relevant time scales

#data_sdei <- calculate_energyindex_country(enerD_upIC, method= "fitdis", scale=NULL, nvars)

# raw data (at hourly, daily, weekly and two-weekly scales)
raw_1h <- calculate_energyindex_country(enerH_upIC, method = "none", scale = NULL, nvars)
raw_1d <- calculate_energyindex_country(enerH_upIC, method = "none", scale = 24, nvars)
raw_1w <- calculate_energyindex_country(enerH_upIC, method = "none", scale = 7*24, nvars)
raw_2w <- calculate_energyindex_country(enerH_upIC, method = "none", scale = 2*7*24, nvars)

# standardised indices (at hourly, daily, weekly and two-weekly scales)
sdei_1h <- calculate_energyindex_country(enerH_upIC, method = "empirical", scale = NULL, nvars)
sdei_1d <- calculate_energyindex_country(enerH_upIC, method = "empirical", scale = 24, nvars)
sdei_1w <- calculate_energyindex_country(enerH_upIC, method = "empirical", scale = 7*24, nvars)
sdei_2w <- calculate_energyindex_country(enerH_upIC, method = "empirical", scale = 2*7*24, nvars)


#################
# Analyse index
#################

### Example time series

country_str <- "Portugal"
variable_str <- "res_load_WS"
year_vec <- seq(2010, 2019)

## Raw values

ylabb <- if(variable_str == "res_load_WS"){"RL (GW)"}else if(variable_str == "PWS"){"REP (GW)"}else{""}

sdei_reduced <- raw_1h$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_1h <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)

sdei_reduced <- raw_1d$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_1d <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)

sdei_reduced <- raw_1w$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_1w <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)

sdei_reduced <- raw_2w$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_2w <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)

dev.new(width=4, height=5, noRStudioGD = TRUE)
gridExtra::grid.arrange(plot_1h, plot_1d, plot_1w, plot_2w, ncol = 1)
# overlap in plots


## SDEI

ylabb <- if(variable_str == "res_load_WS"){"SRLI"}else if(variable_str == "PWS"){"SREPI"}else{""}

sdei_reduced <- sdei_1h$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_1h <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)

sdei_reduced <- sdei_1d$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_1d <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)

sdei_reduced <- sdei_1w$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_1w <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)

sdei_reduced <- sdei_2w$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_2w <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)

dev.new(width=7, height=5, noRStudioGD = TRUE)
gridExtra::grid.arrange(plot_1h, plot_1d, plot_1w, plot_2w, ncol = 1)
 # overlap in plots



### Distributions

ylabb <- if(variable_str == "res_load_WS"){"RL (GW)"}else if(variable_str == "PWS"){"REP (GW)"}else{""}
sdei_reduced <- raw_1d$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_raw <- plot_sdei_dist(sdei_reduced, n_bins = 50, title = ylabb)

ylabb <- if(variable_str == "res_load_WS"){"SRLI"}else if(variable_str == "PWS"){"SREPI"}else{""}
sdei_reduced <- sdei_1d$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
plot_ind <- plot_sdei_dist(sdei_reduced, n_bins = 50, title = ylabb)

dev.new(width=7, height=2.4, noRStudioGD = TRUE)
gridExtra::grid.arrange(plot_raw, plot_ind, nrow = 1)



### Droughts

## Define a Renewable Energy Production Drought as:
# Moderate: -1.5 < SREPI <= -1
# Severe: -2 < SREPI <= -1.5
# Extreme: SREPI < -2

## Define a Residual Load Drought as:
# Moderate: 1 <= SRLI < 1.5
# Severe: 1.5 <= SRLI < 2
# Extreme: SRLI > 2


variable_str <- "res_load_WS"
title_name <- if(variable_str == "res_load_WS"){"RL"}else if(variable_str == "PWS"){"REP"}else{""}
threshvals <- c(1, 1.5, 2)*(2*sign(variable_str!="PWS") - 1)


# extra-function to assess events 
def_sdei_ev <- function(dd, mvar, threshval, higher = T, lag = T){
  # ev is event (yes/no)
  # cat is category (0, 1, 2, 3)
  # mag is magnitude
  # dur is duration
  
  # occurrence
  if(higher){
    if(length(threshval) == 1){
      dd['cat'] <- as.numeric(dd[[mvar]] >= threshval)
    }else{
      dd['cat'] <- sapply(1:nrow(dd), function(i){sum(dd[[mvar]][i] >= threshval)})
      
    }
  }else{
    if(length(threshval) == 1){
      dd['cat'] <- as.numeric(dd[[mvar]] <= threshval)
    }else{
      dd['cat'] <- sapply(1:nrow(dd), function(i){sum(dd[[mvar]][i] <= threshval)})
    }
  }
  dd['ev'] <- as.numeric(dd[['cat']] >= 1)
  
  if(lag){
    for(i in 2:nrow(dd)){
      if(dd[['ev']][i] == 0 & dd[['ev']][i-1] == 1 & sign(dd[[mvar]][i]) == sign(dd[[mvar]][i-1])){
        dd[['ev']][i] <- 1
      } 
    }
  }

  # duration and severity
  mag <- abs(dd[[mvar]] - threshval[1])*(dd[['cat']] > 0)
  dd['dur'] <- c(dd[['ev']][1], numeric(nrow(dd)-1))
  dd['mag'] <- c(dd[[mvar]][1]*dd[['ev']][1], numeric(nrow(dd)-1))
  for(i in 2:nrow(dd)){
    if(dd[['ev']][i]){
      dd[['dur']][i] <- dd[['dur']][i-1] + 1
      dd[['dur']][i-1] <- 0
      
      dd[['mag']][i] <- dd[['mag']][i-1] + mag[i]
      dd[['mag']][i-1] <- 0
    }
  }
    

  dd <- dd[,c("date",mvar,"cat","ev","dur","mag")]

  return(dd)
}
# as in hydrology, a drought stops when the index changes sign - need to change code to address this

sdei_1d_pivot <- lapply(sdei_1d$SDEI, function(x) {data.frame(pivot_wider(x, names_from=type, values_from=SDEI))})
sdei_1d_ev <- lapply(sdei_1d_pivot, def_sdei_ev, variable_str, threshvals, higher = variable_str!="PWS")


## frequency analysis

# number of droughts per year
num_ev <- lapply(sdei_1d_ev, function(x) sum(x$dur != 0)/n_year) 
# number of droughts of each category - only the number of category-zero changes by definition of the drought
num_ev_cat <- lapply(sdei_1d_ev, function(x) table(x$cat[as.logical(x$ev)]))
# magnitude of droughts
mag_ev <- lapply(sdei_1d_ev, function(x) summary(x$mag[x$mag > 0]))
# duration of droughts
dur_ev <- lapply(sdei_1d_ev, function(x) summary(x$dur[x$dur > 0]))


## calculate proportion of droughts that occur in Winter
# add season
sdei_1d_ev <- lapply(sdei_1d_ev, function(x){
  month <- lubridate::month(x$date)
  x$seas <- "W"
  x$seas[month %in% seq(4, 9)] <- "S"
  return(x)
})

season_ev <- unlist(lapply(sdei_1d_ev, function(x) sum(x$ev[x$seas == "W"])/sum(x$ev)))
# warmer countries tend to have a lower proportion of RL droughts in Winter
# countries with a higher installed solar capacity than wind capacity tend to have a higher proportion of REP droughts in Winter


## plot number of droughts in each country (plot on map)

dev.new(width=8, height=3, noRStudioGD = TRUE)
df <- data.frame(n=abbrevs, c=c(unlist(num_ev)*season_ev, unlist(num_ev)*(1-season_ev)), 
                 seas=rep(c("W", "S"), each=length(abbrevs)))
ggplot(df) + geom_bar(aes(x = n, y = c, fill = seas), stat="identity") +
  scale_y_continuous(name="Annual frequency", limits = c(0, 35), expand = c(0, 0)) + scale_x_discrete(name="") + 
  theme_bw() + 
  theme(legend.justification=c(1, 1), legend.position=c(0.99, 0.99), legend.title=element_blank()) +
  ggtitle(title_name)


## plot average and maximum drought magnitude in each country

plot_av <- ggplot(data.frame(c=sapply(mag_ev, function(x) x['Mean']), n=abbrevs)) + 
  geom_bar(aes(x = n, y = c), stat="identity") +
  scale_y_continuous(name="Average magnitude", limits = c(0, 30), expand = c(0, 0)) +
  scale_x_discrete(name="") + theme_bw() + ggtitle(title_name)

plot_max <- ggplot(data.frame(c=sapply(mag_ev, function(x) x['Max.']), n=abbrevs)) + 
  geom_bar(aes(x = n, y = c), stat="identity") +
  scale_y_continuous(name="Maximum magnitude", expand = expansion(c(0, 0.05))) + 
  scale_x_discrete(name="") + theme_bw() + ggtitle(title_name)

gridExtra::grid.arrange(plot_av, plot_max)


## plot average and maximum drought duration in each country

plot_av <- ggplot(data.frame(c=sapply(dur_ev, function(x) x['Mean']), n=abbrevs)) + 
  geom_bar(aes(x = n, y = c), stat="identity") +
  scale_y_continuous(name="Average duration (days)", limits = c(0, 120), expand = c(0, 0)) +
  scale_x_discrete(name="") + theme_bw() + ggtitle(title_name)

plot_max <- ggplot(data.frame(c=sapply(dur_ev, function(x) x['Max.']), n=abbrevs)) + 
  geom_bar(aes(x = n, y = c), stat="identity") +
  scale_y_continuous(name="Maximum duration (days)", expand = expansion(c(0, 0.05))) + 
  scale_x_discrete(name="") + theme_bw() + ggtitle(title_name)

gridExtra::grid.arrange(plot_av, plot_max)


## plot average duration vs average severity

df <- data.frame(mag = sapply(mag_ev, function(x) x['Mean']), 
                 dur = sapply(dur_ev, function(x) x['Mean']))
ggplot(df) + geom_point(aes(x = dur, y = mag)) + 
  scale_x_continuous(name="Average duration (days)") + 
  scale_y_continuous(name="Average magnitude") + theme_bw()
# strong positive relationship, as expected 
# longest droughts include more category-zero events, leading to a non-linear relationship


## plot average duration vs frequency

df <- data.frame(freq = unlist(num_ev), 
                 dur = sapply(dur_ev, function(x) x['Mean']))
ggplot(df) + geom_point(aes(x = dur, y = freq)) + 
  scale_x_continuous(name="Average duration (days)") + 
  scale_y_continuous(name="Frequency") + theme_bw() 
# inverse relationship, as expected



### Correlation analysis

corr_ind <- lapply(sdei_1d_pivot, function(x) cor(x[, nvars]))

var1 <- "PWS"; var2 <- "res_load_WS"

corr_ind_vec <- sapply(corr_ind, function(x) x[var1, var2])


ggplot(data.frame(c=unname(corr_ind_vec), n=abbrevs)) + geom_bar(aes(x = n, y = c), stat="identity") +
  scale_y_continuous(limits=c(-1, 1), name="Correlation", expand = c(0, 0)) + scale_x_discrete(name="") + theme_bw()





##########################
# Some more analysis:
#########################
# Daily

day_sdei <- data_sdei$SDEI
dft_sdei <- setNames(melt(day_sdei, id=names(day_sdei[[1]])), c(names(day_sdei[[1]]), "country"))

save(dft_sdei, file="data/Output_data/dft_sdei.Rda")
# Plot the index 
plot_sdei(day_sdei$Austria, c("2000","2001","2002"))

transformfun <- function(x){
  df_ce <- melt(x, id="date")
  df_ce$variable <- NULL
  names(df_ce) <- c("date","value","country")
  df_ce <- df_ce%>%pivot_wider(.,names_from=country, values_from=value)
  df_ce_sum <- df_ce%>% mutate(total = rowSums(across(where(is.numeric))))
  # Order by number of countries involved
  df_ce_sum_order <- df_ce_sum[order(df_ce_sum$total, decreasing = TRUE),]
  df_s        <- df_ce_sum_order[num,]%>%dplyr::select(-total)
  select_date <- df_s$date
  dates_plot <- seq(select_date-offset, select_date+offset, by=1)
  
} # currently not used
