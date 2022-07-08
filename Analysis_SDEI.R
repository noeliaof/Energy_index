setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./config/ConfigureEnv.R")
ConfigureEnv();

# load data
# general data:energy and meteorology (including HW and CD)
load("data/enerDaily_upIC.Rda")
load("data/enerH_upIC.Rda")


# Remove Montenegro (there was no IC avaible)
enerD_upIC <- enerD_upIC%>%dplyr::filter(country!="Montenegro")
enerH_upIC <- enerH_upIC%>%dplyr::filter(country!="Montenegro")

# Select the variables for the index
nvars <- c("PWS","pw_wind","pw_solar", "res_load_WS") 
# In case we want to use Capacity factors
nvars_cf <- c("cf_wind","cf_solar")

data_sdei <- calculate_energyindex_country(enerD_upIC, method= "fitdis", scale=NULL, nvars)

sdei_6h <- calculate_energyindex_country(enerH_upIC, "fitdis", 6, nvars)
sdei_12h <- calculate_energyindex_country(enerH_upIC, "fitdis",12, nvars)


#################
# Analyse index
#################
# Daily

day_sdei <- data_sdei$SDEI
dft_sdei <- setNames(melt(day_sdei, id=names(day_sdei[[1]])), c(names(day_sdei[[1]]), "country"))

save(dft_sdei, file="data/Output_data/dft_sdei.Rda")
# Plot the index 
plot_sdei(day_sdei$Austria, c("2000","2001","2002"))

##########################
# Some more analysis:
#########################

# See events of low production SDEI < -1.5

mvar <- "pw_wind"
new_dat <- lapply(day_sdei, function(x) {x <- pivot_wider(x, names_from=type, values_from=SDEI)
       x})

x <- lapply(new_dat, def_sdei_ev, mvar, -1.5)
x <- lapply(x, function(x) x[,c("date","ev")])
num_ev <- lapply(x, function(x) sum(x$ev))

# extra-function to assess events
def_sdei_ev <- function(dd, mvar, thresval){
  
  dd <- data.frame(dd)
  dd['ev'] <- as.numeric(dd[[mvar]]< (thresval))
  dd <- dd[,c("date",mvar,"ev")]
  return(dd)
}

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
  
}

