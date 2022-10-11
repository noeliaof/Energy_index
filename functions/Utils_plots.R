plot_ics <- function(df, names = NULL){
  # function to plot installed wind and solar capacities in each country
  
  if(is.null(names)){
    names <- df$country
  }
  
  dev.new(width = 8, height = 3, noRStudioGD = TRUE)
  ggplot(data.frame(n = names, c = c(df$IC17_wind, df$IC17_solar), Source = rep(c("Wind", "Solar"), each = nrow(df)))) + 
    geom_bar(aes(x = n, y = c, fill = Source), position = "dodge", stat = "identity") +
    scale_y_continuous(name = "Installed capacity 2017 (MW)", expand = expansion(c(0, 0.05))) + 
    scale_x_discrete(name = "") + 
    theme_bw() + 
    theme(legend.justification = c(1, 1), legend.position = c(0.99, 0.99), legend.title = element_blank())
}


plot_example <- function(sdei_1d){
  # function to plot example of an energy drought
  
  dev.new(width = 5, height = 3, noRStudioGD = TRUE)
  sdei_reduced <- sdei_1d$SDEI[["Germany"]] %>% dplyr::filter(type == "res_load_WS")
  df <- data.frame(val = sdei_reduced$SDEI[14968:14975], dat = sdei_reduced$date[14968:14975])
  ggplot() + geom_line(data = df, mapping = aes(x = dat, y = val), size = 1) + 
    geom_hline(aes(yintercept = c(0, 1, 1.5, 2)), lty = c("dotted", rep("dashed", 3))) +
    geom_rect(aes(xmin = df$dat[1], xmax = df$dat[length(df$dat)], ymin = 0, ymax = 1), fill = "green4", alpha = 0.2) +
    geom_rect(aes(xmin = df$dat[1], xmax = df$dat[length(df$dat)], ymin = 1, ymax = 1.5), fill = "yellow", alpha = 0.2) +
    geom_rect(aes(xmin = df$dat[1], xmax = df$dat[length(df$dat)], ymin = 1.5, ymax = 2), fill = "orange", alpha = 0.2) +
    geom_rect(aes(xmin = df$dat[1], xmax = df$dat[length(df$dat)], ymin = 2, ymax = 2.8), fill = "red", alpha = 0.2) +
    geom_errorbarh(aes(xmin = df$dat[3], xmax = df$dat[7], y = -1.25), height = 0.2) +
    geom_segment(aes(x = df$dat[3:6], y = 0, xend = df$dat[3:6], yend = df$val[3:6]), col = "grey") +
    annotate(geom = "text", x = df$dat[5], y = -1, label = "Duration: 4 days") +
    theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
    scale_x_datetime(name = "Time", expand = c(0, 0)) + 
    scale_y_continuous(name = "SRLI", limits = c(-1.5, 2.8), expand = c(0, 0))
}


plot_sdei <- function(dx, yy = seq(1979, 2019), vari = "SDEI", title = "Hourly"){
  # function to plot a time series of index values in a data frame
  
    date_lims <- c(ISOdatetime(year = yy[1], month = 1L, day = 1L, hour = 0L, min = 0L, sec = 0, tz = "UTC"),
                   ISOdatetime(year = yy[length(yy)]+1, month = 1L, day = 1L, hour = 0L, min = 0L, sec = 0, tz = "UTC"))
    
  p <- dx %>% dplyr::filter(format(date, "%Y") %in% yy) %>% ggplot(aes(x = date, y = SDEI)) +
    geom_ribbon(aes(ymin = pmin(SDEI, 0), ymax = 0), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(ymin = 0, ymax = pmax(SDEI, 0)), fill = "red", alpha = 0.5) +
    scale_x_datetime(name = "Date", expand = c(0, 0), limits = date_lims) + 
    theme_bw() + ggtitle(title) + theme(plot.margin = margin(0.1, 0.5, 0.1, 0.2, "cm"))
  
  if(vari %in% c("SDEI", "SRLI", "SREPI")){
    p <- p + scale_y_continuous(name = vari, limits = c(-4, 4))
  }else{
    p <- p + scale_y_continuous(name = vari, expand = expansion(c(0, 0.1)))
  }
    
  return(p)
}


plot_time_series <- function(df_h, df_d, df_w, country, vari, year_vec = seq(1979, 2019), ylabb = ""){
  # wrapper to plot time series of values in a data frame at hourly, daily, and weekly timescales
  
  sdei_reduced <- df_h$SDEI[[country]] %>% dplyr::filter(type == vari)
  plot_1h <- plot_sdei(sdei_reduced, year_vec, vari = ylabb)
  
  sdei_reduced <- df_d$SDEI[[country]] %>% dplyr::filter(type == vari)
  plot_1d <- plot_sdei(sdei_reduced, year_vec, vari = ylabb, title = "Daily")
  
  sdei_reduced <- df_w$SDEI[[country]] %>% dplyr::filter(type == vari)
  plot_1w <- plot_sdei(sdei_reduced, year_vec, vari = ylabb, title = "Weekly")
  
  # ensure boundaries of the plot are the same
  g1h <- ggplotGrob(plot_1h); g1d <- ggplotGrob(plot_1d); g1w <- ggplotGrob(plot_1w)
  maxWidth <- grid::unit.pmax(g1h$widths[2:3], g1d$widths[2:3], g1w$widths[2:3])
  g1h$widths[2:3] <- maxWidth; g1d$widths[2:3] <- maxWidth; g1w$widths[2:3] <- maxWidth
  
  dev.new(width = 4, height=5, noRStudioGD = TRUE)
  gridExtra::grid.arrange(g1h, g1d, g1w, ncol = 1)
  grid.newpage()
  grid.draw(rbind(g1h, g1d, g1w, size = "last"))
  
}


plot_sdei_dist <- function(dx, yy = seq(1979, 2019), n_bins = 100, title = ""){
  # function to visualise the distribution of values in a data frame
  
  p <- dx %>% dplyr::filter(format(date, "%Y") %in% yy) %>% 
    ggplot() +
    geom_histogram(aes(x = SDEI), col = "black", bins = n_bins, alpha = 0.4) + 
    xlab(title) + scale_y_continuous(name = "Density", expand = expansion(c(0, 0.05))) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank())

  return(p)
  
}


plot_distributions <- function(variable_str){
  # wrapper to plot the histogram of raw and index values in a data frame
  
  ylabb <- if(variable_str == "res_load_WS"){"SRLI"}else if(variable_str == "PWS"){"SREPI"}else{""}
  sdei_reduced <- sdei_1d$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
  plot_ind <- plot_sdei_dist(sdei_reduced, n_bins = 50, title = ylabb)
  
  xlabb <- if(variable_str == "res_load_WS"){"RL (GW)"}else if(variable_str == "PWS"){"REP (GW)"}else{""}
  sdei_reduced <- raw_1d$SDEI[[country_str]] %>% dplyr::filter(type == variable_str)
  plot_raw <- plot_sdei_dist(sdei_reduced, n_bins = 50, title = xlabb)
  
  x_seq <- seq(min(sdei_reduced$SDEI), max(sdei_reduced$SDEI), length=1000)
  y_seq <- sapply(1:1000, function(i) qnorm((sum(sdei_reduced$SDEI <= x_seq[i]) + 1)/(nrow(sdei_reduced) + 2)))
  plot_map <- ggplot(data.frame(x = x_seq, y = y_seq)) + geom_line(aes(x = x, y = y)) + 
    scale_x_continuous(name = xlabb, expand = expansion(c(0, 0))) + ylab(ylabb) + theme_bw()
  
  dev.new(width = 9, height = 2.4, noRStudioGD = TRUE)
  
  gridExtra::grid.arrange(plot_raw, plot_map, plot_ind, nrow = 1)
}


plot_drought_freq <- function(num_ev, season_ev, title_name, abbrevs){
  # function to plot a bar plot of average annual drought frequencies in summer and winter
  
  dev.new(width = 8, height = 3, noRStudioGD = TRUE)
  
  df <- data.frame(n = abbrevs, c = c(num_ev*season_ev, num_ev*(1 - season_ev)), 
                   seas = rep(c("Winter", "Summer"), each = length(abbrevs)))
  ggplot(df) + geom_bar(aes(x = n, y = c, fill = seas), stat = "identity") +
    scale_y_continuous(name = "Annual frequency", limits = c(0, 35), expand = c(0, 0)) + scale_x_discrete(name = "") + 
    theme_bw() + 
    theme(legend.justification = c(1, 1), legend.position = c(0.99, 0.99), legend.title = element_blank()) +
    ggtitle(title_name)
  
}


get_map_data <- function(){
  # function to extract Europe map data
  
  world <- map_data('world')
  europe <- c('UK', 'France', 'Germany', 'Italy', 'Spain', 'Ukraine',
              'Poland', 'Romania', 'Netherlands', 'Belgium',
              'Czech Republic', 'Greece', 'Portugal', 'Sweden',
              'Hungary', 'Belarus', 'Austria', 'Serbia', 'Switzerland',
              'Bulgaria', 'Denmark', 'Finland', 'Slovakia', 'Norway', 
              'Ireland', 'Croatia', 'Moldova', 'Bosnia and Herzegovina',
              'Albania', 'Lithuania', 'Macedonia', 'Slovenia', 'Latvia', 
              'Estonia', 'Montenegro', 'Luxembourg', 'Malta', 'Iceland', 
              'Andorra', 'Monaco', 'Liechtenstein', 'San Marino', 
              'Vatican', 'Kosovo')
  eur <- world[world$region %in% europe, ]
  eur <- eur[!eur$subregion %in% c('Arnoy', 'Froya', 'Hinnoya', 'Jan Mayen', 'Kvaloya', 
                                   'Langoya', 'Mageroya Moskenesoya', 'Seiland', 'Senja', 
                                   'Smola', 'Soroya', 'Svalbard', 'Vannoy', 'Vestvagoy'), ]
  eur$value <- NA
  
  return(eur)
}


plot_drought_dur <- function(dur_ev, title_name, countries, c_lims = seq(0, 20, 4), functional = "Mean"){
  # function to plot average (or another functional) of drought duration in each country on a map
  
  dev.new(width = 3.5, height = 3.5, noRStudioGD = TRUE)
  
  eur <- get_map_data()
  
  df <- data.frame(c = sapply(dur_ev, function(x) x[functional]), country = countries)
  df$country[df$country == "Czech_Republic"] <- "Czech Republic"; df$country[df$country == "United_Kingdom"] <- "UK" 
  for(i in df$country){eur$value[eur$region == i] <- df$c[df$country == i]}
  ggplot(data = eur, mapping = aes(x = long, y = lat, group = group)) +
    coord_map() + geom_polygon(aes(fill = value), colour = 'black') + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank()) +
    scale_fill_fermenter(name = NULL, palette = "YlOrRd", direction = 1, breaks = c_lims) +
    scale_y_continuous(name = NULL, limits = c(35, 65), expand = c(0, 0)) +
    scale_x_continuous(name = NULL, limits = c(-15, 25), expand = c(0, 0)) +
    ggtitle(title_name)
}


plot_drought_mag <- function(mag_ev, title_name, countries, c_lims = seq(0, 20, 4), functional = "Mean"){
  # function to plot average (or another functional) of drought magnitude in each country on a map
  
  dev.new(width = 3.5, height = 3.5, noRStudioGD = TRUE)
  
  eur <- get_map_data()
  
  df <- data.frame(c = sapply(mag_ev, function(x) x[functional]), country = countries)
  df$country[df$country == "Czech_Republic"] <- "Czech Republic"; df$country[df$country == "United_Kingdom"] <- "UK" 
  for(i in df$country){eur$value[eur$region == i] <- df$c[df$country == i]}
  ggplot(data = eur, mapping = aes(x = long, y = lat, group = group)) +
    coord_map() + geom_polygon(aes(fill = value), colour = 'black') + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank()) +
    scale_fill_fermenter(name = NULL, palette = "YlOrRd", direction = 1, breaks = c_lims) +
    scale_y_continuous(name = NULL, limits = c(35, 65), expand = c(0, 0)) +
    scale_x_continuous(name = NULL, limits = c(-15, 25), expand = c(0, 0)) +
    ggtitle(title_name)
}


plot_correlation <- function(df, nvars, countries){
  # function to plot the correlation between the SREPI and SRLI in each country on a map
  
  dev.new(width = 3.5, height = 3.5, noRStudioGD = TRUE)
  
  corr_ind_vec <- sapply(df, function(x) cor(x[, nvars])[nvars[1], nvars[2]])
  
  eur <- get_map_data()
  
  df <- data.frame(c = unname(corr_ind_vec), country = countries)
  df$country[df$country == "Czech_Republic"] <- "Czech Republic"; df$country[df$country == "United_Kingdom"] <- "UK" 
  for(i in df$country){eur$value[eur$region == i] <- df$c[df$country == i]}
  ggplot(data = eur, mapping = aes(x = long, y = lat, group = group)) +
    coord_map() + geom_polygon(aes(fill = value), colour = 'black') + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank()) +
    scale_fill_fermenter(name = NULL, palette = "RdBu", direction = -1, breaks = seq(-1, 1, 0.25), limits = c(-1, 1)) +
    scale_y_continuous(name = NULL, limits = c(35, 65), expand = c(0, 0)) +
    scale_x_continuous(name = NULL, limits = c(-15, 25), expand = c(0, 0))
}
