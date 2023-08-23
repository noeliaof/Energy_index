
plot_ics <- function(df, names = NULL, ratio = F) {
  # plot installed wind and solar capacities in each country
  
  if (is.null(names)) {
    names <- df$country
  }
  
  if (ratio) {
    df <- data.frame(n = names, 
                     prop = c(df$wind_prop, df$solar_prop),
                     Source = rep(c("Wind", "Solar"), each = nrow(df)))
    p <- ggplot(df) +
      geom_bar(aes(x = n, y = prop, fill = Source), stat = "identity") +
      scale_y_continuous(name = "Proportion of installed capacities", expand = c(0, 0)) + 
      scale_x_discrete(name = "") + 
      theme_bw() + 
      theme(legend.title = element_blank())
  } else {
    p <- ggplot(data.frame(n = names, c = c(df$IC17_wind, df$IC17_solar), 
                           Source = rep(c("Wind", "Solar"), each = nrow(df)))) + 
      geom_bar(aes(x = n, y = c, fill = Source), position = "dodge", stat = "identity") +
      scale_y_continuous(name = "Installed capacity 2017 (MW)", expand = expansion(c(0, 0.05))) + 
      scale_x_discrete(name = "") + 
      theme_bw() + 
      theme(legend.justification = c(1, 1), legend.position = c(0.99, 0.99), legend.title = element_blank())
  }
  
  return(p)
}


plot_annual_mean <- function(data, evar, cnts, scale = "days", title = NULL, ylims = NULL) {
  # plot time series of annual mean of a variable
  
  if (!is.null(scale)) {
    ener_ind <- list()
    for (i in 1:length(cnts)) {
      X <- data %>% filter(country == cnts[i]) %>% dplyr::select(date, all_of(evar))
      dat.xts <- xts(X[[evar]], as.POSIXct(X[["date"]]))
      if (scale == "days") {
        dat.xts <- apply.daily(dat.xts, "sum")
      } else if (scale == "weeks") {
        dat.xts <- apply.weekly(dat.xts, "sum")
      } else{
        warning("scale not recognised")
      }
      ener_ind[[i]] <- dat.xts
    }
    names(ener_ind) <- cnts
    ener_ind <- do.call(merge.xts, ener_ind)
  }
  
  z <- aggregate(ener_ind, yday(index(ener_ind)), mean, na.rm = TRUE)
  df <- data.frame(val = as.vector(z), 
                   c = rep(cnts, each = 366), 
                   doy = index(ener_ind)[1:366])
  p <- ggplot(df) + geom_line(aes(x = doy, y = val, col = c), linewidth = 1) +
    scale_y_continuous(name = title, limits = ylims) +
    scale_x_datetime(name = NULL, expand = c(0, 0)) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')
  
  return(p)
}


plot_example <- function(data, lag = F) {
  # plot example of an energy drought

  data_reduced <- data$Germany$res_load_WS
  df <- data.frame(val = unname(data_reduced[14968:14975]), date = index(data_reduced)[14968:14975])
  p <- ggplot() + geom_line(data = df, mapping = aes(x = date, y = val), linewidth = 1) + 
    geom_hline(aes(yintercept = c(0, 1.28, 1.64, 1.96)), lty = c("dotted", rep("dashed", 3))) +
    geom_rect(aes(xmin = df$date[1], xmax = df$date[nrow(df)], ymin = 1.28, ymax = 1.64), fill = "yellow", alpha = 0.2) +
    geom_rect(aes(xmin = df$date[1], xmax = df$date[nrow(df)], ymin = 1.64, ymax = 1.96), fill = "orange", alpha = 0.2) +
    geom_rect(aes(xmin = df$date[1], xmax = df$date[nrow(df)], ymin = 1.96, ymax = 2.8), fill = "red", alpha = 0.2) +
    theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
    scale_x_datetime(name = "Date", expand = c(0, 0)) + 
    scale_y_continuous(name = "SRLI", limits = c(-1.5, 2.8), expand = c(0, 0))
  
  if (lag) {
    p <- p + geom_rect(aes(xmin = df$date[1], xmax = df$date[nrow(df)], ymin = 0, ymax = 1.28), 
                       fill = "green4", alpha = 0.2) +
      geom_errorbarh(aes(xmin = df$date[3], xmax = df$date[7], y = -1.25), height = 0.2) +
      geom_segment(aes(x = df$date[3:6], y = 0, xend = df$date[3:6], yend = df$val[3:6]), col = "grey") +
      annotate(geom = "text", x = mean(c(df$date[4], df$date[6])), y = -1, label = "Duration: 4 days")
  } else {
    p <- p + geom_errorbarh(aes(xmin = df$date[3], xmax = df$date[6], y = -1.25), height = 0.2) +
      geom_segment(aes(x = df$date[3:5], y = 0, xend = df$date[3:5], yend = df$val[3:5]), col = "grey") +
      annotate(geom = "text", x = mean(c(df$date[4], df$date[5])), y = -1, label = "Duration: 3 days")
  }
  
  return(p)
}


plot_drought_freq <- function(num_ev, summer_ev, names, title = ""){
  # bar plot of average annual drought frequencies in summer and winter
  
  df <- data.frame(n = names, c = c(num_ev*summer_ev, num_ev*(1 - summer_ev)), 
                   seas = rep(c("Summer", "Winter"), each = length(names)))
  p <- ggplot(df) + geom_bar(aes(x = n, y = c, fill = seas), stat = "identity") +
    scale_y_continuous(name = "Annual frequency", limits = c(0, 35), expand = c(0, 0)) + 
    scale_x_discrete(name = "") + 
    theme_bw() + 
    theme(legend.justification = c(1, 1), 
          legend.position = c(0.99, 0.99), 
          legend.title = element_blank()) +
    ggtitle(title)
  
  return(p)
}


plot_exc_prob <- function(char_ev, t = 1:length(char_ev), title = NULL, xlab = NULL) {
  # plot exceedance probability for drought characteristics
  
  df <- data.frame(d = char_ev, t = t)
  p <- ggplot(df) + geom_line(aes(x = t, y = 100*d), linewidth = 1) +
    scale_y_continuous(name = "Frequency (%)", limits = c(0, 100), breaks = seq(0, 100, 20)) +
    scale_x_continuous(name = xlab, limits = range(t), breaks = c(1, seq(5, max(t), 5))) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.title = element_blank(),
          legend.justification = c(1, 1), legend.position = c(0.99, 0.99)) +
    ggtitle(title)
  
  return(p)
}


plot_corr_map <- function(data, evars, countries){
  # plot the correlation between the SREPI and SRLI in each country on a map

  corr_ind_vec <- sapply(data, function(z) cor(z[, evars])[evars[1], evars[2]])
  
  eur <- get_map_data()
  eur$value <- NA
  
  df <- data.frame(c = unname(corr_ind_vec), country = countries)
  df$country[df$country == "Czech_Republic"] <- "Czech Republic"; df$country[df$country == "United_Kingdom"] <- "UK" 
  for(i in df$country) eur$value[eur$region == i] <- df$c[df$country == i]
  
  p <- ggplot(data = eur, mapping = aes(x = long, y = lat, group = group)) +
    coord_map() + geom_polygon(aes(fill = value), colour = 'black') + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank()) +
    scale_fill_fermenter(name = NULL, palette = "RdBu", direction = -1, breaks = seq(-1, 1, 0.25), limits = c(-1, 1)) +
    scale_y_continuous(name = NULL, limits = c(35, 65), expand = c(0, 0)) +
    scale_x_continuous(name = NULL, limits = c(-15, 25), expand = c(0, 0))
  
  return(p)
}


get_map_data <- function(){
  # extract Europe map data
  
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

  return(eur)
}



plot_drought_char_mix <- function(stat_mat, names, ic_wind, clims = c(0, 0.5), cbreaks = seq(0, max(clims), length.out = 11)) {
  # plot drought characteristics as a function of the proportion of wind capacity
  
  # custom colour palette
  getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))
  scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
    binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), 
                 na.value = na.value, guide = guide, ...)  
  }
  
  
  df <- data.frame(val = as.vector(stat_mat), 
                   cnt = names,
                   ws = rep(c(0.05, rep(0.1, 9), 0.05), each = length(names)),
                   ic = ic_wind)
  p <- ggplot(df) + geom_bar(aes(x = cnt, y = ws, fill = val), stat = "identity", width = 1) +
    scale_x_discrete(name = NULL, expand = c(0, 0)) +
    scale_y_continuous(name = "Wind power proportion", limits = c(0, 1), expand = c(0, 0),
                       breaks = ws_ratio_vec) +
    scale_fill_fermenter_custom(breaks = cbreaks, limits = clims, name = "", pal = getPalette(10)) +
    geom_hline(yintercept = seq(0.05, 0.95, 0.1), col = "white") +
    geom_vline(xintercept = seq(1.5, length(names), 1)) +
    geom_point(aes(x = cnt, y = ic), col = "black", shape = 4, size = 2) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.key.height = unit(2, "cm"))
  
  return(p)
}


