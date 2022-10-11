get_abbrevs <- function(countries){
  # function to get abbreviations from a list of country strings
  
  abbrevs <- sapply(seq_along(countries), function(i) substr(countries[i], 1, 3))
  
  if("Slovakia" %in% countries & "Slovenia" %in% countries){
    abbrevs[countries == "Slovakia"] <- "Sva"; abbrevs[countries == "Slovenia"] <- "Sve"
  }
  if("Czech_Republic" %in% countries){
    abbrevs[countries == "Czech_Republic"] <- "CR"
  }
  if("United_Kingdom" %in% countries){
    abbrevs[countries == "United_Kingdom"] <- "UK"
  }
  
  return(abbrevs)
}


scale_data <- function(x, scale){
  # function to aggregate the data across a given timescale
  
  dat.xts <- xts(x[,2], as.POSIXct(x[,1]))
  if(scale %% (7*24) == 0){ 
    ends <- endpoints(x[,1], 'weeks', scale/(7*24))
  }else{
    ends <- endpoints(x[,1], 'hours', scale)
  }
  
  new.x <- period.apply(dat.xts, ends, sum)
  new.x <- data.frame(index(new.x), coredata(new.x))
  colnames(new.x) <- colnames(x)
  
  # remove last entry if it corresponds to an incomplete day or week
  if(length(unique(diff(ends))) != 1){
    return(new.x[-nrow(new.x), ])
  }else{
    return(new.x) 
  }
}


myfits <- function(xx, mtype, th, ...){
  
  require(fitdistrplus)
  
  
  if (mtype%in%c("gev","pareto")){
    if (mtype=="gev"){
      ff <- gev.fit(xx)
      params <- ff$mle
    }else if(mtype=="pareto"){
      thx <- quantile(xx, probs=th)
      ff <- gpd.fit(xx, thx[[1]])
      params <- c(ff$threshold, ff$mle)
    }
    nam.mar <- mtype
    estimates <- params  
    gf <- gofTest(xx, distribution = mtype, test = "ks")
    
    
  }else{
    
    if (mtype!="none"){
      distributions <- mtype
    }else{
      distributions <-  c("norm", "lnorm", "gamma", "weibull", "exp")
    }
    
    
    
    distr_aic <-  list()
    distr_fit <-  list()
    ks.lis    <- list()
    gf.list <- list()
    
    
    for (distribution in distributions) {
      
      distr_fit[[distribution]] <-  tryCatch({fitdist(data=xx, distribution)}, error=function(e) {cat(distribution)}) 
      
      if ( !is.null(distr_fit[[distribution]] )){
        distr_aic[[distribution]] <-  distr_fit[[distribution]]$aic
        pdis <- paste("p",distribution, sep="")
        gf.list[[distribution]] <- gofstat(distr_fit[[distribution]])
        if (distribution == "norm"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, mean= distr_fit[[distribution]]$estimate["mean"],
                                            sd = distr_fit[[distribution]]$estimate["sd"])
        }else if (distribution == "lnorm"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, mean= distr_fit[[distribution]]$estimate["meanlog"],
                                            sd = distr_fit[[distribution]]$estimate["sdlog"])
        }else if (distribution == "gamma"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, shape= distr_fit[[distribution]]$estimate["shape"],
                                            rate = distr_fit[[distribution]]$estimate["rate"])
        }else if (distribution == "weibull"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, shape= distr_fit[[distribution]]$estimate["shape"],
                                            scale = distr_fit[[distribution]]$estimate["scale"])
        }else if (distribution == "exp"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, rate= distr_fit[[distribution]]$estimate["rate"])
        }
      }
    }
    
    # ind.min <- which.min(distr_aic)
    # nam.mar <- names(distr_aic[ind.min])
    # ks.pval <- ks.lis[[ind.min]]$p.value
    # 
    df.bic <- setNames(melt(lapply(gf.list, function(x) x$bic)), c("value", "name"))
    df.aic <- setNames(melt(lapply(gf.list, function(x) x$aic)), c("value", "name"))
    df.kspval <- setNames(melt(lapply(gf.list, function(x) x$ks)), c("value", "name"))
    df.cvpval <- setNames(melt(lapply(gf.list, function(x) x$cvm)), c("value", "name"))
    # select based on the minimum aic and compatible with pval >0.05
    ind <- which.min(df.aic$value)
    if (df.kspval$value[ind]>0.05 | length(df.kspval$value)==1){
      # I will keep ind, otherwise I look for the next lower aic
      nam.mar <- names(distr_aic[ind])
      ks.pval <- df.kspval$value[ind]
      estimates <- distr_fit[[ind]]$estimate
      
    }else{
      # find another index
      new_ind <- order(df.aic$value)[-1] #excluding the first
      ind.ks <-  which(df.kspval$value[new_ind] >0.05)
      
      if ( length(ind.ks)>1 ){
        # take the largest pvalue
        ind.ks    <- order(df.kspval$value[new_ind], decreasing = T)[1] 
        ks.pval   <- df.kspval$value[ind.ks]
        estimates <- distr_fit[[ind.ks]]$estimate
        nam.mar   <- names(distr_aic[ind.ks])
        
      }else{
        
        new.ind.ks    <- order(df.kspval$value[new_ind], decreasing = T)[1] 
        ks.pval   <- df.kspval$value[new.ind.ks]
        estimates <- distr_fit[[new.ind.ks]]$estimate
        nam.mar   <- names(distr_aic[new.ind.ks])
        
      }
    }
    # cdfcomp(distr_fit)
    
  }
  mm <- eval(parse(text=(paste("p",nam.mar, sep=""))))
  if (length(estimates)>2){
    p_non <- mm(xx, estimates[1],estimates[2], estimates[3])
  }else{
    p_non <- mm(xx, estimates[1],estimates[2])
  }
  
  if (mtype%in%c("gev","pareto")){
    
    return(list(data.frame(nam.mar=mtype, pval=gf$p.value), "estimates"=params, "pnon"=p_non))
  }else{
    return(list(data.frame(nam.mar,ks.pval), "estimates"=estimates, "pnon"=p_non))
    
  }
  
}


fun_SDEI <- function(x, method = c("fitdis", "empirical", "none"), scale){
  # function to convert the raw data to standardised indices
  #@x is a data frame containing dates and the corresponding raw values
  
  library(xts)
  if(missing('method')){
    method <- 'fitdis'
  }else{
    method <- match.arg(method)
  }
  
  if(missing(scale)){
    scale <- NULL
  }

  if(!is.null(scale)){
    x <- scale_data(x, scale)
  }
  
  if(method == "fitdis"){
    
    fdat <- myfits(x[,2], "none")
    p <- fdat$pnon
    SDEI <- qnorm(p, 0, 1)
    return(list("SDEI" = data.frame(date = x[,1], SDEI = SDEI), "infodis" = fdat[[1]]))
    
  }else if(method == "empirical"){
    
    n <- length(x[,2]);
    EP <- ecdf(x[,2])  # Get the empirical probability
    p <- (1 + EP(x[,2])*n)/(n + 2)  # Use the Weibull plotting position to ensure values not equal to 0 or 1
    SDEI <- qnorm(p, 0, 1)
    return(list("SDEI" = data.frame(date = x[,1], SDEI = SDEI)))
    
  }else if (method == "none"){
    
    colnames(x) <- c("date", "SDEI")
    return(list("SDEI" = x))
    
  }
  
}


calculate_energy_index <- function(data, nvars, method = "fitdis", scale = NULL){
  # wrapper to convert the raw data to standardised indices for all countries and variables
  
  n_country <- unique(data$country)
  
  ener_ind <- l_sdei <- l_info <- list()
  for(icountry in 1:length(n_country)){
    for(ivar in 1:length(nvars)){
      X <- data %>% dplyr::filter(country==n_country[icountry]) %>% dplyr::select(date,nvars[ivar])
      country_index <- tryCatch({fun_SDEI(X, method, scale = scale)}, 
                                error = function(e) {cat("\n","error to get the index in", n_country[icountry], "for", nvars[ivar])})
      if(is.null(country_index)){
        ener_ind[[ivar]] <- NA
      }else{
        ener_ind[[ivar]] <- country_index
      }
    }
    
    names(ener_ind) <- nvars
    
    n_sdei <- lapply(ener_ind, function(x) x[[1]])
    if(method == "fitdis"){
      info_dis <- lapply(ener_ind, function(x) x[[2]])
      df_info <- setNames(melt(info_dis, id = c("nam.mar", "ks.pval")), c("nam.mar", "ks.pval", "type"))
    }else{
      df_info <- NULL
    }
    
    df_sdei <- setNames(melt(n_sdei, id=c("date","SDEI")), c("date","SDEI","type"))
    l_sdei[[icountry]] <- df_sdei
    l_info[[icountry]] <- df_info
  }
  
  
  if(method == "fitdis"){
    names(l_sdei) <- names(l_info) <- n_country
    return(list("SDEI" = l_sdei, "info_dis" = l_info))
  }else{
    names(l_sdei) <- n_country
    return(list("SDEI" = l_sdei))
  }
  
}


def_energy_drought <- function(dd, mvar, threshval, higher = T, lag = T){
  # function to get a data frame of energy drought characteristics from index values
  
  # the drought characteristics include:
  # occurrence ('occ' = 0, 1)
  # intensity ('ins' = 0, 1, 2, 3, i.e. mild, moderate, severe, extreme)
  # magnitude ('mag')
  # duration ('dur')
  
  # occurrence and intensity
  if(higher){
    if(length(threshval) == 1){
      dd$ins <- as.numeric(dd[[mvar]] >= threshval)
    }else{
      dd$ins <- sapply(1:nrow(dd), function(i){sum(dd[[mvar]][i] >= threshval)})
    }
  }else{
    if(length(threshval) == 1){
      dd$ins <- as.numeric(dd[[mvar]] <= threshval)
    }else{
      dd$ins <- sapply(1:nrow(dd), function(i){sum(dd[[mvar]][i] <= threshval)})
    }
  }
  
  dd$occ <- as.numeric(dd$ins >= 1)
  
  if(lag){
    for(i in 2:nrow(dd)){
      if(dd$occ[i] == 0 & dd$occ[i-1] == 1 & sign(dd[[mvar]][i]) == sign(dd[[mvar]][i-1])){
        dd$occ[i] <- 1
      } 
    }
  }
  
  # duration and magnitude
  mag <- abs(dd[[mvar]])*(dd$occ == 1)
  dd['dur'] <- c(dd$occ[1], numeric(nrow(dd) - 1))
  dd['mag'] <- c(mag[1], numeric(nrow(dd) - 1))
  for(i in 2:nrow(dd)){
    if(dd$occ[i]){
      dd$dur[i] <- dd$dur[i-1] + 1
      dd$dur[i-1] <- 0
      
      dd$mag[i] <- dd$mag[i-1] + mag[i]
      dd$mag[i-1] <- 0
    }
  }
  
  dd <- dd[,c("date", mvar, "ins", "occ", "dur", "mag")]
  
  return(dd)
}


parametric_analysis <- function(var_vec, time_vec, country_vec, dist_vec, index_list_all){
  # function to fit several distributions to a time series and compare them based on their AIC
  
  require(fitdistrplus)
  require(flexsurv)
  
  # define truncated normal distribution
  dtnorm <- function(x, mean, sd) dnorm(x, mean, sd)/(1 - pnorm(0, mean, sd))
  ptnorm <- function(q, mean, sd) (pnorm(q, mean, sd) - pnorm(0, mean, sd))/(1 - pnorm(0, mean, sd))
  
  # define truncated logistic distribution
  dtlogis <- function(x, location, scale) dlogis(x, location, scale)/(1 - plogis(0, location, scale))
  ptlogis <- function(q, location, scale) (plogis(q, location, scale) - plogis(0, location, scale))/(1 - plogis(0, location, scale))
  
  
  
  aic_df <- data.frame(time = rep(time_vec, each = length(country_vec)*length(dist_vec)*length(var_vec)),
                       variable = rep(rep(var_vec, each = length(dist_vec)*length(country_vec)), length(time_vec)),
                       country = rep(rep(country_vec, each = length(dist_vec)), length(time_vec)*length(var_vec)),
                       dist = rep(dist_vec, length(country_vec)*length(var_vec)*length(time_vec)),
                       aic = NA,
                       sig = NA)
  
  for(time in time_vec){

    # select relevant data frame
    index_list <- index_list_all[[time]]
    
    for(vari in var_vec){    
      for(country in country_vec){
        for(dist in dist_list){
          
          print(c(time, vari, country, dist))
          
          # specify start parameters for unknown distributions
          if(dist == "tnorm"){
            start_list = list(mean = 0, sd = 1)
          }else if(dist == "tlogis"){
            start_list = list(location = 0, scale = 1)
          }else{
            start_list = NULL
          }
          
          # log-normal and log-logistic distributions have strictly positive support
          if(dist %in% c("lnorm", "llogis")){
            index_vec[index_vec == 0] <- runif(1, 0, 0.0001) 
          }
          
          ind <- aic_df$time == time & aic_df$variable == vari & aic_df$country == country & aic_df$dist == dist
          index_vec <- index_list[[country]] %>% dplyr::filter(type == vari) %>% dplyr::pull(SDEI)
          model <- tryCatch(fitdist(index_vec, dist, start = start_list), error=function(e) NULL)
          
          if(is.null(model)){
            aic_df$aic[ind] <- NA
            aic_df$sig[ind] <- NA
          }else{
            pars <- lapply(split(model$estimate, names(model$estimate)), unname)
            aic_df$aic[ind] <- model$aic
            aic_df$sig[ind] <- do.call(ks.test, append(list(x = index_vec, y = paste("p", dist, sep = "")), pars))$p.value
          }
          
        }
      }
    }
  }
  
  # extract distribution with the lowest aic
  aic_df_best <- data.frame(time = rep(time_vec, each = length(country_vec)*length(var_vec)),
                            variable = rep(rep(var_vec, each = length(country_vec)), length(time_vec)),
                            country = rep(country_vec, length(var_vec)*length(time_vec)),
                            dist = NA,
                            aic = NA,
                            sig = NA)
  for(time in time_vec){  
    for(vari in var_vec){
      for(country in country_vec){
        ind <- aic_df$time == time & aic_df$variable == vari & aic_df$country == country
        df_red <- aic_df[ind, ]
        ind <- aic_df_best$time == time & aic_df_best$variable == vari & aic_df_best$country == country
        aic_df_best[ind, ] <- df_red %>% dplyr::filter(aic == min(aic, na.rm=T))
      }
    }
  }
  
  return(aic_df_best)
  
}
