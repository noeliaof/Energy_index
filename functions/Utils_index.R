fit.distribution <- function(data,distr,method=c('mle','lmom'),na.thres=10){  
  # function from https://github.com/WillemMaetens/standaRdized
  # I am not using it for now
  distr <- match.arg(arg=distr,choices=c('gamma','gamma3','weibull','weibull3','gev','glogis'))
  
  if (distr %in% c('gamma3','weibull3') & missing('method')){
    method <- 'lmom'
  } else {
    method <- match.arg(method)
  }
  
  # parameter definitions
  if(distr=='gamma'){
    params <- c(shape=as.numeric(NA),rate=as.numeric(NA))
  } else if (distr=='gamma3'){
    params <- c(shape=as.numeric(NA),scale=as.numeric(NA),thres=as.numeric(NA))
  } else if (distr=='weibull'){
    params <- c(shape=as.numeric(NA),scale=as.numeric(NA))
  } else if (distr=='weibull3') {
    params <- c(shape=as.numeric(NA),scale=as.numeric(NA),thres=as.numeric(NA))    
  } else if (distr=='gev'){
    params <- c(shape=as.numeric(NA),scale=as.numeric(NA),location=as.numeric(NA))
  } else if (distr=='glogis'){
    if (method=='mle'){
      params <- c(shape=as.numeric(NA),scale=as.numeric(NA),location=as.numeric(NA))
    } else {
      distr <- 'glo' # change name to conform with lmomco package definition
      params <- c(xi=as.numeric(NA),alpha=as.numeric(NA),kappa=as.numeric(NA))
    }
  }
  
  # initialize data properties and goodness-of-fit statistics vectors
  fit.props <- c(prob.zero=as.numeric(NA),
                 n.obs=as.integer(NA),
                 n.na=as.numeric(NA),
                 pct.na=as.numeric(NA),
                 ks.pval=as.numeric(NA),
                 ad.pval=as.numeric(NA))
  
  # determine n.obs
  fit.props['n.obs'] <- as.integer(length(data))
  # check data for NA values and omit if necessary
  if(length(data)!=0){
    fit.props['n.na'] <- length(data[which(is.na(data))])
    fit.props['pct.na'] <- (fit.props['n.na']/fit.props['n.obs'])*100
    data <- data[!is.na(data)]
  }
  # determine prob.zero
  fit.props['prob.zero'] <- length(which(data==0))/length(data)
  
  # check na.thres
  if(fit.props['pct.na'] >= na.thres | length(data)==0){
    # NA output
    res <- c(params,fit.props)
    attributes(res) <- c(attributes(res),list(data=data,distr=distr,method=method,na.thres=na.thres))
    return(res)
  }
  
  # check data for zeros or negative values in case of gamma or weibull distribution
  if (distr == 'gamma' | distr=='weibull'){ 
    if(any(data<0)){
      warning(paste(distr,'distribution: all data values must be zero or positive, distribution not fitted'))
      return(c(params,fit.props)) # NA output
    }
    data <- data[which(data!=0)]
  }
  
  # fit distribution to data
  
  # maximum likelihood estimation parameter fit
  if (method=='mle'){
    # provide start estimates for the distribution fitting
    if(distr=='gamma3' | distr=='weibull3'){
      stop(paste('method mle not supported for ',distr,', use method lmom',sep=''))
    } else if (distr=='gev' | distr=='glogis'){
      start <- list(shape=1,scale=1,location=0)
    } else {
      start <- NULL
    }
    # fit distribution
    fit <- try(fitdistrplus::fitdist(data=data,distr=distr,method='mle',start=start))
    if(!inherits(fit,'try-error')){
      params <- fit$estimate
    } else {
      warning('distribution fitting failed')
      return(c(params,fit.props)) # NA output
    }
  }
  
  # L-moments parameter fit
  if (method=='lmom'){
    pwm <- lmomco::pwm.ub(data)
    lmom <- lmomco::pwm2lmom(pwm)
    if (!lmomco::are.lmom.valid(lmom) | is.na(sum(lmom[[1]])) | is.nan(sum(lmom[[1]]))) {
      warning('invalid moments, no distribution fitted')
      return(c(params,fit.props)) # NA output
    }
    # remap parameters
    if(distr=='gamma'){
      fit <- lmomco::pargam(lmom)
      params['shape'] <- fit$para['alpha']
      params['rate'] <- 1/fit$para['beta']
    } else if (distr=='gamma3') {
      fit <- lmomco::parpe3(lmom)
      params['shape'] <- (2/fit$para['gamma'])^2
      params['scale'] <- fit$para['sigma']/sqrt(params['shape'])
      params['thres'] <- fit$para['mu'] - (params['shape']*params['scale'])
    } else if (distr=='weibull'){
      # this is the 3 parameter weibull distribution, any threshold should be accounted for by the prob.zero value
      fit <- lmomco::parwei(lmom)
      params['shape'] <- fit$para['delta']
      params['scale'] <- fit$para['beta']
    } else  if (distr=='weibull3'){
      fit <- lmomco::parwei(lmom)
      params['shape'] <- fit$para['delta']
      params['scale'] <- fit$para['beta']
      params['thres'] <- -1*fit$para['zeta']
    } else if (distr=='gev'){
      fit <- lmomco::pargev(lmom)
      params['shape'] <- -1*fit$para['kappa']
      params['scale'] <- fit$para['alpha']
      params['location'] <- fit$para['xi']
    } else if (distr=='glo'){
      fit <- lmomco::parglo(lmom)
      params['xi'] <- fit$para['xi']
      params['alpha'] <- fit$para['alpha']
      params['kappa'] <- fit$para['kappa']
    }
  }
  
  # calculate goodness-of-fit
  if(!any(is.na(params))){
    suppressWarnings(ks.pval <- try(do.call('ks.test',c(list(x=data,y=paste0('p',distr)),as.list(params)))$p.value))
    if(!inherits(ks.pval,'try-error')){
      fit.props['ks.pval'] <- ks.pval
    }
    suppressWarnings(ad.pval <- try(do.call('ad.test',c(list(x=data,null=paste0('p',distr,sep='')),as.list(params)))$p.value))
    if(!inherits(ad.pval,'try-error')){
      fit.props['ad.pval'] <- ad.pval
    }
  }
  
  # return value
  res <- c(params,fit.props)
  attributes(res) <- c(attributes(res),list(data=data,distr=distr,method=method,na.thres=na.thres))
  return(res)
}


funSDEI <- function(x, method=c("fitdis", "empirical"), scale){
  #@x is a data frame with the dates and the values
  library(xts)
  if (missing('method')){
    method <- 'fitdis'
  } else {
    method <- match.arg(method)
  }
  
  if (missing('scale')){
    scale <- NULL
  }
  
  if (!is.null(scale)){
    
    dat.xts <- xts(x[,2],as.POSIXct(x[,1]))
    ends <- endpoints(x[,1],'hours',scale)
    new.x <- period.apply(dat.xts,ends,sum)
    fdat  <- myfits(data.frame(new.x[,1])[,1], "none")
    p     <- fdat$pnon
    SDEI <- qnorm(p,0,1)
    return(list("SDEI"= data.frame(date=index(new.x), SDEI=SDEI), "infodis"= fdat[[1]]))
    
  }else{
    
    if (method == "fitdis"){
      fdat  <- myfits(x[,2], "none")
      p     <- fdat$pnon
      SDEI <-qnorm(p,0,1)
      return(list("SDEI"= data.frame(date=x[,1], SDEI=SDEI), "infodis"= fdat[[1]]))
      
    }else if (method == "empirical"){
      
      n   <- length(x[,2]);
      EP  <- ecdf(x[,2])  # Get the empirical probability
      p  <- EP(x[,2])*n/(n+1)  # Use the Weibull plotting position
      SDEI <-qnorm(p,0,1)
      return(list("SDEI"= data.frame(date=x[,1], SDEI=SDEI)))
    }
  }
  
}


SDEI_Events <- function(values, type) {
  values <- as.numeric(values)
  sti.extremely.hot   <- sum(values >= 2.00, na.rm=T)
  sti.very.hot        <- sum(values < 2.00 & values >= 1.50, na.rm=T)
  sti.moderately.hot  <- sum(values < 1.50 & values >= 1.00, na.rm=T)
  sti.near.normal     <- sum(values < 1.00 & values > -1.00, na.rm=T)
  sti.moderately.cold <- sum(values <= -1.00 & values > -1.50, na.rm=T)
  sti.very.cold       <- sum(values <= -1.50 & values > -2.00, na.rm=T)
  sti.extremely.cold  <- sum(values <= -2.00, na.rm=T)
  r <- c(sti.extremely.hot, sti.very.hot, sti.moderately.hot, sti.near.normal, sti.moderately.cold, sti.very.cold, sti.extremely.cold)
  names(r) = c("Extremely hot","Very hot","Moderately hot","Near normal","Moderately cold","Very cold","Extremely cold") 
  if (sti.near.normal == 0) {
    return (NA)
  } else {
    return (r)
  }
}

calculate_energyindex_country <- function(data, method="fitdis", scale, nvars){
  
  n_country <- unique(data$country)
  ener_ind <- l_sdei <- l_info <-  list()
  for ( icountry in 1:length(n_country)){
    for( ivar in 1:length(nvars)){
      X <- data%>%dplyr::filter(country==n_country[icountry])%>%dplyr::select(date,nvars[ivar])
      # ener_ind[[ivar]] <- funSDEI(X, method, scale = scale)
      country_index <- tryCatch({ funSDEI(X, method, scale = scale)}, 
                                   error=function(e) {cat("\n","error to get the index in", n_country[icountry], "for",nvars[ivar])})
      if (is.null(country_index)){
        ener_ind[[ivar]]<- NA
      }else{
        ener_ind[[ivar]] <- country_index
        # plot_sdei(country_index$SDEI)
      }
      
    }
    names(ener_ind) <- nvars
    
    n_sdei   <- lapply(ener_ind, function(x) x[[1]])
    info_dis <- lapply(ener_ind, function(x) x[[2]])
    df_info <- setNames(melt(info_dis, id=c("nam.mar","ks.pval")), c("nam.mar","ks.pval", "type"))
    df_sdei <- setNames(melt(n_sdei, id=c("date","SDEI")), c("date","SDEI","type"))
    # pivot_wider(df_sdei, names_from=type, values_from=SDEI)
    l_sdei[[icountry]] <- df_sdei
    l_info[[icountry]] <- df_info
  }
  
  names(l_sdei) <- names(l_info) <- n_country
  
  return(list("SDEI"= l_sdei, "info_dis"=l_info))
  
}


plot_sdei <- function(dx, yy){
  # Visualise the index
  # ggplot(dx, aes(x=date,y=SDEI)) +
  #   geom_ribbon(aes(ymin=pmin(SDEI,0), ymax=0), fill="blue", alpha=0.5) +
  #   geom_ribbon(aes(ymin=0, ymax=pmax(SDEI,0)), fill="red", alpha=0.5) +
  #   geom_hline(aes(yintercept=0), color="grey") + theme_bw()
  
  p <- dx%>%dplyr::filter(format(date,"%Y")%in%yy)%>%ggplot( aes(x=date,y=SDEI)) +
    geom_ribbon(aes(ymin=pmin(SDEI,0), ymax=0), fill="blue", alpha=0.5) +
    geom_ribbon(aes(ymin=0, ymax=pmax(SDEI,0)), fill="red", alpha=0.5) +
    geom_hline(aes(yintercept=0), color="grey") +
    geom_hline(aes(yintercept=1), color="grey",linetype="dashed")+
    geom_hline(aes(yintercept=-1), color="grey",linetype="dashed")+
    facet_wrap(~type) + 
    theme_bw()
  return(p)
  
}

