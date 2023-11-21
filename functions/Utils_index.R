
get_abbrevs <- function(countries) {
  # get abbreviations from list of country strings
  
  abbrevs <- sapply(seq_along(countries), function(i) substr(countries[i], 1, 3))
  
  if ("Slovakia" %in% countries & "Slovenia" %in% countries) {
    abbrevs[countries == "Slovakia"] <- "Sva"
    abbrevs[countries == "Slovenia"] <- "Sve"
  }
  if ("Czech_Republic" %in% countries) {
    abbrevs[countries == "Czech_Republic"] <- "CR"
  }
  if ("United_Kingdom" %in% countries) {
    abbrevs[countries == "United_Kingdom"] <- "UK"
  }
  
  return(abbrevs)
}


calculate_std_index <- function(data, evars, countries, refdata = data, 
                                dist = "empirical", rescale = NULL){
  # wrapper to convert the raw data to standardised indices for all countries and variables
  
  ener_ind <- list()
  for (icountry in 1:length(countries)) {
    ener_ind[[icountry]] <- list()
    cnt <- countries[icountry]
    for (ivar in 1:length(evars)) {
      evar <- evars[ivar]
      X <- data %>% filter(country == cnt) %>% dplyr::select(date, evar)
      X <- xts(X[[evar]], as.POSIXct(X[["date"]]))
      X_ref <- refdata %>% filter(country == cnt) %>% dplyr::select(date, evar)
      X_ref <- xts(X_ref[[evar]], as.POSIXct(X_ref[["date"]]))
      if (dist == "none") {
        if (!is.null(rescale)) {
          if (rescale == "days") {
            X <- apply.daily(X, "sum")
          } else if (rescale == "weeks") {
            X <- apply.weekly(X, "sum")
          }
        }
        ener_ind[[icountry]][[ivar]] <- X
      } else {
        ener_ind[[icountry]][[ivar]] <- std_index(X, X_ref, timescale = "hours", dist = dist, rescale = rescale)
      }
    }
    names(ener_ind[[icountry]]) <- evars
    ener_ind[[icountry]] <- do.call(merge.xts, ener_ind[[icountry]])
  } 
  names(ener_ind) <- countries

  return(ener_ind)
}


change_energy_mix <- function(data, evars, countries, threshvals, ws_ratio_vec) {
  # wrapper to get drought characteristics for different energy mix configurations
  
  ratio_results <- list()
  for (ws in seq_along(ws_ratio_vec)) {
    ws_ratio <- ws_ratio_vec[ws]
    print(ws_ratio)
    
    data_mixed <- mutate(data, 
                         IC17_tot = IC17_wind + IC17_solar,
                         IC17_wind = ws_ratio*IC17_tot,
                         IC17_solar = (1 - ws_ratio)*IC17_tot,
                         pw_wind = cf_wind*IC17_wind,
                         pw_solar = cf_solar*IC17_solar,
                         PWS = pw_wind + pw_solar,
                         res_load_WS = WD - PWS)
    
    sei_1d_mix <- calculate_std_index(data_mixed, refdata = data, evars, countries, 
                                      dist = "empirical", rescale = "days")
    
    drought_df_mix_rl <- lapply(countries, function(z) 
      get_drought(sei_1d_mix[[z]][, "res_load_WS"], threshvals))
    drought_df_mix_pws <- lapply(countries, function(z) 
      get_drought(sei_1d_mix[[z]][, "PWS"], -threshvals, exceed = FALSE))
    
    # supply droughts
    num_ev <- sapply(drought_df_mix_rl, function(z) mean(z$occ)) # drought frequency
    dur_ev <- sapply(drought_df_mix_rl, function(z) mean(z$dur[z$dur > 0])) #drought duration
    mag_ev <- sapply(drought_df_mix_rl, function(z) mean(z$mag[z$mag > 0])) #drought magnitude
    ev_stat_RL <- cbind(num_ev, dur_ev, mag_ev)
    
    # production droughts
    num_ev <- sapply(drought_df_mix_pws, function(z) mean(z$occ)) # drought frequency
    dur_ev <- sapply(drought_df_mix_pws, function(z) mean(z$dur[z$dur > 0])) #drought duration
    mag_ev <- sapply(drought_df_mix_pws, function(z) mean(z$mag[z$mag > 0])) #drought magnitude
    ev_stat_PWS <- cbind(num_ev, dur_ev, mag_ev)
    
    ratio_results[[ws]] <- list(ind = sei_1d_mix, PWS_drought = ev_stat_PWS, RL_drought = ev_stat_RL)
  }
  
  return(ratio_results)
}


change_storage <- function(X_ref, threshvals, storage_lens) {
  # wrapper to get drought characteristics for different storage configurations
  
  storage_results <- list()
  for (ss in seq_along(storage_lens)) {
    storage_len <- storage_lens[ss]
    print(storage_len)
    
    X_new <- mutate(X_ref, store = NA)
    for (i in seq_along(X_new$PWS)) {
      if (i > 1) {
        if (X_new$res_load_WS[i] > 0) {
          if (sum(X_new$store[pmax(i - (1:storage_len), 0)]) > 0) {
            for (j in storage_len:1) {
              if (j >= i) next
              if (X_new$store[i - j] == 0) next
              if (X_new$res_load_WS[i] > X_new$store[i - j]) {
                X_new$PWS[i] <- X_new$PWS[i] + X_new$store[i - j]
                X_new$store[i - j] <- 0
                X_new$res_load_WS[i] <- X_new$ED[i] - X_new$PWS[i]
              } else if (X_new$res_load_WS[i] > 0) {
                X_new$PWS[i] <- X_new$PWS[i] + X_new$res_load_WS[i]
                X_new$store[i - j] <- X_new$store[i - j] - X_new$res_load_WS[i]
                X_new$res_load_WS[i] <- X_new$ED[i] - X_new$PWS[i]
                break
              }
            }
          }
        }
      }
      X_new$store[i] <- pmax(0, -X_new$res_load_WS[i])
    }
    X_new <- select(X_new, c("Index", "res_load_WS", "PWS"))
    
    ener_ind <- list()
    for (ivar in 1:length(evars)) {
      evar <- evars[ivar]
      ener_ind[[evar]] <- std_index(as.xts(X_new[, evar], X_new[, "Index"]),
                                    as.xts(X_ref[, evar], X_ref[, "Index"]),
                                    timescale = "days", dist = "empirical")
    }

    sei_1d_ev_store_rl <- get_drought(ener_ind[["res_load_WS"]], threshvals)
    sei_1d_ev_store_pws <- get_drought(ener_ind[["PWS"]], -threshvals, exceed = F)
    
    storage_results[[ss]] <- list(PWS_drought = sei_1d_ev_store_pws, RL_drought = sei_1d_ev_store_rl)
  }
  names(storage_results) <- storage_lens
  
  return(storage_results)
}


parametric_analysis <- function(evars, times, countries, dists, raw_list){
  # fit several distributions to a time series and compare them based on their AIC

  # initialise data frame with aic and ks values
  aic_df <- expand.grid(times, evars, countries, dists, KEEP.OUT.ATTRS = F)
  colnames(aic_df) <- c("time", "evar", "cnt", "dist")
  aic_df$aic <- aic_df$ks_pval <- NA
  
  for (time in times) {
    raw_df <- raw_list[[time]]
    for (evar in evars) {    
      raw_vec <- as.vector(raw_df[[cnt]][, evar])
      for (cnt in countries) {
        for (dist in dists) {
          
          print(c(time, evar, cnt, dist))
          
          # some distributions have strictly positive support
          if (dist %in% c("tnorm", "tlogis", "exp", "lnorm", "llogis")) 
            raw_vec[raw_vec == 0] <- runif(sum(raw_vec == 0), 0, 0.0001) 
          
          # fit model
          ind <- (aic_df$time == time) & (aic_df$evar == evar) & (aic_df$cnt == cnt) & (aic_df$dist == dist)
          if (dist == "tnorm" | dist == "tlogis") {
            start <- list(loc = mean(raw_vec), sc = sd(raw_vec))
            if (dist == "tnorm") {
              dtnorm <- function(x, loc, sc) dnorm(x, loc, sc)/(1 - pnorm(0, loc, sc))
              ptnorm <- function(q, loc, sc) (pnorm(q, loc, sc) - pnorm(0, loc, sc))/(1 - pnorm(0, loc, sc))
            } else {
              dtlogis <- function(x, loc, sc) dlogis(x, loc, sc)/(1 - plogis(0, loc, sc))
              ptlogis <- function(q, loc, sc) (plogis(q, loc, sc) - plogis(0, loc, sc))/(1 - plogis(0, loc, sc))
            }
            model <- tryCatch(fitdist(data = raw_vec, distr = dist, start = start), error = function(cond) NULL)
            if (!is.null(model)) {
              aic_df$aic[ind] <- model$aic
              aic_df$ks_pval[ind] <- do.call('ks.test', c(list(x = raw_vec, y = paste0('p', dist)), model$estimate))$p.value
            } else {
              aic_df$ks_pval[ind] <- aic_df$aic[ind] <- NA
            }
          } else {
            model_fit <- fit_dist(raw_vec, dist)$fit_props
            aic_df$aic[ind] <- model_fit['aic']
            aic_df$ks_pval[ind] <- model_fit['ks_pval']
          }
        }
      }
    }
  }
  
  # extract distribution with the lowest aic
  aic_df_best <- expand.grid(times, evars, countries, KEEP.OUT.ATTRS = F)
  colnames(aic_df_best) <- c("time", "evar", "cnt")
  aic_df_best$aic <- aic_df_best$ks_pval <- aic_df_best$dist <- NA
  
  for (time in times) {  
    for (evar in evars) {
      for (cnt in countries) {
        ind <- (aic_df$time == time) & (aic_df$evar == evar) & (aic_df$cnt == cnt)
        df_red <- aic_df[ind, ]
        ind <- (aic_df_best$time == time) & (aic_df_best$evar == evar) & (aic_df_best$cnt == cnt)
        aic_df_best[ind, ] <- df_red %>% filter(aic == min(aic, na.rm=T))
      }
    }
  }
  
  return(aic_df_best)
  
}
