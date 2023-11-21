start <- Sys.time()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

libs <- c('devtools', 'xts', 'zoo', 'lubridate', 'ggplot2', 'dplyr', 'SEI')

message('Configuring and checking environment...')

# Installation of all packages
sapply(libs, function(x) 
  if ( !(x %in% rownames(installed.packages()))) { 
    print(paste('Installing ', x, ' package...'))
    install.packages(x, clean=TRUE, verbose=FALSE, quiet=TRUE) 
    })

sapply(libs, function(x) library(x, character.only=TRUE))

# Source all functions used in the main program
sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source)
message('Environment configured sucessfully!')

message("Execution time \"ConfigureEnv.R\": ", round(Sys.time() - start, 4), " secs")

rm(libs, start)