ConfigureEnv <- function() {

  start <- Sys.time()
  
  # Load file configuration, install and load packages needed to run the application
  # This function assumes that the user has permissions to install packages
  #
  # Args: N/A
  # 
  # Returns: N/A

  message('Configuring and checking environment...')
  source('./config/config.R')
  
  # Installation of all packages specified in the config file ('config.yml')
  sapply(libs, function(x) 
                  if ( !(x %in% rownames(installed.packages()))) { 
                      print(paste('Installing ', x, ' package...'))
                      install.packages(x, clean=TRUE, verbose=FALSE, quiet=TRUE) 
                  })
  
  sapply(libs, function(x) library(x, character.only=TRUE));
  
  # Source all functions used in the main program
  sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source);
  message('Environment configured sucessfully!')
  
  message("Execution time \"ConfigureEnv.R\": ", round(Sys.time() - start, 4), " secs")

}
