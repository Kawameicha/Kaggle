kg <<- reticulate::import('kaggle')

setwd("~/Documents/Kaggle/")
compName <- 'tmdb-box-office-prediction'

if(!dir.exists(compName)) {
  dir.create(compName)
} else {
  print(paste(compName, 'directory already exists'))
}

setwd(paste0('./', compName))

kg$api$competition_download_files(competition = compName)

suppressWarnings(unzip(zipfile = paste0(compName, '.zip')))
