kg <<- reticulate::import('kaggle')

setwd("~/Documents/GitHub/Kaggle/")
compName <- 'competitive-data-science-predict-future-sales'

if(!dir.exists(compName)) {
  dir.create(compName)
} else {
  print(paste(compName, 'directory already exists'))
}

setwd(paste0('./', compName))

kg$api$competition_download_files(competition = compName)

suppressWarnings(unzip(zipfile = paste0(compName, '.zip')))
