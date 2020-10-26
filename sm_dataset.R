kg <<- reticulate::import('kaggle')

# Titanic
kg$api$competition_submit(file_name = 'titanic_submission.csv',
                          message = '',
                          competition = 'titanic')

# Houses
kg$api$competition_submit(file_name = 'houses_submission.csv',
                         message = '',
                         competition = 'house-prices-advanced-regression-techniques')
