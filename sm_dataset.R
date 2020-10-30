kg <<- reticulate::import('kaggle')

# Titanic
kg$api$competition_submit(file_name = 'titanic_submission.csv',
                          message = '',
                          competition = 'titanic')

# Houses
kg$api$competition_submit(file_name = 'houses_submission.csv',
                         message = '',
                         competition = 'house-prices-advanced-regression-techniques')

# Sales
kg$api$competition_submit(file_name = 'sales_submission.csv',
                          message = '',
                          competition = 'competitive-data-science-predict-future-sales')
