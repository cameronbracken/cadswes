save_data <- function()
    source('setup/save_data.R')
save_data()

data <- scan('config/data',what=character(0),quiet=T)

for(i in data)
    load(file.path('data',data))