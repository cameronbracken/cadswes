source('setup/save_data.R')


data <- scan('config/data',what=character(0),quiet=T)

for(i in data)
    load(file.path('data',data))