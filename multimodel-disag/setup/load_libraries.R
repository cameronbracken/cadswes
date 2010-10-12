libs <- scan('config/libraries',what=character(0),quiet=T)

for(i in libs)
    suppressMessages(require(i, character.only = TRUE))