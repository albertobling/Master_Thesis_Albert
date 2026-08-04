# 00 Dependencies ##############################################################

#Loading surveydown package
library(surveydown)

# Setting WD to the folder containing correct survey files
setwd("/Users/albertobling/Desktop/Statskundskab/Kandidat/Masters Thesis/Surveydown")

# 01 Launch App to Shinyapp.io #################################################

# Connecting to Shinyapp.io server
# rsconnect::setAccountInfo(name='albertmasterthesis',
                         # token='REDACTED',
                         # secret='REDACTED')

# Configuring the PostgreSQL database with credentials
surveydown::sd_db_config()

# Deploying app
rsconnect::deployApp(appName = "Survey")

