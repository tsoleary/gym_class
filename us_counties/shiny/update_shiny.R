# ------------------------------------------------------------------------------
# Update Shiny App on shinyapps.io
# February 23, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

library(rsconnect)
rsconnect::deployApp(here::here("us_counties/shiny"))
