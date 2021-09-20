##################################
# Created by EPI-interactive
# 15 Sep 2021
# https://www.epi-interactive.com
##################################

ui <- fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")     
    ),
    uiOutput("page")
)
