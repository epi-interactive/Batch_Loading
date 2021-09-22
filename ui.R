##################################
# Created by EPI-interactive
# 15 Sep 2021
# https://www.epi-interactive.com
##################################

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
        tags$link(rel="stylesheet", href="https://use.typekit.net/fkz2upm.css")
    ),
    uiOutput("page")
)
