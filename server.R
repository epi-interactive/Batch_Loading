##################################
# Created by EPI-interactive
# 15 Sep 2021
# https://www.epi-interactive.com
##################################

server <- function(input, output) {
    
    
    # Batch-loading logic -----------------------------------------------------
    
    
    currentBatches <- reactiveVal(1)
    currentMaxShapes <- reactiveVal(0)
    batchSizes <- reactiveVal(numeric())
    
    maxBatches <- reactiveVal(1)
    maxShapes <- nrow(g_mainData)
    
    
    # Reactives ---------------------------------------------------------------
    
    mainData <- reactive({
        g_mainData
    })
    
    batchSourceData <- reactive({
        req(!is.null(input$batchSize))
        
        batchSize <- as.numeric(input$batchSize)
        
        out <- mainData() %>%
            mutate(batch = ((row_number()-1) %/% (batchSize)) + 1)
        
        maxBatches(length(unique(out$batch)))
        
        return(out)
    })
    
    baseMap <- reactive({
        
        out <- leaflet(options = leafletOptions(minZoom = 3)) %>%
            addTiles() %>%
            renderBatch(1)
        
        return(out)
    })
    
    
    # UI elements -------------------------------------------------------------
    
    
    output$page <- renderUI({
        div(class = "map-container",
            leafletOutput("map"),
            uiOutput("controls")
        )
    })
    
    
    output$map <- renderLeaflet({
        baseMap()
    })
    
    output$controls <- renderUI({
        absolutePanel(
            top = 15,
            right = 15,
            wellPanel(
                tags$h3("Batch loading data"),
                p("Tress owned/maintained by Parks, Sport and Recreation Business Unit, Wellington City Council."),
                p("Access the data", tags$a(href = "https://data-wcc.opendata.arcgis.com/datasets/WCC::wcc-trees/about", target = "_blank", "here")),
                uiOutput("batchLoadingControls")
            )
        )
    })
    
    output$batchLoadingControls <- renderUI({
        tagList(
            radioButtons("batchSize", "Batch size", choices = c(50, 100, 500, 1000), selected = 50),
            uiOutput("currentBatchesText"),
            uiOutput("batchLoadingButtons")
        )
    })
    
    output$batchLoadingButtons <- renderUI({
        tagList(
            actionButton(inputId = "showLess", "Show less"),
            actionButton(inputId = "showMore", "Show more"),
            actionButton(inputId = "reset", "Reset")
        )
    })
    
    output$currentBatchesText <- renderUI({
        
        div(style = "margin-bottom: 20px",
            paste0(
                "Showing ", 1, "-", format(batchSizes()[length(batchSizes())], big.mark = ","), 
                " of ", format(maxShapes, big.mark = ","), " locations"
            )
        )
    })
    
    

# Batch functions ---------------------------------------------------------

    renderBatch <- function(map, currentBatch) {
        
        # limit shapes to one specific batch
        currentBatchData <- batchSourceData() %>%
            filter(batch == currentBatch)
            
        if(currentBatch == 1) {
            batchSizes(isolate(input$batchSize))
        }
        else {
            batchSizesVar <- isolate(as.numeric(batchSizes()))
            lastBatchValue <- ifelse(length(batchSizesVar) == 0, 0, batchSizesVar[length(batchSizesVar)])
            batchSizes(c(batchSizesVar, lastBatchValue + nrow(currentBatchData)))
        }
        
        icons <- awesomeIcons(
            icon = 'leaf',
            iconColor = 'white',
            library = 'ion',
            markerColor = "darkgreen"
        )
            
        map <- map %>%
            addAwesomeMarkers(
                group = paste0("shapes", currentBatch),
                data = currentBatchData,
                layerId = ~OBJECTID,
                icon = icons,
                popup = ~popupText
            )
    }
    
    
    clearBatch <- function(map, keep) {

        for(batch in (keep+1):maxBatches()) {
            shapeGroupName <- paste0("shapes", batch)

            map <- map %>%
                clearGroup(shapeGroupName)
        }

        batchSizes(batchSizes()[1:keep])
    }
    
    
    # Observe events for batch logic and map updates --------------------------
    
    
    observeEvent(input$showMore, {

        current <- currentBatches()
        new <- min(current + 1, maxBatches())
        if(new != currentBatches()) {
            map <- leafletProxy("map")
            renderBatch(map, new)
            enable("showLess")
        }
        else {
            disable("showMore")
        }
        currentBatches(new)
    })


    observeEvent(input$showLess, {

        current <- currentBatches()
        new <- max(current - 1, 1)
        if(new != currentBatches()) {
            map <- leafletProxy("map")
            clearBatch(map, new)
            enable("showMore")
        }
        else {
            disable("showLess")
        }
        currentBatches(new)
    })
    
    
    observeEvent(batchSourceData(), {
        
        map <- leafletProxy("map")
        currentBatches(1)
        clearBatch(map, 0)
        renderBatch(map, 1)
        
    }, ignoreNULL = T, ignoreInit = T)
    
    
    observeEvent(input$reset, {
        
        map <- leafletProxy("map")
        clearBatch(map, 1)
        currentBatches(1)
        batchSizes(batchSizes()[1])
        
    })
}
