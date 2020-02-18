


### Create Server role ---------------------------------------------------

# Define the server role as a function for compatibility with shiny app
server <- function(input, output, session) {
    
    add_cols <- c(
        "Protein IDs", "Majority protein IDs", "Protein names", "Gene names",
        "Sequence coverage [%]", "Q-value", "Score", "Only identified by site")
    
    
    
    # Import the PCT data
    my_data <- eventReactive(input$dataset, {
        readRDS(file = paste0("../inst/extdata/", input$dataset))
    })
    
    observe({
        print(paste0("crossmap: ", dim(my_data()$crossmap), "; pg: ", dim(my_data()$protein)))
    })
    
    # 
    my_genes <- reactive({
        my_data()$crossmap %>%
            dplyr::arrange(., value) %>%
            .[["value"]] %>%
            unique(.)
    })
    
    # 
    my_abund_cols <- reactive({
        my_data()$protein %>%
            dplyr::filter(., !is.na(Experiment)) %>%
            .[["key_no_lab"]] %>%
            unique(.)
    })
    
    # 
    observe({
        
        updateSelectInput(
            session = session,
            inputId = "my_gene",
            label = "Gene names / Protein IDs",
            choices = my_genes()
        )
        
        updateSelectInput(
            session = session,
            inputId = "my_yaxis",
            label = "Abundance type",
            choices = my_abund_cols()
        )
        
    })
    
    # 
    my_res <- reactive({
        
        req(input$my_gene, input$my_yaxis)
        
        my_id <- unique(my_data()$crossmap[
            my_data()$crossmap$value == input$my_gene, ][["id"]])
        
        x_axis <- "Duration"
        y_axis <- input$my_yaxis
        
        if (length(my_id) == 1) {
            
            my_res <- duration_plot(
                target = my_id,
                df = my_data()$protein,
                x = x_axis,
                y = y_axis,
                add_cols = add_cols)
            return(my_res)
            
        } else (
            warning(
                "The target was not found, try using UniProt IDs instead!")
        )
        
    })
    
    # 
    observeEvent(my_res, {
        
        output$profile_plot <- renderPlotly({
            ggplotly(my_res()[["plot"]])# %>%
            #layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
        })
        
        output$profile_df <- renderDT(
            my_res()[["table"]]
        )
        
    })
    
    
    
    # This section correspond to the cluster tab
    
    # Display the work in progress
    output$inprogress <- renderImage({
        list(
            src = file.path("../inst/www/inprogress.png"),
            height = "600px",
            alt = "Work in progress")
    }, deleteFile = FALSE)
    
    
    
}


