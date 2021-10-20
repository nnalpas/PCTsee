


### Create Server role ---------------------------------------------------

# Define the server role as a function for compatibility with shiny app
server <- function(input, output, session) {
    
    add_cols <- c(
        "Protein IDs", "Majority protein IDs", "Protein names", "Gene names",
        "Sequence coverage [%]", "Q-value", "Score", "Only identified by site")
    
    
    
    # Import the PCT data
    my_data <- eventReactive(input$dataset, {
        readRDS(file = paste0("./inst/extdata/", input$dataset))
    })
    
    # Include OtherName columns if any
    add_cols <- reactive({
        c(add_cols, grep(
            "^OtherName", unique(my_data()$protein$key_no_lab), value = TRUE))
    })
    
    #observe({
    #    print(paste0("crossmap: ", dim(my_data()$crossmap), "; pg: ", dim(my_data()$protein)))
    #    print(colnames(my_data()$protein))
    #})
    
    # 
    my_genes <- reactive({
        my_data()$crossmap %>%
            dplyr::arrange(., value) %>%
            .[["value"]] %>%
            unique(.) %>%
            set_names(x = ., value = .) %>%
            c(my_data()$default$`Gene names / Protein IDs`, .)
    })
    
    # 
    my_uniid <- reactive({
        my_data()$crossmap %>%
            dplyr::filter(., key_no_lab == "Protein IDs") %>%
            dplyr::arrange(., value) %>%
            .[["value"]] %>%
            unique(.) %>%
            set_names(x = ., value = .)
    })
    
    # 
    my_sample_cols <- reactive({
        colnames(my_data()$protein)[
            !colnames(my_data()$protein) %in% c(
                "id", "value", "Label", "key_no_lab", "Replicates")] %>%
            set_names(x = ., value = .) %>%
            c(my_data()$default$`X-axis`, .)
    })
    
    # 
    my_abund_cols <- reactive({
        my_data()$protein %>%
            dplyr::filter(., !is.na(Experiment)) %>%
            .[["key_no_lab"]] %>%
            unique(.) %>%
            set_names(x = ., value = .) %>%
            c(my_data()$default$`Y-axis`, .)
    })
    
    # 
    my_group_cols <- reactive({
        colnames(my_data()$protein)[
            !colnames(my_data()$protein) %in% c(
                "id", "value", "key_no_lab")] %>%
            set_names(x = ., value = .) %>%
            c(my_data()$default$`Colour per`, .)
    })
    
    # 
    observe({
        
        updateSelectInput(
            session = session,
            inputId = "p_gene",
            label = "Gene names / Protein IDs",
            choices = my_genes()
        )
        
        updateSelectInput(
            session = session,
            inputId = "p_xaxis",
            label = "X-axis",
            choices = my_sample_cols()
        )
        
        updateSelectInput(
            session = session,
            inputId = "p_yaxis",
            label = "Y-axis",
            choices = my_abund_cols()
        )
        
        #updateSelectInput(
        #    session = session,
        #    inputId = "p_group",
        #    label = "Group per",
        #    choices = my_group_cols()
        #)
        
        updateSelectInput(
            session = session,
            inputId = "p_colour",
            label = "Colour per",
            choices = my_group_cols()
        )
        
        updateSelectInput(
            session = session,
            inputId = "p_shape",
            label = "Point shape per",
            choices = my_group_cols()
        )
        
    })
    
    # 
    observe({
        
        updateSelectInput(
            session = session,
            inputId = "pv_uniid",
            label = "UniProt Accession ID",
            choices = my_uniid()
        )
        
    })
    
    
    
    # This section correpond to profile view
    my_profiles <- reactive({
        
        req(input$p_gene, input$p_xaxis, input$p_yaxis)
        
        my_id <- unique(my_data()$crossmap[
            my_data()$crossmap$value == input$p_gene, ][["id"]])
        
        #x_axis <- "Duration"
        #y_axis <- input$p_yaxis
        
        if (length(my_id) == 1) {
            
            my_res <- duration_plot(
                target = my_id,
                df = my_data()$protein,
                x = input$p_xaxis,
                y = input$p_yaxis,
                #group = input$p_group,
                colour = input$p_colour,
                shape = input$p_shape,
                add_cols = add_cols(),
                add_ref_value = input$p_add_ref)
            return(my_res)
            
        } else (
            warning(
                "The target was not found, try using UniProt IDs instead!")
        )
        
    })
    
    # 
    observeEvent(my_profiles, {
        
        output$p_profile <- renderPlotly({
            ggplotly(my_profiles()[["plot"]])# %>%
            #layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
        })
        
        output$p_prot_info <- renderDT(
            my_profiles()[["table"]]
        )
        
    })
    
    
    
    # This section corresponds to protvista, but is entirely within the UI
    
    
    
    # This section correspond to the cluster tab
    # Display the work in progress
    output$inprogress <- renderImage({
        list(
            src = file.path("./inst/www/inprogress.png"),
            height = "600px",
            alt = "Work in progress")
    }, deleteFile = FALSE)
    
    
    
}


