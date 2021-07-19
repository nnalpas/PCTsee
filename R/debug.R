

input <- list(
    dataset = "SCy004.RDS",
    p_gene = "sll0735", 
    p_xaxis = "Label duration",
    p_yaxis = "Ratio normalized",
    p_colour = "Label reference",
    p_shape = "Label reference")


add_cols <- c(
    "Protein IDs", "Majority protein IDs", "Protein names", "Gene names",
    "Sequence coverage [%]", "Q-value", "Score", "Only identified by site")



# Import the PCT data
my_data <- readRDS(file = paste0("../inst/extdata/", input$dataset))

#observe({
#    print(paste0("crossmap: ", dim(my_data()$crossmap), "; pg: ", dim(my_data()$protein)))
#    print(colnames(my_data()$protein))
#})

# 
my_genes <- reactive({
    my_data()$crossmap %>%
        dplyr::arrange(., value) %>%
        .[["value"]] %>%
        unique(.)
})

# 
my_uniid <- reactive({
    my_data()$crossmap %>%
        dplyr::filter(., key_no_lab == "Protein IDs") %>%
        dplyr::arrange(., value) %>%
        .[["value"]] %>%
        unique(.)
})

# 
my_sample_cols <- reactive({
    colnames(my_data()$protein)[
        !colnames(my_data()$protein) %in% c(
            "id", "value", "Label", "key_no_lab", "Replicates")]
})

# 
my_abund_cols <- reactive({
    my_data()$protein %>%
        dplyr::filter(., !is.na(Experiment)) %>%
        .[["key_no_lab"]] %>%
        unique(.)
})

# 
my_group_cols <- reactive({
    colnames(my_data()$protein)[
        !colnames(my_data()$protein) %in% c(
            "id", "value", "key_no_lab")]
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
    
    #req(input$p_gene, input$p_xaxis, input$p_yaxis)
    
    my_id <- unique(my_data$crossmap[
        my_data$crossmap$value == input$p_gene, ][["id"]])
    
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
            add_cols = add_cols)
        return(my_res)
        
    } else (
        warning(
            "The target was not found, try using UniProt IDs instead!")
    )
    
})



