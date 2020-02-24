


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
        print(colnames(my_data()$protein))
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
            inputId = "p_gene",
            label = "Gene names / Protein IDs",
            choices = my_genes()
        )
        
        updateSelectInput(
            session = session,
            inputId = "p_yaxis",
            label = "Abundance type",
            choices = my_abund_cols()
        )
        
    })
    
    # 
    my_profiles <- reactive({
        
        req(input$p_gene, input$p_yaxis)
        
        my_id <- unique(my_data()$crossmap[
            my_data()$crossmap$value == input$p_gene, ][["id"]])
        
        x_axis <- "Duration"
        y_axis <- input$p_yaxis
        
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
    observeEvent(my_profiles, {
        
        output$p_profile <- renderPlotly({
            ggplotly(my_profiles()[["plot"]])# %>%
            #layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
        })
        
        output$p_prot_info <- renderDT(
            my_profiles()[["table"]]
        )
        
    })
    
    
    
    # This section correspond to the cluster tab
    observeEvent(input$sbMenu, {
        
        if (input$sbMenu == "cluster") {
            
            # Protein info columns
            feature_cols <- c(
                "Protein IDs", "Majority protein IDs", "Protein names", "Gene names")
            
            # Update the various input widgets that depend on data
            observe({
                updateSelectInput(
                    session = session,
                    inputId = "c_xaxis",
                    label = "X-axis (samples to cluster)",
                    choices = my_data()$protein %>%
                        dplyr::select(
                            ., -id, -value, -Label, -key_no_lab) %>%
                        colnames(.)
                )
            })
            observe({
                updateSelectInput(
                    session = session,
                    inputId = "c_yaxis",
                    label = "Y-axis (protein abundance)",
                    choices = my_abund_cols()
                )
            })
            observe({
                updateSelectInput(
                    session = session,
                    inputId = "c_labels",
                    label = "Labels to filter out",
                    choices = unique(pg_format()[["Label"]])
                )
            })
            observe({
                updateSelectInput(
                    session = session,
                    inputId = "c_samples",
                    label = "Samples to filter out",
                    choices = pheno()[[input$c_xaxis]]
                )
            })
            observe({
                updateSelectInput(
                    session = session,
                    inputId = "c_gene",
                    label = "Gene names / Protein IDs",
                    choices = c(pg_cluster()$`Gene names`, pg_cluster()$`Protein IDs`)
                )
            })
            
            # Retrieve the protein annotation columns for later export
            my_feature <- reactive({
                req(my_data)
                my_data()$protein %>%
                    dplyr::filter(., key_no_lab %in% feature_cols) %>%
                    dplyr::select(., id, key_no_lab, value) %>%
                    tidyr::spread(
                        data = ., key = key_no_lab, value = value,
                        fill = NA, convert = FALSE, drop = TRUE)
            })
            
            # Extract the abundance values requested by the user
            pg_format <- reactive({
                req(my_data, input$c_yaxis)
                my_data()$protein %>%
                    dplyr::filter(
                        ., key_no_lab == input$c_yaxis & !is.na(Experiment))
            })
            
            # Retrieve the phenodata for the current dataset
            pheno <- reactive({
                req(pg_format)
                pg_format() %>%
                    dplyr::select(
                        ., -id, -value, -Label, -key_no_lab) %>%
                    unique(.)
            })
            
            # Merge replicates based on user specification
            pg_merge <- reactive({
                req(pg_format, input$c_xaxis)
                if (!is.null(input$c_merge) & input$c_merge != "NA") {
                    pg_format() %>%
                        dplyr::group_by(., id, !!as.name(input$c_xaxis), Label) %>%
                        dplyr::summarise(., value = do.call(
                            what = input$c_merge,
                            args = list(x = as.numeric(value), na.rm = TRUE))) %>%
                        dplyr::ungroup(.)
                } else {
                    pg_format()
                }
            })
            
            observe({
                print(paste("Merge table:", dim(pg_merge())))
            })
            
            # Filter out the user-selected samples
            pg_s_filt <- reactive({
                req(pg_merge)
                if (!is.null(input$c_samples) & length(input$c_samples) != 0) {
                    pg_merge() %>%
                        dplyr::filter(
                            ., !(!!as.name(input$c_xaxis) %in% input$c_samples))
                } else {
                    pg_merge()
                }
            })
            
            observe({
                print(paste("Sample filtered table:", dim(pg_s_filt())))
            })
            
            # Filter out the user-selected labels
            pg_l_filt <- reactive({
                req(pg_s_filt)
                if (!is.null(input$c_labels) & length(input$c_labels) != 0) {
                    pg_s_filt() %>%
                        dplyr::filter(., !Label %in% input$c_labels)
                } else {
                    pg_s_filt()
                }
            })
            
            observe({
                print(paste("Label filtered table:", dim(pg_l_filt())))
            })
            
            # Compile the user-requested abundance values into a matrix
            # with proteins as rows
            pg_abundance <- reactive({
                req(pg_l_filt, input$c_xaxis)
                abund <- pg_l_filt() %>%
                    dplyr::mutate(
                        ., Samples = paste(
                            !!as.name(input$c_xaxis), Label, sep = "__")) %>%
                    dplyr::select(., id, Samples, value) %>%
                    tidyr::spread(
                        data = ., key = "Samples", value = "value", convert = TRUE) %>%
                    as.data.frame(.)
                rownames(abund) <- abund[["id"]]
                abund$id <- NULL
                base::as.matrix(abund)
            })
            
            # Filter out proteins with too many missing and low abundance values
            pg_abundance_filt <- reactive({
                req(pg_abundance, input$c_filt_perc, input$c_filt_thres)
                if (!is.null(input$c_filt_perc) & input$c_filt_perc != "NA" & input$c_filt_perc != 0) {
                    na_count <- apply(
                        X = pg_abundance(),
                        MARGIN = 1,
                        FUN = function(x) {
                            sum(!is.na(x))})/ncol(pg_abundance())
                    zero_count <- apply(
                        X = pg_abundance(),
                        MARGIN = 1,
                        FUN = function(x) {
                            sum(x > input$c_filt_thres)})/ncol(pg_abundance())
                    my_res <- pg_abundance()[
                        na_count == input$c_filt_perc &
                            zero_count == input$c_filt_perc, ]
                } else {
                    my_res <- pg_abundance()
                }
                my_res %<>%
                    na.omit(.)
                same_filt <- apply(X = my_res, MARGIN = 1, function(x) {
                    var(x) != 0
                }) %>%
                    unlist(.)
                my_res[same_filt, ]
            })
            
            observe({
                print(paste("Abundance filtered table:", dim(pg_abundance_filt())))
            })
            
            # Create the color map for the heatmap
            my_palette <- colorRampPalette(c("forestgreen", "yellow", "red"))(n = 30)
            
            # Normalise the abundance data (per protein/rows)
            pg_scaled <- reactive({
                req(pg_abundance_filt)
                pg_abundance_filt() %>%
                    #na.omit(.) %>%
                    t(.) %>%
                    scale(., center = T, scale = T) %>%
                    t(.)
            })
            
            # Compute the protein cluster dendogram
            pg_dendo <- reactive({
                req(pg_scaled)
                validate(
                    need(
                        expr = nrow(pg_scaled()) != 0,
                        message = "The table is empty, you may need to remove more samples")
                )
                
                print(paste("my validation test:", nrow(pg_scaled())))
                
                pg_distance <- dist(x = pg_scaled(), method ="euclidean")
                pg_hcluster <- hclust(d = pg_distance, method ="ward.D2")
                my_dendo <- as.dendrogram(pg_hcluster)
                if (input$c_kcluster != 0) {
                    cols_branches <- rainbow(n = input$c_kcluster)
                    color_branches(
                        dend = my_dendo,
                        k = input$c_kcluster,
                        col = cols_branches)
                }
                my_dendo
            })
            
            # Generate the protein cluster heatmap
            observe({
                
                req(pg_scaled, pg_dendo, input$c_yaxis)
                
                col_labels <- get_leaves_branches_col(pg_dendo())
                col_labels <- col_labels[order(order.dendrogram(pg_dendo()))]
                
                output$c_heatmap <- renderPlot({
                    heatmap.2(
                        x = pg_scaled(),
                        main = paste("Euclidean distance - Ward.D2 clustering"),
                        trace = "none",
                        margins = c(5, 7),
                        col = my_palette,
                        #breaks = col_breaks,
                        dendrogram = "row",
                        Rowv = pg_dendo(),
                        Colv = FALSE,
                        key.xlab = paste("Scaled", input$c_yaxis),
                        cexRow = 0.4,
                        cexCol = 1,
                        na.rm = TRUE,
                        RowSideColors = col_labels,
                        colRow = col_labels)
                })
                
            })
            
            # Merge clustering and protein profiles
            pg_cluster <- reactive({
                
                req(pg_dendo, pg_scaled, pg_format)
                
                # Extract the cluster number and colour coding
                h_cols <- data.frame(
                    id = as.integer(labels(pg_dendo())),
                    col = get_leaves_branches_col(pg_dendo()),
                    stringsAsFactors = FALSE)
                h_cutree <- cutree(tree = pg_dendo(), k = input$c_kcluster) %>%
                    as.data.frame(.) %>%
                    set_colnames("k_cluster")
                h_cutree$id <- as.integer(row.names(h_cutree))
                h_cutree %<>%
                    dplyr::left_join(x = ., y = h_cols) %>%
                    dplyr::left_join(
                        x = .,
                        y = my_feature())
                
                # Merge to each protein, the corresponding cluster and protein info
                pg_clust <- pg_scaled() %>%
                    as.data.frame(.) %>%
                    tibble::rownames_to_column(.data = ., var = "id") %>%
                    dplyr::mutate(., id = as.integer(id)) %>%
                    tidyr::gather(
                        data = ., key = "key", value = "scaled_value", -id,
                        na.rm = FALSE, convert = TRUE)
                pg_clust %>%
                    tidyr::separate(
                        data = ., col = "key", into = c(input$c_xaxis, "Label"),
                        sep = "__", remove = TRUE, convert = TRUE) %>%
                    dplyr::left_join(
                        x = ., y = pg_format(),
                        by = c("id", input$c_xaxis, "Label")) %>%
                    dplyr::left_join(
                        x = ., y = h_cutree, by = "id") %>%
                    dplyr::mutate(., size = 0.4)
                
            })
            
            observe({
                print(paste("Cluster table:", dim(pg_cluster())))
            })
            
            # Colour code specific protein profiles within cluster
            pg_toplot <- reactive({
                req(pg_cluster)
                my_prot_cl <- pg_cluster()
                if (!is.null(input$c_gene) & length(input$c_gene) != 0) {
                    my_prot_cl[
                        my_prot_cl$`Gene names` %in% input$c_gene |
                            my_prot_cl$`Protein IDs` %in% input$c_gene, "col"] <- "black"
                    my_prot_cl[
                        my_prot_cl$`Gene names` %in% input$c_gene |
                            my_prot_cl$`Protein IDs` %in% input$c_gene, "size"] <- 1
                }
                my_prot_cl
            })
            
            observe({
                print(paste("To plot table:", dim(pg_toplot())))
            })
            
            ## Generate the scaled protein cluster profiles
            #observe({
            #    
            #    req(pg_toplot)
            #    
            #    output$c_scaled_cluster <- renderPlot({
            #        ggplot() +
            #            geom_line(
            #                data = pg_toplot(),
            #                mapping = aes(
            #                    x = !!as.name(input$c_xaxis),
            #                    y = as.numeric(scaled_value),
            #                    group = id, colour = col,
            #                    size = size)) +
            #            geom_smooth(
            #                data = pg_toplot(),
            #                mapping = aes(
            #                    x = !!as.name(input$c_xaxis),
            #                    y = as.numeric(scaled_value)),
            #                se = FALSE, colour = "#424242",
            #                size = 1, linetype = "dashed") +
            #            theme_bw() +
            #            theme(legend.position = "right") +
            #            labs(
            #                x = input$c_xaxis,
            #                y = paste("Scaled", input$c_yaxis)) +
            #            scale_colour_identity() +
            #            scale_size_identity() +
            #            facet_wrap(facets = "k_cluster")
            #    })
            #    
            #})
            #
            ## Generate the raw protein cluster profiles
            #observe({
            #    
            #    req(pg_toplot)
            #    
            #    my_title <- paste("Raw", input$c_yaxis)
            #    my_y_col <- "value"
            #    if (!is.null(input$c_log) & input$c_log != "NA") {
            #        my_y_col <- paste0(input$c_log, "_value")
            #        pg_toplot()[[my_y_col]] <- do.call(
            #            what = input$c_log,
            #            args = list(x = as.numeric(pg_toplot()$value)))
            #        my_title <- paste(input$c_log, my_title)
            #    }
            #    
            #    output$c_raw_cluster <- renderPlot({
            #        ggplot() +
            #            geom_line(
            #                data = pg_toplot(),
            #                mapping = aes(
            #                    x = !!as.name(input$c_xaxis),
            #                    y = !!as.name(my_y_col),
            #                    group = id, colour = col,
            #                    size = size)) +
            #            geom_smooth(
            #                data = pg_toplot(),
            #                mapping = aes(
            #                    x = !!as.name(input$c_xaxis),
            #                    y = !!as.name(my_y_col)),
            #                se = FALSE, colour = "#424242",
            #                size = 1, linetype = "dashed") +
            #            theme_bw() +
            #            theme(legend.position = "right") +
            #            labs(
            #                x = input$c_xaxis,
            #                y = my_title) +
            #            scale_colour_identity() +
            #            scale_size_identity() +
            #            facet_wrap(facets = "k_cluster")
            #    })
            #    
            #})
            
        }
        
    })
    
}


