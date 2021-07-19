

# Function to plot abundance values over a duration
duration_plot <- function(
    target,
    df,
    x,
    y,
    colour,
    shape,
    add_cols,
    add_ref_value) {
    
    toplot <- df %>%
        dplyr::filter(., key_no_lab == y & id == target)
    
    toplot_df <- toplot %>%
        dplyr::filter(., is.na(Experiment)) %>%
        dplyr::mutate(
            ., parameter = paste0(key_no_lab, " (", Label, ")")) %>%
        dplyr::select(., parameter, value)
    toplot_df <- df %>%
        dplyr::filter(
            ., key_no_lab %in% add_cols & is.na(Experiment) & id == target) %>%
        dplyr::select(., parameter = key_no_lab, value) %>%
        dplyr::bind_rows(., toplot_df)
    
    if (add_ref_value & grepl("\\/L$", toplot$Label)) {
        
        lab_refs <- toplot %>%
            dplyr::filter(., !is.na(`Label reference`)) %>%
            .[["Label reference"]] %>%
            unique(.)
        references <- df %>%
            dplyr::filter(
                ., `Label reference` %in% lab_refs &
                    id == target & Label %in% lab_refs)
        references_format <- references %>%
            dplyr::mutate(
                ., Label = paste0(Label, "/", Label),
                value = "1",
                key_no_lab = y) %>%
            unique(.)
        
        toplot %<>%
            dplyr::bind_rows(., references_format)
        
    }
    
    pl <- ggplot(
        data = toplot %>% dplyr::filter(., !is.na(Experiment)),
        mapping = aes(
            x = !!as.name(x),
            y = as.numeric(value),
            #group = paste(group, collapse = " "),
            fill = !!as.name(colour),
            colour = !!as.name(colour),
            shape = as.character(!!as.name(shape)))) +
        geom_smooth(method = "loess", se = TRUE) +
        geom_point() +
        theme_pubr() +
        theme(legend.position = "right") +
        labs(
            x = x,
            y = y,
            colour = colour,
            fill = colour,
            shape = shape)
    
    dt <- DT::datatable(toplot_df, rownames = FALSE)
    
    return(list(plot = pl, table = dt))
}


