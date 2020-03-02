

# Function to plot abundance values over a duration
duration_plot <- function(
    target,
    df,
    x,
    y,
    add_cols) {
    
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
    
    pl <- ggplot(
        data = toplot %>% dplyr::filter(., !is.na(Experiment)),
        mapping = aes(
            x = !!as.name(x), y = as.numeric(value),
            group = paste(Replicates, Label),
            fill = Replicates, colour = Replicates,
            shape = Label)) +
        geom_line() +
        geom_point() +
        theme_pubr() +
        theme(legend.position = "right") +
        labs(
            x = x,
            y = y,
            colour = "Replicates",
            fill = "Replicates",
            shape = "Labels")
    
    dt <- DT::datatable(toplot_df, rownames = FALSE)
    
    return(list(plot = pl, table = dt))
}


