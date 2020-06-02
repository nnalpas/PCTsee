


### Create UI framework --------------------------------------------------

# Define the UI as a function for compatibility with shiny app
ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(title = "PCTsee"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            id = "sbMenu",
            menuItem(
                text = "Profile",
                tabName = "profile",
                icon = shiny::icon("chart-line"),
                selected = TRUE),
            menuItem(
                text = "Cluster",
                tabName = "cluster",
                icon = shiny::icon("stream"),
                selected = FALSE
            )
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(
                tabName = "profile",
                fluidRow(
                    column(
                        width = 8,
                        selectInput(
                            inputId = "dataset",
                            label = "Select a PCT dataset (few seconds to load)",
                            choices = list.files(
                                path = "../inst/extdata", pattern = "\\.RDS"),
                            multiple = FALSE,
                            width = "100%"
                        )
                    ),
                    column(
                        width = 4,
                        selectInput(
                            inputId = "p_gene",
                            label = "Gene names / Protein IDs",
                            choices = NULL,
                            multiple = FALSE
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 2,
                        selectInput(
                            inputId = "p_xaxis",
                            label = "Sample type",
                            choices = NULL,
                            multiple = FALSE
                        )
                    ),
                    column(
                        width = 2,
                        selectInput(
                            inputId = "p_yaxis",
                            label = "Abundance type",
                            choices = NULL,
                            multiple = FALSE
                        )
                    ),
                    #column(
                    #    width = 2,
                    #    selectInput(
                    #        inputId = "p_group",
                    #        label = "Group per",
                    #        choices = NULL,
                    #        multiple = TRUE
                    #    )
                    #),
                    column(
                        width = 2,
                        selectInput(
                            inputId = "p_colour",
                            label = "Colour per",
                            choices = NULL,
                            multiple = FALSE
                        )
                    ),
                    column(
                        width = 2,
                        selectInput(
                            inputId = "p_shape",
                            label = "Point shape per",
                            choices = NULL,
                            multiple = FALSE
                        )
                    )
                ),
                br(),
                br(),
                fluidRow(
                    column(
                        width = 8,
                        align = "center",
                        plotlyOutput("p_profile")
                    ),
                    column(
                        width = 4,
                        align = "center",
                        DTOutput("p_prot_info")
                    )
                )
            ),
            tabItem(
                tabName = "cluster",
                fluidRow(
                    column(
                        width = 4,
                        selectInput(
                            inputId = "c_xaxis",
                            label = "X-axis (samples to cluster)",
                            choices = NULL,
                            multiple = FALSE
                        ),
                        selectInput(
                            inputId = "c_yaxis",
                            label = "Y-axis (protein abundance)",
                            choices = NULL,
                            multiple = FALSE
                        ),
                        numericInput(
                            inputId = "c_kcluster",
                            label = "Number of k-mean clusters (turn-off with 0)",
                            value = 1,
                            min = 0,
                            step = 1
                        ),
                        selectInput(
                            inputId = "c_samples",
                            label = "Samples to filter out",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        selectInput(
                            inputId = "c_labels",
                            label = "Labels to filter out",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        selectInput(
                            inputId = "c_merge",
                            label = "Replicates merging method",
                            choices = c(
                                Mean = "mean", Median = "median"),
                            multiple = FALSE
                        ),
                        selectInput(
                            inputId = "c_log",
                            label = "Transform abundances",
                            choices = c(
                                None = NA, Log2 = "log2", Log10 = "log10"),
                            multiple = FALSE
                        ),
                        numericInput(
                            inputId = "c_filt_perc",
                            label = "Percentage samples with valid abundance (turn-off with 0)",
                            value = 0,
                            min = 0,
                            max = 1,
                            step = 0.01
                        ),
                        numericInput(
                            inputId = "c_filt_thres",
                            label = "Minimum abundance threshold",
                            value = 0,
                            min = 0,
                            step = 1
                        ),
                        selectInput(
                            inputId = "c_normalise",
                            label = "Normalise abundances",
                            choices = c(
                                Yes = TRUE, No = FALSE),
                            multiple = FALSE
                        ),
                        selectInput(
                            inputId = "c_gene",
                            label = "Gene names / Protein IDs",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        actionButton(
                            inputId = "c_apply",
                            label = "Apply")
                    ),
                    column(
                        width = 8,
                        align = "center",
                        plotOutput("c_heatmap")
                    )
                ),
                fluidRow(
                    column(
                        width = 6,
                        align = "center",
                        plotOutput("c_scaled_cluster")
                    ),
                    column(
                        width = 6,
                        align = "center",
                        plotOutput("c_raw_cluster")
                    )
                )
            )
        )
    ),
    skin = "black"
)


