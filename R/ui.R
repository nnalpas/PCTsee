


### Create UI framework --------------------------------------------------

# Define the UI as a function for compatibility with shiny app
ui <- dashboardPage(
    header = dashboardHeader(title = "PCTshowoff"),
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
                        width = 6,
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
                        width = 3,
                        selectInput(
                            inputId = "my_gene",
                            label = "Gene names / Protein IDs",
                            choices = NULL,
                            multiple = FALSE
                        )
                    ),
                    column(
                        width = 3,
                        selectInput(
                            inputId = "my_yaxis",
                            label = "Abundance type",
                            choices = NULL,
                            multiple = FALSE
                        )
                    )
                ),
                br(),
                uiOutput("profile_param"),
                br(),
                fluidRow(
                    column(
                        width = 8,
                        align = "center",
                        plotlyOutput("profile_plot")
                    ),
                    column(
                        width = 4,
                        align = "center",
                        DTOutput("profile_df")
                    )
                )
            ),
            tabItem(
                tabName = "cluster",
                fluidRow(
                    column(
                        width = 12,
                        align = "center",
                        imageOutput("inprogress")
                    )
                )
            )
        )
    ),
    skin = "black"
)


