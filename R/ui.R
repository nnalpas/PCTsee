


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
                        width = 12,
                        align = "center",
                        selectInput(
                            inputId = "dataset",
                            label = "Select a PCT dataset",
                            choices = list.files(
                                path = "../inst/extdata", pattern = "\\.RDS"),
                            multiple = FALSE,
                            width = "100%",
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


