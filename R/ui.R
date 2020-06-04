


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
                text = "ProtVista",
                tabName = "protvista",
                icon = shiny::icon("map-signs"),
                selected = FALSE),
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
                            label = "X-axis",
                            choices = NULL,
                            multiple = FALSE
                        )
                    ),
                    column(
                        width = 2,
                        selectInput(
                            inputId = "p_yaxis",
                            label = "Y-axis",
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
                    ),
                    column(
                        width = 2,
                        checkboxInput(
                            inputId = "p_add_ref",
                            label = "Add reference ratio",
                            value = FALSE
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
                tabName = "protvista",
                fluidRow(
                    column(
                        width = 3,
                        selectizeInput(
                            inputId = "pv_uniid",
                            label = "UniProt Accession ID",
                            choices = NULL,
                            multiple = FALSE,
                            options = list(create = TRUE)
                        )
                    ),
                    column(
                        width = 3,
                        actionButton(
                            inputId = "pv_start",
                            label = "Show")
                    )
                ),
                br(),
                fluidRow(
                    tags$head(tags$script(src='https://ebi-uniprot.github.io/CDN/protvista/protvista.js')),
                    tags$head(tags$link(rel="stylesheet", type = "text/css", href = "https://ebi-uniprot.github.io/CDN/protvista/css/main.css")),
                    tags$body(
                        HTML(
                            paste0(
                                "<div id='yourDiv'/>
                                <script>
                                window.onload = function() {
                                    document.getElementById('pv_start').onclick = function fun_protvista() {
                                        var yourDiv = document.getElementById('yourDiv');
                                        var ProtVista = require('ProtVista');
                                        var selectacc = document.getElementById('pv_uniid').value;
                                        var instance = new ProtVista({
                                            el: yourDiv,
                                            uniprotacc: selectacc
                                        });
                                    }
                                }
                                </script>
                                </div id='yourDiv'>")
                        )
                    ),
                    width = 800, height = 800
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


