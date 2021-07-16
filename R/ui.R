


### Create UI framework --------------------------------------------------

# Define the UI as a function for compatibility with shiny app
ui <- dashboardPage(
    header = dashboardHeader(title = "PCTsee"),
    sidebar = dashboardSidebar(
        shinybusy::add_busy_spinner(
            spin = "fading-circle",
            timeout = 200,
            position = "bottom-right",
            onstart = TRUE),
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
        tags$head(tags$style(HTML("
            .skin-black .main-header .logo {
                color:#fff;
                background-color: #222D32;}
            .skin-black .main-header .logo:hover {
                color: #222D32;
                background-color: #fff;}
            .skin-black .main-header .navbar {
                background-color: #222D32;}
            .content-wrapper, .right-side {
                background-color: #b2b2b8;}
            .skin-black .main-header .navbar .sidebar-toggle{
                color:#fff;
                background-color: #222D32;}
            .skin-black .main-header .navbar .sidebar-toggle:hover{
                color: #222D32;
                background-color: #fff;}
            .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                color: #222D32;
                background-color: #fff;}
            .box.box-solid>.box-header {
                color:#fff;
                background:#222D32;
                box-shadow: 0px 5px 2px -5px grey;}
            .box.box-solid{
                /*border-style: dotted;
                border-width: 2px;
                border-color: #222D32;*/
                border-bottom-color:#222D32;
                border-left-color:#222D32;
                border-right-color:#222D32;
                border-top-color:#222D32;
                box-shadow: 5px 5px 2px grey;}
        "))),
        tabItems(
            tabItem(
                tabName = "profile",
                box(
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
                ), title = "Parameters", width = 12, collapsible = TRUE,
                solidHeader = TRUE#,
                #style = "background-color:  #222D32; box-shadow: 5px 5px 2px grey;"
                ),
                br(),
                br(),
                fluidRow(
                    column(
                        width = 8,
                        align = "center",
                        box(
                            plotlyOutput("p_profile"),
                            title = "Visualisation",
                            width = 12, solidHeader = TRUE#,
                            #style = "background-color:  #222D32; box-shadow: 5px 5px 2px grey;"
                            )
                    ),
                    column(
                        width = 4,
                        align = "center",
                        box(
                            DTOutput("p_prot_info"),
                            title = "Information",
                            width = 12, solidHeader = TRUE#,
                            #style = "background-color:  #222D32; box-shadow: 5px 5px 2px grey;"
                            )
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


