shinyUI(
    fluidPage(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        
        useShinyjs(),
        useWaiter(),
        # useWaitress(),
        useShinyFeedback(),

        sidebarPanel(
            p("This application aims to show how to use async code through Shiny."),
            selectizeInput(
                inputId = "cities",
                label = "Select a pool of cities to calculate the best route",
                choices = unique(tb_map$name_muni),
                multiple = TRUE,
                options = list(maxItems = 7, minItems = 3)
            ),
            actionButton(
                inputId = "compute", 
                label = "Compute",
                icon = icon("cog")
            ),
            br(),
            br(),
            tags$pre(thecode)
        ),
        mainPanel(
            column(
                width = 4,
                div(
                    id = "cities_results_spinner",
                    style = "height: 450px;",
                    DTOutput("cities_results", height = "400px")
                )
            ),
            column(
                width = 8,
                align = "center",
                DTOutput("cities_distance", height = "400px"),
                div(
                    id = "cities_plot_spinner",
                    style = "height: 420px;",
                    plotOutput("cities_plot", height = "400px")
                ),
                uiOutput("cities_layout_plot")
            )
        )
    )
)