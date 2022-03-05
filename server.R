shiny::shinyServer(function(input, output, session) {
    # input <- list(
    #     cities = unique(tb_map$name_muni)[1:3]
    # )

    result_val <- reactiveVal()
    result_plot <- reactiveVal()

    observe({
        if (length(input$cities) < 3) {
            showFeedbackWarning(
                inputId = "cities",
                text = "Please, select two or more cities"
            )
            disable("compute")
        } else {
            hideFeedback(inputId = "cities")
            enable("compute")
        }
    })

    tb_cities_distance <- eventReactive(input$compute, {
        req(input$cities)
        
        if (length(input$cities) > 2) {
            tb_cities_distance <- tb_dis |>
                dplyr::filter(start_city %in% input$cities) |>
                select(start_city, input$cities) |>
                arrange(start_city)

            tb_cities_distance <- tb_cities_distance[, c("start_city", tb_cities_distance$start_city)]

            tb_cities_distance
        } else {
            NULL
        }

    })

    observe({
        req(tb_cities_distance())

        tb <- tb_cities_distance()
        result_val(NULL)
        waiter_show(id = "cities_results_spinner", html = waiter::spin_3(), color = waiter::transparent(.5))
        waiter_show(id = "cities_plot_spinner", html = waiter::spin_3(), color = waiter::transparent(.5))

        future_promise({
            tsp_naivy(tb)
        }) %...>%
            result_val()

        # Return something other than the promise so shiny remains responsive
        NULL
    })

    observe({
        req(result_val())

        d <- result_val() |>
            arrange(cost)
        
        plots <- list()
        
        future_promise({
            library(sf)

            for (i in 1:nrow(d)) {
                map <- d[i, ]

                map2 <- tibble::as_tibble(tb_map) |> 
                    left_join(
                        tibble(name_muni = simplify(strsplit(map$routes, ";"))) |>
                            mutate(direction = 1:n(), fill = "1"),
                        by = "name_muni"
                    ) |>
                    mutate(fill = replace_na(fill, "0"))

                p <- map2 |>
                       ggplot() +
                       geom_sf(aes(geometry = geom, fill = fill)) +
                       geom_path(
                           data = map2 |> filter(!is.na(direction)) |> arrange(direction),
                           aes(x = longitude, y = latitude)
                       ) +
                       geom_point(
                           data = map2 |> filter(!is.na(direction) & direction == 1),
                           aes(x = longitude, y = latitude), size = 3
                       ) +
                       theme_minimal() +
                       theme(legend.position = "none") +
                       scale_fill_manual(values = c("#834d29", "#251ac5")) +
                       labs(x = "", y = "", title = paste0("Path traveled: ", formatC(map$cost / 1000, format = "f", big.mark = ",", digits = 2), " km"))
                plots <- append(plots, list(p))
            }

            plots
        }) %...>%
            result_plot()

        # Return something other than the promise so shiny remains responsive
        NULL
    })

    output$cities_layout_plot <- renderUI({
        req(result_plot())

        waiter_hide("cities_plot_spinner")

        fluidRow(
            sliderInput(
                inputId = "cities_routes_plot", 
                label = "",
                min = 1, 
                max = length(result_plot()), 
                value = 1, 
                step = 1,
                animate = animationOptions(1000)
            )
        )
    })

    output$cities_plot <- renderPlot({
        req(result_plot(), input$cities_routes_plot)
        result_plot()[[input$cities_routes_plot]]
    })

    output$cities_distance <- renderDT({
        req(tb_cities_distance())

        waiter_show(id = "cities_distance", html = waiter::spin_3(), color = waiter::transparent(.5))

        tb_cities_distance() |>
            datatable(
                rownames = FALSE,
                options = list(
                    dom = 't'
                )
            )
    })

    output$cities_results <- renderDT({
        req(result_val())
        
        waiter_hide("cities_results_spinner")

        result_val() |> 
            datatable(
                rownames = FALSE
            )
    })



})