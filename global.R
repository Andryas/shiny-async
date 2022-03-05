library(shiny)
library(shinyjs)
library(shinyFeedback)
library(waiter)

library(sf)
library(tidyverse)

library(DT)

# Async
library(promises)
library(future)
plan(multisession)

load("data/parana_info.RData")

tsp_naivy <- function(m) { 
    rotes <- m$start_city
    start_city <- rotes[1]
    rotes <- rotes[-1]

    tb_routes <- map(
        map(combinat::permn(rotes),  ~ c(start_city, .x)),
        function(.x) {
            cost <- 0
            for (i in 1:(length(.x) - 1)) {
                cost <- cost + (m |>
                    filter(start_city == .x[i]) |>
                    select(.x[i + 1]) |>
                    pull())
            }
            
            tibble(routes = str_c(.x, collapse = ";"), cost = cost)
        }
    ) |> 
    bind_rows()

    return(tb_routes)
}

thecode <- paste0("
m <- matrix(
    c(Inf, 123, 321, 123, Inf, 234, 321, 234, Inf),
    byrow = TRUE,
    ncol = 3
)
tsp_naivy <- function(m) {
    rotes <- m$start_city
    start_city <- rotes[1]
    rotes <- rotes[-1]

    tb_routes <- map(
        map(combinat::permn(rotes), ~ c(start_city, .x)),
        function(.x) {
            cost <- 0
            for (i in 1:(length(.x) - 1)) {
                cost <- cost + (m |>
                    filter(start_city == .x[i]) |>
                    select(.x[i + 1]) |>
                    pull())
            }

            tibble(routes = str_c(.x, collapse = ';'), cost = cost)
        }
    ) |>
        bind_rows()

    return(tb_routes)
}
"
)