welfareViz_server <- function(
    id,
    simulation_results,
    state_names,
    state_len,
    shp_file,
    focus_pool,
    state_toggle,
    statesINI_list,
    states_list
) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # Create a dataset if welfare_results is NULL
        plotting_values <- shiny::reactiveVal({
            if (is.null(simulation_results())) {
                welfare_results <- data.frame(
                    orig = state_names(),
                    welfare = rep(1, state_len),
                    nominal_wage = rep(1, state_len),
                    price_index = rep(1, state_len)
                )
            } else {
                welfare_results <- simulation_results()$new_welfare
            }

            # Renaming for clarity
            names(welfare_results) <- c(
                "State",
                "Expenditure",
                "Wage",
                "Price"
            )

            # Making them percentages (assumption: 50% intermediate inputs)
            welfare_results$Expenditure <- 100 *
                ((welfare_results$Expenditure^(1 / (1 - .5))) - 1)
            welfare_results$Wage <- 100 *
                ((welfare_results$Wage^(1 / (1 - .5))) - 1)
            welfare_results$Price <- 100 *
                ((welfare_results$Price^(1 / (1 - .5))) - 1)

            welfare_results
        })

        # Observe click events on the map
        clicked_info <- reactiveVal(NULL)
        shiny::observeEvent(input$map_click, {
            req(plotting_values()) # Ensure plotting_values is not NULL

            # Join the filtered values with the shapefile data
            shp_file_joined <- dplyr::left_join(
                shp_file,
                plotting_values(),
                by = getJoinKey(plotting_values())
            )
            click <- input$map_click
            clicked_point <- tryCatch(
                {
                    suppressWarnings(
                        # Supress warnings since shpfile is not true geography, but a hexagon
                        sf::st_nearest_feature(
                            sf::st_sfc(
                                sf::st_point(c(click$x, click$y)),
                                crs = sf::st_crs(shp_file_joined)
                            ),
                            shp_file_joined
                        )
                    )
                },
                error = function(e) NULL # Handle errors gracefully
            )

            # Check if a valid feature was found
            if (!is.null(clicked_point) && clicked_point > 0) {
                # Store information about the clicked hexagon
                clicked_info(shp_file_joined[clicked_point, ])
            } else {
                # No feature detected, set clicked_info to NULL
                clicked_info(NULL)
            }
        })

        output$map <- shiny::renderPlot(
            {
                req(plotting_values()) # Ensure plotting_values is not NULL

                hexagon_map(
                    plotting_values = plotting_values(),
                    focus_pool = focus_pool(),
                    clicked_info = clicked_info(),
                    shp_file = shp_file,
                    input = input
                )
            },
            res = 96
        )

        output$bars <- shiny::renderPlot({
            req(focus_pool())
            req(plotting_values())

            bars_plot(
                plotting_values = plotting_values(),
                focus_pool = focus_pool(),
                state_toggle = state_toggle(),
                input = input
            )
        })

        # Placeholder for the results table
        output$results_table <- DT::renderDT(
            {
                # Add your table logic here
                plotting_values() %>%
                    dplyr::mutate(
                        "Expenditure" = round(Expenditure, 2),
                        "Wage" = round(Wage, 2),
                        "Price" = round(Price, 2)
                    ) %>%
                    dplyr::rename(
                        "State" = "State",
                        "Expenditure (%)" = "Expenditure",
                        "Wage (%)" = "Wage",
                        "Price (%)" = "Price"
                    )
            },
            options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            ),
            extensions = c('Buttons'),
            rownames = FALSE
        )

        output$download_map_png <- downloadHandler(
            filename = function() {
                paste0("hexagon_map_", Sys.Date(), ".png")
            },
            content = function(file) {
                p <- hexagon_map(
                    plotting_values = plotting_values(),
                    focus_pool = focus_pool(),
                    clicked_info = clicked_info(),
                    shp_file = shp_file,
                    input = input
                )
                p <- p +
                    ggplot2::annotate(
                        "text",
                        x = -Inf,
                        y = -Inf,
                        label = "https://noejn2.github.io",
                        hjust = -0.05,
                        vjust = -1.2,
                        size = 4,
                        color = "#888888"
                    )
                png(file, width = 1200, height = 900, res = 150)
                print(p)
                dev.off()
            }
        )

        output$download_bar_png <- downloadHandler(
            filename = function() {
                paste0("bar_plot_", Sys.Date(), ".png")
            },
            content = function(file) {
                p <- bars_plot(
                    plotting_values = plotting_values(),
                    focus_pool = focus_pool(),
                    state_toggle = state_toggle(),
                    input = input
                )
                p <- p +
                    ggplot2::annotate(
                        "text",
                        x = Inf,
                        y = -Inf,
                        label = "https://noejn2.github.io",
                        hjust = 1.05,
                        vjust = -1.2,
                        size = 4,
                        color = "#888888"
                    )
                png(file, width = 1200, height = 900, res = 150)
                print(p)
                dev.off()
            }
        )
    })
}
