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
        shiny::observeEvent(input$figure1_click, {
            req(plotting_values()) # Ensure plotting_values is not NULL

            # Join the filtered values with the shapefile data
            shp_file_joined <- dplyr::left_join(
                shp_file,
                plotting_values(),
                by = getJoinKey(plotting_values())
            )
            click <- input$figure1_click
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

        # Placeholder for Figure 1
        output$figure1 <- shiny::renderPlot(
            {
                req(plotting_values()) # Ensure plotting_values is not NULL

                # Join the filtered values with the shapefile data
                shp_file_joined <- dplyr::left_join(
                    shp_file,
                    plotting_values(),
                    by = getJoinKey(plotting_values())
                ) %>%
                    dplyr::mutate(
                        State = if ("AL" %in% plotting_values()$State) {
                            Code
                        } else {
                            Name
                        }
                    ) # Add a new column for state names

                # Plot the map with the selected variable
                shp_file_joined %>%
                    dplyr::mutate(
                        "Variable" = round(
                            get(input$variable_selector),
                            2
                        ),
                        line_size = ifelse(
                            State %in%
                                c(
                                    focus_pool(),
                                    clicked_info()$Name,
                                    clicked_info()$Code
                                ),
                            1.5,
                            .3
                        ),
                        line_color = ifelse(
                            State %in% focus_pool(),
                            # Use the same color palette as figure2, mapped to State
                            c(
                                "#1b9e77",
                                "#d95f02",
                                "#7570b3",
                                "#e7298a",
                                "#66a61e",
                                "#e6ab02",
                                "#a6761d",
                                "#666666"
                            )[match(State, sort(unique(focus_pool())))],
                            "grey70"
                        )
                    ) %>%
                    ggplot2::ggplot() +
                    ggplot2::geom_sf(
                        ggplot2::aes(
                            fill = Variable,
                            linewidth = line_size,
                            color = line_color
                        )
                    ) +
                    ggplot2::scale_linewidth_identity() +
                    ggplot2::geom_sf_text(
                        ggplot2::aes(label = Code),
                        size = 3,
                        color = "black"
                    ) +
                    ggplot2::scale_fill_gradient2(
                        low = "brown",
                        mid = "lightgray",
                        high = "darkgreen",
                        midpoint = 0,
                        na.value = "grey90",
                        guide = ggplot2::guide_colorbar(
                            position = "inside",
                            title = "Change (%)",
                            title.position = "top",
                            title.hjust = 0.5,
                            barwidth = 20,
                            barheight = 0.5
                        )
                    ) +
                    ggplot2::scale_color_identity() +
                    ggplot2::coord_sf(
                        xlim = c(-123, -68),
                        ylim = c(20, 60),
                        expand = FALSE
                    ) +
                    ggplot2::labs(
                        title = NULL,
                        x = NULL,
                        y = NULL,
                        caption = "Click a state for details."
                    ) +
                    ggplot2::theme_minimal(base_size = 16) +
                    ggplot2::theme(
                        aspect.ratio = 0.7,
                        legend.position = c(0.4, 0.9),
                        legend.direction = "horizontal",
                        legend.title = ggplot2::element_blank(),
                        legend.text = ggplot2::element_text(size = 13),
                        legend.background = ggplot2::element_rect(
                            fill = "transparent",
                            color = NA
                        ),
                        legend.key = ggplot2::element_rect(
                            fill = "transparent",
                            color = NA
                        ),
                        axis.text.x = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_blank(),
                        axis.title = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        plot.title = ggplot2::element_blank(),
                        plot.caption = ggplot2::element_text(
                            size = 12,
                            hjust = 0.5,
                            color = "#888888",
                            margin = ggplot2::margin(t = 10)
                        ),
                        plot.margin = ggplot2::margin(10, 10, 30, 10),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.background = ggplot2::element_rect(
                            fill = "white",
                            color = NA
                        ),
                        plot.background = ggplot2::element_rect(
                            fill = "white",
                            color = NA
                        )
                    ) +
                    ggplot2::annotate(
                        "text",
                        x = -119,
                        y = 25,
                        label = if (!is.null(clicked_info())) {
                            paste0(
                                clicked_info()$Name,
                                " (",
                                clicked_info()$Code,
                                ")",
                                "\n",
                                input$variable_selector,
                                ": ",
                                if (is.numeric(clicked_info()$Expenditure)) {
                                    round(
                                        clicked_info()[[
                                            input$variable_selector
                                        ]],
                                        2
                                    )
                                } else {
                                    0
                                },
                                " %"
                            )
                        } else {
                            ""
                        },
                        size = 5.5,
                        fontface = "bold",
                        hjust = 0,
                        vjust = 1,
                        color = "#34495e"
                    )
            },
            res = 96
        )

        output$figure2 <- shiny::renderPlot({
            req(focus_pool())
            req(plotting_values())

            # Define the color palette
            color_palette <- c(
                "#1b9e77", # green
                "#d95f02", # orange
                "#7570b3", # purple
                "#e7298a", # pink
                "#66a61e", # olive
                "#e6ab02", # yellow
                "#a6761d", # brown
                "#666666" # gray
            )

            # Prepare data for plotting
            plotting_data <- plotting_values() %>%
                dplyr::filter(State %in% focus_pool()) %>%
                dplyr::mutate(
                    Variable = round(get(input$variable_selector), 2)
                ) %>%
                dplyr::arrange(State) # Alphabetical order

            # Assign colors alphabetically
            state_levels <- sort(unique(plotting_data$State))
            color_map <- setNames(
                color_palette[seq_along(state_levels)],
                state_levels
            )
            plotting_data$State <- factor(
                plotting_data$State,
                levels = state_levels
            )

            ggplot2::ggplot(
                plotting_data,
                ggplot2::aes(
                    x = reorder(State, Variable),
                    y = Variable,
                    fill = State
                )
            ) +
                ggplot2::geom_col(
                    width = 0.65,
                    show.legend = FALSE,
                    color = "grey30",
                    alpha = 0.9
                ) +
                ggplot2::geom_text(
                    ggplot2::aes(label = paste0(Variable, "%")),
                    vjust = -0.5,
                    size = 5,
                    fontface = "bold",
                    color = "black"
                ) +
                ggplot2::scale_fill_manual(values = color_map) +
                ggplot2::labs(
                    title = paste(
                        "Change in",
                        input$variable_selector,
                        "for Focus States"
                    ),
                    x = NULL,
                    y = NULL
                ) +
                ggplot2::theme_minimal(base_size = 15) +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = if (state_toggle()) 0 else 40,
                        hjust = 1,
                        vjust = 1,
                        size = 14,
                        face = "bold"
                    ),
                    axis.text.y = ggplot2::element_text(size = 13),
                    axis.title.y = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(
                        size = 16,
                        face = "bold",
                        hjust = 0.5
                    ),
                    panel.grid.major.x = ggplot2::element_blank(),
                    panel.grid.minor.x = ggplot2::element_blank(),
                    panel.background = ggplot2::element_rect(
                        fill = "white",
                        color = NA
                    ),
                    plot.background = ggplot2::element_rect(
                        fill = "white",
                        color = NA
                    )
                ) +
                ggplot2::scale_y_continuous(
                    expand = ggplot2::expansion(mult = c(0, 0.18))
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
    })
}
