tradeflowViz_server <- function(
    id,
    state_names,
    focus_pool, # Focus pool
    simulation_results, # Simulation results
    trade_data # Trade data
) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Initialize plotting data
        plotting_values <- shiny::reactiveVal(NULL)

        # Initialize include_ls
        other_selection <- shiny::reactiveVal(NULL)

        # Initilize varibale to include self-trade in visualization
        self_selection <- shiny::reactiveVal(NULL)

        # Initialize variable to include net exports in visualization
        net_exports <- shiny::reactiveVal(NULL)

        # Initlize variable to include baseline/simulation in visualization
        baseline_simulation <- shiny::reactiveVal(NULL)

        shiny::observeEvent(input$baseline, {
            if (input$baseline) {
                baseline_simulation(TRUE)
            } else {
                baseline_simulation(FALSE)
            }
        })

        shiny::observeEvent(input$net_exports, {
            if (input$net_exports) {
                net_exports(TRUE)
            } else {
                net_exports(FALSE)
            }
        })

        # Observe the focus pool and update the plotting values accordingly
        shiny::observeEvent(input$include_others, {
            if (input$include_others) {
                other_selection(TRUE)
            } else {
                other_selection(FALSE)
            }
        })

        # Observe the include_others input and update the plotting values accordingly
        shiny::observeEvent(input$include_self, {
            if (input$include_self) {
                self_selection(TRUE)
            } else {
                self_selection(FALSE)
            }
        })

        # Observe the focus pool and update the plotting values accordingly
        shiny::observe({
            if (is.null(simulation_results())) {
                trade_results <- expand.grid(
                    Origin = state_names(),
                    Destination = state_names()
                )
                # This happens when clear simulation is clicked.
                trade_results <- trade_data()
            } else {
                trade_results <- simulation_results()$new_trade
            }

            # If baseline is selected, use the trade data
            if (baseline_simulation()) trade_results <- trade_data()

            # Renaming for clarity
            names(trade_results) <- c(
                "Origin",
                "Destination",
                "simTrade"
            )

            #trade_results$`simTrade` <- round(trade_results$`simTrade`, 2)
            trade_results$`Trade` <- trade_data()$flow

            plotting_values(trade_results)
        })

        # Add summary_table output
        output$summary_table <- DT::renderDT({
            req(focus_pool(), plotting_values())

            summary_data_plotting <- plotting_values()

            if (!baseline_simulation()) {
                summary_data_plotting <- summary_data_plotting %>%
                    dplyr::select(!Trade) %>%
                    dplyr::rename(Trade = simTrade)
            }

            # create self trade column
            firstpart <- summary_data_plotting %>%
                dplyr::group_by(Origin) %>%
                dplyr::summarise(
                    `Self Consumption` = sum(Trade[Origin == Destination]),
                    `Total Exports` = sum(Trade[Origin != Destination]),
                    .groups = "drop"
                ) %>%
                dplyr::rename(State = Origin)

            secondpart <- summary_data_plotting %>%
                dplyr::group_by(Destination) %>%
                dplyr::summarise(
                    `Total Imports` = sum(Trade[Origin != Destination]),
                    .groups = "drop"
                ) %>%
                dplyr::rename(State = Destination)

            dplyr::left_join(firstpart, secondpart, by = "State") %>%
                dplyr::mutate(
                    `Self Consumption` = round(`Self Consumption`, 2),
                    `Total Exports` = round(`Total Exports`, 2),
                    `Total Imports` = round(`Total Imports`, 2)
                ) %>%
                dplyr::mutate(
                    `Self Consumption` = paste0(
                        "$",
                        formatC(
                            `Self Consumption`,
                            format = "f",
                            digits = 2,
                            big.mark = ","
                        )
                    ),
                    `Total Exports` = paste0(
                        "$",
                        formatC(
                            `Total Exports`,
                            format = "f",
                            digits = 2,
                            big.mark = ","
                        )
                    ),
                    `Total Imports` = paste0(
                        "$",
                        formatC(
                            `Total Imports`,
                            format = "f",
                            digits = 2,
                            big.mark = ","
                        )
                    )
                ) %>%
                dplyr::mutate(
                    State = factor(
                        State,
                        levels = c(
                            focus_pool(),
                            "Other",
                            setdiff(unique(State), focus_pool())
                        )
                    )
                ) %>%
                dplyr::arrange(State) %>%
                DT::datatable(
                    rownames = FALSE,
                    extensions = c("Buttons", "Scroller"),
                    options = list(
                        dom = "Bfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print"),
                        scrollY = 400,
                        scrollX = TRUE,
                        paging = FALSE
                    )
                )
        })

        output$left <- DT::renderDT({
            req(focus_pool(), plotting_values())

            trade_data_plotting <- plotting_values()

            # regions not in focus pool are set to "Other"
            trade_data_plotting$Origin[
                !trade_data_plotting$Origin %in% focus_pool()
            ] <- "Other"
            trade_data_plotting$Destination[
                !trade_data_plotting$Destination %in% focus_pool()
            ] <- "Other"

            # Get unique regions (up to 8) and always put 'Other' last
            unique_regions <- unique(c(
                trade_data_plotting$Origin,
                trade_data_plotting$Destination
            ))
            unique_regions <- unique_regions[unique_regions != "Other"]
            unique_regions <- head(unique_regions, 8)
            region_levels <- c(unique_regions, "Other")

            # Set factor levels to ensure color mapping is consistent
            trade_data_plotting$Origin <- factor(
                trade_data_plotting$Origin,
                levels = region_levels
            )
            trade_data_plotting$Destination <- factor(
                trade_data_plotting$Destination,
                levels = region_levels
            )

            trade_data_plotting <- trade_data_plotting %>%
                dplyr::group_by(Origin, Destination) %>%
                dplyr::summarise(
                    Trade = sum(Trade),
                    simTrade = sum(simTrade),
                    .groups = "drop"
                ) %>%
                dplyr::mutate(netTrade = simTrade - Trade) %>%
                dplyr::select(
                    Origin,
                    Destination,
                    netTrade
                )

            if (!other_selection()) {
                trade_data_plotting <- trade_data_plotting %>%
                    dplyr::filter(Origin != "Other") %>%
                    dplyr::filter(Destination != "Other")
            }

            # Prepare matrix for table
            trade_matrix <- trade_data_plotting %>%
                tidyr::pivot_wider(
                    names_from = Destination,
                    values_from = netTrade,
                    values_fill = 0
                ) %>%
                as.data.frame()

            # Ensure unique row names
            trade_matrix <- trade_matrix %>%
                dplyr::group_by(Origin) %>%
                dplyr::summarise(across(everything(), sum), .groups = "drop")

            # Optionally, round values for display
            trade_matrix <- trade_matrix %>%
                dplyr::mutate(across(
                    where(is.numeric),
                    ~ paste0(
                        "$",
                        formatC(., format = "f", digits = 2, big.mark = ",")
                    )
                )) %>%
                dplyr::rename(
                    `Change in trade flows` = Origin
                )

            DT::datatable(
                trade_matrix,
                rownames = FALSE,
                extensions = c("Buttons", "Scroller"),
                options = list(
                    dom = "Bfrtip",
                    buttons = c("copy", "csv", "excel", "pdf", "print"),
                    scrollY = 400,
                    scrollX = TRUE,
                    paging = FALSE
                )
            )
        })

        output$chord <- chorddiag::renderChorddiag({
            req(
                focus_pool(),
                plotting_values()
            )

            chord_flows(
                plotting_values = plotting_values(),
                focus_pool = focus_pool(),
                other_selection = other_selection(),
                net_exports = net_exports(),
                self_selection = self_selection()
            )
        })

        output$download_chord_png <- downloadHandler(
            filename = function() {
                paste0("chord_diagram_", Sys.Date(), ".png")
            },
            content = function(file) {
                # Save the interactive chorddiag as a temporary HTML file
                chord <- chord_flows(
                    plotting_values = plotting_values(),
                    focus_pool = focus_pool(),
                    other_selection = other_selection(),
                    net_exports = net_exports(),
                    self_selection = self_selection()
                )
                temp_html <- tempfile(fileext = ".html")
                htmlwidgets::saveWidget(chord, temp_html, selfcontained = FALSE)
                # Use webshot2 to capture the widget as PNG, then add watermark
                temp_png <- tempfile(fileext = ".png")
                webshot2::webshot(
                    temp_html,
                    file = temp_png,
                    vwidth = 900,
                    vheight = 900,
                    zoom = 2
                )
                # Add watermark using magick
                img <- magick::image_read(temp_png)
                img <- magick::image_annotate(
                    img,
                    text = "https://noejn2.github.io",
                    gravity = "southwest",
                    location = "+20+20",
                    size = 28,
                    color = "#888888"
                )
                magick::image_write(img, path = file, format = "png")
            }
        )
    })
}
