emergency_planning_tool <- function() {

  # ---- UI ----
  ui <- fluidPage(
    tabsetPanel(
      id = "tabs",

      # Selected Areas - UI -------------
      tabPanel(
        title = "Select Areas",
        value = "selected_areas",

        # Dropdown to select area (module)
        selectAreasDropdownUI("test"),

        # Map to select area (module)
        selectedAreaMapUI("test"),

        # Button once selected area - don't know if need this? TO DO: Think about UX
        #  actionButton("selected_area_button", "Next page")
      ),

      # Vulnerabilities - UI -------------

      tabPanel(
        title = "Vulnerabilities",
        value = "vulnerabilities",
        fluidRow(

          # Metric of % of most vulnerable neighborhoods (module)
          topVulnUI("test")
        ),
        fluidRow(
          # Vulnerability index map (module)
          vulnMapUI("test")
        ),
        fluidRow(
          # Table of top drivers of vulnerability for clicked LSOA (module)
          topDriversTableUI("test")
        )
      ),

      # Charities - UI -------------

      tabPanel(
        title = "Organisations",
        value = "organisations",
        fluidRow(
          column(
            4,
            # Map of charities working within the area (module)
            charitiesMapUI("test")
          ),
          column(
            8,
            # Table of charities (module)
            charitiesTableUI("test")
          )
        )
      )
    )
  )


  server <- function(input, output, session) {

    # Selected Areas - Server -------------

    # Set an empty global reactive values list to be passed between modules
    selected_ltlas <- reactiveVal(c())

    # Dropdown to select area (module)
    selectAreasDropdownServer("test",
      selected_ltlas = selected_ltlas
    )

    # Map to select area (module)
    selectedAreaMapServer("test",
      boundaries_data = boundaries_ltlas,
      selected_ltlas = selected_ltlas
    )

    # Vulnerabilities - Server -------------

    # Only render the vulnerability tab components when the tab is selected
    observeEvent(input$tabs, {
      if (input$tabs == "vulnerabilities") {

        # Subset the vulnerability scores data with selected LSOAs
        lsoa_vuln_scores_subset <- subsetVulnDataServer(
          "test",
          lsoa_data = vuln_scores_flood,
          ltlas_for_filtering = selected_ltlas
        )

        # Metric of % of most vulnerable neighborhoods (module)
        topVulnServer("test",
          lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset
        )

        # Vulnerability index map (module)
        vulnMapServer("test",
          lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset,
          lsoas_clicked = lsoas_clicked_global
        )

        # # Variable to store the LSOA of clicked
        lsoas_clicked_global <- reactiveVal()

        # Table of top drivers of vulnerability for clicked LSOA (module)
        topDriversTableServer("test",
          vuln_drivers = vuln_drivers_flood,
          lsoas_clicked = lsoas_clicked_global
        )
      }
    })

    # Charities - Server -------------

    observeEvent(input$tabs, {
      if (input$tabs == "organisations") {
        charities_subset <- subsetCharitiesDataServer(
          "test",
          charities_data = charities_data,
          charities_ltla_lookup_data = charities_ltla_lookup,
          ltlas_for_filtering = selected_ltlas
        )

        # Map of charities working within the area (module)
        charitiesMapServer("test",
          charities_data_subset = charities_subset
        )

        # Table of charities (module)
        charitiesTableServer("test",
          charities_data_subset = charities_subset
        )
      }
    })
  }

  shinyApp(ui, server)
}
