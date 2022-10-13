library(shiny)
library(sf)
library(leaflet)
library(readr)
library(dplyr)
library(shinyWidgets)
library(DT)

emergencyplanning <- function() {

  # ---- UI ----
  ui <- fluidPage(
    # for hotjar tracking
    tags$head(includeScript(paste0(getwd(), "/www/hotjar.js"))),
    fluidRow(
      column(
        9,
        align = "left",
        # Tool explanation
        explanation()
      ),
      column(
        3,
        # Dropdown to select emergency
        selectInput(
          "emergency_type",
          label = "Select emergency event",
          choices = c("Flooding")
        )
      )
    ),
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

        # Button to move to next page
        actionButton("selected_areas_next_button", "Next page")
      ),

      # Vulnerabilities - UI -------------

      tabPanel(
        title = "Vulnerabilities",
        value = "vulnerabilities",
        br(),
        fluidRow(
          # Button to show licence (module)
          floodingLisenceUI("test")
        ),
        br(),
        fluidRow(
          column(
            6,
            # Vulnerability index map (module)
            vulnMapUI("test")
          ),
          column(
            6,
            fluidRow(
              column(width = 1),
              # Metric of % of most vulnerable neighborhoods (module)
              column(10,
                align = "center",
                topVulnUI("test")
              ),
              column(width = 1)
            ),
            br(),
            fluidRow(
              align = "center",
              # Table of top drivers of vulnerability for clicked LSOA (module)
              topDriversTableUI("test")
            )
          )
        ),

        # Button to move back page
        actionButton("vulnerabilities_back_button", "Back page"),

        # Button to move to next page
        actionButton("vulnerabilities_next_button", "Next page")
      ),

      # Charities - UI -------------

      tabPanel(
        title = "Organisations",
        value = "organisations",
        br(),
        fluidRow(
          # Dropdowns to select categories from top drivers of vulnerability for that LTLA
          subsetCharitiesDataUI("test")
        ),
        fluidRow(
          column(
            5,
            charitiesMapUI("test")
          ),
          column(
            7,
            #   downloadButton("download_data"),
            charitiesTableUI("test")
          )
        ),

        # Button to move back page
        actionButton("organisations_back_button", "Back page"),

        # Button to move to next page
        actionButton("organisations_next_button", "Next page")
      ),

      # Resources - UI -------------

      tabPanel(
        title = "Resources",
        value = "resources",
        "Link here to VCSEP website flooding page.",

        # Button to move back page
        actionButton("resources_back_button", "Back page"),

        # Button to move to next page
        actionButton("resources_next_button", "Next page")
      ),

      # Licence, methodology & data - UI -------------

      tabPanel(
        title = "Methodology & Data",
        value = "methodology_data",
        "Info here on the flooding vulnerability index (links to Sayers), any lisences & dates of data used.",

        # Button to move back page
        actionButton("methodology_data_back_button", "Back page"),
      )
    )
  )


  # ---- Server ----

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

    # Flooding vulnerability licence (module
    floodingLisenceServer("test")

    # Only render the vulnerability tab components when the tab is selected
    observeEvent(input$tabs, {
      if (input$tabs == "vulnerabilities") {

        # Subset the vulnerability scores data with selected LSOAs
        lsoa_vuln_scores_subset <- subsetVulnDataServer(
          "test",
          lsoa_data = vuln_scores_flood_lsoa,
          ltlas_for_filtering = selected_ltlas
        )

        # Metric of % of most vulnerable neighborhoods (module)
        topVulnServer("test",
          lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset
        )

        # Vulnerability index map (module)
        vulnMapServer("test",
          lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset,
          flood_risk_data = lsoa_flood_risk_ltla_lookup,
          lsoas_clicked = lsoas_clicked_global
        )

        # # Variable to store the LSOA of clicked
        lsoas_clicked_global <- reactiveVal()

        # Table of top drivers of vulnerability for clicked LSOA (module)
        topDriversTableServer("test",
          vuln_drivers = vuln_drivers_flood_lsoa,
          lsoas_clicked = lsoas_clicked_global,
          selected_ltlas = selected_ltlas
        )
      }
    })

    # Charities - Server -------------

    observeEvent(input$tabs, {
      if (input$tabs == "organisations") {

        # Subset the vulnerability scores data with selected LSOAs
        # Repeated from when the 'vulnerabilities' tab is selected
        lsoa_vuln_scores_subset <- subsetVulnDataServer(
          "test",
          lsoa_data = vuln_scores_flood_lsoa,
          ltlas_for_filtering = selected_ltlas
        )

        # Subset charity data based on LTLAs selected
        charities_subset <- subsetCharitiesDataServer(
          "test",
          charities_vuln_drivers_flood_lookup = charities_vuln_drivers_flood_lookup,
          charities_lat_long = charities_lat_long,
          charities_ltla_lookup = charities_ltla_lookup,
          vuln_drivers_flood_ltla = vuln_drivers_flood_ltla,
          ltlas_for_filtering = selected_ltlas
        )


        # Map of charities working within the area (module)
        charitiesMapServer("test",
          charities_subset = charities_subset,
          lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset
        )

        # Table of charities (module)
        charitiesTableServer("test",
          charities_subset = charities_subset
        )
      }
    })

    # Navigation buttons - Server -------------

    observeEvent(input$selected_areas_next_button, {
      updateTabsetPanel(session, "tabs", selected = "vulnerabilities")
    })

    observeEvent(input$vulnerabilities_back_button, {
      updateTabsetPanel(session, "tabs", selected = "selected_areas")
    })

    observeEvent(input$vulnerabilities_next_button, {
      updateTabsetPanel(session, "tabs", selected = "organisations")
    })

    observeEvent(input$organisations_back_button, {
      updateTabsetPanel(session, "tabs", selected = "vulnerabilities")
    })

    observeEvent(input$organisations_next_button, {
      updateTabsetPanel(session, "tabs", selected = "resources")
    })

    observeEvent(input$resources_back_button, {
      updateTabsetPanel(session, "tabs", selected = "organisations")
    })

    observeEvent(input$resources_next_button, {
      updateTabsetPanel(session, "tabs", selected = "methodology_data")
    })

    observeEvent(input$methodology_data_back_button, {
      updateTabsetPanel(session, "tabs", selected = "resources")
    })
  }



  shinyApp(ui, server)
}
