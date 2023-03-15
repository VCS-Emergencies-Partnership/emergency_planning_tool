library(shiny)
library(sf)
library(leaflet)
library(readr)
library(dplyr)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(cicerone)
library(plotly)

emergencyplanning <- function() {

  # ---- UI ----
  ui <- function(request) {
    fluidPage(
    # for hotjar tracking
    tags$head(includeScript(paste0(getwd(), "/www/hotjar.js"))),
    # for onetrust pop-up
    tags$head(includeScript(paste0(getwd(), "/www/onetrust.js"))),
    # For use of box() function
    useShinydashboard(),
    # Add title for page in browser
    tags$head(HTML("<title>Emergency Planning Tool</title> <link rel='icon' type='image/gif/png' href='vcsep_logo_edited.png'>")),
    # For use of user guide
    use_cicerone(),
    # Make error messages red to stand out and select colour of box and bar chart
    # https://stackoverflow.com/questions/59760316/change-the-color-of-text-in-validate-in-a-shiny-app
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
      .box.box-solid.box-primary>.box-header {
                color:#fff;
                background:#5C747A
                }
              .box.box-solid.box-primary{
              border-bottom-color:#5C747A;
              border-left-color:#5C747A;
              border-right-color:#5C747A;
              border-top-color:#5C747A;
              }
            .bar-chart-bar {
          background-color: #e8e8e8;
          display: block;
          position:relative;
          width: 100%;
          height: 20px;
      }
    "))
    ),
    fluidRow(
      column(
        9,
        align = "left",
        # Tool explanation
        explanation()
      ),
      column(
        3,
        fluidRow(
          align = "right",
             bookmarkButton()
          ),
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
        title = "Select an area",
        value = "selected_areas",
        fluidRow(
          column(
            6,
            align = "left",
            # Dropdown to select area (module)
            selectAreasDropdownUI("test")
          ),
          column(
            6,
            align = "right",
            # Button to move to next page
            actionButton("selected_areas_next_button", "Next page")
          )
        ),

        # Map to select area (module)
        selectedAreaMapUI("test"),
      ),

      # Vulnerabilities - UI -------------

      tabPanel(
        title = "Vulnerabilities",
        value = "vulnerabilities",
        fluidRow(
          column(
            6,
            align = "left",
            # Button to move back page
            actionButton("vulnerabilities_back_button", "Back page"),
          ),
          column(
            6,
            align = "right",
            # Button to move to next page
            actionButton("vulnerabilities_next_button", "Next page")
          )
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
            align = "centre",
            fluidRow(
              # Table of top drivers of vulnerability for clicked LSOA (module)
              topDriversTableUI("test")
            ),
            fluidRow(
              # Table of top drivers of vulnerability for clicked LSOA (module)
              jitterPlotUI("test")
            )
            # br(),
            # fluidRow(
            #   column(width = 1),
            #   # Metric of % of most vulnerable neighborhoods (module)
            #   column(10,
            #          align = "center",
            #          topVulnUI("test")
            #   ),
              # column(width = 1)
            # ),
          )
        ),
        fluidRow(
          align = "left",
          # Button to show licence (module)
          floodingLisenceUI("test")
        )
      ),

      # Charities - UI -------------

      tabPanel(
        title = "Organisations",
        value = "organisations",
        fluidRow(
          column(
            6,
            align = "left",
            # Button to move back page
            actionButton("organisations_back_button", "Back page"),
          ),
          column(
            6,
            align = "right",
            # Button to move to next page
            actionButton("organisations_next_button", "Next page")
          )
        ),
        br(),
        fluidRow(
          # Text to show top drivers of vulnerability for that LTLA
          # & dropdown to subset data based on these top drivers
          subsetCharitiesDataUI("test")
        ),
        fluidRow(
          column(
            5,
            # Map of charities working within the area (module)
            charitiesMapUI("test")
          ),
          column(
            7,
            # Table of charities working within the area (module)
            charitiesTableUI("test")
          )
        )
      ),

      # Resources - UI -------------

      tabPanel(
        title = "Resources",
        value = "resources",
        fluidRow(
          column(
            6,
            align = "left",
            # Button to move back page
            actionButton("resources_back_button", "Back page"),
          ),
          column(
            6,
            align = "right",
            # Button to move to next page
            actionButton("resources_next_button", "Next page")
          )
        ),
        br(),
        resources_page()
        ),

      # Licence, methodology & data - UI -------------

      tabPanel(
        title = "Methodology & data",
        value = "methodology_data",
        fluidRow(
          column(
            6,
            align = "left",
            # Button to move back page
            actionButton("methodology_data_back_button", "Back page")
          )
        ),
        br(),
        methodology_writeup()
      ),

      # Cookie page - UI -------------

      tabPanel(
        title = "Cookie Policy",
        value = "cookie_policy",
        fluidRow(
          column(
            6,
            align = "left",
            # Button to move back page
            actionButton("cookie_data_back_button", "Back page")
          )
        ),
        br(),
        cookie()
      )
    )
  )
}


  # ---- Guided tour of UI ----
  guide <- Cicerone$
    new()$
    step(
      el = "card_header",
      title = "Welcome to the Emergency Planning Tool",
      description = "Use this interactive tool to understand the risk of emergency events, such as flooding, in your local area. The tools uses a combination of <b>social vulnerability</b> and <b>hazard risk</b> data. Factors driving social vulnerability are explored and these are linked to charities working within your local area."
      )$
    # <br><br><a href=https://www.w3schools.com>Watch this short video</a> describing the tool.
    step(
      "[data-value='selected_areas']",
      "Select an area",
      "Select an area of interest based on clicking on the region on the map or using the dropdown box.",
      is_id = FALSE
    )$
    # User guide on the next button
    step(
      "selected_areas_next_button",
      "Navigate between tabs",
      "Once an area of interest has been selected, move through the tabs using this button."
    )

  # ---- Server ----

  server <- function(input, output, session) {

    # initialise then start the guide
    guide$init()$start()

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
          lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset,
          selected_ltlas = selected_ltlas
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

        # Jitter plot (module)
        jitterPlotServer("test",
          lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset,
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
          charities_subset = charities_subset$data,
          error_text = charities_subset$error_text,
          lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset
        )

        # Table of charities (module)
        charitiesTableServer("test",
          charities_subset = charities_subset$data
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

    observeEvent(input$resources_next_button, {
      updateTabsetPanel(session, "tabs", selected = "cookie_policy")
    })

    observeEvent(input$methodology_data_back_button, {
      updateTabsetPanel(session, "tabs", selected = "resources")
    })
  }

  enableBookmarking("url")

  shinyApp(ui, server)
}
