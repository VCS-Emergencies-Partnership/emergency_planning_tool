library(shiny)
library(sf)
library(leaflet)
library(readr)
library(dplyr)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(cicerone)

emergencyplanning <- function() {

  # ---- UI ----
  ui <- function(request) {
    fluidPage(
    # for hotjar tracking
    tags$head(includeScript(paste0(getwd(), "/www/hotjar.js"))),
    # For use of box() function
    useShinydashboard(),
    # For use of user guide
    use_cicerone(),
    # Make error messages red to stand out
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
        title = "Select area of interest",
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
        a("Click here", href="https://vcsep.org.uk/winter-preparedness-2022"), "to find resources on Winter Preapredeness.",
        br(),
        br(),
        "In the future, tailored links to flooding resources based on top vulnerability drivers will be found here."
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
      )
    )
  )
}


  # ---- Guided tour of UI ----
  guide <- Cicerone$
    new()$
    step(
      "head",
      "Welcome to the Emergency Planning Tool",
      "This tool can be used to understand the risk of emergency events such as flooding, with social and hazard risk data included. Factors driving vulnerability are explored and these are linked to charities working within the local area.")$
    step(
      "[data-value='selected_areas']",
      "Select an area",
      "Select an area of interest based on clicking on the region on the map or using the dropdown box.",
      is_id = FALSE
    )$
    step(
      "[data-value='vulnerabilities']",
      "Get more information",
      "Please watch this short video describing the functionality of this tool.",
      is_id = FALSE)$
    # User guide on the methodology and data tab
    step(
      "[data-value='methodology_data']",
      "Get more information",
      "Click this tab for help and further information on the model.",
      is_id = FALSE)$
    # User guide on the next button
    step(
      "selected_areas_next_button",
      "Navigate between tabs",
      "Once an area of interest has been selected, move through the tabs based on this button."
    )
    # User guide on all tabs in the tool
    # step(
    #   "tabs",
    #   "Navigate between tabs",
    #   "The map shows resilience in Local Authorities. Resilience is a combination of vulnerability, capacity to cope, and exposure to shocks.
    # <span style = 'color:#3E2948; font-weight:bold;'>Areas coloured mauve</span> are highly vulnerable, with low capacity to cope."
    # )

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

    observeEvent(input$methodology_data_back_button, {
      updateTabsetPanel(session, "tabs", selected = "resources")
    })
  }

  enableBookmarking("url")

  shinyApp(ui, server)
}
