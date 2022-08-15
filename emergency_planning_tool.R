emergency_planning_tool <- function() {

  # ---- UI ----
  ui <- fluidPage(
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

        # Button once selected area - don't know if need this? TO DO: Think about UX
        #  actionButton("selected_area_button", "Next page")
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
        )
      ),

      # Charities - UI -------------

      tabPanel(
        title = "Organisations",
        value = "organisations",
        br(),
        fluidRow(
          # Dropdowns to select categories and services of charities
          subsetCharitiesDataCategoriesUI("test",
            charities_categories_data = charities_categories_data
          )
        ),
        fluidRow(
          # Radiobutton to select whether filtering charities within or outwith area
          radioButtons("organisation_within_area",
            label = "",
            choices = list(
              "Charities that operate & registered
                                      address is within your selected area" = TRUE,
              "Charities that operate in your selected
                                      area but the registered address is either
                                      out-with your selected area or not registered" = FALSE
            ),
            selected = TRUE,
            inline = TRUE
          )
        ),
        fluidRow(
          column(
            5,
            charitiesMapUI("test")
          ),
          column(
            7,
            downloadButton("download_data"),
            charitiesTableUI("test")
          )
        )
    ),

    # Resources - UI -------------

    tabPanel(
      title = "Resources",
      value = "resources",
    ),
    
    # Licence, methodology & data - UI -------------
    
    tabPanel(
      title = "Methodology & Data",
      value = "methodology_data",
      
      "Info here on the flooding vulnerability index (links to Sayers), any lisences & dates of data used."
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
    
    # Flooding vulnerability licence (module
    floodingLisenceServer("test")

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
          lsoas_clicked = lsoas_clicked_global,
          selected_ltlas = selected_ltlas
        )
      }
    })

    # Charities - Server -------------

    observeEvent(input$tabs, {
      if (input$tabs == "organisations") {

        # Subset charity data based on LTLAs selected
        charities_subset <- subsetCharitiesDataServer(
          "test",
          charities_data = charities_data,
          charities_ltla_lookup_data = charities_ltla_lookup,
          ltlas_for_filtering = selected_ltlas
        )

        # Subset charity data based on categories and services selected
        charities_categories_subset <- subsetCharitiesDataCategoriesServer(
          "test",
          subset_charities_data = charities_subset,
          charities_categories_data = charities_categories_data
        )

        # Map of charities working within the area (module)
        charitiesMapServer("test",
          charities_data_subset = charities_categories_subset,
          filter_for_within_area = reactive(input$organisation_within_area)
        )

        # Table of charities (module)
        charitiesTableServer("test",
          charities_data_subset = charities_categories_subset,
          filter_for_within_area = reactive(input$organisation_within_area)
        )
        
        # Download of the subset charity data 
        
        output$download_data <- downloadHandler(
          filename = function() {
            "ept_charities_extract.csv"
          },
          content = function(file) {
            write.csv(charities_categories_subset(), file)
          }
        )
      }
    })
  }

  shinyApp(ui, server)
}
