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
        column(
          5,
          charitiesMapUI("test")
        ),
        column(
          7,
          #   downloadButton("download_data"),
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

