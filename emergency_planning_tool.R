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

        # Button once selected area
        actionButton("selected_area_button", "Next page")
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
    selected_lsoas <- reactiveVal(c())

    # To be replaced with selected values from page 1
    # lsoas_selected <- reactive({
    #   c(
    #     "E01012000", "E01011951", "E01011999", "E01011967", "E01011964", "E01011965",
    #     "E01011954", "E01011952", "E01011953", "E01011992", "E01011969", "E01011963",
    #     "E01011960", "E01011968", "E01012001", "E01011950", "E01011987", "E01011986",
    #     "E01011985", "E01011984", "E01011970", "E01011971", "E01033465", "E01033467",
    #     "E01011974", "E01011955", "E01011994", "E01011993", "E01011991", "E01011977",
    #     "E01011979", "E01011981", "E01011980", "E01011962", "E01011949", "E01011978",
    #     "E01011990", "E01011989", "E01011988", "E01011961", "E01012003", "E01011959",
    #     "E01011983", "E01011982", "E01011997", "E01011996", "E01011995", "E01011998",
    #     "E01012005", "E01011957", "E01033466", "E01032540", "E01032541", "E01012002",
    #     "E01011976", "E01011975", "E01011973", "E01011966"
    #   )
    # })

    # Dropdown to select area (module)
    selectAreasDropdownServer("test",
      selected_ltlas = selected_ltlas
    )

    # Map to select area (module)
    selectedAreaMapServer("test",
      boundaries_data = boundaries_ltlas,
      selected_ltlas = selected_ltlas
    )


    observeEvent(input$selected_area_button, {
      
      # print(selected_lsoas())
      # print(selected_ltlas())
      # Once click button then find the LSOAs within the selected_ltlas
      lsoas <- subsetLSOAServer("test",
                       lsoa_ltla_lookup = lsoa_ltla_lookup,
                       ltlas_for_filtering = selected_ltlas)
      
       selected_lsoas(lsoas)
       
       observe(print(selected_lsoas()))
       observe(print(lsoas()))
      # 
      # print(lsoas)
      # print(selected_lsoas())
      # print(selected_lsoas)
      
      # Swap to vulnerability tab
      # updateTabsetPanel(session,
      #   "tabs",
      #   selected = "vulnerabilities"
      # )


    # observeEvent(input$selected_area_button, {
    #   # Swap to vulnerability tab
    #   updateTabsetPanel(session,
    #     "tabs",
    #     selected = "vulnerabilities"
    #   )
    # })

    # Vulnerabilities - Server -------------

    # Only render the vulnerability tab components when the tab is selected
    # observeEvent(input$tabs, {
    #   if (input$tabs == "vulnerabilities") {

    # Subset the vulnerability scores data with selected LSOAs
    lsoa_vuln_scores_subset <- subsetVulnDataServer(
      "test",
      lsoa_data = vuln_scores_flood,
      lsoas_for_filtering = selected_lsoas
    )

     observe(print(lsoa_vuln_scores_subset()))
       
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
    # }
    # })

    # Charities - Server -------------

    # observeEvent(input$tabs, {
    #
    #   # Only render the vulnerability tab components when the tab is selected
    #   if (input$tabs == "organisations") {
    

      
    charities_subset <- subsetCharitiesDataServer(
      "test",
      charities_data = charities_data,
      charities_ltla_lookup_data = charities_ltla_lookup,
      ltlas_for_filtering = selected_ltlas
    )

 #   observe(print(charities_subset()))
    
    # Map of charities working within the area (module)
    charitiesMapServer("test",
      charities_data_subset = charities_subset
    )

    # Table of charities (module)
    charitiesTableServer("test",
      charities_data_subset = charities_subset
    )
    })
    # }
    # })
  }

  shinyApp(ui, server)
}
