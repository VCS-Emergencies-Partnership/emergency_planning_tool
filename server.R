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
                    flood_risk_data = lsoa_flood_risk_ltla_lookup,
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
      
      # Subset the vulnerability scores data with selected LSOAs
      # Repeated from when the 'vulnerabilities' tab is selected
      lsoa_vuln_scores_subset <- subsetVulnDataServer(
        "test",
        lsoa_data = vuln_scores_flood,
        ltlas_for_filtering = selected_ltlas
      )
      
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
                         lsoa_vuln_scores_sf_subset = lsoa_vuln_scores_subset
      )
      
      # Table of charities (module)
      charitiesTableServer("test",
                           charities_data_subset = charities_categories_subset
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

