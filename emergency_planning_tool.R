emergency_planning_tool <- function() {

  # ---- UI ----
  ui <- fluidPage(
    fluidRow(

      # Metric of % of most vulnerable neighborhoods (module)
      topVulnOutput("test")
    ),
    fluidRow(
        # Vulnerability index map (module)
        vulnMapUI("test")
      ),
  fluidRow(
        # Table of top drivers of vulnerability for clicked LSOA (module)
        topDriversOutput("test")
      )
    )
  


  server <- function(input, output, session) {

    # # Variable to store the LSOA of clicked
    lsoas_clicked_global <- reactiveVal()

    # To be replaced with selected values from page 1
    lsoas_selected <- reactive({
      c(
        "E01012000", "E01011951", "E01011999", "E01011967", "E01011964", "E01011965", 
        "E01011954", "E01011952","E01011953", "E01011992", "E01011969", "E01011963",
        "E01011960", "E01011968", "E01012001", "E01011950", "E01011987", "E01011986",
        "E01011985", "E01011984", "E01011970", "E01011971", "E01033465", "E01033467", 
        "E01011974", "E01011955", "E01011994", "E01011993", "E01011991", "E01011977", 
        "E01011979", "E01011981", "E01011980", "E01011962", "E01011949", "E01011978",
        "E01011990", "E01011989", "E01011988", "E01011961", "E01012003", "E01011959",
        "E01011983", "E01011982", "E01011997", "E01011996", "E01011995", "E01011998", 
        "E01012005", "E01011957", "E01033466", "E01032540", "E01032541", "E01012002", 
        "E01011976", "E01011975", "E01011973", "E01011966"
      )
    })

    # Subset the vulnerability scores data with selected LSOAs
    lsoa_vuln_scores_subset <- subsetVulnDataServer(
      "test",
      lsoa_data = vuln_scores_flood,
      lsoas_for_filtering = lsoas_selected
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

    # Table of top drivers of vulnerability for clicked LSOA (module)
    topDriversServer("test",
      vuln_drivers = vuln_drivers_flood,
      lsoas_clicked = lsoas_clicked_global
    )
  }

  shinyApp(ui, server)
}
