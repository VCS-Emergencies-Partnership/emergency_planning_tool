# Server ----
subsetDataServer <- function(id, lsoa_data, lsoas_for_filtering) {
  
  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(lsoas_for_filtering))
  
  moduleServer(id, function(input, output, session) {
    reactive({
      lsoa_data |>
        dplyr::filter(lsoa11_code %in% lsoas_for_filtering())
      
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------
  
# lsoa_vuln_scores_flood <- read_rds("data/flooding_vuln_scores_sf.rds")
# 
# subsetDataTest <- function() {
#   ui <- fluidPage(
# 
#   )
#   server <- function(input, output, session) {
#     lsoa_vuln_scores_subset_flood <-  subsetDataServer(
#       "test",
#       lsoa_data = lsoa_vuln_scores_flood,
#       lsoas_for_filtering = reactive(c("E01000001", "E01000002", "E01000003", "E01000007"))
#     )
# 
#     observe(print(lsoa_vuln_scores_subset_flood()))
#   }
#   shinyApp(ui, server)
# }
# 
# # Run test
# subsetDataTest()
