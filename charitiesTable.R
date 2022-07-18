# UI -----
charitiesTableUI <- function(id) {
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Contact info is within area",
      DT::DTOutput(NS(id, "charities_table_within_area"))
    ),
    tabPanel(
      "Contact info is outwith area",
      DT::DTOutput(NS(id, "charities_table_outwith_area"))
    )
  )
}

# Server -----
charitiesTableServer <- function(id, charities_data_subset) {

  # Checks to ensure the input is reactive
  stopifnot(is.reactive(charities_data_subset))

  moduleServer(id, function(input, output, session) {
    charities_data_subset_clean <- reactive({
      charities_data_subset() |>
        # avoided replace_na() as from tidyr (package not used elsewhere yet)
        mutate_at(
          c(
            "charity_contact_web",
            "charity_contact_email",
            "charity_contact_phone",
            "charity_contact_ltla_name",
            "charity_activities"
          ),
          ~ replace(., is.na(.), "-")
        ) |>
        select(
          "Name" = "charity_name",
          "Website" = "charity_contact_web",
          "Email" = "charity_contact_email",
          "Phone" = "charity_contact_phone",
          "Contact Info Local Authority" = "charity_contact_ltla_name",
          "Actvities" = "charity_activities",
          "flag_contact_in_ltla"
        )
    })

    # one table where the contact info is within the chosen LTLA
    output$charities_table_within_area <- DT::renderDT(
      {
        charities_data_subset_clean() |>
          dplyr::filter(flag_contact_in_ltla == TRUE) |>
          select(-flag_contact_in_ltla)
      },
      options = list(
        scrollX = TRUE,
        scrollCollapse = TRUE,
        # widen certain columns which contain a low of text
        columnDefs = list(
          list(width = "1100px", targets = c(6))
        ),
        autoWidth = TRUE
      )
    )

    # one table where the contact info is within the chosen LTLA
    output$charities_table_outwith_area <- DT::renderDT(
      {
        charities_data_subset_clean() |>
          dplyr::filter(flag_contact_in_ltla == FALSE) |>
          select(-flag_contact_in_ltla)
      },
      options = list(
        scrollX = TRUE,
        scrollCollapse = TRUE,
        # widen certain columns which contain a low of text
        columnDefs = list(
          list(width = "1100px", targets = c(6))
        ),
        autoWidth = TRUE
      )
    )
  })
}


#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# charities_ltla_lookup <- read_rds("data/charities_ltla_lookup.rds")
# charities_data <- read_rds("data/charities_list_latlong.rds")
# 
# source("subsetCharitiesData.R")
# 
# charitiesTableTest <- function() {
#   ui <- fluidPage(
#     charitiesTableUI("test")
#   )
# 
#   server <- function(input, output, session) {
#     charities_subset_test <- subsetCharitiesDataServer(
#       "test",
#       charities_data = charities_data,
#       charities_ltla_lookup_data = charities_ltla_lookup,
#       ltlas_for_filtering = reactive(c("E08000014"))
#     )
# 
#     observe(print(charities_subset_test()))
# 
#     charitiesTableServer("test",
#       charities_data_subset = charities_subset_test
#     )
#   }
#   shinyApp(ui, server)
# }
# 
# charitiesTableTest()
