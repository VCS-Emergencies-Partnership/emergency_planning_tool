# UI ----
subsetCharitiesDataCategoriesUI <- function(id,
                                            charities_categories_data) {
  fluidRow(
    column(
      6,
      # Dropdown to select categories of charities
      pickerInput(
        NS(id, "charity_category"),
        label = "Select charity category:",
        choices = unique(filter(charities_categories_data, what == "category")$value),
        selected = unique(filter(charities_categories_data, what == "category")$value),
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      )
    ),
    column(
      6,
      # Dropdown to select service of charities
      pickerInput(
        NS(id, "charity_service"),
        label = "Select charity service:",
        choices = unique(filter(charities_categories_data, what == "service")$value),
        selected = unique(filter(charities_categories_data, what == "service")$value),
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      )
    )
  )
}


# Server ----
subsetCharitiesDataCategoriesServer <- function(id,
                                                subset_charities_data,
                                                charities_categories_data) {

  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(subset_charities_data))

  moduleServer(id, function(input, output, session) {
    reactive({
      category_charities <- charities_categories_data |>
        filter(value %in% input$charity_category) |>
        select(organisation_number) |>
        pull()

      service_charities <- charities_categories_data |>
        filter(value %in% input$charity_service) |>
        select(organisation_number) |>
        pull()

      org_number_list <- intersect(category_charities, service_charities)

      subset_charities_data() |>
        filter(organisation_number %in% org_number_list)
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

charities_data <- read_rds("data/charities_list_latlong.rds")
charities_categories_data <- read_rds("data/charities_categories.rds")

subsetCharitiesDataCategoriesTest <- function() {
  ui <- fluidPage(
    subsetCharitiesDataCategoriesUI("test",
      charities_categories_data = charities_categories_data
    )
  )

  server <- function(input, output, session) {
    charities_subset <- subsetCharitiesDataCategoriesServer(
      "test",
      subset_charities_data = reactive(charities_data),
      charities_categories_data = charities_categories_data
    )

    observe(print(charities_subset()))
  }
  shinyApp(ui, server)
}

# Run test
subsetCharitiesDataCategoriesTest()
