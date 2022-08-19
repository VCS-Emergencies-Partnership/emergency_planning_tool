# UI -----
charitiesTableUI <- function(id) {
  DT::DTOutput(NS(id, "charities_table"))
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
          "Contact Info Local Authority" = "charity_contact_ltla_name",
          "Name" = "charity_name",
          "Website" = "charity_contact_web",
          "Email" = "charity_contact_email",
          "Phone" = "charity_contact_phone",
          "Actvities" = "charity_activities",
          "flag_contact_in_ltla"
        ) |>
        mutate(Website = ifelse(Website == "-", Website, paste0("<a href='", Website, "' target='_blank'>", Website, "</a>")))
    })

    output$charities_table <- DT::renderDT(
      {
        # Catch errors if no area has been selected - show message as at top of the page
        shiny::validate(need(nrow(charities_data_subset()) != 0, "Please select an area on the first tab."))
        
        charities_data_subset_clean() |>
          arrange(desc(flag_contact_in_ltla), Name) |>
          select(-flag_contact_in_ltla)
      },
      rownames = FALSE,
      escape = FALSE, # needed for the hyperlink column
      extensions = c("Buttons", "ColReorder"),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        dom = "Bfrtip",
        buttons =  list(
          list(
            extend = "colvis",
            text = "Hide/show columns"
          ),
          list(
            extend = "csv",
            text = "Download data as csv"
          )
        ),
        colReorder = TRUE,
        autoWidth = TRUE,
       # widen certain columns which contain a low of text
        # columnDefs = list(
        #   list(width = "500px", targets = c(6))
        # )
    )
    )
  })
}


#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

charities_ltla_lookup <- read_rds("data/charities_ltla_lookup.rds")
charities_data <- read_rds("data/charities_list_latlong.rds")

source("subsetCharitiesData.R")

charitiesTableTest <- function() {
  ui <- fluidPage(
    charitiesTableUI("test")
  )

  server <- function(input, output, session) {
    charities_subset_test <- subsetCharitiesDataServer(
      "test",
      charities_data = charities_data,
      charities_ltla_lookup_data = charities_ltla_lookup,
      ltlas_for_filtering = reactive(c("Hartlepool"))
    )

   # observe(print(charities_subset_test()))

    charitiesTableServer("test",
      charities_data_subset = charities_subset_test
    )
  }
  shinyApp(ui, server)
}

charitiesTableTest()
