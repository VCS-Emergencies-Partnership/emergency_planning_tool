# UI -----
charitiesTableUI <- function(id) {
  DT::DTOutput(NS(id, "charities_table"))
}

# Server -----
charitiesTableServer <- function(id,
                                 charities_subset) {

  # Checks to ensure the input is reactive
  stopifnot(is.reactive(charities_subset))

  moduleServer(id, function(input, output, session) {
    charities_subset_clean <- reactive({
      charities_subset() |>
        filter(flag_contact_in_ltla == 1) |>
        select(
          "Contact Info Local Authority" = "charity_contact_ltla_name",
          "Name" = "charity_name",
          "Actvities" = "charity_activities",
          "Website" = "charity_contact_web",
          "Email" = "charity_contact_email",
          "Phone" = "charity_contact_phone",
          "flag_contact_in_ltla"
        ) |>
        mutate(Website = ifelse(Website == "-", Website, paste0("<a href='", Website, "' target='_blank'>", Website, "</a>")))
    })

    output$charities_table <- DT::renderDT(
      {


        # Future idea: could order by proximity to the LTLA the user has selected
        charities_subset_clean() |>
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
        autoWidth = TRUE
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

# load("data/charities_vuln_drivers_flood_lookup.rda")
# load("data/charities_lat_long.rda")
# load("data/charities_ltla_lookup.rda")
# load("data/vuln_drivers_flood_ltla.rda")
#
# source("R/subsetCharitiesData.R")
#
# charitiesTableTest <- function() {
#   ui <- fluidPage(
#     subsetCharitiesDataUI("test"),
#     charitiesTableUI("test")
#   )
#
#   server <- function(input, output, session) {
#     charities_subset <- subsetCharitiesDataServer(
#       "test",
#       charities_vuln_drivers_flood_lookup = charities_vuln_drivers_flood_lookup,
#       charities_lat_long = charities_lat_long,
#       charities_ltla_lookup = charities_ltla_lookup,
#       vuln_drivers_flood_ltla = vuln_drivers_flood_ltla,
#       ltlas_for_filtering = reactive(c("Hartlepool"))
#     )
#
#     observe(print(charities_subset$data()))
#
#       charitiesTableServer("test",
#                            charities_subset = charities_subset$data
#       )
#   }
#
#   shinyApp(ui, server)
# }
#
# # Run test
# charitiesTableTest()
#
