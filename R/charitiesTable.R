# UI -----
charitiesTableUI <- function(id) {
  tagList(
    div(
      style = "text-align: left; font-size: 120%",
      h4(strong("Charity commissioner data")),
      p("This table provides information on charities operating within the local area and has been produced using the Charity Commissioner data, based on annual returns provided by trustees.")
    ),
    DT::DTOutput(NS(id, "charities_table")),
    div(
      style = "text-align: left; font-size: 120%",
    p("Please note: The Emergencies Partnership does not have responsibility for the reputability of organisations listed.")
    )
  )
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
          "Website" = "charity_contact_web",
          "Email" = "charity_contact_email",
          "Phone" = "charity_contact_phone",
          "Actvities" = "charity_activities",
          "flag_contact_in_ltla"
        ) |>
        mutate(Website = ifelse(Website == "-", Website, paste0("<a href='", Website, "' target='_blank'>", Website, "</a>")),
               `Contact details` = paste(Website, Email, Phone, sep = "</br>")) |>
        select(-Email, -Phone, -Website)
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
       # widen certain columns which contain a lot of text
        # columnDefs = list(
        #   list(width = "500px", targets = c(3))
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
