# UI -----
partnersTableUI <- function(id) {
  tagList(
    div(
      style = "text-align: left; font-size: 120%",
      h4(strong("VCS Emergencies Partnership Partners")),
      p("This table provides information on partners of the Emergencies Partnership, working within the local area.")
    ),
    DT::DTOutput(NS(id, "partners_table"))
  )
}

# Server -----
partnersTableServer <- function(id,
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

    output$partners_table <- DT::renderDT(
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
