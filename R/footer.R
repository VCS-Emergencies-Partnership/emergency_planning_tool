footer_image <- function() {
  fluidRow(
    column(
      2,
      align = "left",
      tags$div(
        style = "width:30px",
        class = "vcsep-logo",
        tags$a(
          href = "https://vcsep.org.uk/",
          target = "_blank",
          img(src = "vcsep_logo_edited.jpg",
              height = "100%",
              width = "100%"
          )
        )
      )
    ),
    column(
      8,
      align = "centre",
      HTML("<footer><small>&copy; Voluntary and Community Sector Emergencies Partnership</small></footer>")
    ),
    column(
      2,
      align = "right",
     includeHTML(paste0(getwd(), "/www/onetrust_footer.js"))
    )
  )
}
