explanation <- function() {
  fluidRow(
    column(
      3,
       align = "center",
      tags$div(
        class = "vcsep-logo",
        tags$a(
          href = "https://vcsep.org.uk/",
          target = "_blank",
          img(src = "vcsep_logo.jpg",
              height = "100%",
              width = "100%"
           )
          )
      )
     ),
    column(
      id = "card_header",
      8,
      tags$h2(
        "Emergency Planning Tool"
      )
      ),
    br(),
    column(
      11,
      tags$p(
        "The aim of this tool is to hightlight neighbourhoods vulnerable to potential emergency events, bringing together organisations and resources. We want to support organisations to make more informed decisions when planning for emergencies."
        ),
      tags$p(
        "Select an area of interest from the map or the dropdown box and then press next to navigate between the tabs."
      )
    )
  )
}
