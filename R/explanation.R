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
        "Use this tool to find areas of vulnerability to potential
        emergency events, what factors are driving the vulnerability and
        organisations that work in the area."
      ),
      tags$p(
        "Select an area of interest from the map or the dropdown box and
        then press next to navigate between the tabs."
      )
    )
  )
}
