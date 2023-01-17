explanation <- function() {
  fluidRow(
    column(
      1,
       align = "center",
      # img(src = "vcsep_logo.png"),
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
      11,
      tags$h4(
        "Emergency Planning Tool"
      ),
      tags$p(
        "Use this tool to find areas of vulnerability to potential
        emergency events, what factors are driving the vulnerability and
        organisations that work in the area."
      ),
      tags$p(
        "Select an area of interst from the map or the dropdown box and
        then press next to navigate between the tabs."
      )
    )
  )
}
