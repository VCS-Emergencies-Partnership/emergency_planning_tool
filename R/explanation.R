explanation <- function() {
  fluidRow(
    column(
      2,
      align = "left",
      br(),
      # img(src = "vcsep_logo.png")
      tags$div(
        class = "vcsep-logo",
        tags$a(
          href = "https://vcsep.org.uk/",
          target = "_blank",
          img(src = "vcsep_logo.png",
              height = "60%",
              width = "60%")
          )
      )
    ),
    column(
      10,
      tags$h4(
        "Emergency Planning Tool"
      ),
      tags$p(
        "Use this tool to find areas of vulnerability to potential
        emergency events, what factors are driving the vulnerability and
        organisations that work in the area."
      )
    )
  )
}
