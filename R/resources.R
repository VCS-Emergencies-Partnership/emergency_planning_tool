resources_page <- function() {
  fluidPage(
  fluidRow(
    column(
      11,
      align = "left",
      tags$p("Here is a collection of useful publicly available insight resources that could be of use to the VCS community during a flood. The resources shown are general resources but resources will be provided that are linked to the reasons causing social flood vulernability. Other resorces can be found on the ",
             tags$a(href = "https://vcsep.org.uk/winter-preparedness-2022", target = "_blank", " VCS EP website.")
             )
      ),
    column(
      11,
      align = "left",
      tags$h3("General resources")
    )
    ),
  flowLayout(
      box(title = "GOV.UK: Report a flood or possible cause of flooding", "Who you need to contact depends on what’s flooding.",
          width = 18, height = 240, background = "purple",
          shiny::actionButton(inputId='ab1', label="Click here",
                              # style="color: #fff; background-color: #E7550F; border-color: #2e6da4",
                              onclick ="window.open('https://www.gov.uk/report-flood-cause', '_blank')")
      ),
      box(title = "MET: Weather warnings guide", "A guide to understanding the MET’s  Severe Weather Warning Service.",
          width = 18, height = 240, background = "purple",
          shiny::actionButton(inputId='ab1', label="Click here",
                              onclick ="window.open('https://www.metoffice.gov.uk/weather/guides/warnings', '_blank')")
      ),
      box(title = "MET: Weather warning impacts and what they mean", "The Met Office issues weather warnings, when severe weather has the potential to bring impacts to the UK.", width = 18, height = 240, background = "purple",
          shiny::actionButton(inputId='ab1', label="Click here",
                              onclick ="window.open('https://www.metoffice.gov.uk/weather/guides/severe-weather-advice', '_blank')")
      ),
      box(title = "GOV: Prepare for flooding", "Plan how you’ll respond to a flood. Use the templates in the link provided to make a personal flood plan; community or group flood plan, business flood plan.", width = 18, height = 240, background = "purple",
          shiny::actionButton(inputId='ab1', label="Click here",
                              onclick ="window.open('https://www.gov.uk/prepare-for-flooding/future-flooding', '_blank')")
      ),
      box(title = "National Flood Forum", "National Flood Forum is a charity that helps, supports and represents people at risk of flooding. Here you can access advice for preparing, responding to and recovering from flooding.      ", width = 18, height = 240, background = "purple",
          shiny::actionButton(inputId='ab1', label="Click here",
                              onclick ="window.open('https://nationalfloodforum.org.uk/', '_blank')")
      )
  )
  )
}
