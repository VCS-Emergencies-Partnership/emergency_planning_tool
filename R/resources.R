resources_page <- function() {
  fluidPage(
    fluidRow(
      column(
        11,
        align = "left",
        tags$p(
          "Here is a collection of useful publicly available insight resources that could be of use to the VCS community during a heatwave The resources shown are general resources. Other resources can be found on the ",
          tags$a(href = "https://vcsep.org.uk/winter-preparedness-2022", target = "_blank", " VCS EP website.")
        )
      ),
      column(
        11,
        align = "left",
        tags$h3("National resources")
      )
    ),
    flowLayout(
      box(
        title = "Met: Sign up to the Heat-health Alert Service", "The service forewarning you of periods of high temperatures.",
        width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          # style="color: #fff; background-color: #E7550F; border-color: #2e6da4",
          onclick = "window.open('https://www.metoffice.gov.uk/weather/warnings-and-advice/seasonal-advice/heat-health-alert-service', '_blank')"
        )
      ),
      box(
        title = "MET: Weather-health alerting system guide", "A guide to understanding the new impact-based weather-health alerting system.",
        width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1155634/User-guide-impact-based-weather-and-health-alerting-system-v2.pdf', '_blank')"
        )
      ),
      box(
        title = "GOV: Heat-Health Alert action card for the VCS", "Action card summarising the suggested actions that the VCS should consider.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://www.gov.uk/government/publications/hot-weather-and-health-action-cards/heat-health-alert-action-card-for-the-voluntary-and-community-sector', '_blank')"
        )
      ),
      box(
        title = "GOV: Hot weather and health", "Guidance on how to suppport vulnerable people.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://www.gov.uk/government/publications/hot-weather-and-health-supporting-vulnerable-people', '_blank')"
        )
      )
    ),
    fluidRow(
      column(
        11,
        align = "left",
        tags$h3("Flooding resources")
      )
    ),
    column(
      11,
      align = "left",
      tags$p("As a result of a heatwave, the ground becomes dry and therefore flash flooding can occur. This surface water flooding often occurs quickly and with little warning.")
    ),
    flowLayout(
      box(
        title = "National Flood Forum", "National Flood Forum is a charity that helps, supports and represents people at risk of flooding. Here you can access advice for preparing, responding to and recovering from flooding.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://nationalfloodforum.org.uk/', '_blank')"
        )
      ),
      box(
        title = "GOV: Flood alerts and warnings", "National flood warnings and alerts.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://check-for-flooding.service.gov.uk/alerts-and-warnings', '_blank')"
        )
      )
    ),
    fluidRow(
      column(
        11,
        align = "left",
        tags$h3("Wildfire resources")
      )
    ),
    flowLayout(
      box(
        title = "NFCC: Wildfire Prevention", "Guidance and resources on wildfires.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://www.nationalfirechiefs.org.uk/Wildfire-Prevention-', '_blank')"
        )
      )
    ),
    fluidRow(
      column(
        11,
        align = "left",
        tags$h3("VCS resources")
      )
    ),
    flowLayout(
      box(
        title = "BRC: Heatwave advice", "Advice on how to cope with extreme heat.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://www.redcross.org.uk/get-help/prepare-for-emergencies/heatwaves-uk', '_blank')"
        )
      ),
      box(
        title = "BRC: Heatwave checklist", "A checklist for how to prepare before and during a heatwave.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://www.redcross.org.uk/-/media/documents/about-us/we-speak-up-for-change/brc_heatwave-checklist-final.pdf', '_blank')"
        )
      ),
      box(
        title = "VCSEP: Heatwave TTX", "Report and resoruces from a capability exercise on heatwaves.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://www.vcsep.org.uk/resources/heatwave-ttx-report-and-resources', '_blank')"
        )
      ),
      box(
        title = "VCSEP: Heatwave scenario exercise", "Example heatwave scenarios that can be used for prepardness.", width = 18, height = 240, background = "purple",
        shiny::actionButton(
          inputId = "ab1", label = "Click here",
          onclick = "window.open('https://mcusercontent.com/83011ab5f15b06afda2f15115/files/16a2dff5-335a-968d-6ed9-67619c580032/Heatwave_scenario_exercise_plan_1_.pdf', '_blank')"
        )
      )
    )
  )
}
