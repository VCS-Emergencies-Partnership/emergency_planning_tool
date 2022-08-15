# UI ----
floodingLisenceUI <- function(id) {
  actionButton(NS(id, "flood_licence_button"), "Attribution & licence for flooding vulnerability data from Sayers & Partners.")
}

# Server ----
floodingLisenceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  observeEvent(input$flood_licence_button, {
    showModal(modalDialog(
      title = "Flood vulnerability data licence agreement",
"The BRC for use on publicly accessible emergency planning tool for use in local assessments – not regional or national scale analysis. For use by to help improve how voluntary/community organisations – not commercial service providers.

Non-commercial research
All sectors but pricing structures reflects different users – free to non-commercial research subject to citation and non resale or re-publication agreements.

Commercial use
For commercial use (including contract research) a small cost is levied to enable us to respond to data requests and maintain the data up to date.  Note: The UK dataset is available via Oasis for a download fee.

For SME and non-profit research organisations the UK data add may be provided free on request

The NFVI data sets are provided under licence (from Sayers and Partners). Any use must be cited as

Sayers, P.B., Horritt, M., Penning Rowsell, E., and Fieth, J (2017). Present and future flood vulnerability, risk and disadvantage: A UK assessment. A report for the Joseph Rowntree Foundation published by Sayers and Partners LLP.

The data, or new data products derived from the data, cannot be used by, shared with, or provided to, any third party. Map imagery can be included in report and academic papers associated with the above project.

The IPR remains fully with Sayers and Partners. There is no implied or real transfer of IPR.
The licence lasts as along as the data is may available via the platform.

Inappropriate use may lead to implausible or misleading results. Sayers and Partners LLP provide no warranty as to the accuracy of the results. No support, technical software or use guidance will be provided.

Associated trademarks
Future Flood Explorer (is registered with Sayers and Partners)
Neighbourhood Flood Vulnerability Index (in review)
",
      easyClose = TRUE
    ))
  })
  }
)
}

# --------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
# --------------------------------------------------------------------------------

# floodingLisenceTest <- function() {
#   ui <- fluidPage(
#     floodingLisenceUI("test")
#   )
#   server <- function(input, output, session) {
#     floodingLisenceServer("test")
#   }
#   shinyApp(ui, server)
# }
# # Run test
# floodingLisenceTest()
