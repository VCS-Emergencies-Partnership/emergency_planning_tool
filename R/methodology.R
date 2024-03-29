methodology_writeup <- function() {
  fluidRow(
    column(
      11,
      align = "left",
      tags$h1(
        "About the Emergency Planning Tool"
        ),
      tags$p(
        "The flooding model used in this emergency planning tool is formed of two parts: ",tags$b("social vulnerability")," and ",tags$b("flood risk"),". These are used to create a Social Flood Risk Index (SFRI)."
      ),
      br(),
      tags$h2(
        "Social Vulnerability Index"
      ),
      tags$p(
        "The model initially considers the social vulnerability of a neighbourhood to flooding. Small neighbourhood units of around 1000 to 3000 people are used (known as Lower Super Output Areas). The social vulnerability index is broken down into five domains:"
      ),
      tags$p(
        "•	Susceptibility: characteristics that influence susceptibility to harm such as the age and health of local populations"
      ),
      tags$p(
        "• Ability to prepare: characteristics that influence a persons ability to prepare for a flood such as income, knowledge and property tenure"
      ),
      tags$p(
        "• Ability to respond: characteristics that influence a persons ability to respond to a flood such as income, knowledge, mobility and access to transport"
      ),
      tags$p(
        "• Ability to recover: characteristics that influence a persons ability to recover from a flood such as income, information use, and mobility/transport"
      ),
      tags$p(
        "• Community support: social networks, housing characteristics and availability of support"
      ),
      tags$p(
        "These five domains are built based on 27 underlying vulnerability variables (such as the percentage of people over 75 years in the neighbourhood). A list of all indicators in the model can be",
        tags$a(href = "https://www.climatejust.org.uk/sites/default/files/Sayers_et_al_2017_indicator_list %28table 3-2 p27%29-46789%2BMP.pdf", target = "_blank", "found here."), "More information on each indicator in the flooding model can be ",
        tags$a(href = "https://www.climatejust.org.uk/sites/default/files/INFO_Sheet_Combined_Flood_New_Sayers_et_al_2017_final-67813%2BMP.pdf", target = "_blank", "found here"), ", including information on the data source and the reasons for inclusion of that indicator in the model. Many of the indicators are based on the 2011 census, but we are in the process of updating the model to include the 2021 census data."
      ),
      tags$p(
        ""
      ),
      tags$h2(
        "Flood risk"
      ),
      tags$p(
        "The model additionally includes data looking at the flood risk of a neighbourhood. This includes the expected annual probability of flooding and the number of people within a floodplain."
      ),
      tags$h2(
        "Social Flood Risk Index (SFRI)"
      ),
      tags$p(
        "The SFRI is a measure of ‘flood disadvantage’ which combines flood risk and social vulnerability to flooding. The SFRI is used to identify those areas where the largest number of the most vulnerable people are exposed to frequent flooding. It is a relative index and has no units; the greater the value, the higher the level of social flood risk. More information can be ",
        tags$a(href = "https://www.climatejust.org.uk/sites/default/files/Sayers et al 2017 - Assessment Methodology.pdf", target = "_blank", "found here.")
      ),
      br(),
      tags$h2(
        "Methodology for creating the Social Flood Risk Index (SFRI)"
      ),
      tags$p(
        "",tags$b("Stage 1: Determine the z-score of the individual indicators")," Each indicator (for example ‘age’, ‘health’ and ‘income’) is normalised to a z-score based on subtracting the mean value and then dividing by the standard deviation. For indicators that are ranked (for example the Index of Multiple Deprivation), the z-score is calculated based on assuming the rank is drawn from a normal distribution and calculating the number of standard deviations from the mean associated with that rank."
      ),
      tags$p(
        "",tags$b("Stage 2: Determine the z-score for each domain")," Individual indicator z-scores are combined based on equal weighting to create the domains (susceptibility, ability to prepare/respond/recover and community support). The only exception to this is that the ‘direct flood experience’ indicator is negatively weighted as it acts to reduce the relative vulnerability of a neighbour compared to another. The resulting values for each domain are transformed into a z-score."
      ),
      tags$p(
        "",tags$b("Stage 3: Calculation of the Social Vulnerability Index")," The z-score calculated for each indicator are summed with equal weighting. These resulting values are transformed into a z-score."
      ),
      tags$p(
        "",tags$b("Stage 4: Calculation of SFRI")," SFRI = Expected Annual Probability of Flooding ",tags$b("x")," Number of people within the floodplain ",tags$b("x")," Social Vulnerability Index"
      ),
      tags$div(
        img(src = "sfri_diagram.jpg")
      ),
      tags$p(
        "Schematic diagram detailing the process of building the Social Flood Risk Index (SFRI). Initially, 27 underlying variables (not shown) are combined to produce the 12 indicators (yellow). These indicators are combined to produce the 5 domains (orange) and these are combined to produce the social vulnerability index (blue). This index is combined with flood risk data to produce the final SFRI value for each neighbourhood.  "
      ),
      br(),
      tags$h2(
        "Organisations linked to vulnerabilities"
      ),
      tags$p(
        "The ‘organisations’ tabs within this tool links the social drivers of vulnerability to flooding (for example old age) to charities that work within this field in the local area (for example Age UK). This dataset has been built using the Charity Commissioner data, providing information only on registered charities. More information on this data can be",
        tags$a(href = "https://register-of-charities.charitycommission.gov.uk/register/full-register-download", target = "_blank", "found here.")
      ),
      tags$p(
        "The charity table is ordered based on the charities contact address so that organisations within the local authority are shown first (and these are shown on the corresponding map). Using the contact address to filter for charities within a local authority has been used so that local support can be sought. However, national charities, who will likely have their contact address listed as their headquarters, will not be shown within the map due to this."
      ),
      tags$p(
        "There are known limitations with the Charity Commissioner data, in that it does not give a full picture of the charity sector. Data is collected based on charities submitting an annual return and there are known issues with data completeness/quality."
      ),
      tags$p(
        "Matching of the vulnerabilities within the flooding model to charities is based initially on the ",
        tags$a(href = "https://register-of-charities.charitycommission.gov.uk/documents/34602/422354/Data+Definition.docx/f0a342ce-ef45-1401-ee75-26225f6f0d4f?t=1617010186385", target = "_blank", "charity classification data table"), " providing information on (1) What – what the charity does, (2) How – how the charity helps, and (3) Who – who the charity helps. Secondly, the ",
        tags$a(href = "https://register-of-charities.charitycommission.gov.uk/documents/34602/422354/Data+Definition.docx/f0a342ce-ef45-1401-ee75-26225f6f0d4f?t=1617010186385", target = "_blank", "charity data table"), " is used to link the charity activities field to the vulnerabilities based on key-terms. The charity activities field is a free-text explanation of what the charity does and who they help, provided by the trustees."
      ),
      tags$p(
        "The VCS Emergencies Partnership has not verified the reputability of the organisations listed and has not verified the information provided. "
      ),
      br(),
      tags$h2(
        "Data licences"
      ),
      tags$h3(
        "The Social Flood Risk Index (SFRI)"
      ),
      tags$p(
        "The VCS Emergencies Partnership for use on the publicly accessible emergency planning tool for use in local assessments, not regional or national scale analysis. For use by to help improve how voluntary/community organisations, not commercial service providers.

The NFVI data sets are provided under licence (from Sayers and Partners). Any use must be cited as ‘Sayers, P.B., Horritt, M., Penning Rowsell, E., and Fieth, J (2017). Present and future flood vulnerability, risk and disadvantage: A UK assessment. A report for the Joseph Rowntree Foundation published by Sayers and Partners LLP’.

The data, or new data products derived from the data, cannot be used by, shared with, or provided to, any third party. Map imagery can be included in report and academic papers associated with the above project.

The IPR remains fully with Sayers and Partners. There is no implied or real transfer of IPR. The licence lasts as along as the data is may available via the platform.

Inappropriate use may lead to implausible or misleading results. Sayers and Partners LLP provide no warranty as to the accuracy of the results. No support, technical software or use guidance will be provided.

Associated trademarks
Future Flood Explorer (is registered with Sayers and Partners)
Neighbourhood Flood Vulnerability Index (in review)"
      ),
      tags$h3(
        "Other data licences"
      ),
      tags$p(
        "The Charity Commissioners data is public sector information licensed under the ",tags$a(href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target = "_blank", "Open Government Licence v3.0.")
      ),
      br(),
      tags$h1(
        "About this app"
      ),
      tags$p(
        "The emergency planning tool app was designed and developed by Aileen McDonald, Victoria Latham, Matt Thomas and Mike Page. If you have any questions or comments or issues, please contact ", tags$a(href="mailto:victorialatham@vcsep.org.uk", "Victoria"),"."
      )
    )
  )
}
