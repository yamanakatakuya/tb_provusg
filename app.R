# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display provisional monthly or quarterly TB notifications for
# 24 USAID priority countries in 2024 
# using JSON data retrieved from the WHO global tuberculosis database.
# Using the Echarts for R package instead of ggplot2.
# Takuya Yamanaka Apr 2025
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.0"

library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(echarts4r)
library(htmlwidgets)
library(ggplot2)
library(ggtext)
library(shinythemes)
library(lubridate)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <-
  navbarPage(
    "Provisional TB notifications",
  tabPanel(
    "USAID priority countries in 2024",
  #--------------------- by country ---------------------#
  fluidPage(theme = shinytheme("sandstone"),
  title = "Provisional number of people notified with new or relapse episodes of TB",

  # add CSS to colour headings and to prevent printing of the country selector dropdown
  tags$style(HTML("
    #page_header {
        padding-top: 10px;
        padding-left: 20px;}
    #annual_heading {
        color: #1790cf;
        padding-left: 5px;}
    #provisional_heading {
        color: #4b8f36;
        padding-left: 5px;}
    @media print {
        #entities, #page_header, #metadata {display: none;}
    }")),

  fluidRow(tags$div(id = "page_header",
                    HTML("Select from countries that were USAID priorities for bilateral funding for TB in 2024 and
                         reported provisional notifications to the World Health Organization (WHO)<br />"),
                    uiOutput(outputId = "entities"))
  ),

  fluidRow(

    column(width = 6,
           tags$div(style = "padding-left: 20px;"),
           textOutput(outputId = "annual_heading", container = h3),
           echarts4rOutput("annual_usaid_plot")
    ),
    column(width = 6,
           tags$div(style = "padding-left: 20px;"),
           textOutput(outputId = "provisional_heading", container = h3),
           echarts4rOutput("prov_usaid_plot")
    )
  ),

  fluidRow(tags$div(style = "padding-left: 20px; padding-right: 20px;",
                    textOutput(outputId = "page_footer"))
  ),

  fluidRow(tags$div(id = "metadata2",
                    style = "padding: 20px; font-style: italic; font-size: 80%;",

                    # Add app version number and links to GTB and Github
                    HTML(paste0(app_version,
                                ", Source code on <a href='https://github.com/yamanakatakuya/tb_provusg/' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
  ))
  )
  )

)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Back end server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {

  json_url <- "https://extranet.who.int/tme/generateJSON.asp"

  # Get the latest list of countries with provisional data to use in country dropdown
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  country_list_json <- reactive({

    url <- paste0(json_url, "?ds=c_newinc_countries")

    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))

    return(json)
  })


  # Build the select country control
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  output$entities <- renderUI({

    already_selected <- input$iso2

    # Create a named list for selectInput
    country_list <- country_list_json()$countries %>%
      select(iso2, country) %>%
      filter(iso2 %in% c("AF", "BD", "CD", "ET", "ID", "IN", "KE", "KG", "KH", "MM", "MW",
                         "MZ", "NG", "PH", "PK", "TJ", "TZ", "UA", "UG", "UZ", "VN", "ZA",
                         "ZM", "ZW")) %>%
      arrange(country)

    country_list <- setNames(country_list[,"iso2"], country_list[,"country"])

    selectInput(inputId = "iso2",
                label = "",
                choices = country_list,
                # next line needed to avoid losing the selected country when the language is changed
                selected = already_selected,
                selectize = FALSE,
                width = "380px")
  })


  # Get the data as a JSON file for the chosen country
  # - - - - - - - - - - - - - - - - - - - - - - - - - -

  pdata <- reactive({

    url <- paste0(json_url, "?ds=c_newinc&iso2=", input$iso2)

    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
    return(json)
  })


  # Find out if there is a complete year's worth of provisional notifications
  # to add to the published annual notifications
  # notifications for the latest published year will be NA if not all periods are filled
  publication_year_notifications <- reactive({

    # Make sure there are data to use
    req(pdata()$c_newinc_prov)
    req(pdata()$dcyear_published)

    pdata()$c_newinc_prov %>%

      filter(year == pdata()$dcyear_published) %>%
      mutate(c_newinc = ifelse(report_frequency == 71,
                               q_1 + q_2 + q_3 + q_4,
                               m_01 + m_02 + m_03 + m_04 + m_05 + m_06 +
                                 m_07 + m_08 + m_09 + m_10 + m_11 + m_12),
             year = paste0(year, "*")) %>%
      select(year, c_newinc) %>%
      filter(!is.na(c_newinc))

  })

  # Create the charts
  source("build_charts_usaidfunded.r", local = TRUE)

}

# Run the application
shinyApp(ui = ui, server = server)
