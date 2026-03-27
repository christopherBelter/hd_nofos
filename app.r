library(tidyverse)
library(shiny)
library(bslib)
library(DT)
mcols <- c("#325C6E", "#A8E5FF", "#68A2BA", "#6E4D27", "#BA9468", "#6A3D6E", "#FAC2FF", "#B57BBA", "#4D6E32", "#98BA7B")

the_forecasts <- read.csv("data/nofo_forecast_relevant_current.csv", stringsAsFactors = FALSE)
the_forecasts <- the_forecasts %>% 
  rename(
    `NOFO Number` = opportunity_number,
    `NOFO Type` = nofo_type,
    `Opportunity Title` = opportunity_title,
    `Post Date` = summary.post_date,
    `NICHD Theme(s)` = all_concepts
  )

hd_nofos <- read.csv("data/hd_nofos_current.csv", stringsAsFactors = FALSE)
hd_nofos <- hd_nofos %>% 
  rename(
    `NOFO Number` = opportunity_number,
    `NOFO Type` = nofo_type,
    `Opportunity Title` = opportunity_title,
    `Post Date` = summary.post_date,
    `Close Date` = summary.close_date,
    `Status` = opportunity_status,
    `NICHD Theme(s)` = all_concepts,
    `HD Involvement` = hd_involvement
  )

ui <- page_sidebar(
  title = "NICHD NOFO Tracking",
  theme = bs_theme(bootswatch = "cerulean", primary = mcols[1], secondary = mcols[2]),
  sidebar = sidebar(
    title = "Filter Options",
    selectInput(
      "theme",
      label = "Strategic Plan Theme",
      choices = c("All", "Developmental Biology", "Reproductive Health", "Pregnancy & Lactation", "Pediatrics", "Pharmacology & Therapeutics", "Rehabilitation"),
      selected = "All"
    ),
    selectInput(
      "nofo_type", 
      label = "NOFO Type",
      choices = c("All", "PA", "PAR", "RFA", "OTH", "UNK"),
      selected = "All"
    ),
    hr(),
    p("Data source: simpler.grants.gov API"),
    p(paste("Data updated:", unique(the_forecasts$run_date)))
  ),
  navset_pill(
    nav_panel(
      title = "HD Primary NOFOs",
      hr(),
      dataTableOutput("hd_table")
    ),
    nav_panel(
      title = "HD Secondary NOFOs",
      hr(),
      dataTableOutput("hd_secondary_table")
    ),
    nav_panel(
      title = "Relevant Non-HD Forecasts",
      hr(),
      dataTableOutput("forecast_table")
    ),
    #nav_panel(
    #  title = "Expiring Sign-Ons",
    #  hr(),
    #  "Table goes here"
    #),
    nav_panel(
      title = "About the Data",
      hr(),
      card(
        card_header("Data Sourcing and Updating"),
        p("The data in these tables comes from the simpler.grants.gov API. To generate the data, I search the API for all NOFOs available in grants.gov with a status of \"Forecasted\", \"Posted\", or \"Closed\" that were posted by the NIH. I update the data on Monday or Tuesday each week."),
      ),
      card(
        card_header("Data Processing"),
        p("Once I have the data, I then add columns to describe each NOFO’s document type, NICHD involvement, and NICHD strategic plan theme(s)."),
        p("The document type is derived from the NOFO opportunity number (i.e. PAR-25-250 is a PAR document type)."),
        p("NICHD’s involvement is estimated based on several factors. If NICHD or NCMRR is mentioned in the summary description, if the opportunity number contains \"-HD-\", if NICHD’s opportunity assistance number (93.865) is the only number listed, or if the opportunity number is in a known list of numbers, NICHD’s involvement is set to \"HD Primary\". If none of these are true, but NICHD’s assistance listing number is included in the NOFO, NICHD’s involvement is set to \"HD Secondary.\" If none of these are true, NICHD’s involvement is set to \"No HD Involvement.\""),
        p("Plan themes")
      )
    )
  )
)


server <- function(input, output) {
  filtered_forecasts <- reactive({
    if (input$nofo_type == "All" && input$theme == "All") {
      the_forecasts <- the_forecasts
    }
    else if (input$nofo_type != "All" && input$theme == "All") {
      the_forecasts <- the_forecasts %>% 
        filter(`NOFO Type` == input$nofo_type)
    }
    else if (input$nofo_type == "All" && input$theme != "All") {
      the_forecasts <- the_forecasts %>% 
        filter(grepl(input$theme, `NICHD Theme(s)`))
    }
    else {
      the_forecasts <- the_forecasts %>% 
        filter(`NOFO Type` == input$nofo_type, grepl(input$theme, `NICHD Theme(s)`))
    }
  })
  output$forecast_table <- renderDataTable(filtered_forecasts()[,1:5], escape = FALSE, rownames = FALSE)
  ## set filter = "top" to add a row of filter options to the top of the table, so you don't even have to do input options if you don't want to
  ## could also potentially hide columns in the display but have them available to filter on
  filtered_hd <- reactive({
    if (input$nofo_type == "All" && input$theme == "All") {
      hd_nofos <- hd_nofos
    }
    else if (input$nofo_type != "All" && input$theme == "All") {
      hd_nofos <- hd_nofos %>% 
        filter(`NOFO Type` == input$nofo_type)
    }
    else if (input$nofo_type == "All" && input$theme != "All") {
      hd_nofos <- hd_nofos %>% 
        filter(grepl(input$theme, `NICHD Theme(s)`))
    }
    else {
      hd_nofos <- hd_nofos %>% 
        filter(`NOFO Type` == input$nofo_type, grepl(input$theme, `NICHD Theme(s)`))
    }
  })
  output$hd_table <- renderDataTable(filtered_hd()[filtered_hd()$`HD Involvement` == "HD Primary",1:7], escape = FALSE, rownames = FALSE)
  output$hd_secondary_table <- renderDataTable(filtered_hd()[filtered_hd()$`HD Involvement` == "HD Secondary",1:7], escape = FALSE, rownames = FALSE)
  #bs_themer()
}

shinyApp(ui = ui, server = server)