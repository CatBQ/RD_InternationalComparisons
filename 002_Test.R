# 002 - DASHBOARD TEST ---------------------------------------------------------

# v0.1.0

# LIBRARIES --------------------------------------------------------------------
# shiny
library(shiny)

# widgets
library(plotly)
library(maps)
library(networkD3)

# core
library(scales)
library(janitor)
library(tidyverse)


# DASHBOARD --------------------------------------------------------------------

# |- list of countries ----
ctry_nm <-
  oecd_dt %>%
  select(country) %>%
  distinct(country) %>%
  arrange(country)

# |- USER INTERFACE ----

ui <- navbarPage(
  title = "R&I: International Comparisons",
  tabPanel(
    "R&I: IC Dashboard",
    fluidPage(
      # header
      tags$h2("Research & Innovation: International Comparisons"),
      # horizontal line
      tags$hr(),
      # break
      tags$br(),
      tags$h3("Summary:"),
      # paragraph
      tags$p(
        "This dashboard compares the United Kingdom with multiple countries
        across the world using research and innovation (R&I) metrics."
      ),
      tags$p(
        "The dashboard was developed to help stakeholders in being able to make
        these comparisons visually."
      ),
      tags$br(),
      tags$h3("Terminology:"),
      tags$p("1) R&I: research and innovation"),
      tags$p("2) R&D: research and development"),
      tags$p("3) GERD: gross domestic expenditure on R&D"),
      tags$br(),
      tags$h3("Data Register:"),
      tags$p(
        "Data included in this dashboard are available in the public domain."
      ),
      tags$p("Data was collected from: OECD."),
      tags$br(),
      tags$h3("Product development:"),
      tags$p("June 2022")
    )
  ),
  navbarMenu(
    title = "R&I Funding",
    tabPanel(
      "GERD: Percentage of GDP",
      fluidPage(fluidRow(
        tags$h3("GERD as a percentage of GDP"),
        # input function - heat map year box
        radioButtons(
          inputId = "year_button",
          label = "Choose year of interest",
          choices = c(2015,
                      2016,
                      2017,
                      2018,
                      2019,
                      2020)
        ),
        plotlyOutput("heatmap")
      )),
      fluidRow(
        dataTableOutput("table1")
      )
    ),
    tabPanel("GERD: Other",
             fluidPage(
               tags$h2("Gross Domestic Expenditure on R&D (GERD)"),
               sidebarPanel(
                 # input function - to select the country
                 checkboxGroupInput(
                   inputId = "list_of_countries",
                   label = "Select country or countries:",
                   choices = ctry_nm$country,
                   selected = "United Kingdom"
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                     tags$h3("GERD at current PPP $"),
                     plotlyOutput("gerd_ppp_gpt")
                   ),
                   column(6,
                     tags$h3("GERD per capita population (current PPP $)"),
                     wellPanel(
                       radioButtons(
                         inputId = "yar_button",
                         label = "Choose year:",
                         choices = c(2015,
                                     2016,
                                     2017,
                                     2018,
                                     2019,
                                     2020)
                       )
                     ),
                     plotlyOutput("gerd_gxpop")
                   )
                 ),
                 fluidRow(
                   tags$h3(
                     "Percentage of GERD financed by the business,
                         government, higher education and rest of the world"
                   ),
                   wellPanel(
                     radioButtons(
                       inputId = "yr_button",
                       label = "Choose year:",
                       choices = c(2015,
                                   2016,
                                   2017,
                                   2018,
                                   2019,
                                   2020)
                     )
                   ),
                   sankeyNetworkOutput("g_x_sankey")
                   
                 )
               )
             ))
    
  )
)







# |- SERVER ----

server <- function(input, output) {
  # reactive () function for heatmap
  htmap_data <- reactive({
    oecd_world_dt %>%
      filter(year %in% input$year_button)
  })
  # heatmap output
  output$heatmap <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_polygon(data = htmap_data(),
                     aes(
                       x = long,
                       y = lat,
                       group = group,
                       fill = percentage
                     )) +
        theme_minimal() +
        theme(axis.ticks = element_blank()) +
        labs(x = NULL, y = NULL)
    )
  })
  
  # include the table 
  output$table1 <- renderDataTable({
    G_XGDP %>% 
      select(country, year, percentage)
  })
  
  # reactive () function for geompoint
  G_PPP_R <- reactive({
    G_PPP %>%
      filter(country %in% input$list_of_countries)
  })
  
  # output - g_ppp_geompt
  output$gerd_ppp_gpt <- renderPlotly({
    ggplotly(
      ggplot(
        data = G_PPP_R(),
        mapping = aes(x = year,
                      y = value,
                      color = country),
        na.rm = TRUE
      ) +
        geom_point() +
        scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(x = "calendar year",
             y = "current PPP($)")
    )
  })
  
  # graph 4 - population per capita
  G_XPOP_R <- reactive({
    G_XPOP %>% 
      filter(country %in% input$list_of_countries,
             year %in% input$yar_button)
  })
  #output 
  output$gerd_gxpop <- renderPlotly({
    ggplotly(
      ggplot(G_XPOP_R(), aes(country, value, fill = country)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = label_dollar(prefix = "", suffix = "$")) +
        labs(x = "country",
             y = "current PPP($)") +
        theme_minimal() +
        theme(axis.ticks = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(angle = 90)) 
    )
  })
  
  
  # graph 4 - sankey
  G_XSects_R <- reactive({
    G_XSects %>%
      filter(country %in% input$list_of_countries,
             year %in% input$yr_button)
  })
  # output sankey diagram
  output$g_x_sankey <- renderSankeyNetwork({
    sankeyNetwork(
      Links = G_XSects_R(),
      Nodes = nodes,
      Source = "IDsector",
      Target = "IDcountry",
      Value = "value",
      NodeID = "name",
      fontSize = 16,
      nodeWidth = 100
    )
  })
  
  
  
  
  
  
  
  
}
# |- SHINY ----
shinyApp(ui = ui, server = server)