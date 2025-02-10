#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)

tb_1 <- readRDS("rff-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb_2 <- readRDS("linear-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb <- rbind(tb_1, tb_2)

tb$penalty_f = factor(tb$penalty, 
                      levels = c("None", "10e-3", "10e-2", "10e-1", "10",
                                 "10e+1", "10e+2", "10e+3"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "VoC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Timing Strategy", tabName = "line_plot", icon = icon("image")),
      menuItem("Estimated Return", tabName = "line_plot_return", icon = icon("image")),
      menuItem("Boxplots", tabName = "box_plot", icon = icon("image")),
      menuItem("Table", tabName = "stats_table", icon = icon("table")),
      menuItem("Data", tabName = "download", icon = icon("download")),
      menuItem("About", tabName = "about", icon = icon("circle-info"))
    )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "line_plot",
              fluidRow(
                box(
                  title = "Plot Options",
                  selectInput("selector_window", h4("Window Size"),
                              choices = c(12, 60, 120, 180, 240, 360), 
                              selected = 120, width = '100px'),
                  selectInput("selector_method", h4("Estimation Method"),
                              choices = c("RFF", "Linear", "Both"), 
                              selected = "Both", width = '100px'),
                  width = '3'
                ),
                box(plotOutput("line"), width = '9', height = 650),
              )
      ),

      tabItem(tabName = "line_plot_return",
              fluidRow(
                box(
                  title = "Plot Options",
                  selectInput("selector_window", h4("Window Size"),
                              choices = c(12, 60, 120, 180, 240, 360), 
                              selected = 120, width = '100px'),
                  selectInput("selector_method", h4("Estimation Method"),
                              choices = c("RFF", "Linear", "Both"), 
                              selected = "Both", width = '100px'),
                  width = '3'
                ),
                box(plotOutput("line_return"), width = '9', height = 650),
              )
      ),
      
      tabItem(tabName = "box_plot",
              fluidRow(
                box(plotOutput("box"), width = '9', height = 650),
              )
      ),
      
      tabItem(tabName = "stats_table",
              h2("Sharpe Ratios"),
              dataTableOutput("table")
      ),
      tabItem(tabName = "download",
              h2("Raw Data"),
              "Download the data as a csv file.",
              br(),
              downloadButton('download', "Download the data")
      ),
      tabItem(tabName = "about",
              h2("About this App"),
              "This app is an online supplement for Elmore and Strauss (2025+)."
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    df <- reactive({
      if(input$selector_method == "Both"){
        tb |> dplyr::filter(window == input$selector_window,
                          date >= ymd("1950-01-01")) 
      } else if(input$selector_method == "RFF"){
        tb |> dplyr::filter(window == input$selector_window,
                            date >= ymd("1950-01-01"),
                            method == "RFF") 
      } else {
        tb |> dplyr::filter(window == input$selector_window,
                            date >= ymd("1950-01-01"),
                            method == "Linear") 
      }
    })

  output$table <- renderDataTable({
    datatable(tb |> 
                filter(date >= lubridate::ymd("1950-01-01")) |> 
                group_by(method, penalty, window) |> 
                summarize(m = mean(ts, na.rm = T),
                          s = sd(ts, na.rm = T),
                          sharpe = sqrt(12)*m/s) |> 
                ungroup() |> 
                rbind(
                  tb_2 |> 
                    filter(date >= lubridate::ymd("1950-01-01")) |> 
                    group_by(method, penalty, window) |> 
                    summarize(m = mean(ts, na.rm = T),
                              s = sd(ts, na.rm = T),
                              sharpe = sqrt(12)*m/s) |> 
                    ungroup()
                ), 
              rownames = F) |> 
      formatRound(c(4:6), c(5, 5, 5))
  })
  
  output$line <- renderPlot({
    p <- ggplot(data = df(),
                aes(x = date, y = ts, col = penalty_f, group = penalty_f))
    p + geom_line() + 
      facet_grid(penalty_f ~ method) +
      scale_color_brewer("Ridge Penalty", palette = "Dark2") +
      labs(x = "Date",
           y = "Estimated Timing Strategy") +
      scale_x_date(date_breaks = "5 year", date_minor_breaks = "2.5 year") +
      guides(x =  guide_axis(angle = 45)) +
      theme_bw()
  }, height = 600)

  output$line_return <- renderPlot({
    p <- ggplot(data = df(),
                aes(x = date, y = y_hat, col = penalty_f, group = penalty_f))
    p + geom_line() + 
      facet_grid(penalty_f ~ method) +
      scale_color_brewer("Ridge Penalty", palette = "Dark2") +
      labs(x = "Date",
           y = "Estimated Return") +
      scale_x_date(date_breaks = "5 year", date_minor_breaks = "2.5 year") +
      guides(x =  guide_axis(angle = 45)) +
      theme_bw()
  }, height = 600)
  
  output$box <- renderPlot({
    p <- ggplot(data = tb |> 
                  filter(date >= lubridate::ymd("1950-01-01")),
                aes(x = penalty_f, y = ts, fill = penalty_f))
    p + geom_boxplot() + 
      facet_grid(method ~ .) +
      scale_fill_brewer("Ridge Penalty", palette = "Dark2") +
      labs(x = "Ridge Penalty",
           y = "Timing Strategy") +
      scale_y_continuous(limits = c(-0.001, 0.001)) +
      theme_bw()
  }, height = 600)
  
  output$download <- downloadHandler(
    filename = function() {
      paste("rff-icv", ".csv", sep="")
    },
    content = function(file) {
      write.csv(tb, file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
