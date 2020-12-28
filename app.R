library(tidyverse)
library(plotly)
library(shiny)

source("gpa_exploration_functions.R")

# read data
dat <- read.csv("uiuc-gpa-dataset.csv")

# make the term and yearterm into factors
dat$Term <- factor(dat$Term, c("Spring", "Summer", "Fall", "Winter"))
dat$YearTerm <- factor(dat$YearTerm, paste(rep(2010:2020, each = 4), c("sp", "su", "fa", "wi"), sep = "-"))

dat <- dat %>% 
  mutate(
    Size = A. + A + A..1 + B. + B + B..1 + C. + C + C..1 + D. + D + D..1 + `F`,
    Primary.Instructor = ifelse(Primary.Instructor == "", "-", Primary.Instructor)
  )

# calculate Average GPA & Class Size for each class
gpa <- c(4,4,3.67,3.33,3.00,2.67,2.33,2.00,1.67,1.33,1.00,0.67,0.00)
lettergrades <- colnames(dat)[str_detect(colnames(dat), "^[ABCDEF]\\.?\\.?[[:digit:]]?$")]
dat$Mean_GPA <- apply(dat[,lettergrades], MARGIN = 1, function(x) sum(x * gpa)) / dat$Size
dat$SD_GPA <- sqrt(apply(dat[,c(lettergrades, "Mean_GPA")], MARGIN  = 1, function(x) sum((gpa - x[length(x)])^2 * x[1:(length(x) - 1)])) / dat$Size )
dat$perc_A <- 100 * (dat$A. + dat$A) / dat$Size

all_subjs <- sort(unique(dat$Subject))

ui <- fluidPage(
  titlePanel("UIUC GPA Exploration", windowTitle = "UIUC GPA Shiny APP"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Subj", label = "Subject:", choices = all_subjs, 
        multiple = FALSE, selectize = TRUE
      ), 
      selectInput(
        inputId = "CourseN", label = "Course Number:", choices = NULL,
        multiple = FALSE, selectize = TRUE
      ),
      selectInput(
        inputId = "PI", label = "Primary Instructor", choices = NULL,
        multiple = TRUE, selectize = TRUE
      ),
      p("Only top 8 instructors with the highest class size are included"),
      br(),
      actionButton(
        inputId = "action", label = "Generate"
      ),
      width = 3
    ),
    mainPanel(
      plotlyOutput("gpaplotly", height = "130%"),
      plotlyOutput("percAplotly", height = "130%"),
      dataTableOutput("all_inst_summary"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  # update course number list based on 
  observeEvent(input$Subj, {
    updateSelectizeInput(
      session, "CourseN", label = "Choose Course Number:", 
      choices = sort(unique(filter(dat, Subject == input$Subj)[["Number"]])))
  })
  
  # update PIs 
  observe({
    updateSelectizeInput(
      session, "PI", label = "Primary Instructor (up to 8)", 
      choices = c("Leave Blank for Top 8" = "", sort(unique(filter(dat, Subject == input$Subj & Number == input$CourseN)[["Primary.Instructor"]])))
    )
  })
  
  gpadat_all_inst <- eventReactive(input$action, {
    retrieve_data(dat, input$Subj, input$CourseN)
  })
  
  gpadat <- reactive({
    get_plotly_gpadat(gpadat_all_inst(), input$PI)
  })
  
  gpaplotly <- eventReactive(input$action, {
    plotly_average_gpa(gpadat(), input$Subj, input$CourseN)
  })
  
  percAplotly <- eventReactive(input$action, {
    plotly_percentage_A(gpadat(), input$Subj, input$CourseN)
  })
  
  output$gpaplotly <- renderPlotly(gpaplotly())
  output$percAplotly <- renderPlotly(percAplotly())
  output$all_inst_summary <- renderDataTable(summarise_by_inst(gpadat_all_inst()),
                                             options = list(lengthMenu = c(5, 10, 50), pageLength = 5))
}

shinyApp(ui, server)
