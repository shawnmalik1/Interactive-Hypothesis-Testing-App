library(shiny)

ui <- fluidPage(
  titlePanel(
    div(
      "Interactive Hypothesis Testing App",
      tags$h4("Created by Shawn Malik, Billy Kitchel, and Andy Chen")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "test_type", 
        "Choose Hypothesis Test:",
        choices = c("One-Sample t-Test", "Two-Sample t-Test", "Proportion Test")
      ),
      conditionalPanel(
        condition = "input.test_type == 'One-Sample t-Test'",
        textInput("data_one", "Enter Sample Data (comma-separated):", "12, 15, 14, 16, 13, 14, 15"),
        numericInput("mu_one", "Population Mean (H0):", 14)
      ),
      conditionalPanel(
        condition = "input.test_type == 'Two-Sample t-Test'",
        textInput("data_two_a", "Enter Sample 1 Data (comma-separated):", "12, 15, 14, 16, 13"),
        textInput("data_two_b", "Enter Sample 2 Data (comma-separated):", "14, 17, 15, 18, 14")
      ),
      conditionalPanel(
        condition = "input.test_type == 'Proportion Test'",
        numericInput("successes", "Number of Successes:", 40),
        numericInput("trials", "Number of Trials:", 100),
        numericInput("p_null", "Null Proportion (H0):", 0.5)
      ),
      numericInput("alpha", "Significance Level (α):", 0.05, min = 0, max = 1, step = 0.01),
      actionButton("run_test", "Run Test", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Results",
          h3("Test Results"),
          verbatimTextOutput("test_output"),
          plotOutput("result_plot")
        ),
        tabPanel(
          "Help",
          h3("What is Hypothesis Testing?"),
          p("Hypothesis testing is a method of making decisions or inferences about population parameters based on sample data."),
          p("In this app, you can perform the following tests:"),
          tags$ul(
            tags$li("One-Sample t-Test: Compare the mean of a sample to a known population mean."),
            tags$li("Two-Sample t-Test: Compare the means of two independent samples."),
            tags$li("Proportion Test: Test the proportion of successes against a hypothesized proportion.")
          ),
          p("Use the Results tab to see outputs such as p-values, test statistics, and visualizations.")
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$run_test, {
    test_result <- reactive({
      if (input$test_type == "One-Sample t-Test") {
        sample_data <- as.numeric(unlist(strsplit(input$data_one, ",")))
        t.test(sample_data, mu = input$mu_one)
      } else if (input$test_type == "Two-Sample t-Test") {
        sample_data_a <- as.numeric(unlist(strsplit(input$data_two_a, ",")))
        sample_data_b <- as.numeric(unlist(strsplit(input$data_two_b, ",")))
        t.test(sample_data_a, sample_data_b)
      } else if (input$test_type == "Proportion Test") {
        prop.test(input$successes, input$trials, p = input$p_null)
      }
    })
    
    output$test_output <- renderPrint({
      test_result()
    })
    
    output$result_plot <- renderPlot({
      if (input$test_type == "One-Sample t-Test") {
        sample_data <- as.numeric(unlist(strsplit(input$data_one, ",")))
        hist(
          sample_data, 
          main = "Sample Data Distribution", 
          xlab = "Values", 
          col = "#232D4B", 
          border = "white"
        )
        abline(v = input$mu_one, col = "#E57200", lwd = 2, lty = 2)
        legend("topright", legend = "Population Mean (H0)", col = "#E57200", lty = 2, lwd = 2)
      } else if (input$test_type == "Two-Sample t-Test") {
        sample_data_a <- as.numeric(unlist(strsplit(input$data_two_a, ",")))
        sample_data_b <- as.numeric(unlist(strsplit(input$data_two_b, ",")))
        boxplot(
          sample_data_a, sample_data_b,
          names = c("Sample 1", "Sample 2"),
          col = c("#232D4B", "#E57200"),
          main = "Boxplot of Two Samples"
        )
      } else if (input$test_type == "Proportion Test") {
        barplot(
          c(input$successes, input$trials - input$successes),
          names.arg = c("Successes", "Failures"),
          col = c("#E57200", "#232D4B"),
          main = "Proportion Test Breakdown"
        )
      }
    })
  })
}

shinyApp(ui = ui, server = server)
