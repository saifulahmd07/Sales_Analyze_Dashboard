library(shiny)
library(ggplot2)
library(dplyr)
library(lmtest)
library(DT)
library(nortest)
library(shinydashboard)

# Load data
data_A <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5.0, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Statistics", tabName = "desc"),
      menuItem("Fitting Model (Regression)", tabName = "regression"),
      menuItem("Model Prediction", tabName = "prediction"),
      menuItem("Assumptions Test", tabName = "assumptions")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "desc",
              fluidRow(
                column(width = 12, dataTableOutput("full_data")),
                column(width = 12, verbatimTextOutput("summary")),
                column(width = 12, plotOutput("desc_plot"))
              )
      ),
      tabItem(tabName = "regression",
              fluidRow(
                column(width = 12, plotOutput("regression_plot")),
                column(width = 12, verbatimTextOutput("regression_summary")),
                column(width = 12, verbatimTextOutput("model_equation"))
              )
      ),
      tabItem(tabName = "prediction",
              fluidRow(
                column(width = 6, 
                       numericInput("input_x1", "Website Visitors:", value = 200000),
                       numericInput("input_x2", "Monthly Transactions:", value = 10000),
                       numericInput("input_x3", "Avg. Items per Transaction:", value = 5),
                       sliderInput("input_x4", "Customer Satisfaction (1-10):", value = 8, min = 1, max = 10),
                       numericInput("input_x5", "Online Ads per Month:", value = 30000)
                ),
                column(width = 6, verbatimTextOutput("predicted_sales")),
                column(width = 12, plotOutput("predicted_plot"))
              )
      ),
      tabItem(tabName = "assumptions",
              fluidRow(
                column(width = 6,
                       style = "display: flex; flex-wrap: wrap;",
                       box(verbatimTextOutput("dw_test"), title = "Autocorrelation Durbin-Watson Test", width = 12),
                       box(verbatimTextOutput("bp_test"), title = "Homogeneity Breusch-Pagan Test", width = 12)
                ),
                column(width = 6,
                       style = "display: flex; flex-wrap: wrap;",
                       box(verbatimTextOutput("norm_test"), title = "Kolmogorov-Smirnov Normality Test", width = 12),
                       box(verbatimTextOutput("vif_test"), title = "Multicollinearity VIF Test", width = 12)
                )
              )
      )
    )
  )
)


# Server
server <- function(input, output) {
  
  # Descriptive Statistics
  output$full_data <- renderDataTable({
    datatable(data_A, options = list(scrollX = TRUE))
  })
  
  output$summary <- renderPrint({
    summary(data_A)
  })
  
  output$desc_plot <- renderPlot({
    par(mfrow = c(1, 5))
    for (i in 2:6) {
      hist(data_A[, i], main = colnames(data_A)[i], xlab = colnames(data_A)[i], col = "skyblue")
    }
  })
  
  # Fitting Model (Regression)
  output$regression_plot <- renderPlot({
    par(mfrow = c(1, 1))
    # Scatterplot matrix
    plot(data_A[-1], pch = 19)
  })
  
  output$regression_summary <- renderPrint({
    model_reg <- lm(y ~ ., data = data_A[, -1])
    summary(model_reg)
  })
  
  output$model_equation <- renderPrint({
    model_reg <- lm(y ~ ., data = data_A[, -1])
    coefficients <- format(round(coef(model_reg), 4), nsmall = 4)
    equation <- paste("Model Equation: y =", paste(coefficients, names(coefficients), collapse = " + "))
    cat(equation, sep = "\n")
  })
  
  # Model Prediction
  output$predicted_sales <- renderText({
    new_data <- data.frame(
      x1 = input$input_x1,
      x2 = input$input_x2,
      x3 = input$input_x3,
      x4 = input$input_x4,
      x5 = input$input_x5
    )
    model_reg <- lm(y ~ ., data = data_A[, -1])
    predicted_sales <- predict(model_reg, newdata = new_data)
    paste("Predicted Sales:", round(predicted_sales, 2))
  })
  
  output$predicted_plot <- renderPlot({
    new_data <- data.frame(
      x1 = input$input_x1,
      x2 = input$input_x2,
      x3 = input$input_x3,
      x4 = input$input_x4,
      x5 = input$input_x5
    )
    
    model_reg <- lm(y ~ ., data = data_A[, -1])
    predicted_sales <- predict(model_reg, newdata = new_data)
    
    actual <- data_A$y
    plot(actual, type = "b", xlab = "Index", ylab = "Sales",
         main = "Predicted vs Actual Sales", col = "red", xlim = c(1, 14), ylim = c(-250, 350))
    
    points(length(actual) + 1, predicted_sales, col = "blue", pch = 19)
    legend("topleft", legend = c("Predicted", "Actual"), col = c("blue", "red"), lty = 1)
  })
  
  # Assumptions Test
  output$dw_test <- renderPrint({
    model_reg <- lm(y ~ ., data = data_A[, -1])
    dw_test <- lmtest::dwtest(model_reg)
    dw_test
  })
  
  output$bp_test <- renderPrint({
    model_reg <- lm(y ~ ., data = data_A[, -1])
    bp_test <- lmtest::bptest(model_reg, studentize = TRUE)
    bp_test
  })
  
  output$norm_test <- renderPrint({
    model_reg <- lm(y ~ ., data = data_A[, -1])
    norm_test <- nortest::lillie.test(model_reg$residuals)
    norm_test
  })
  
  output$vif_test <- renderPrint({
    model_reg <- lm(y ~ ., data = data_A[, -1])
    vif_test <- car::vif(model_reg)
    vif_test
  })
}

shinyApp(ui = ui, server = server)
