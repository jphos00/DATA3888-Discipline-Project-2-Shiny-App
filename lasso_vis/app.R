# install.packages("shinythemes")


# Loading required packages
library(shiny)
library(shinythemes)
library(glmnet)
library(tidyverse)
library(plotly)
library(caret)
load("reduced_dat.RData")


n = length(binary_outcome)
train_id = sample(1:n, round(n/2))
reduced_dat = reduced_dat[, 1:20]

X_train = reduced_dat[train_id, ]
X_test = reduced_dat[-train_id, ]
y_train = binary_outcome[train_id]
y_test = binary_outcome[train_id]

calc_metrics = function(confus_tab){
    TN = confus_tab[1, 1]; FN = confus_tab[1, 2]
    FP = confus_tab[2, 1]; TP = confus_tab[2, 2]
    
    accuracy = (TN + TP)/(TP + FN + FP + TN)
    precision = (TP)/(TP+FP)
    recall = (TP)/(TP+FN)
    F1 = 2*(precision * recall)/(precision + recall)
    
    metrics = c(accuracy, precision, recall, F1)
    ifelse(is.na(metrics), 0, metrics)
}

mod_0 = glmnet(x=as.matrix(X_train), y_train, family = "binomial", lambda = 0)
stored_betas = as.matrix(mod_0$beta) %>% t()
stored_lambdas = c(0)
pred_0 = predict(mod_0, type="response",newx=as.matrix(X_test)) %>%  
    round() %>%  
    factor(., levels = c(0, 1))
stored_metrics = calc_metrics(confusionMatrix(pred_0, factor(y_test, levels = c(0, 1)))$table)

getIntroPage = function() {
    includeHTML("intro.html")
}

# Define UI for application that draws a histogram
ui = navbarPage(theme = shinytheme("flatly"),
                title = "An Introduction to Logistic LASSO Regression",
                position = "fixed-top",
                tabPanel("Introduction",
                         tags$div(tags$br(),
                                  tags$br(),
                                  tags$br()),
                                  
                         htmlOutput("intro_page")
                         ),
                tabPanel("Optimise a Logistic LASSO Regression Model",
                         tags$div(tags$br(),
                                  tags$br(),
                                  tags$br()),
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("lambda",withMathJax("Select a \\( \\lambda \\):"),
                                             min = 0, max = 0.3, value = 0),
                                 radioButtons("metric", "Select a metric to optimise:",
                                              c("Accuracy" = "Accuracy",
                                                "Precision" = "Precision",
                                                "Recall" = "Recall",
                                                "F1 score" = "F1")),
                                 ),
                        mainPanel(
                            verbatimTextOutput("confus_mat"),
                            plotlyOutput("shrinkage"),
                            plotlyOutput("metric_analysis")
                            )
                        )
                    ),
                tabPanel("Additional information",
                         tags$div(tags$br(),
                                  tags$br(),
                                  tags$br())
                    
                    )
                )

# Define server logic required to draw a histogram
server = function(input, output, session) {
    
    output$intro_page = renderUI({getIntroPage()})
    
    new_model = reactive({
        mod = glmnet(x=as.matrix(X_train), y_train, family = "binomial", lambda = input$lambda)
        predictions = predict(mod, type="response",newx=as.matrix(X_test)) %>%  
            round() %>%  
            factor(., levels = c(0, 1))
        confus_matrix = confusionMatrix(predictions, factor(y_test, levels = c(0, 1)))$table
        new_betas = as.matrix(mod$beta) %>% t()
        stored_betas <<- rbind(stored_betas, new_betas)
        stored_lambdas <<- c(stored_lambdas, input$lambda)
        
        stored_metrics <<- rbind(stored_metrics, calc_metrics(confus_matrix))
        
        list("confus_mat" = confus_matrix, 
             "betas" = stored_betas, 
             "lambdas" = stored_lambdas,
             "metrics" = stored_metrics)
    })
    
    output$confus_mat <- renderPrint({
        new_model()$confus_mat
    })
    
    output$shrinkage <- renderPlotly({
        lambdas = new_model()$lambdas
        betas = new_model()$betas
        unique_idx = !duplicated(lambdas)
        
        df = data.frame(lambda = lambdas[unique_idx], betas[unique_idx, ])
        
        df_long = df %>% pivot_longer(cols = 2:ncol(df),
                                      names_to = "gene",
                                      values_to = "coefficient")
        
        
        p = df_long %>% 
            ggplot(aes(x = lambda, y = coefficient, colour = gene)) +
            geom_line() +
            geom_hline(yintercept = 0, colour = "black")
        
        plot = ggplotly(p) %>%
            layout(yaxis = list(hoverformat = ".2d"))
    })
    
    output$metric_analysis <- renderPlotly({
        lambdas = new_model()$lambdas
        metrics = new_model()$metrics
        unique_idx = !duplicated(lambdas)
        df = data.frame(cbind(lambda = lambdas[unique_idx], matrix(metrics[unique_idx, ], ncol = 4)))
        
        
        names(df) = c("lambda", "Accuracy", "Precision", "Recall", "F1")
        
        df_select = df %>% select(lambda, input$metric)
        
        p = df_select %>% ggplot(aes(x = lambda, y = !!sym(input$metric))) +
            geom_line()
        
        ggplotly(p)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
