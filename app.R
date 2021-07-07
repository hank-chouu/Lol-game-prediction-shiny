
library(shinythemes)
library(shiny)
library(recipes)
library(tidyverse)

df_train <- read_csv("Challenger_Ranked_Games.csv") %>% select(3:26)
df_test <- read_csv("Master_Ranked_Games.csv") %>% select(3:26)
var_option <- colnames(df_train[, -1])


 
transforming_df <- function(df, index){
    
    
    
    num_index <- index[!(grepl("First", index))]
    bin_index <- index[(grepl("First", index))]
    
    
    if (!identical(num_index, character(0)) & !identical(bin_index, character(0))){
        
        num_df <- df[, -1] %>% select(num_index)
        
        
        
        bin_df <- df[, -1] %>% select(bin_index) %>% apply(2, factor) %>% as_tibble
        
        new_df <- df[, 1] %>% cbind(num_df) %>% cbind(bin_df)
    }
    else if (!identical(num_index, character(0)) & identical(bin_index, character(0))){
        
        num_df <- df[, -1] %>% select(num_index)
        
        
        new_df <- df[, 1] %>% cbind(num_df)
    }
    else if (identical(num_index, character(0)) & !identical(bin_index, character(0))){
        
        bin_df <- df[, -1] %>% select(bin_index) %>% apply(2, factor) %>% as_tibble
        
        new_df <- df[, 1] %>% cbind(bin_df)
        
        
    }
    
    new_df
    
}







# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("sandstone"),
    
    titlePanel("Game prediction on League of Legend"), 
    
    sidebarLayout(
        
        sidebarPanel(
            
            selectInput(
                "choice", 
                "Choose the variables you want and edit the value to predict result!",
                var_option, 
                multiple = T
            ),
            
            actionButton(
                "train", 
                "Predict!"
            ),
            
            verbatimTextOutput("status"),
            
            verbatimTextOutput("r2"),
            verbatimTextOutput("acc"),
            verbatimTextOutput("result")
        ),
        mainPanel(
            
            fluidRow(
                column(12, align = "center", 
                       conditionalPanel(
                           "input.train", 
                           plotOutput("pic")
                       ))
            ),
            
            column(4,
            conditionalPanel(
                condition = "input.choice.includes('blueFirstBlood')",
                selectInput(
                    "blueFirstBlood", 
                    "First blood", 
                    c("yes", "no"), selected = "no"
                )
            ),
            conditionalPanel(
                condition = "input.choice.includes('blueFirstTower')",
                selectInput(
                    "blueFirstTower", 
                    "First Tower", 
                    c("yes", "no"), selected = "no"
                )
            ),
            conditionalPanel(
                condition = "input.choice.includes('blueFirstBaron')",
                selectInput(
                    "blueFirstBaron", 
                    "First Baron", 
                    c("yes", "no"), selected = "no"
                )
            ), 
            conditionalPanel(
                condition = "input.choice.includes('blueFirstDragon')",
                selectInput(
                    "blueFirstDragon", 
                    "First Dragon", 
                    c("yes", "no"), selected = "no"
                )
            ),
            conditionalPanel(
                condition = "input.choice.includes('blueFirstInhibitor')",
                selectInput(
                    "blueFirstInhibitor", 
                    "First Inhibitor", 
                    c("yes", "no"), selected = "no"
                )
            ),
            conditionalPanel(
                condition = "input.choice.includes('blueDragonKills')",
                sliderInput(
                    "blueDragonKills", 
                    "Dragon Kills", 
                    max = 7, min = 0, value = 0
                )
            ),
            conditionalPanel(
                condition = "input.choice.includes('blueBaronKills')",
                sliderInput(
                    "blueBaronKills", 
                    "Baron Kills", 
                    max = 5, min = 0, value = 0
                )
            ), 
            conditionalPanel(
                condition = "input.choice.includes('blueTowerKills')",
                sliderInput(
                    "blueTowerKills", 
                    "Tower Kills", 
                    max = 11, min = 0, value = 0
                )
            )
            ),
            column(4,
                   conditionalPanel(
                       condition = "input.choice.includes('blueInhibitorKills')",
                       sliderInput(
                           "blueInhibitorKills", 
                           "Inhibitor Kills", 
                           max = 10, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueWardPlaced')",
                       sliderInput(
                           "blueWardPlaced", 
                           "Ward Placed", 
                           max = 230, min = 0, value = 0
                       )
                   ), 
                   conditionalPanel(
                       condition = "input.choice.includes('blueWardkills')",
                       sliderInput(
                           "blueWardkills", 
                           "Ward Kills", 
                           max = 100, min = 0, value = 0
                       )
                   ), 
                   conditionalPanel(
                       condition = "input.choice.includes('blueKills')",
                       sliderInput(
                           "blueKills", 
                           "Kills", 
                           max = 100, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueDeath')",
                       sliderInput(
                           "blueDeath", 
                           "Death", 
                           max = 100, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueAssist')",
                       sliderInput(
                           "blueAssist", 
                           "Assist", 
                           max = 240, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueChampionDamageDealt')",
                       sliderInput(
                           "blueChampionDamageDealt", 
                           "Champion Damage Dealt", 
                           max = 350000, min = 0, value = 0
                       )
                   ) 
                   
            ),
            column(4, 
                   conditionalPanel(
                       condition = "input.choice.includes('blueTotalGold')",
                       sliderInput(
                           "blueTotalGold", 
                           "Total Gold", 
                           max = 120000, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueTotalMinionKills')",
                       sliderInput(
                           "blueTotalMinionKills", 
                           "Total Minion Kills", 
                           max = 1200, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueTotalLevel')",
                       sliderInput(
                           "blueTotalLevel", 
                           "Total Level", 
                           max = 90, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueAvgLevel')",
                       sliderInput(
                           "blueAvgLevel", 
                           "Average Level", 
                           max = 18, min = 0, value = 0, step = 0.1
                       )
                   ), 
                   conditionalPanel(
                       condition = "input.choice.includes('blueJungleMinionKills')",
                       sliderInput(
                           "blueJungleMinionKills", 
                           "Jungle Minion Kills", 
                           max = 400, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueKillingSpree')",
                       sliderInput(
                           "blueKillingSpree", 
                           "Killing Spree", 
                           max = 30, min = 0, value = 0
                       )
                   ),
                   conditionalPanel(
                       condition = "input.choice.includes('blueTotalHeal')",
                       sliderInput(
                           "blueTotalHeal", 
                           "Total Heal", 
                           max = 160000, min = 0, value = 0
                       )
                   ), 
                   conditionalPanel(
                       condition = "input.choice.includes('blueObjectDamageDealt')",
                       sliderInput(
                           "blueObjectDamageDealt", 
                           "Object Damage Dealt", 
                           max = 160000, min = 0, value = 0
                       )
                   )
            )
            
        )#mainpanel    
    )
    
)#fluidpage


server <- function(input, output) {
    
    selected <- reactive({
        input$choice
    })
    
    new_observe <- reactive({
        new_obs_df <- tibble(
            
            blueWins = 0,
            
            blueFirstBlood = ifelse(input$blueFirstBlood == 'yes', 1, 0), 
            blueFirstTower = ifelse(input$blueFirstTower == 'yes', 1, 0),
            blueFirstBaron = ifelse(input$blueFirstBaron == 'yes', 1, 0),
            blueFirstDragon = ifelse(input$blueFirstDragon == 'yes', 1, 0),
            blueFirstInhibitor = ifelse(input$blueFirstInhibitor == 'yes', 1, 0),
            blueDragonKills = input$blueDragonKills,
            blueBaronKills = input$blueBaronKills,
            blueTowerKills = input$blueTowerKills,
            
            blueInhibitorKills = input$blueInhibitorKills,
            blueWardPlaced = input$blueWardPlaced,
            blueWardkills = input$blueWardkills,
            blueKills = input$blueKills,
            blueDeath = input$blueDeath,
            blueAssist = input$blueAssist, 
            blueChampionDamageDealt = input$blueChampionDamageDealt, 
            blueTotalGold = input$blueTotalGold,
            
            blueTotalMinionKills = input$blueTotalMinionKills,
            blueTotalLevel = input$blueTotalLevel,
            blueAvgLevel = input$blueAvgLevel,
            blueJungleMinionKills = input$blueJungleMinionKills,
            blueKillingSpree = input$blueKillingSpree,
            blueTotalHeal = input$blueTotalHeal,
            blueObjectDamageDealt = input$blueObjectDamageDealt
            
            
        )
        
        new_obs_df
    })
    
    
    
    
    observeEvent(input$train, {
        
        if (!is.null(selected())){
            
            
            
            # reset
            
            output$r2 <- renderPrint({})
            output$acc <- renderPrint({})
            output$result <- renderPrint({})
            output$pic <- renderImage({})
            
            # new one 
            
            df_train_new <- transforming_df(df_train, selected())
            df_test_new <- transforming_df(df_test, selected())
            
            model <- glm(blueWins ~ ., df_train_new, family = binomial)
            
            
            probs <- predict(model, df_test_new, type = "response")
            category <- ifelse(probs > 0.5, 1, 0)
            accuracy <- sum(pull(df_test_new, blueWins) == category) / length(category)
            
            
            
            
            output$r2 <- renderPrint({
                paste("R-squared:", round(with(summary(model), 1 - deviance/null.deviance), 2))
            })
            
            output$acc <- renderPrint({
                
                paste("Accuracy:", round(accuracy*100, 1), "%")
                
            })
            
            
            df_predict <- df_test
            df_predict[1, ] <- new_observe()
            
            df_predict <- transforming_df(df_predict, selected())
            
            probs <- predict(model, df_predict, type = "response")
            category <- ifelse(probs > 0.5, 1, 0)
            
            
            
            if (category[1] == 1){
                
                output$result <- renderPrint({
                    paste("This game will win!")
                })
                
                output$pic <- renderImage({
                    list(
                        src = "victory.jpg",
                        contentType = "image/jpeg",
                        width = 533, height = 300
                    )
                }, deleteFile = F)
                
            }
            else {
                
                output$result <- renderPrint({
                    paste("This game will lose!")
                })
                output$pic <- renderImage({
                    list(
                        src = "defeat.jpg",
                        contentType = "image/jpeg",
                        width = 533, height = 300
                    )
                }, deleteFile = F)
                
            }
            
            
            
            
            }#if 
            
        })
        
}#server
        

# Run the application 
shinyApp(ui = ui, server = server)
