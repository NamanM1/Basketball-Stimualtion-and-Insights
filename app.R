library(shiny)
library(shinyjs)

ui <- fluidPage(
  
  useShinyjs(),
  
  style = "background-color: #FFF6F6;",
  div(
    style = "color: #F875AA; font-size: 32px; font-family: Algerian;",
    "Basketball Game Stimulator"
  ),
  
  fluidRow(
    column(
      width = 3,
      wellPanel(
          style = "background-color: #FFDFDF;",
        h4("Team A", style = "font-weight: bold; font-size: 22px;"),
        selectizeInput(
          inputId = "FirstPlayer_A",
          label   = "Player 1",
          choices = top_50$Player,
          multiple = T,
          options = list(maxItems = 1)
                ),
        selectizeInput(
          inputId = "SecondPlayer_A",
          label   = "Player 2",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          inputId = "ThirdPlayer_A",
          label   = "Player 3",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          inputId = "FourthPlayer_A",
          label   = "Player 4",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          inputId = "FifthPlayer_A",
          label   = "Player 5",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        ))),
    
    
    column(
      width = 6,
      align = 'right',
      mainPanel(
        uiOutput("scoreboard"),
          actionButton(
            inputId = "Stimulation",
            label = "Stimulate",
            style = "background-color: #40F8FF; color: white; font-size: 20px; font-weight: bold; font-family: Comic Sans MS",
          ),

        tableOutput("table_output"),
        tags$div(
          style = "color: red; font-size: 24px; font-family: Times New Roman; margin-top: 30px;",  # Adjust the style and margin as needed
          textOutput("Winner")
        )
      )
  ),
  
    
    column(
      width = 3,
      wellPanel(
        style = "background-color: #FFDFDF;",
        h4("Team B", style = "font-weight: bold; font-size: 22px;"),
        selectizeInput(
          inputId = "FirstPlayer_B",
          label   = "Player 1",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          inputId = "SecondPlayer_B",
          label   = "Player 2",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          inputId = "ThirdPlayer_B",
          label   = "Player 3",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          inputId = "FourthPlayer_B",
          label   = "Player 4",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          inputId = "FifthPlayer_B",
          label   = "Player 5",
          choices = NULL,
          multiple = T,
          options = list(maxItems = 1)
        )))
  )
)

server <- function(input, output, session) {
  
  
  selected_players_A <- reactive({
    c(input$FirstPlayer_A, input$SecondPlayer_A, input$ThirdPlayer_A, input$FourthPlayer_A, input$FifthPlayer_A)
  })
  
  selected_players_B <- reactive({
    c(input$FirstPlayer_B, input$SecondPlayer_B, input$ThirdPlayer_B, input$FourthPlayer_B, input$FifthPlayer_B)
  })
  
 
  observe({
    if(!is.null(input$FirstPlayer_A)){
        updateSelectizeInput(session, "FirstPlayer_B", choices = setdiff(top_50$Player, input$FirstPlayer_A ))
    }
    })
  observe({
    A <- c(input$FirstPlayer_A, input$FirstPlayer_B)
    
    if(!is.null(input$FirstPlayer_B)){
      updateSelectizeInput(session, "SecondPlayer_A", choices = setdiff(top_50$Player, A ))
    }
  })
  observe({
    A <- c(input$FirstPlayer_A, input$FirstPlayer_B, input$SecondPlayer_A)
    
    if(!is.null(input$SecondPlayer_A)){
      updateSelectizeInput(session, "SecondPlayer_B", choices = setdiff(top_50$Player, A ))
    }
  })
  observe({
    A <- c(input$FirstPlayer_A, input$FirstPlayer_B, input$SecondPlayer_A, input$SecondPlayer_B)
    
    if(!is.null(input$SecondPlayer_B)){
      updateSelectizeInput(session, "ThirdPlayer_A", choices = setdiff(top_50$Player, A ))
    }
  })
  observe({
    A <- c(input$FirstPlayer_A, input$FirstPlayer_B, input$SecondPlayer_A, input$SecondPlayer_B, input$ThirdPlayer_A)
    
    if(!is.null(input$ThirdPlayer_A)){
      updateSelectizeInput(session, "ThirdPlayer_B", choices = setdiff(top_50$Player, A ))
    }
  })
  observe({
    A <- c(input$FirstPlayer_A, input$FirstPlayer_B, input$SecondPlayer_A, input$SecondPlayer_B, input$ThirdPlayer_A, input$ThirdPlayer_B)
    
    if(!is.null(input$ThirdPlayer_B)){
      updateSelectizeInput(session, "FourthPlayer_A", choices = setdiff(top_50$Player, A ))
    }
  })
  observe({
    A <- c(input$FirstPlayer_A, input$FirstPlayer_B, input$SecondPlayer_A, input$SecondPlayer_B, input$ThirdPlayer_A, input$ThirdPlayer_B, input$FourthPlayer_A)
    
    if(!is.null(input$FourthPlayer_A)){
      updateSelectizeInput(session, "FourthPlayer_B", choices = setdiff(top_50$Player, A ))
    }
  })
  observe({
    A <- c(input$FirstPlayer_A, input$FirstPlayer_B, input$SecondPlayer_A, input$SecondPlayer_B, input$ThirdPlayer_A, input$ThirdPlayer_B, input$FourthPlayer_A, input$FourthPlayer_B)
    
    if(!is.null(input$FourthPlayer_B)){
      updateSelectizeInput(session, "FifthPlayer_A", choices = setdiff(top_50$Player, A ))
    }
  })
  observe({
    A <- c(input$FirstPlayer_A, input$FirstPlayer_B, input$SecondPlayer_A, input$SecondPlayer_B, input$ThirdPlayer_A, input$ThirdPlayer_B, input$FourthPlayer_A, input$FourthPlayer_B, input$FifthPlayer_A)
    
    if(!is.null(input$FifthPlayer_A)){
      updateSelectizeInput(session, "FifthPlayer_B", choices = setdiff(top_50$Player, A ))
    }
  })
  
  
  observe({
    if (length(selected_players_A()) == 5 && length(selected_players_B()) == 5) {
      shinyjs::enable("Stimulation")
    } else {
      shinyjs::disable("Stimulation")
    }
  })
  
  
  output$scoreboard <- renderUI({
    score_A <- 0
    score_B <- 0
    quarters <- 1:4
    quarter_scores <- data.frame(Quarter = quarters, TeamA = score_A, TeamB = score_B)
    
    table_output <- renderTable({
      quarter_scores
    }, striped = TRUE, bordered = TRUE)
  })
  
  
  weights <- c(Points = 0.6, Rebounds = 0.2, Assists = 0.2, Steals = 0.1, Turnovers = -0.1)
  player_Score <- function(name){
    player <- top_50[top_50$Player == name,]
    
   R1 <- runif(1, -7, 7)
    
    score <- (
      ((player$PTS + R1) * weights["Points"]) +
      ((player$TRB + R1) * weights["Rebounds"]) +
      ((player$AST + R1) * weights["Assists"]) +
      ((player$STL + R1) * weights["Steals"]) +
      ((player$TOV + R1) * weights["Turnovers"])
    )
    return(score)
  }
  
  
  
  score_A <- 0
  score_B <- 0
  quarters <- 1:4
  quarter_scores <- data.frame(Quarter = quarters, TeamA = score_A, TeamB = score_B)
  q <- 1

  
  observeEvent(input$Stimulation, {
    
    
  while (q < 5) {
  TeamA_Score <- player_Score(input$FirstPlayer_A) + player_Score(input$SecondPlayer_A) + player_Score(input$ThirdPlayer_A) + player_Score(input$FourthPlayer_A) + player_Score(input$FifthPlayer_A)
  TeamB_Score <- player_Score(input$FirstPlayer_B) + player_Score(input$SecondPlayer_B) + player_Score(input$ThirdPlayer_B) + player_Score(input$FourthPlayer_B) + player_Score(input$FifthPlayer_B)

  
  print(paste("TeamA_Score: ", TeamA_Score))
  print(paste("TeamB_Score: ", TeamB_Score))
  
  
  if (TeamA_Score > TeamB_Score) {
    quarter_scores[quarter_scores$Quarter == q, ]$TeamA <- 1
  } else if (TeamB_Score > TeamA_Score) {
    quarter_scores[quarter_scores$Quarter == q, ]$TeamB <- 1
  } else {
    quarter_scores[quarter_scores$Quarter == q, "TeamA"] <- 0
    quarter_scores[quarter_scores$Quarter == q, "TeamB"] <- 0
  }
  
  if (sum(quarter_scores$TeamA) == 3 || sum(quarter_scores$TeamB) == 3) {
    q <- 5  # If one team has already won 3 quarters, skip the 4th quarter
  } else {
    q <- q + 1
  }
  
}

   
  # Update the table output
  output$scoreboard <- renderTable({
    quarter_scores
  },striped = TRUE, bordered = TRUE)
  
  if(sum(quarter_scores$TeamA) > sum(quarter_scores$TeamB))
    {
     tect <- "Team A WINS!!"
  } 
  else if(sum(quarter_scores$TeamA) < sum(quarter_scores$TeamB))
    {
    tect <- "Team B WINS!!"
  }
  else { 
    tect <- "Tie :("
  }
  
  output$Winner <- renderText({
    tect
  })
  })
}
 

shinyApp(ui, server)
