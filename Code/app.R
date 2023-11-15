library(shiny)
library(shinyjs)
library(shinydashboard)
library(fresh)
library(ggplot2)
library(dplyr)

load("Player_database.Rdata")
load("top_50.Rdata")
load("Team_database.Rdata")

my_theme = create_theme(
  adminlte_color(
    light_blue = "#900C3F"
  )
)


df1 <- data.frame(
  Steals = Player_database$STL,
  Points = Player_database$PTS,
  Assists = Player_database$AST,
  Blocks  = Player_database$BLK,
  Turnovers = Player_database$TOV,
  Rebounds = Player_database$TRB,
  Teams = Player_database$Tm,
  Position = Player_database$Pos
)


ui <- dashboardPage(
  dashboardHeader(title = "BASKETBALL"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Analysis", tabName = "page1"),
      menuItem("Player Analysis", tabName = "page2"),
      menuItem("Simulation", tabName = "page3"),
      menuItem("Comparison", tabName = "page4")
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    tabItems(
      tabItem(tabName = "page3",
              fluidPage(
                
                useShinyjs(),
                
                style = "background-color: #FFF6F6;",
                div(
                  style = "color: #F875AA; font-size: 32px; font-family: Algerian;",
                  "Basketball Game Simulator"
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
                        label = "Simulate",
                        style = "background-color: #40F8FF; color: white; font-size: 20px; font-weight: bold; font-family: Comic Sans MS"
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
                
              
      ),

      tabItem(tabName = "page2",
              fluidRow(
                useShinyjs(),
                style = "background-color: #FFF6F6;",
                
                column(
                  width = 3,
                  wellPanel(
                    style = "background-color:#FFDFDF ;",
                    selectInput(
                      inputId = "x_variable",
                      label = "SELECT VARIABLE 1",
                      choices = names(df1)[1:6]
                    ),
                    selectInput(
                      inputId = "y_variable",
                      label = "SELECT VARIABLE 2",
                      choices = names(df1)[1:6]
                    ),
                    selectInput(
                      inputId = "size",
                      label = "SELECT SIZE",
                      choices = names(df1)[1:6]
                    ),
                    selectInput(
                      inputId = "color",
                      label = "SELECT COLOUR",
                      choices = names(df1)[7:8]
                    )
                  )
                ),
                
                column(
                  width = 9,
                  mainPanel(
                    plotOutput("scatter_plot")
                  )
                )),
                
                fluidRow(
                  useShinyjs(),
                  style = "background-color: #FFF6F6;",
                
                column(
                  width = 3,
                  wellPanel(
                    style = "background-color:#FFDFDF ;",
                    selectInput(
                      inputId = "Players",
                      label = "SELECT PLAYER",
                      choices = Player_database$Player
                    )
                  )
                ),
                
                column(
                  width = 9,
                  mainPanel(
                    plotOutput("SpiderMap")
                  )
                )
              ),
              
              
      ),
      
     
    tabItem(tabName = "page1",
            fluidRow(
              useShinyjs(),
              style = "background-color: #FFF6F6;",
              
              column(
                width = 3,
                wellPanel(
                  style = "background-color:#FFDFDF ;",
                  selectInput(
                    inputId = "Teams",
                    label = "SELECT TEAM",
                    choices = unique(Player_database$Tm)
                  )
                )
              ),
              
              column(
                width = 9,
                mainPanel(
                  plotOutput("SpiderMap2.o")
                )
              )
            ),
            
            
            fluidRow(
              useShinyjs(),
              style = "background-color: #FFF6F6;",
              
              column(
                width = 3,
                wellPanel(
                  style = "background-color:#FFDFDF ;",
                  selectInput(
                    inputId = "variable",
                    label = "SELECT VARIABLE",
                    choices = names(Team_database)
                  )
                )
              ),
              
              column(
                width = 9,
                mainPanel(
                  plotOutput("best_team_plot")
                )
              )
            )
            
    ),
    
    tabItem(tabName = "page4",
            fluidPage(
              
              useShinyjs(),
              style = "background-color: #FFF6F6;",
              
              sidebarLayout(
                sidebarPanel(
                  style = "background-color:#FFDFDF ;",
                  selectInput(
                    inputId  = "Team_1",
                    label    = "SELECT FIRST TEAM",
                    choices  = unique(Player_database$Tm)
                  ),
                  selectInput(
                    inputId  = "Team_2",
                    label    = "SELECT SECOND TEAM",
                    choices  = unique(Player_database$Tm)
                  ),
                ),
                
                mainPanel(
                  plotOutput("SpiderMap3.0")
                )
              )
            ))
  
)
))

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
  
  y=Player_database[,c(24,25,26,28,30)]
  y$TRB = as.double(y$TRB)
  y$AST = as.double(y$AST)
  y$STL = as.double(y$STL)
  y$TOV = as.double(y$TOV)
  y$PTS = as.double(y$PTS)
  
  o = as.data.frame(scale(y))
  colnames(o) = c("Rebounds", "Assists", "Steals", "Turnovers", "Points")
  o$name = Player_database$Player
  # Create a radial plot
  radial_plot = function(name){
    k = as.data.frame(t(o[o$name == name,]))
    k = as.data.frame(k)
    k$Category = row.names(k)
    colnames(k) = c("Value", 'Category')
    ggplot(k[-c(6),] , aes(x = Category, y = Value)) +
      geom_bar(stat = "identity" , fill = c("Skyblue","Coral","yellow","Green", "Pink")) +
      coord_polar(start = 0)  +
      labs(title = name)
  }
  
  output$SpiderMap <- renderPlot({
    player_name <- input$Players  # Get the selected player from the input
    # Call your radial_plot function with the selected player
    radial_plot(player_name)
  })
  
  Player_database$TRB = as.double(Player_database$TRB)
  Player_database$AST = as.double(Player_database$AST)
  Player_database$STL = as.double(Player_database$STL)
  Player_database$TOV = as.double(Player_database$TOV)
  
  Team_database = Player_database %>% group_by(Tm) %>% summarise(Points = mean(PTS) , Rebounds = mean(TRB) , Turnovers = mean(TOV) , Assists = mean(AST) , Steals = mean(STL) ) 
  h = as.data.frame(scale(Team_database[,-c(1)]))
  h$Tm = Team_database$Tm
  radial_plot_team = function(name){
    k = as.data.frame(t(h[h$Tm == name,]))
    k = as.data.frame(k)
    k$Category = row.names(k)
    colnames(k) = c("Value", 'Category')
    ggplot(k[-c(6),] , aes(x = Category, y = Value)) +
      geom_bar(stat = "identity" , fill = c("Skyblue","Coral","yellow","Green", "Pink")) +
      coord_polar(start = 0)  +
      labs(title = name)
  }
  
  radial_plot_team("TOT")
  
  output$SpiderMap2.o <- renderPlot({
    Team_name <- input$Teams  # Get the selected player from the input
    # Call your radial_plot function with the selected player
    radial_plot_team(Team_name)
  })
  
  scetterploter <- function(x_axis, y_axis, color, size) {
    ggplot(data = df1, aes(x = !!sym(x_axis), y = !!sym(y_axis), color = !!sym(color), size = !!sym(size))) + geom_point()
  }
  
  output$scatter_plot <- renderPlot({
    scetterploter(input$x_variable, input$y_variable, input$color, input$size)
  })
  
  Team_database = Player_database %>% group_by(Tm) %>% summarise(Points = mean(PTS) , Rebounds = mean(TRB) , Turnovers = mean(TOV) , Assists = mean(AST) , Steals = mean(STL) , FG = sum(FG)/sum(FGA)  , Three_Point. = sum(`3P`)/sum(`3PA`) ) 
  best_team = function(category){
    m=Team_database[order(Team_database[[category]] , decreasing = TRUE),][1:10, ]
    ggplot(m , aes(x = Tm, y = !!sym(category))) + geom_bar(stat="identity" , fill = "skyblue") + xlab("Teams") + ylab("Points")
    
  }
  
  output$best_team_plot <- renderPlot({
    best_team(input$variable)
  })
  
  Team_database1 = Player_database %>% group_by(Tm) %>% summarise(Points = mean(PTS), Rebounds = mean(TRB), Turnovers = mean(TOV), Assists = mean(AST), Steals = mean(STL))
  h = as.data.frame(scale(Team_database1[,-c(1)]))
  h$Tm = Team_database1$Tm
  
  radial_plot_teams = function(name1, name2) {
    k1 = as.data.frame(t(h[h$Tm == name1,]))
    k1 = as.data.frame(k1)
    k1$Category = row.names(k1)
    colnames(k1) = c("Value", 'Category')
    k1$Team = name1
    
    k2 = as.data.frame(t(h[h$Tm == name2,]))
    k2 = as.data.frame(k2)
    k2$Category = row.names(k2)
    colnames(k2) = c("Value", 'Category')
    k2$Team = name2
    
    combined_data = rbind(k1, k2)
    combined_data = combined[-c(6,12),]
    
    plot <- ggplot(combined_data, aes(x = Category, y = Value, fill = Team)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_polar(start = 0) +
      labs(title = paste(name1, " vs ", name2))
    
    return(plot)
  }
  
  output$SpiderMap3.0 <- renderPlot({
    radial_plot <- radial_plot_teams(input$Team_1, input$Team_2)
    print(radial_plot)
  })
}

shinyApp(ui, server)
