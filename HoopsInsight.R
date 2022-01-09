library(shiny)
library(shinyjs)
library(magrittr)
library(dplyr)
library(DT)
library(data.table)


subdir <- "PBP"
subdir2 <- "Rosters"
PBPfiles <- list.files(path = subdir, pattern = ".csv", full.names = T)
Rosterfiles <- list.files(path = subdir2, pattern = ".csv", full.names = T)
PBPlist = lapply(PBPfiles, function(x)read.csv(x, header= T, as.is = 1, stringsAsFactors = FALSE))
Rosterlist = lapply(Rosterfiles, function(x)read.csv(x, header= T, as.is = 1, stringsAsFactors = FALSE))
PBP <- rbindlist(PBPlist)
Roster <- rbindlist(Rosterlist)
colnames(Roster) <- c("Team","Player","Character")
PBP$Date <- as.Date(PBP$Date, format= "%m/%d/%Y")
Teams <- subset(PBP, select=c("Team.Index", "Season"))
Teams<- Teams[order(Teams[,1])]
Teams <-distinct(Teams)
Teams <- Teams[order(Teams$Season, decreasing = TRUE),]    
url <- a("Stats Glossary", href="https://hoopsinsight.com/stats-glossary")
videourl <-a("Video Tutorial", href="https://youtu.be/XNEUWsvhG0k")

ui <- fluidPage(
  useShinyjs(),
    titlePanel("Hoops Insight Men's College Bball Stats Engine"),

     # Create sidebar
     
        tabPanel("tab",
                 div(id = "Sidebar",
                     sidebarPanel(
           # First option is a dropdown box populated by the teams in the PBP files
          selectInput(inputId = 'team',
                            label = 'Select team/season:',
                            choices = c(" ",Teams$Team.Index)
                        ),
          # Next are a list of games and the roster to select in/out players, which all start blank (will be updated later)
 #         actionButton("selectallgames","Select All Games"),
  #        actionButton("unselectallgames","Unselect All Games"),
          checkboxGroupInput(inputId  = "game",
                   label = "Select the games to include:",
                   choices  = NULL,
                   inline   = TRUE,
                   selected = NULL,
                   width = 450),
          checkboxGroupInput(inputId = 'Included',
                   label = "Select players that must be IN the game:",
                   choices = NULL,
                   inline = TRUE,
                   selected = NULL,
                   width = 400),
          checkboxGroupInput(inputId = 'Excluded',
                   label = "Select players that must be OUT of the game:",
                   choices = NULL,
                   selected = NULL,
                   inline = TRUE,
                   width = 400),
      # Default to garbage time not selected
     checkboxInput(inputId = 'Quality',
              label = "Include garbage time?",
              value = FALSE,
              width = 400),
     # Sliders to specify time in game and margin, which default to generic values
    sliderInput(inputId = 'TimeRange',
            label = 'Select range of game time (in minutes) to include:',
            min = 0,
            max = 40, #max(PBP$Time)/60,
            value=c(0,40),#max(PBP$Time)/60),
            step = 1,
            round = 1),
    sliderInput(inputId = 'Margin',
            label = 'Select range of game margin to include:',
            min = -50,
            max = 50,
            value=c(-50,50)
        ),
    sliderInput(inputId = 'OppStarters',
                label = 'Select number of opposing starters in game:',
                min = 0,
                max = 5,
                value=c(0,5)
    ),
     width=5)),  # Experimented with width to find value that works for browser
# Define main panel tabs
 mainPanel(
   id = "Main",
   actionButton("showSidebar", "Show sidebar"),
   actionButton("hideSidebar", "Hide sidebar"),
   tabsetPanel(type = "tabs",
              tabPanel("Team Stats", dataTableOutput("totals"), dataTableOutput("games")),
              tabPanel("Lineup Stats", dataTableOutput("lineups")),
              tabPanel("Player Stats", selectInput(inputId = 'player',
                                                   label = 'Select player:',
                                                   choices = NULL
                                                  ),
                       dataTableOutput("PlayerAdvanced"),
                       dataTableOutput("PlayerPer40"),
                       checkboxGroupInput(inputId = 'PlayerShotPossType',
                                          label = "Include these possession types:",
                                          choiceNames = c("Transition (1-10 sec)","Halfcourt (11-20 sec)", "Late Clock (21+ sec)", "After Off. Rebound"),
                                          choiceValues = c("<10 sec","11-20 sec","20+ sec", "After Oreb"),
                                          selected = unique(PBP$Poss.Type),
                                          inline = TRUE),
                       dataTableOutput("PlayerShooting")),
              tabPanel("Shooting", 
                       checkboxGroupInput(inputId = 'ShotPossType',
                                          label = "Include these possession types:",
                                          choiceNames = c("Transition (1-10 sec)","Halfcourt (11-20 sec)", "Late Clock (21+ sec)", "After Off. Rebound"),
                                          choiceValues = c("<10 sec","11-20 sec","20+ sec", "After Oreb"),
                                          selected = unique(PBP$Poss.Type),
                                          inline = TRUE),
                       dataTableOutput("Shooting"), dataTableOutput("OppShooting")),
              tabPanel("Misc.", 
                        dataTableOutput("PossStart"), dataTableOutput("OppPossStart")),
              tabPanel("Download Data",
                       textOutput("downloadtext"),
                       downloadButton("DownloadPBP"),
                       dataTableOutput("preview")),
              tabPanel("About", verbatimTextOutput("aboutme"), tagList("Glossary of stats I reference: ", url), tags$br(), tagList("Video tutorial of the Stats Engine:", videourl))

            )
 
    ,width = 7) # End of main panel
  )# End of sidebarlayout
)

  

server <- function(input, output, session) {

  # SelectionPBP <- PBP
    SelectionPBP <- reactive({
        filter(PBP, PBP$Team.Index == input$team)
    })
    SelectionRoster <- reactive({
        filter(Roster, Roster$Team == input$team)
    })
    observeEvent(input$showSidebar, {
      removeCssClass("Main", "col-sm-12")
      addCssClass("Main", "col-sm-7")
      shinyjs::show(id = "Sidebar")
    })
    observeEvent(input$hideSidebar, {
      removeCssClass("Main", "col-sm-7")
      addCssClass("Main", "col-sm-12")
      shinyjs::hide(id = "Sidebar")
    })
    
     observeEvent(SelectionPBP(), {
         updateCheckboxGroupInput(session, "game", choices = unique(SelectionPBP()$Game.Label), inline = TRUE, selected = unique(SelectionPBP()$Game.Label))
        updateSliderInput(session, 'TimeRange', max = round(max(SelectionPBP()$Time)/60,0), value=c(0,round(max(SelectionPBP()$Time)/60),0), step = 1)
        updateSliderInput(session, 'Margin', max = max(SelectionPBP()$Plus.Minus), min = min(SelectionPBP()$Plus.Minus), value=c(min(SelectionPBP()$Plus.Minus),max(SelectionPBP()$Plus.Minus)), step = 1)
     })
     # observeEvent(input$selectallgames, {
     #   updateCheckboxGroupInput(session, "game", selected = unique(SelectionPBP()$Game.Label))
     # })
     # observeEvent(input$unselectallgames, {
     #   updateCheckboxGroupInput(session, "game", selected = NULL)
     # })
     observeEvent(SelectionRoster(), {
        updateCheckboxGroupInput(session, 'Included', choices = unique(SelectionRoster()$Player), inline = TRUE, selected = NULL)
        updateCheckboxGroupInput(session, 'Excluded', choices = unique(SelectionRoster()$Player), inline = TRUE, selected = NULL)
        updateSelectInput(session,'player', choices = unique(SelectionRoster()$Player))
    })

  OutputPBP <- reactive ({
       GameQuality <- c("Regular")
        if(input$Quality == TRUE) {
            GameQuality <-c("Regular","Garbage Time")
        }

        TempPBP<- SelectionPBP() %>%
            filter_at(vars(P1,P2,P3,P4,P5), all_vars(!(. %in% input$Excluded))) %>%
            filter_at(vars(Game.Label), any_vars(. %in% input$game))  %>%
            filter_at(vars(Game.Quality), any_vars(. %in% GameQuality))  %>%
            filter_at(vars(Time), any_vars(. >= input$TimeRange[1] *60 & . <= input$TimeRange[2] *60)) %>%
            filter_at(vars(Plus.Minus), any_vars(. >= input$Margin[1] & . <= input$Margin[2])) %>%
            filter_at(vars(Opp.Starters), any_vars(. >= input$OppStarters[1] & . <= input$OppStarters[2]))

        if(!is.null(input$Included)) {
            for (i in 1:length(input$Included)){
                IncludedPBP <- reactive({
                    TempPBP %>% filter_at(vars(P1,P2,P3,P4,P5), any_vars(. %in% input$Included[i]))
                })
                TempPBP <- IncludedPBP()
            }

        }


        TempPBP
    })
    DownloadPBP <- reactive({
      out<- OutputPBP() %>% select(3:53)
      if(!is.data.frame(out)) {
        validate(paste0("No team selected, please select a team"))
      }
      out
    })
  
  
    SelectGameStats <- reactive({
    OutputPBP() %>% group_by(Game = Game.Label, Date = Date) %>% arrange(Date) %>%
            summarise("Possessions" = sum(Poss),"+/-" = sum(Pts.For)-sum(Pts.Allowed), "Adj margin per 100 poss" = 100*sum(Adj.Per.Poss)/sum(Poss), "Pts/100 poss" = 100*sum(Pts.For)/Possessions, "Opp pts/100 poss" = 100*sum(Pts.Allowed)/Possessions, "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/(sum(TwoPA)+sum(ThreePA)),
                      "2pt FG%" = sum(TwoPM)/sum(TwoPA), "3pt FG%" = sum(ThreePM)/sum(ThreePA), "TO%" = sum(TOs)/Possessions, "Assist%" = sum(Assists)/(sum(TwoPM)+sum(ThreePM)),"DReb%" = sum(DReb)/(sum(DReb)+sum(Opp.OReb)), "OReb%" = sum(OReb)/(sum(OReb)+sum(Opp.DReb)), "FTA per FGA" = sum(FTA)/(sum(TwoPA)+sum(ThreePA)), "3PA per FGA" = sum(ThreePA)/(sum(TwoPA)+sum(ThreePA)), "Opp eFG%" = (sum(Opp.2PM)+1.5*sum(Opp.3PM))/(sum(Opp.2PA)+sum(Opp.3PA)),"Opp 2pt FG%" = sum(Opp.2PM)/sum(Opp.2PA), "Opp 3pt FG%" = sum(Opp.3PM)/sum(Opp.3PA), "Opp TO%" = sum(Opp.TOs)/Possessions, "Opp FTA/FGA" = sum(Opp.FTA)/(sum(Opp.2PA)+sum(Opp.3PA)), "Opp 3PA/FGA" = sum(Opp.3PA)/(sum(Opp.2PA)+sum(Opp.3PA)))
    })
    Totals <- reactive({
        SelectionPBP()  %>%
            summarise("Set" = "Season Totals", "Possessions" = sum(Poss),"+/-" = sum(Pts.For)-sum(Pts.Allowed),"Adj margin per 100 poss" = 100*sum(Adj.Per.Poss)/sum(Poss),"Pts/100 poss" = 100*sum(Pts.For)/Possessions, "Opp pts/100 poss" = 100*sum(Pts.Allowed)/Possessions, "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/(sum(TwoPA)+sum(ThreePA)),
                      "2pt FG%" = sum(TwoPM)/sum(TwoPA), "3pt FG%" = sum(ThreePM)/sum(ThreePA), "TO%" = sum(TOs)/Possessions, "Assist%" = sum(Assists)/(sum(TwoPM)+sum(ThreePM)),"DReb%" = sum(DReb)/(sum(DReb)+sum(Opp.OReb)),"OReb%" = sum(OReb)/(sum(OReb)+sum(Opp.DReb)), "FTA per FGA" = sum(FTA)/(sum(TwoPA)+sum(ThreePA)), "3PA per FGA" = sum(ThreePA)/(sum(TwoPA)+sum(ThreePA)), "Opp eFG%" = (sum(Opp.2PM)+1.5*sum(Opp.3PM))/(sum(Opp.2PA)+sum(Opp.3PA)),"Opp 2pt FG%" = sum(Opp.2PM)/sum(Opp.2PA), "Opp 3pt FG%" = sum(Opp.3PM)/sum(Opp.3PA), "Opp TO%" = sum(Opp.TOs)/Possessions, "Opp FTA/FGA" = sum(Opp.FTA)/(sum(Opp.2PA)+sum(Opp.3PA)), "Opp 3PA/FGA" = sum(Opp.3PA)/(sum(Opp.2PA)+sum(Opp.3PA)))
    })
    SelectedTotals <- reactive({
        OutputPBP() %>%
            summarise("Set" = "Selection", "Possessions" = sum(Poss),"+/-" = sum(Pts.For)-sum(Pts.Allowed),"Adj margin per 100 poss" = 100*sum(Adj.Per.Poss)/sum(Poss),"Pts/100 poss" = 100*sum(Pts.For)/Possessions, "Opp pts/100 poss" = 100*sum(Pts.Allowed)/Possessions, "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/(sum(TwoPA)+sum(ThreePA)),
                      "2pt FG%" = sum(TwoPM)/sum(TwoPA), "3pt FG%" = sum(ThreePM)/sum(ThreePA), "TO%" = sum(TOs)/Possessions, "Assist%" = sum(Assists)/(sum(TwoPM)+sum(ThreePM)),"DReb%" = sum(DReb)/(sum(DReb)+sum(Opp.OReb)),"OReb%" = sum(OReb)/(sum(OReb)+sum(Opp.DReb)), "FTA per FGA" = sum(FTA)/(sum(TwoPA)+sum(ThreePA)), "3PA per FGA" = sum(ThreePA)/(sum(TwoPA)+sum(ThreePA)), "Opp eFG%" = (sum(Opp.2PM)+1.5*sum(Opp.3PM))/(sum(Opp.2PA)+sum(Opp.3PA)),"Opp 2pt FG%" = sum(Opp.2PM)/sum(Opp.2PA), "Opp 3pt FG%" = sum(Opp.3PM)/sum(Opp.3PA), "Opp TO%" = sum(Opp.TOs)/Possessions, "Opp FTA/FGA" = sum(Opp.FTA)/(sum(Opp.2PA)+sum(Opp.3PA)), "Opp 3PA/FGA" = sum(Opp.3PA)/(sum(Opp.2PA)+sum(Opp.3PA)))
    })
    aggregate <- reactive({rbind(Totals(),SelectedTotals())})

    output$totals <- DT::renderDataTable({
        DT::datatable(aggregate(), extensions = "FixedColumns", rownames = FALSE, caption = "Totals for Season/Selection", options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2), dom = 't')) %>% formatPercentage(c('eFG%','2pt FG%','3pt FG%','TO%','Assist%', 'DReb%','OReb%','Opp eFG%','Opp 2pt FG%', 'Opp 3pt FG%', 'Opp TO%')) %>% formatRound(c('FTA per FGA', '3PA per FGA', 'Opp FTA/FGA', 'Opp 3PA/FGA'), digits = 2) %>% formatRound(c('Adj margin per 100 poss','Pts/100 poss','Opp pts/100 poss'), digits =1)
    })
    output$games <- DT::renderDataTable({
      DT::datatable(SelectGameStats(), extensions = "FixedColumns", rownames = FALSE, caption = "Per Game Stats",options = list(order = list(1,'asc'), scrollX = TRUE, fixedColumns = list(leftColumns = 2))) %>% formatPercentage(c('eFG%','2pt FG%','3pt FG%','TO%','Assist%','DReb%','OReb%','Opp eFG%','Opp 2pt FG%', 'Opp 3pt FG%', 'Opp TO%')) %>% formatRound(c('FTA per FGA', '3PA per FGA', 'Opp FTA/FGA', 'Opp 3PA/FGA'), digits = 2) %>% formatRound(c('Adj margin per 100 poss','Pts/100 poss','Opp pts/100 poss'), digits =1)
    })


    LineupStats <- reactive({
      OutputPBP() %>% group_by(Lineup.Code) %>%
        summarise( "Possessions" = sum(Poss),"+/-" = sum(Pts.For)-sum(Pts.Allowed), "Adj margin per 100 poss" = 100*sum(Adj.Per.Poss)/sum(Poss), "Pts/100 poss" = 100*sum(Pts.For)/Possessions, "Opp pts/100 poss" = 100*sum(Pts.Allowed)/Possessions, "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/(sum(TwoPA)+sum(ThreePA)),
                   "2pt FG%" = sum(TwoPM)/sum(TwoPA), "3pt FG%" = sum(ThreePM)/sum(ThreePA), "TO%" = sum(TOs)/Possessions, "Assist%" = sum(Assists)/(sum(TwoPM)+sum(ThreePM)), "DReb%" = sum(DReb)/(sum(DReb)+sum(Opp.OReb)), "OReb%" = sum(OReb)/(sum(OReb)+sum(Opp.DReb)), "FTA per FGA" = sum(FTA)/(sum(TwoPA)+sum(ThreePA)), "3PA per FGA" = sum(ThreePA)/(sum(TwoPA)+sum(ThreePA)), "Opp eFG%" = (sum(Opp.2PM)+1.5*sum(Opp.3PM))/(sum(Opp.2PA)+sum(Opp.3PA)),"Opp 2pt FG%" = sum(Opp.2PM)/sum(Opp.2PA), "Opp 3pt FG%" = sum(Opp.3PM)/sum(Opp.3PA), "Opp TO%" = sum(Opp.TOs)/Possessions, "Opp FTA/FGA" = sum(Opp.FTA)/(sum(Opp.2PA)+sum(Opp.3PA)), "Opp 3PA/FGA" = sum(Opp.3PA)/(sum(Opp.2PA)+sum(Opp.3PA)))
    })


   LineupTable  <- reactive({
     TempLineups <- as.data.frame(unique(LineupStats()$Lineup.Code))
     colnames(TempLineups) <- c("Lineup.Code")
     TempLineups$Player.1 <- substr(TempLineups$Lineup.Code,1,1)
     TempLineups$Player.2 <- substr(TempLineups$Lineup.Code,2,2)
     TempLineups$Player.3 <- substr(TempLineups$Lineup.Code,3,3)
     TempLineups$Player.4 <- substr(TempLineups$Lineup.Code,4,4)
     TempLineups$Player.5 <- substr(TempLineups$Lineup.Code,5,5)
     holder <- TempLineups$Player.1
     holder[] <- lapply(TempLineups$Player.1,function(x) SelectionRoster()$Player[match(x, SelectionRoster()$Character)])
     TempLineups$Player.1 <- holder
     holder <- TempLineups$Player.2
     holder[] <- lapply(TempLineups$Player.2,function(x) SelectionRoster()$Player[match(x, SelectionRoster()$Character)])
     TempLineups$Player.2 <- holder
     holder <- TempLineups$Player.3
     holder[] <- lapply(TempLineups$Player.3,function(x) SelectionRoster()$Player[match(x, SelectionRoster()$Character)])
     TempLineups$Player.3 <- holder
     holder <- TempLineups$Player.4
     holder[] <- lapply(TempLineups$Player.4,function(x) SelectionRoster()$Player[match(x, SelectionRoster()$Character)])
     TempLineups$Player.4 <- holder
     holder <- TempLineups$Player.5
     holder[] <- lapply(TempLineups$Player.5,function(x) SelectionRoster()$Player[match(x, SelectionRoster()$Character)])
     TempLineups$Player.5 <- holder
     TempLineupData <- merge(TempLineups, LineupStats(), by = "Lineup.Code")
     TempLineupData <- TempLineupData %>% subset(select =-c(Lineup.Code))
     TempLineupData
     })

    
    output$lineups <- DT::renderDataTable({
      DT::datatable(LineupTable(), extensions ="FixedColumns", filter = 'top', rownames = FALSE, caption = "Stats by Lineup", options = list(order=list(5,'desc'),scrollX = TRUE, fixedColumns = list(leftColumns = 5))) %>% formatPercentage(c('eFG%','2pt FG%','3pt FG%','TO%','Assist%','DReb%','OReb%','Opp eFG%','Opp 2pt FG%', 'Opp 3pt FG%', 'Opp TO%')) %>% formatRound(c( 'FTA per FGA', '3PA per FGA', 'Opp FTA/FGA', 'Opp 3PA/FGA'), digits = 2) %>% formatRound(c('Adj margin per 100 poss','Pts/100 poss','Opp pts/100 poss'),digits =1)
    })

    PlayerStats <- reactive({
      OutputPBP() %>% filter(Player == input$player)
      })


    PlayerTable <- reactive({
      PlayerPBP<- OutputPBP() %>% filter_at(vars(P1,P2,P3,P4,P5), any_vars(. == input$player))
      TeamFGAs <- sum(PlayerPBP$TwoPA,PlayerPBP$ThreePA)
      TeamFTAs <- sum(PlayerPBP$FTA)
      TeamTOs <- sum(PlayerPBP$TOs)
      TeamFGM <- sum(PlayerPBP$TwoPM,PlayerPBP$ThreePM)
      Minutes <- sum(PlayerPBP$Poss.Time)/60
      TeamPoss <- sum(PlayerPBP$Poss)
      ORebOpps <- sum(PlayerPBP$OReb, PlayerPBP$Opp.DReb)
      DRebOpps <- sum(PlayerPBP$DReb, PlayerPBP$Opp.OReb)
      AssistPBP <- OutputPBP() %>% filter(Assister == input$player)
      PlayerAssists <- sum(AssistPBP$Assists)
      PlayerStats() %>% summarise("Usage" = (sum(TwoPA)+sum(ThreePA)+.44*sum(FTA)+sum(TOs))/sum(.44*TeamFTAs + TeamTOs+TeamFGAs),"2ptFG%" = sum(TwoPM)/sum(TwoPA), "3ptFG%" = sum(ThreePM)/sum(ThreePA), "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/(sum(TwoPA)+sum(ThreePA)), "% FGAs" = (sum(TwoPA)+sum(ThreePA))/TeamFGAs, "FT%" = sum(FTM)/sum(FTA), "3PA/FGA" = sum(ThreePA)/(sum(TwoPA)+sum(ThreePA)), "FTA/FGA" = sum(FTA)/(sum(TwoPA)+sum(ThreePA)), "Assist Rate" = PlayerAssists/(TeamFGM-sum(TwoPM)-sum(ThreePM)), "TO Rate" = sum(TOs)/TeamPoss, "OReb%" = sum(OReb)/ORebOpps, "DReb%" = sum(DReb)/DRebOpps )
    })

    PlayerRateStats <- reactive({
      PlayerPBP <- OutputPBP() %>% filter_at(vars(P1,P2,P3,P4,P5), any_vars(. == input$player))
      Minutes <- sum(PlayerPBP$Poss.Time)/60
      AssistPBP <- OutputPBP() %>% filter(Assister == input$player)
      PlayerAssists <- sum(AssistPBP$Assists)
      PlayerStats() %>% summarise("Pts/40" = sum(Pts.For)/Minutes*40, "Ast/40" = PlayerAssists/Minutes *40, "TO/40" = sum(TOs)/Minutes *40, "Reb/40" = (sum(OReb)+sum(DReb))/Minutes*40, "OReb/40" = sum(OReb)/Minutes *40, "DReb/40" = sum(DReb)/Minutes *40, "Foul/40" = sum(Def.PF)/Minutes *40)
    })

    PlayerShotTypeStats <- reactive({
      PlayerPBP <- OutputPBP() %>% filter(Player == input$player) %>% filter_at(vars(Poss.Type), any_vars(. %in% input$PlayerShotPossType))
      TotalFGA <- sum(PlayerPBP$TwoPA, PlayerPBP$ThreePA)
      PlayerPBP %>% filter(Shot.type != "", Shot.type != "n/a", Shot.type != "0") %>% group_by("Shot Type" = Shot.type) %>% summarise("FGA" = sum(TwoPA)+sum(ThreePA), "FGM" = sum(TwoPM)+ sum(ThreePM), "%FGAs" = FGA/TotalFGA, "FG%" = FGM/FGA, "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/FGA)
    })

    PlayerShotTypeTotals <- reactive({
      PlayerPBP <- OutputPBP() %>% filter(Player == input$player) %>% filter_at(vars(Poss.Type), any_vars(. %in% input$PlayerShotPossType))
      TotalFGA <- sum(PlayerPBP$TwoPA, PlayerPBP$ThreePA)
      PlayerPBP %>% filter(Shot.type != "", Shot.type != "n/a", Shot.type != "0")  %>% summarise("Shot Type" = "Total", "FGA" = sum(TwoPA)+sum(ThreePA), "FGM" = sum(TwoPM)+ sum(ThreePM), "%FGAs" = FGA/TotalFGA, "FG%" = FGM/FGA, "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/FGA)
    })

    aggregatePlayerShotStats <- reactive({rbind(PlayerShotTypeStats(),PlayerShotTypeTotals())})

    output$PlayerAdvanced <- DT::renderDataTable({
      DT::datatable(PlayerTable(), rownames = FALSE, caption = "Advanced Stats", options = list(scrollX = TRUE, dom = 't')) %>% formatPercentage(c("Usage","2ptFG%","3ptFG%","eFG%","% FGAs", "FT%", "Assist Rate", "TO Rate", "OReb%","DReb%")) %>% formatRound(c("FTA/FGA","3PA/FGA"), digits = 2)
    })

    output$PlayerPer40 <- DT::renderDataTable({
      DT::datatable(PlayerRateStats(), rownames = FALSE, caption = "Per 40 minute Stats", options = list(scrollX = TRUE, dom = 't')) %>% formatRound(c("Pts/40","Ast/40","TO/40","Reb/40","OReb/40","DReb/40","Foul/40"),digits = 1)
    })

    output$PlayerShooting <- DT::renderDataTable({
      DT::datatable(aggregatePlayerShotStats(), rownames = FALSE, caption = "Shooting by Location", options = list(scrollx = TRUE, dom = 't')) %>% formatPercentage(c("%FGAs","FG%","eFG%"))
    })
    
    
    ShotTypeStats <- reactive({
      ShotPBP <- OutputPBP() %>% filter_at(vars(Poss.Type), any_vars(. %in% input$ShotPossType))
        TotalFGA <- sum(ShotPBP$TwoPA, ShotPBP$ThreePA)
      ShotPBP %>% filter(Shot.type != "", Shot.type != "n/a") %>% group_by("Shot Type" = Shot.type) %>% summarise("FGA" = sum(TwoPA)+sum(ThreePA), "FGM" = sum(TwoPM)+ sum(ThreePM), "%FGAs" = FGA/TotalFGA, "FG%" = FGM/FGA, "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/FGA)
    })

    ShotTypeTotals <- reactive({
      ShotPBP <- OutputPBP() %>% filter_at(vars(Poss.Type), any_vars(. %in% input$ShotPossType))
      TotalFGA <- sum(ShotPBP$TwoPA, ShotPBP$ThreePA)
      ShotPBP %>% filter(Shot.type != "", Shot.type != "n/a", Shot.type != "0")  %>% summarise("Shot Type" = "Total", "FGA" = sum(TwoPA)+sum(ThreePA), "FGM" = sum(TwoPM)+ sum(ThreePM), "%FGAs" = FGA/TotalFGA, "FG%" = FGM/FGA, "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/FGA)
    })

    aggregateShotStats <- reactive({rbind(ShotTypeStats(),ShotTypeTotals())})

    OppShotTypeStats <- reactive({
      ShotPBP <- OutputPBP() %>% filter_at(vars(Poss.Type), any_vars(. %in% input$ShotPossType))
      OppTotalFGA <- sum(ShotPBP$Opp.2PA, ShotPBP$Opp.3PA)
      ShotPBP %>% filter(Shot.type != "0",Shot.type != "", Shot.type != "n/a") %>% group_by("Shot Type" = Shot.type) %>% summarise("OppFGA" = sum(Opp.2PA)+sum(Opp.3PA), "OppFGM" = sum(Opp.2PM)+ sum(Opp.3PM), "% Opp FGAs" = OppFGA/OppTotalFGA, "Opp FG%" = OppFGM/OppFGA, "Opp eFG%" = (sum(Opp.2PM)+1.5*sum(Opp.3PM))/OppFGA)
    })

    OppShotTypeTotals <- reactive({
      ShotPBP <- OutputPBP() %>% filter_at(vars(Poss.Type), any_vars(. %in% input$ShotPossType))
      OppTotalFGA <- sum(ShotPBP$Opp.2PA, ShotPBP$Opp.3PA)
      ShotPBP %>% filter(Shot.type != "0",Shot.type != "", Shot.type != "n/a") %>% summarise("Shot Type" = "Total", "OppFGA" = sum(Opp.2PA)+sum(Opp.3PA), "OppFGM" = sum(Opp.2PM)+ sum(Opp.3PM), "% Opp FGAs" = OppFGA/OppTotalFGA, "Opp FG%" = OppFGM/OppFGA, "Opp eFG%" = (sum(Opp.2PM)+1.5*sum(Opp.3PM))/OppFGA)
    })

    aggregateOppShotStats <- reactive({rbind(OppShotTypeStats(),OppShotTypeTotals())})

     output$Shooting <- DT::renderDataTable({
       DT::datatable(aggregateShotStats(), rownames = FALSE, caption = "Shooting by Location", options = list(scrollx = TRUE)) %>% formatPercentage(c("%FGAs","FG%","eFG%"))
    })
    output$OppShooting <- DT::renderDataTable({
      DT::datatable(aggregateOppShotStats(), rownames = FALSE, caption = "Opponent Shooting by Location", options = list(scrollx = TRUE)) %>% formatPercentage(c("% Opp FGAs","Opp FG%","Opp eFG%"))
    })

    PossStartStats <- reactive({
      PossPBP <- OutputPBP() 
      PossPBP %>% filter(Poss.Start.for.Hero != "") %>% group_by("Poss Start Event" = Poss.Start.for.Hero) %>% summarise("Poss" = n_distinct(UniqueGamePoss), "Pts per 100 poss" =  sum(Pts.For)/sum(Poss)*100, "2ptFG%" = sum(TwoPM)/sum(TwoPA), "3ptFG%" = sum(ThreePM)/sum(ThreePA), "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/(sum(TwoPA)+sum(ThreePA)), "TO%" = sum(TOs)/Poss, "3PA per FGA" = sum(ThreePA)/(sum(TwoPA)+sum(ThreePA)), "FTA per FGA" = sum(FTA)/(sum(TwoPA)+sum(ThreePA)))
    })

    PossStartTotals <- reactive({
      PossPBP <- OutputPBP() #%>% filter_at(vars(Poss.Type), any_vars(. %in% input$PossType))
      PossPBP %>% filter(Poss.Start.for.Hero != "")  %>% summarise("Poss Start Event" = "Total", "Poss" = n_distinct(UniqueGamePoss), "Pts per 100 poss" = sum(Pts.For)/sum(Poss)*100, "2ptFG%" = sum(TwoPM)/sum(TwoPA), "3ptFG%" = sum(ThreePM)/sum(ThreePA), "eFG%" = (sum(TwoPM)+1.5*sum(ThreePM))/(sum(TwoPA)+sum(ThreePA)), "TO%" = sum(TOs)/Poss, "3PA per FGA" = sum(ThreePA)/(sum(TwoPA)+sum(ThreePA)), "FTA per FGA" = sum(FTA)/(sum(TwoPA)+sum(ThreePA)))
    })

    aggregatePossStartStats <- reactive({rbind(PossStartStats(),PossStartTotals())})

    output$PossStart <- DT::renderDataTable({
      DT::datatable(aggregatePossStartStats(), rownames = FALSE, caption = "Offense by Possession Start Event", options = list(scrollx = TRUE)) %>% formatPercentage(c("2ptFG%","3ptFG%","eFG%","TO%")) %>% formatRound(c('FTA per FGA', '3PA per FGA'), digits = 2) %>% formatRound(c('Pts per 100 poss'), digits = 1)
    })

    OppPossStartStats <- reactive({
      OutputPBP() %>% filter(Poss.Start.for.Opp != "") %>% group_by("Poss Start Event" = Poss.Start.for.Opp) %>% summarise("Poss" = n_distinct(UniqueGamePoss), "Pts per 100 poss" =  sum(Pts.Allowed)/sum(Poss)*100, "2ptFG%" = sum(Opp.2PM)/sum(Opp.2PA), "3ptFG%" = sum(Opp.3PM)/sum(Opp.3PA), "eFG%" = (sum(Opp.2PM)+1.5*sum(Opp.3PM))/(sum(Opp.2PA)+sum(Opp.3PA)), "TO%" = sum(Opp.TOs)/Poss, "3PA per FGA" = sum(Opp.3PA)/(sum(Opp.2PA)+sum(Opp.3PA)), "FTA per FGA" = sum(Opp.FTA)/(sum(Opp.2PA)+sum(Opp.3PA)))
    })

    OppPossStartTotals <- reactive({
      OutputPBP() %>% filter(Poss.Start.for.Opp != "")  %>% summarise("Poss Start Event" = "Total", "Poss" = n_distinct(UniqueGamePoss), "Pts per 100 poss" = sum(Pts.Allowed)/sum(Poss)*100, "2ptFG%" = sum(Opp.2PM)/sum(Opp.2PA), "3ptFG%" = sum(Opp.3PM)/sum(Opp.3PA), "eFG%" = (sum(Opp.2PM)+1.5*sum(Opp.3PM))/(sum(Opp.2PA)+sum(Opp.3PA)), "TO%" = sum(Opp.TOs)/Poss, "3PA per FGA" = sum(Opp.3PA)/(sum(Opp.2PA)+sum(Opp.3PA)), "FTA per FGA" = sum(Opp.FTA)/(sum(Opp.2PA)+sum(Opp.3PA)))
    })

    aggregateOppPossStartStats <- reactive({rbind(OppPossStartStats(),OppPossStartTotals())})

    output$OppPossStart <- DT::renderDataTable({
      DT::datatable(aggregateOppPossStartStats(), rownames = FALSE, caption = "Opponent Offense by Possession Start Event", options = list(scrollx = TRUE)) %>% formatPercentage(c("2ptFG%","3ptFG%","eFG%","TO%")) %>% formatRound(c('FTA per FGA', '3PA per FGA'), digits = 2) %>% formatRound(c('Pts per 100 poss'), digits = 1)
    })

   
    output$aboutme <- renderText("This stats engine was created by Sean Vinsel of Hoops Insight.
    You can contact me at sean@hoopsinsight.com or on Twitter @hoopsinsight.
    It allows the user to filter stats from play-by-play for a variety of factors.
    I'll be keeping it updated during the season for Louisville and Kentucky,
    and adding more historical data as time allows.")
    output$downloadtext <- renderText("Click the button below to download formatted PBP data for the selected team")
    output$preview <- renderDataTable({
      DT::datatable(head(DownloadPBP()), rownames = FALSE, caption = "Preview of data for download", options = list(scrollx = TRUE)) 
    })
    output$DownloadPBP <- downloadHandler(
      filename = function() {
      paste0(input$team,"_PBP.csv")
    },
    content = function(file) {
      write.csv(DownloadPBP(), file)
    }
    )

}

shinyApp(ui = ui, server = server)
