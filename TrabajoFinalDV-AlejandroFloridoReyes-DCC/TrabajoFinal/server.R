
# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
  
  ### Classification ----
  
  ## Image ----

  output$simbolo0 <- renderImage({
    
    return(list(src = "img/laLigaEscudo.png",contentType = "image/png",alt = "laLigaEscudo", height = 120, width = 150, style="display: block; margin-left: auto; margin-right: auto;"))
    
  }, deleteFile = FALSE)
      
  ## Table ----

  output$table <- DT::renderDT({
    
    laLiga_season <- filter(laLiga, season == input$season)
    laLiga_season = arrange(laLiga_season, Position)
    laLiga_season <- laLiga_season[,c("team","Points","Played","Won","Draw", "Lost","Winrate","Drawrate","Loserate")]
    
    colores=c(laLiga_season$team[1],
              laLiga_season$team[2],
              laLiga_season$team[3],
              laLiga_season$team[4],
              laLiga_season$team[5],
              laLiga_season$team[6],
              laLiga_season$team[length(laLiga_season$team)],
              laLiga_season$team[length(laLiga_season$team)-1],
              laLiga_season$team[length(laLiga_season$team)-2])
    
    DT::datatable(laLiga_season,
                  options = list(pageLength = 25)
                  ) %>% formatStyle(
                    "team",
                    target = 'row',
                    backgroundColor = styleEqual(colores,
                                                 c('#2297E6','#61D04F','#61D04F','#61D04F','#F5C710','#F5C710', '#DF536B','#DF536B','#DF536B'))
                    ) # Nueva tabla (filtrada). Nombre a asignar.
    
  })
  
  ## Download Data
  
  reactive_file_name <- reactive({
    
    filename = paste0('season_', input$season, '.csv')
    
  })
  
  output$download <- downloadHandler(
    filename = function(){reactive_file_name()},
    content = function(fname){
      write.csv(laLiga_season, fname)
    }
  )
  
  ### Choose your team ----
  
  ## RenderUI & Image ----
  
  output$uip21 <- renderUI({
    
    tagList(
      
      pickerInput(
        inputId = 'myteam', 
        label = 'Choose your favourite team:',
        choices = sort(unique(laLiga$team)), 
        options = list(style = "btn-danger"),
        selected = input$myteam2
      ),
      
      imageOutput('escudo1', height = 'auto'),
      
      pickerInput(
        inputId = "param21",
        label = "What parameter do you want to study?",
        choices = colnames(laLiga_basic_params[3:11]), 
        options = list(style = "btn-info"),
        selected = 'Winrate'
      )
      
    )
    
  })
  
  output$Simbolo1 <- renderImage({
    
    return(list(src = "img/laLigaEscudo.png",contentType = "image/png",alt = "laLigaEscudo", height = 120, width = 150, style="display: block; margin-left: auto; margin-right: auto;"))
    
  }, deleteFile = FALSE)
  
  ## Functions to manage data (1) ----
  
  # We have to choose the column to show in the chart (and the season column, which will show the moment):
  
  laLiga_basic_param <- reactive({
    laLiga_myteam <- laLiga_basic_params %>% 
      dplyr::filter(team == input$myteam)
    
  })
  
  # We filter the previous datset in function of the statistic that you chose: 
  
  variable_y <- reactive({
    
    var_to_show <- input$param21
    
    if ( "Won" %in% var_to_show) return(laLiga_basic_param()$Won)
    if ( "Lost" %in% var_to_show) return(laLiga_basic_param()$Lost)
    if ( "Draw" %in% var_to_show) return(laLiga_basic_param()$Draw)
    if ( "Winrate" %in% var_to_show) return(laLiga_basic_param()$Winrate)
    if ( "Loserate" %in% var_to_show) return(laLiga_basic_param()$Loserate)
    if ( "Drawrate" %in% var_to_show) return(laLiga_basic_param()$Drawrate)
    if ( "Points" %in% var_to_show) return(laLiga_basic_param()$Points)
    if ( "Played" %in% var_to_show) return(laLiga_basic_param()$Played)
    if ( "Position" %in% var_to_show) return(laLiga_basic_param()$Position)
    
  })
  
  escudo_1 <- reactive({
    
    name = paste0('img/', input$myteam, '.png')
    
  })
    
    output$escudo1 <- renderImage({
      
      return(list(src = escudo_1(), 
                  contentType = "image/png",
                  alt = "escudo1",
                  height = 120,
                  width = 150,
                  style="display: block; margin-left: auto; margin-right: auto;"))
      
    }, deleteFile = FALSE)
  
    ## Plotly (1) ----
    
    output$plotly1 <- renderPlotly({
      
      # Now we can show exactly the data we wanted to. Note we only want the data related to our team:
      
      plot1 <- ggplot(laLiga_basic_param(), aes(col=team, fill=team)) 

      # Conditionals to link the options of charttype12 to the chart:

      # We want to draw a bar chart if we click on that icon:

      if (input$charttype21 == 'line') {
        plot1 <- plot1 +
          geom_line(aes(x = year, y = variable_y())) +
          geom_point(aes(x = year, y = variable_y())) +
          scale_x_continuous(n.breaks = 10)
      }

      # In the same way, we want to draw a line chart if we click in the appropriate icon:

      if (input$charttype21 == 'bar') {
        plot1 <- plot1 +
          geom_col(aes(x = season, y = variable_y()))
      }
      
      # Finally, we draw an area chart:
      
      if (input$charttype21 == 'area') {
        plot1 <- plot1 +
          geom_area(aes(x = year, y = variable_y())) +
          scale_x_continuous(n.breaks = 10)
      }

      # We got the geometry so we can add some settings now:
      
      plot1 <- plot1 +
        ylab(input$param21) +
        theme(
          # get rid of panel grids
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change plot and panel background
          plot.background=element_rect(fill = "gray"),
          panel.background = element_rect(fill = 'black'),
          # Change legend 
          legend.position = c(0.6, 0.07),
          legend.direction = "horizontal",
          legend.background = element_rect(fill = "black", color = NA),
          legend.key = element_rect(color = "gray", fill = "black"),
          legend.title = element_text(color = "gray"),
          legend.text = element_text(color = "white"),
          axis.text.x = element_text(angle = 45, color = 'black')
        )
      
      # We show the chart:
      
      ggplotly(plot1)
      
    })
    
    ### Fast Comparison ----

  
    ## RenderUI  & Image----
    
    output$uip22 <- renderUI({
      
      tagList(
        
        pickerInput(
          inputId = 'myteam2',
          label = 'Choose another team:',
          choices = sort(unique(laLiga$team)), 
          options = list(style = "btn-success"),
          selected = input$myteam
        ),
        
        imageOutput('escudo2', height = 'auto'),
        
        pickerInput(
          inputId = 'ateam',
          label = 'Choose another team:',
          choices = sort(unique(laLiga$team)), 
          options = list(style = "btn-success"),
          selected = 'Sevilla'
        ),
        
        imageOutput('escudo3', height = 'auto'),
  
        pickerInput(
          inputId = "param22",
          label = "Basic Statistics",
          choices = colnames(laLiga_basic_params[3:11]),
          options = list(style = "btn-primary"),
          selected = 'Winrate'
        )
        
      )

    })
  
  # The next observeEvent was wrote to fix a problem that happened when you click on 'p22', then in 'p21'  and then in 'p22' again, 
  # causing that the chart of p22 were not drawn depending on what you had done in those tabs.
  
  observeEvent(input$onoff, {
    
    if(input$onoff == T){
      updatePickerInput(session, 'myteam2', choices = sort(unique(laLiga2$team)), label = "Choose another team:", selected = 'Betis')
      updatePickerInput(session, 'ateam', choices = sort(unique(laLiga2$team)), label = "Choose another team:", selected = 'Sevilla')
    }
    else{
      updatePickerInput(session, 'myteam2', choices = sort(unique(laLiga$team)), label = "Choose another team:", selected = input$myteam)
      updatePickerInput(session, 'ateam', choices = sort(unique(laLiga$team)), label = "Choose another team:", selected = 'Sevilla')
    }
    
  })
  
  # We want to show certain statistics (basic or advanced) depending on the state of the MaterialSwitch button:
  
  observeEvent(input$onoff, {
    
    if(input$onoff == T){
      updatePickerInput(session, 'param22', choices = colnames(laLiga_advanced_params[3:14]), label = "Advanced Statistics")
      }
    else{
      updatePickerInput(session, 'param22', choices = colnames(laLiga_basic_params[3:11]), label = "Basic Statistics")
      }
    
  })
  
  # The next observeEvent was wrote to fix a problem that happened when you click on 'p21' after put 'onoff'==T: 
  
  observeEvent(input$myteam, {

      updateMaterialSwitch(session, 'onoff', value = F)

  })
  
  output$simbolo2 <- renderImage({

    return(list(src = "img/laLigaEscudo.png",contentType = "image/png",alt = "laLigaEscudo", height = 120, width = 150, style="display: block; margin-left: auto; margin-right: auto;"))

  }, deleteFile = FALSE)
  
  escudo_2 <- reactive({
    
    name = paste0('img/', input$myteam2, '.png')
    
  })
  
  output$escudo2 <- renderImage({
    
    return(list(src = escudo_2(), 
                contentType = "image/png",
                alt = "escudo2",
                height = 120,
                width = 150,
                style="display: block; margin-left: auto; margin-right: auto;"))
    
  }, deleteFile = FALSE)
  
  escudo_3 <- reactive({
    
    name = paste0('img/', input$ateam, '.png')
    
  })
  
  output$escudo3 <- renderImage({
    
    return(list(src = escudo_3(), 
                contentType = "image/png",
                alt = "escudo3",
                height = 120,
                width = 150,
                style="display: block; margin-left: auto; margin-right: auto;"))
    
  }, deleteFile = FALSE)
  
  

  ## Functions to manage data (2) ----
  
  # We have to choose the column to show in the chart (and the season column, which will show the moment):
  
  laLiga_basic_param_rep <- reactive({
    laLiga_myteam <- laLiga_basic_params %>% 
      dplyr::filter(team == input$myteam2)
    
  })
  
  laLiga_basic_param_rep_A <- reactive({
    laLiga_myteam <- laLiga_advanced_params %>% 
      dplyr::filter(team == input$myteam2)
    
  })
  
  laLiga_basic_param2 <- reactive({
    
    laLiga_another_team <- laLiga_basic_params %>%
      dplyr::filter(team == input$ateam)
    
  })
  
  laLiga_basic_param2_A <- reactive({
    
    laLiga_another_team <- laLiga_advanced_params %>%
      dplyr::filter(team == input$ateam)
    
  })
  
  laLiga_basic_param3 <- reactive({
    
    comp_dataset <- rbind(laLiga_basic_param_rep(), laLiga_basic_param2())
    
  })
  
  laLiga_basic_param3_A <- reactive({
    
    comp_dataset <- rbind(laLiga_basic_param_rep_A(), laLiga_basic_param2_A())
    
  })
  
  variable_y_basic <- reactive({
    
    var_to_show2 <- input$param22
    
    if ( "Won" %in% var_to_show2) return(laLiga_basic_param3()$Won)
    if ( "Lost" %in% var_to_show2) return(laLiga_basic_param3()$Lost)
    if ( "Draw" %in% var_to_show2) return(laLiga_basic_param3()$Draw)
    if ( "Winrate" %in% var_to_show2) return(laLiga_basic_param3()$Winrate)
    if ( "Loserate" %in% var_to_show2) return(laLiga_basic_param3()$Loserate)
    if ( "Drawrate" %in% var_to_show2) return(laLiga_basic_param3()$Drawrate)
    if ( "Points" %in% var_to_show2) return(laLiga_basic_param3()$Points)
    if ( "Played" %in% var_to_show2) return(laLiga_basic_param3()$Played)
    if ( "Position" %in% var_to_show2) return(laLiga_basic_param3()$Position)
    
  })
  
  
  variable_y_adv <- reactive({
    
    var_to_show3 <- input$param22
    
    if ( var_to_show3 == "home_win" ) return(laLiga_basic_param3_A()$home_win) # One way to write it.
    if ( "home_loss" %in% var_to_show3 ) return(laLiga_basic_param3_A()$home_loss) # Another way to write it.
    if ( "away_win" %in% var_to_show3) return(laLiga_basic_param3_A()$away_win)
    if ( "away_loss" %in% var_to_show3) return(laLiga_basic_param3_A()$away_loss)
    if ( "matches_won" %in% var_to_show3) return(laLiga_basic_param3_A()$matches_won)
    if ( "matches_lost" %in% var_to_show3) return(laLiga_basic_param3_A()$matches_lost)
    if ( "matches_drawn" %in% var_to_show3) return(laLiga_basic_param3_A()$matches_drawn)
    if ( "home_goals" %in% var_to_show3) return(laLiga_basic_param3_A()$home_goals)
    if ( "away_goals" %in% var_to_show3) return(laLiga_basic_param3_A()$away_goals)
    if ( "goals_scored" %in% var_to_show3) return(laLiga_basic_param3_A()$goals_scored)
    if ( "goals_conceded" %in% var_to_show3) return(laLiga_basic_param3_A()$goals_conceded)
    if ( "goal_difference" %in% var_to_show3) return(laLiga_basic_param3_A()$goal_difference)
    
  })
  
    ## Plotly (2) ----

  output$plotly2 <- renderPlotly({
    
    # This process is similar to that followed in the section "Plotly (1)".
    
    # Now we can show exactly the data we wanted to. Note we only want the data related to our teams:
    
    if (input$onoff == F){
      
      plot2 <- ggplot(laLiga_basic_param3(), aes(group=team, col=team, fill=team))
      
      # Conditionals to link the options of charttype12 to the chart:
      
      # We want to draw a bar chart if we click on that icon:
      
      if (input$charttype22 == 'line2') {
        plot2 <- plot2 +
          geom_line(aes(x = year, y = variable_y_basic())) +
          geom_point(aes(x = year, y = variable_y_basic())) + 
          scale_x_continuous(n.breaks = 10)
      }
      
      # In the same way, we want to draw a line chart if we click in the appropriate icon:
      
      if (input$charttype22 == 'bar2') {
        plot2 <- plot2 +
          geom_col(aes(x = season, y = variable_y_basic()), position = 'dodge2')
      }
      
      # Finally, we draw an area chart:
      
      if (input$charttype22 == 'area2') {
        plot2 <- plot2 +
          geom_area(aes(x = year, y = variable_y_basic())) +
          scale_x_continuous(n.breaks = 10)
      }
      
      plot2 <- plot2 + ylab(input$param22)
      
    }
    
    
    # We got the geometry so we can show the chart:
    
    else{
      
      plot2 <- ggplot(laLiga_basic_param3_A(), aes(group=team, col=team, fill=team))
      
      if (input$charttype22 == 'line2') {
        plot2 <- plot2 +
          geom_line(aes(x = year, y = variable_y_adv())) +
          geom_point(aes(x = year, y = variable_y_adv())) + 
          scale_x_continuous(n.breaks = 10)
      }
      
      # In the same way, we want to draw a line chart if we click in the appropriate icon:
      
      if (input$charttype22 == 'bar2') {
        plot2 <- plot2 +
          geom_col(aes(x = season, y = variable_y_adv()), position = 'dodge2')
      }
      
      # Finally, we draw an area chart:
      
      if (input$charttype22 == 'area2') {
        plot2 <- plot2 +
          geom_area(aes(x = year, y = variable_y_adv())) +
          scale_x_continuous(n.breaks = 10)
      }
      
      plot2 <- plot2 + ylab(input$param22)
      
    }
    
    plot2 <- plot2 +
      theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change plot and panel background
        plot.background=element_rect(fill = "gray"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        legend.position = c(0.6, 0.07),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "gray"),
        legend.text = element_text(color = "white"),
        axis.text.x = element_text(angle = 45, color = 'black')
      )
    
    ggplotly(plot2)
    
  })
  
  
  
})
