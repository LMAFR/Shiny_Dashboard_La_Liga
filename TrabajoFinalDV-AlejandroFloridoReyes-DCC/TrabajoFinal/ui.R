
ui <- library(shinyWidgets) # Libreria con botones bonitos. Son los que hay que usar en la practica final.

dashboardPage(
    dashboardHeader(title = 'TeamResearch'),
    
    # Sidebar ----
    
    dashboardSidebar(
        sidebarMenu(
            menuItem('Classification', tabName = 'p1', icon = icon('crown')), # El tabName es el identificador de la pestania.
            menuItem('Choose and learn', tabName = 'p2', icon = icon('graduation-cap'), # Pagina para buscar los iconos https://fontawesome.com/
                     menuSubItem('Choose your team',tabName = 'p21', icon = icon('star')),
                     menuSubItem('Fast comparison', tabName = 'p22', icon = icon('bolt'))),
            menuItem('About us', tabName = 'p3', icon = icon('info-circle')) 
        )
    ),
    
    # Body ----
    
    dashboardBody(
        
        tabItems(
            tabItem(
                tabName = 'p1', # Classification ---- 
                fluidRow(
                    
                    tags$img(
                        src = "https://st.depositphotos.com/1010338/1333/i/950/depositphotos_13334710-stock-photo-football-soccer-goals-and-ball.jpg",
                        style = 'position: absolute',
                        width="1750",
                        height="1000"
                    ),
                    
                    column(
                        2,
                        imageOutput('simbolo0', height = 'auto'),
                        HTML('&emsp;'),
                        HTML('&emsp;'),
                        pickerInput(
                            inputId = 'season',
                            label = 'Choose the season:',
                            choices = temporadas, 
                            options = list(style = "btn-primary")
                        ),
                        box(title = span("Legend", 
                                         style = "color: #000000; text-align: center"),
                            style="background-color:powderblue",
                            status = 'info',
                            solidHeader = T,
                            width = 12,
                            helpText("Champion",
                                     style = "background-color:#2297E6; color: #000000; text-align: center"),
                            helpText("Access to Champion League",
                                     style = "background-color:#61D04F; color: #000000; text-align: center"),
                            helpText("Access to Europe League",
                                     style = "background-color:#F5C710; color: #000000; text-align: center"),
                            helpText("Drop to 2nd division",
                                     style = "background-color:#DF536B; color: #000000; text-align: center")
                        ),
                        div(downloadButton('download',"Download the data"),
                            style="text-align:center")
                    ),
                    column(10,
                           box(title = 'Classification board',
                               status = 'info',
                               solidHeader = T,
                               width = 12,
                               
                               DT::DTOutput("table"),
                               HTML('<footer  style="width:100%; margin-left: 0px;"  >
                             
                             <div class="copyright" style="background-color:  #FFFFFF;">
                             <div class="container-fluid" style=" color: #000000; text-align:center">
                            This project has been carried out for educational purposes and not for economical profit. All the images used belong to their respective entities.
                             </div>
                             </div>')
                           )),
                ),
            ),
            
                tabItem(
                    tabName = 'p21', # Choose your team ----
                    fluidRow(
                        
                        tags$img(
                            src = "https://st.depositphotos.com/1010338/1333/i/950/depositphotos_13334710-stock-photo-football-soccer-goals-and-ball.jpg",
                            style = 'position: absolute',
                            width="1750",
                            height="850"
                        ),
                        
                        column(
                            2,
                            imageOutput('Simbolo1', height = 'auto'),
                            HTML('&emsp;'),
                            HTML('&emsp;'),
                            uiOutput('uip21')
                        ),
                        column(10,
                               
                               box(title = 'A chart about your team',
                                   status = 'info',
                                   solidHeader = T,
                                   style="background-color:powderblue",
                                   width = 12,
                                   
                                   radioGroupButtons(inputId = "charttype21", 
                                                        choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", 
                                                                    `<i class='fa fa-line-chart'></i>` = "line", 
                                                                    `<i class="fas fa-chart-area"></i>` = "area"), 
                                                        justified = TRUE,
                                                        selected = 'bar',
                                                        individual = TRUE),
                                   plotly::plotlyOutput('plotly1'),
                                   HTML('<footer  style="width:100%; margin-left: 0px;"  >
                             
                             
                             <div class="copyright" style="background-color: #0d47a1;">
                             <div class="container-fluid" style="background-color: #0d47a1; color: #bbdefb; text-align:center">
                             This project has been carried out for educational purposes and not for economical profit. All the images used belong to their respective entities.
                             </div>
                             </div>')
                               )),
                    ),
                    
                ),
            
            tabItem(
                tabName = 'p22', # Fast Comparison ----
                fluidRow(
                    
                    tags$img(
                        src = "https://st.depositphotos.com/1010338/1333/i/950/depositphotos_13334710-stock-photo-football-soccer-goals-and-ball.jpg",
                        style = 'position: absolute',
                        width="1750",
                        height="850"
                    ),
                    
                    column(
                        2,
                        imageOutput('simbolo2', height = 'auto'),
                        HTML('&emsp;'),
                        HTML('&emsp;'),
                        uiOutput('uip22'),
                        materialSwitch(inputId = "onoff", 
                                       label = HTML('<p><strong><h4>Show advanced statistics</h4></strong></p>'),
                                       value = F, 
                                       status = "info"
                        )
                    ),
                    column(10,
                           box(title = 'A chart to compare your team against other',
                               status = 'info',
                               style="background-color:powderblue",
                               solidHeader = T,
                               width = 12,
                               
                               radioGroupButtons(inputId = "charttype22", 
                                                    choices = c(`<i class='fa fa-bar-chart'></i>` = "bar2", 
                                                                `<i class='fa fa-line-chart'></i>` = "line2", 
                                                                `<i class="fas fa-chart-area"></i>` = "area2"), 
                                                    justified = TRUE,
                                                    selected = 'bar2'),
                               plotly::plotlyOutput('plotly2'),
                               HTML('<footer  style="width:100%; margin-left: 0px;"  >
                             
                             
                             <div class="copyright" style="background-color: #0d47a1;">
                             <div class="container-fluid" style="background-color: #0d47a1; color: #bbdefb; text-align:center">
                             This project has been carried out for educational purposes and not for economical profit. All the images used belong to their respective entities.
                             </div>
                             </div>')
                            )
                           ),
                    ),
                ),
            
            # About us ----
            
            tabItem(
                tabName = 'p3', 
                 includeHTML('file://www/html/NuestraInformacion.html')
                    
                )
            )
        )
    )
