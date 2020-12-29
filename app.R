

library(tidyverse)
library(dplyr)
library(reshape2)
library(shiny)
library(DT)


vg <- read.csv('/Users/riverhua/Desktop/IE6600 FP/vgsales.csv')
is.null(vg)
head(vg)


#In our project, we just analyze the top 1000 games
top1000 <- head(vg,1000)

unique(vg$Genre)


#function that find the corresponding dataset.
find_data <- function(gen, plt){
  df <- top1000 %>% filter(Genre == gen & Platform == plt)
  return (df)
}





sht_ps3 <- top1000 %>% filter(Genre == 'Shooter' | Platform == 'PS3') %>%
  select(Name, Publisher, NA_Sales,EU_Sales,JP_Sales,Other_Sales)

s3 <- sht_ps3 %>% gather('Area', 'Sales', -c(Name, Publisher))

ggplot(data = s3)+
  geom_bar(aes(x = Name, y = Sales, fill = Area), stat = 'identity')+
  theme(axis.text.x = element_text(angle = 60,hjust = 1))


##Plots of 10 best games in NA, Japan, Euro, and rest of the world
n <- top1000[order(top1000$NA_Sales, decreasing = TRUE),]
na <- head(n, 10)

bestna <-ggplot(na, aes(x=Name, y=NA_Sales, fill=Genre)) + geom_bar(stat='identity') + 
         coord_flip() + geom_text(aes(label = NA_Sales), hjust= 1.2,  color='black')+ 
         labs(title = 'Top 10 Best Selling games in North America', x = 'Game Name', y='Sales in North America (in millions)' )

j <- top1000[order(top1000$JP_Sales, decreasing = TRUE),]
japan <- head(j, 10)


bestjapan <- ggplot(japan, aes(x=Name, y=JP_Sales, fill=Genre)) + geom_bar(stat='identity') + 
                    coord_flip() + geom_text(aes(label = JP_Sales), hjust= 1.2,  color='black')+ 
                    labs(title = 'Top 10 Best Selling games in Japan', x = 'Game Name', y='Sales in Japan (in millions)' )

e <- top1000[order(top1000$EU_Sales, decreasing = TRUE),]
euro <- head(e, 10)

besteuro <- ggplot(euro, aes(x=Name, y=EU_Sales, fill=Genre)) + geom_bar(stat='identity') + 
  coord_flip() + geom_text(aes(label = EU_Sales), hjust= 1.2,  color='black')+ 
  labs(title = 'Top 10 Best Selling games in Europe', x = 'Game Name', y='Sales in Europe (in millions)' )

  
r <- top1000[order(top1000$Other_Sales, decreasing = TRUE),]
rest <- head(r, 10)

bestrest<- ggplot(rest, aes(x=Name, y=Other_Sales, fill=Genre)) + geom_bar(stat='identity') + 
  coord_flip() + geom_text(aes(label = Other_Sales), hjust= 0.8,  color='black')+ 
  labs(title = 'Top 10 Best Selling games in Rest of the World', x = 'Game Name', y='Sales in Rest of the World (in millions)' )






#function that select game type and find the popular publisher
genre_publisher <- function(gen, plt){
  game1 <- top1000 %>% filter(Genre == gen & Platform == plt)
  game_new <- game1 %>% group_by(Publisher)%>%
  summarise(Global_Sales = sum(Global_Sales))
  
  ggplot(data = game_new)+ 
       geom_bar(aes(x=Publisher, y=Global_Sales), stat = 'identity')+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  	coord_flip()
}



genre_publisher('Shooter','PS3')




#function that choose genre and platform then check the sales in different areas
type_check <- function(gen, plt){
  game1 <- top1000 %>% filter(Genre == gen & Platform == plt) %>%
  select(Name, Publisher, NA_Sales,EU_Sales,JP_Sales,Other_Sales)
  
  game2 <- game1 %>% gather('Area', 'Sales', -c(Name, Publisher))
  
  ggplot(data = game2)+
  geom_bar(aes(x = Name, y = Sales, fill = Area), stat = 'identity')+
  theme(axis.text.x = element_text(angle = 60,hjust = 1))
}



type_check('Misc','Wii')


ui2 <- shinyUI(navbarPage(title='Visualization of Video Games Sales', inverse=TRUE,selected='About Topic',
                          
####
tabPanel('About Topic', 
h3('Exploring Sales of video games'),
br(),
img(src='topic.png',style="display: block; margin-left: auto; margin-right: auto;"),
br(),                          
h4('In this project, we will do some exploratory analysis to understand the data, visualize the data and generate the insights for future references.'),
br(),
h5('We will introduce two functions later, then we can see popular publishers and the game sales of variuos geographical areas by selecting different game genres and game platforms.')),

####

tabPanel('Data Page',
         h4('Take a look at the selected dataset'),
fluidRow(column(4,selectInput('genre', 'Select a game genre', 
                                            choices = c('Action', 'Adenture', 'Fighting', 'Misc', 'Platform','Puzzle', 'Racing', 'Role-Playing', 'Shooter', 'Simulation','Sports', 'Strategy')),
selectInput('platform', 'Select a platform',
                                          choices = c('2600','3DS','DS','GB', 'GBA','GC','GEN','N64','NES','PC','PS',
'PS2','PS3','PS4','PSP','SNES','Wii', 'WiiU','X360','XB', 'XOne')),
tableOutput('data'))
)),

####
tabPanel('Best Selling Games', 
         h4('The best selling games in North America, Europe, Japan and rest of the world'),
         fluidRow(column(6,
                  plotOutput('bestna'),
                  plotOutput('bestjapan')),
                  column(6, plotOutput('besteuro'), plotOutput('bestrest')))),

#####
tabPanel('Vedio Games Sales by Areas',          
         fluidPage( titlePanel('Chart of Vedio Games Sales'),
    sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput('gen',
                  label = 'Choose a game genre', 
                  choices = c('Action', 'Adenture', 'Fighting', 'Misc', 'Platform', 'Puzzle', 'Racing', 'Role-Playing', 'Shooter', 'Simulation','Sports', 'Strategy'),
                  ),
      br(),
      selectInput('plt', 
                  label = 'Select a game platform',
                  choices = c('2600','3DS','DS','GB', 'GBA','GC','GEN','N64','NES','PC','PS',               'PS2','PS3','PS4','PSP','SNES','Wii', 'WiiU','X360','XB', 'XOne')),
    ),
    mainPanel(
      column(6,plotOutput("gameplot", width = "500px", height = "400px"),
)))
)),

tabPanel('Find Best Publisher',          
    fluidPage(
  titlePanel('Visualization of Top Publishers'),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput('gen1',
                  label = 'Choose a game genre', 
                  choices = c('Action', 'Adenture', 'Fighting', 'Misc', 'Platform', 'Puzzle', 'Racing', 'Role-Playing', 'Shooter', 'Simulation','Sports', 'Strategy'),
                  ),
      br(),
      selectInput('plt1', 
                  label = 'Select a game platform',
                  choices = c('2600','3DS','DS','GB', 'GBA','GC','GEN','N64','NES','PC','PS',               'PS2','PS3','PS4','PSP','SNES','Wii', 'WiiU','X360','XB', 'XOne')),
    ),
    mainPanel(
      column(6,plotOutput("publisher", width = "400px", height = "300px")
)))
))
###
))



server1 <- function(input, output){

  output$data <- renderTable(find_data(input$genre, input$platform))
#########  
  output$bestna <- renderPlot(bestna)
  output$bestjapan <- renderPlot(bestjapan)
  output$besteuro <- renderPlot(besteuro)
  output$bestrest <- renderPlot(bestrest)
  
  output$gameplot <- renderPlot({
    type_check(input$gen, input$plt)
  })
#########
  output$publisher <- renderPlot({
    genre_publisher(input$gen1, input$plt1)
  })
#######
}



shinyApp(ui=ui2, server=server1)








