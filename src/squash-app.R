library(shiny)
library(tidyverse)
library(DT)
library(alluvial)
library(callr)
library(reprex)
library(knitr)


# read in and prep data:
squash <-  read.csv("result.csv")

squash <- squash %>% drop_na() %>% select(day:id)


name.legend <- data.frame(name = c("noah", "dave", "beni", "kim", "joscha", "steve"),
                          name.id = c(0:5))
name.legend$name <- as.character(name.legend$name)

squash <- squash %>% left_join(name.legend %>% 
          rename(player1 = name.id, player1.name = name), by = "player1") %>% 
  left_join(name.legend %>% 
          rename(player2 = name.id, player2.name = name), by = "player2")

squash <- squash %>% mutate(pairing = paste(player1.name, player2.name, sep = "_"))

pairing <- as.data.frame(t(combn(name.legend$name,2))) %>% 
  mutate(merged1 = paste(V1,V2, sep = "_")) 

squash$pairing <- ifelse(squash$pairing %in% pairing$merged1, 
       squash$pairing, paste(squash$player2.name,squash$player1.name, sep = "_"))

squash <- squash %>% mutate(winner = ifelse(score1 > score2, player1.name, player2.name))




#################################################################################################
# win rate over time

i=1
over.time <- pairing  %>%  select(V1, merged1) %>%
  rbind(pairing %>% select(V2, merged1) %>% rename(V1 = V2)) %>%
  rename(winner = V1) %>% rename(pairing = merged1) %>% 
  left_join(squash %>%  filter(day <= i) %>%
              group_by(pairing, winner) %>% 
              summarise(n = n())) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  left_join(squash %>%  filter(day <= i) %>% group_by(pairing) %>%  
              summarise(n.pairing = n()), by = "pairing") %>% 
  mutate(win.rate = n/n.pairing) %>% mutate(day = i)

for(i in 2:max(squash$day)){
  interim <- pairing  %>%  select(V1, merged1) %>%
  rbind(pairing %>% select(V2, merged1) %>% rename(V1 = V2)) %>%
  rename(winner = V1) %>% rename(pairing = merged1) %>% 
  left_join(squash %>%  filter(day <= i) %>%
              group_by(pairing, winner) %>% 
              summarise(n = n())) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  left_join(squash %>%  filter(day <= i) %>% group_by(pairing) %>%  
              summarise(n.pairing = n()), by = "pairing") %>% 
  mutate(win.rate = n/n.pairing) %>% mutate(day = i)
over.time <- over.time %>% rbind(interim)
}
#################################################################################################


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Squash",
  ########################################
   tabPanel("win rates",
   sidebarLayout(
      sidebarPanel(
        radioButtons("Spieler1.1",
                    "Spieler1",
                    choices = as.list(name.legend$name), selected = "noah"),
        radioButtons("Spieler2.1",
                     "Spieler2",
                    choices = as.list(name.legend$name), selected = "beni")
      ),
      mainPanel(
         plotOutput("squash")
         #,
         #plotOutput("squash2")
      )
   )
   ),
   ########################################
   tabPanel("win rates over time",
            sidebarLayout(
              sidebarPanel(
                radioButtons("Spieler1",
                             "Spieler1",
                             choices = as.list(name.legend$name), selected = "noah"),
                radioButtons("Spieler2",
                             "Spieler2",
                             choices = as.list(name.legend$name), selected = "beni")
              ),
              mainPanel(
                plotOutput("squash2")
              )
            )
   ),
   ########################################
  tabPanel("alluvial",
           mainPanel(
           plotOutput("alluvial")
           )
  ),
  
  ########################################
  tabPanel("alluvial2",
           mainPanel(
           plotOutput("alluvial2")
           )
  ),
  ########################################
  tabPanel("code",
           
             includeHTML("code.html")
           
  ),
  ########################################
  tabPanel("about",
           textOutput("about")
  )
   
  )
)


#####################################################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$squash <- renderPlot({
      
     
     
     interim <- pairing %>% 
       filter((V1 == input$Spieler1.1 & V2 == input$Spieler2.1) | 
              (V1 == input$Spieler2.1 & V2 == input$Spieler1.1))
   pairing.input <- interim$merged1
 
   if(nrow(interim) != 0){
     
  
  pairing %>% select(V1, merged1) %>%
       rbind(pairing %>% select(V2, merged1) %>% rename(V1 = V2)) %>%
       rename(winner = V1) %>% rename(pairing = merged1) %>% 
     left_join(squash %>% group_by(pairing, winner) %>% summarise(n = n())) %>% 
     mutate(n = ifelse(is.na(n), 0, n)) %>% 
    left_join(squash %>% group_by(pairing) %>%  
                summarise(n.pairing = n()), by = "pairing") %>% 
    mutate(win.rate = n/n.pairing) %>% 
    filter(pairing == pairing.input) %>%
       ggplot(aes(winner, win.rate, label = round(win.rate,2))) + geom_col() +
       geom_label(nudge_y = -0.1) +
       ggtitle(paste(pairing.input))} else{
         
         data.frame(x=c(1:10),y=c(1:10)) %>% ggplot(aes()) + 
           ggtitle("you can't play against yourself")
       }
   
     
   })
   output$squash2 <- renderPlot({
     
     interim <- pairing %>% 
       filter((V1 == input$Spieler1 & V2 == input$Spieler2) | 
                (V1 == input$Spieler2 & V2 == input$Spieler1))
     pairing.input <- interim$merged1
     
     if(nrow(interim) != 0){
      
    over.time %>% filter(pairing == pairing.input) %>% 
       ggplot(aes(x=day, y= win.rate )) + geom_line(aes(group = winner, color = winner)) +
       ggtitle(paste(input$pairing1))} else{
         
         data.frame(x=c(1:10),y=c(1:10)) %>% ggplot(aes()) + 
           ggtitle("you can't play against yourself")
       }
     
    
   })
   
   output$summary <- renderPrint({
     summary(squash)
   })
     
     
     output$about <- renderText({ 
       "created by @fulowa"
   })
     
     output$alluvial <- renderPlot({
     over.time.int <- over.time %>% filter(n.pairing > 20, day == max(day)) 
     alluvial(over.time.int %>% select(pairing, winner),
              freq = over.time.int$n ,
              col = case_when(over.time.int$pairing == "noah_steve" ~ "yellow",
                              over.time.int$pairing == "noah_joscha" ~ "orange",
                              over.time.int$pairing == "noah_dave" ~ "green",
                              over.time.int$pairing == "noah_beni" ~ "red",
                              over.time.int$pairing == "dave_beni" ~ "blue",
                              TRUE ~ "grey"))
     })
     
     output$alluvial2 <- renderPlot({
       over.time.int <- over.time %>% filter(n.pairing > 20, day == max(day)) 
       alluvial(over.time.int %>% select(winner, pairing),
                freq = over.time.int$n ,
                col = case_when(over.time.int$winner == "steve" ~ "yellow",
                                over.time.int$winner == "joscha" ~ "orange",
                                over.time.int$winner == "beni" ~ "green",
                                over.time.int$winner == "noah" ~ "red",
                                over.time.int$winner == "dave" ~ "blue",
                                TRUE ~ "grey"))
     })
     
     
     
}
#####################################################################################################################
# Run the application 
shinyApp(ui = ui, server = server)