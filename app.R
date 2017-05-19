##shiny app for spawning data calendar plots

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)
library(htmltools)

ui <- fluidPage(
  tags$h2("Frequency of Aiptasia spawning in Pringle Lab"),
  column(3,
         checkboxGroupInput(inputId = "target", label = "Choose any/all incubators:", 
                            choices = c("Blue (Cabinet)", "Red (New)", "Green (Newest)","Orange (Old)", "Yellow (Ron)"),
                            selected = c("Blue (Cabinet)", "Red (New)", "Green (Newest)","Orange (Old)", "Yellow (Ron)")),
         dateRangeInput(inputId = "daterange", label = "Enter a date range", 
                 format = "yyyy-mm-dd", start = "2016-01-01")
      
      #selectInput("select", label = "Select y-axis", 
       #           choices = list("raw count" = "n", "normalized count" = "norm.Count"), 
        #          selected = 1)
        ),
  column(9,
         plotOutput(outputId = "spawnplot1"),
         plotOutput(outputId = "spawnplot2"))
  
)

SpawnDF <- read.csv(file="Data/SpawningData20170507.csv", header = TRUE)
SpawnDF$Spawn.Date=as.Date(SpawnDF$Spawn.Date, format = "%m/%d/%y")
#remove leading/trailing whitespace
SType=trimws(SpawnDF$Spawn.Type, "right")
#reassign levels
levels(SType)= c("eggs", "sperm", "embryos", "larvae")
#replace column data
SpawnDF$Spawn.Type = SType

server <- function(input, output){

  output$spawnplot1 <- renderPlot({
    SpawnDF %>% 
      filter(Spawn.Date >= input$daterange[1] & Spawn.Date <= input$daterange[2]) %>% 
      filter(Incubator %in% input$target) %>%
      group_by(Spawn.Date, Total...of.tanks) %>% 
      count(Spawn.Type) %>%
      mutate(norm.Count = n/ Total...of.tanks )  %>%
        ggplot( aes(x=Spawn.Date, y= norm.Count, fill=Spawn.Type) ) +
        geom_bar(stat = "identity") +
        ggtitle("Spawning Frequency by calendar dates") + ylab("normalized counts")+
        theme_bw()+ theme(text = element_text(size = 16))
  })
  output$spawnplot2 <-renderPlot({
    SpawnDF %>% 
      filter(Spawn.Date >= input$daterange[1] & Spawn.Date <= input$daterange[2]) %>%
      filter(Incubator %in% input$target) %>%
      group_by(Day.of.Cycle) %>% 
      count(Spawn.Type) %>%
      ggplot( aes(x=Day.of.Cycle, y=n, fill=Spawn.Type)) +
        geom_bar(stat = "identity") +
        scale_x_continuous(breaks = c(1:30)) + 
        ggtitle("Spawning Frequency by Lunar Cycle") + ylab(" raw counts")+
        theme_bw()+theme(text = element_text(size = 16))
  })
}


shinyApp(ui = ui, server = server)