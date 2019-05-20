library(shiny)
library(ggmap)

memory.limit(40000)

counties = map_data("county")
data(state)

df = read.csv("aff_download/CFS_2007_00A21_with_ann.csv", stringsAsFactors = F)
valus = as.character(unname(df[1,9:14]))
df = df[-1,]
moders = unique(df$DMODE.display.label)

chooser = setNames(list("net", "Inflow", "Outflow"), list("Net", "Inflow", "Outflow"))
cat("foo1\n")
# Define UI for miles per gallon application
shinyUI(navbarPage("Trade Flows",
                   tabPanel("States",
                            sidebarPanel(
                              selectInput("state2", "State:",
                                          setNames(list(unique(counties$region)), "States"),
                                          selected = unique(counties$region)[1]),
                              # Simple integer interval
                              sliderInput("year2", "Year:", 
                                          min=2007, max=2007, value=2007,sep=""),
                              
                              radioButtons("flow2", "Choose one:", choiceNames = list("Net Inflow", "Inflow", "Outflow"), 
                                           choiceValues = list("Net Inflow", "Inflow", "Outflow"), selected = "Outflow"),
                              
                              selectInput("modes", "Mode of Transporation:",
                                          setNames(list(moders), "Modes"),
                                          selected = moders[1]),
                              
                              selectInput("units", "Type of Units:",
                                          setNames(list(valus), "Value Type"),
                                          selected = valus[1]),
                              
                              checkboxInput("exclude2", "Exclude State of Interest", FALSE)
                            ),
                            mainPanel(h3(textOutput("caption2")),
                                      plotOutput("map2"))
                   ),
                   tabPanel("Help",
                            mainPanel(h4(htmlOutput("caption4"))))
))