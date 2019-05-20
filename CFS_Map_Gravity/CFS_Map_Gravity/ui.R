library(shiny)
library(ggmap)

memory.limit(40000)

counties = map_data("county")
data(state)

df = read.csv("aff_download/CFS_2007_00A21_with_ann.csv", stringsAsFactors = F)
valus = as.character(unname(df[1,c(9,11,13)]))
df = df[-1,]
moders = unique(df$DMODE.display.label)

chooser = setNames(list("Inflow", "Outflow"), list("Inflow", "Outflow"))
cat("foo1\n")
# Define UI for miles per gallon application
shinyUI(navbarPage("Trade Flows",
                   tabPanel("States",
                            sidebarPanel(
                              selectInput("state2", "State:",
                                          setNames(list(unique(counties$region)), "States"),
                                          selected = unique(counties$region)[1]),
                              
                              radioButtons("flow2", "Choose one:", choiceNames = list("Inflow", "Outflow"), 
                                           choiceValues = list("Inflow", "Outflow"), selected = "Outflow"),

                              selectInput("units", "Type of Units:",
                                          setNames(list(valus), "Value Type"),
                                          selected = valus[1]),
                              
                              checkboxInput("exclude2", "Exclude State of Interest", TRUE)
                            ),
                            mainPanel(h3(textOutput("caption2")),
                                      plotOutput("map2"))
                   ),
                   tabPanel("Help",
                            mainPanel(h4(htmlOutput("caption4"))))
))