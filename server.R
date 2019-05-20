library(shiny)
library(datasets)
library(readxl)
library(ggmap)
library(ggplot2)
library(dplyr)
library(stringr)

memory.limit(40000)

counties <- map_data("county")
states <- map_data("state")
data(state)

cat("First")

df = read.csv("aff_download/CFS_2007_00A21_with_ann.csv",stringsAsFactors = F)

valus = as.character(unname(df[1,9:14]))
df = df[-1,]

df$VAL = as.numeric(as.character(df$VAL))
df$VALP = as.numeric(as.character(df$VALP))
df$TON = as.numeric(as.character(df$TON))
df$TONP = as.numeric(as.character(df$TONP))
df$TMILE = as.numeric(as.character(df$TMILE))
df$TMILEP = as.numeric(as.character(df$TMILEP))

df$GEO.display.label = tolower(df$GEO.display.label)
df$DDESTGEO.display.label = tolower(df$DDESTGEO.display.label)

cat("Second")

# Dataset 
df_origs = inner_join(df, states, by = c("DDESTGEO.display.label" = "region"))
df_dest = inner_join(df, states, by = c("GEO.display.label" = "region"))

df_origs$state_int = df_origs$GEO.display.label
df_dest$state_int = df_dest$DDESTGEO.display.label
df_origs$not_int = df_origs$DDESTGEO.display.label
df_dest$not_int = df_dest$GEO.display.label

cat("Third")

# Net Inflows
outs = df[match(paste(df$DDESTGEO.display.label, df$GEO.display.label,df$DMODE.display.label), 
                paste(df$GEO.display.label, df$DDESTGEO.display.label,df$DMODE.display.label)),]
outs[is.na(outs)] = 0
df_outs = df
df_outs[,9:14] = df_outs[,9:14] - outs[,9:14]

df_net = inner_join(df_outs, states, by = c("GEO.display.label" = "region"))

df_net$state_int = df_net$DDESTGEO.display.label
df_net$not_int = df_net$GEO.display.label

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

cat("Fourth")


##########
# Region #
##########
# df$orig_region = state.division[match(df$GEO.display.label, tolower(state.name))]
# df$destin_region = state.division[match(df$DDESTGEO.display.label, tolower(state.name))]
# 
# 
# df_origs_match = df %>%
#   group_by(orig_region, destin_region, DMODE.display.label) %>%
#   summarize(VAL = sum(VAL,na.rm=T), VALP = sum(VALP,na.rm=T),
#             TON = sum(TON,na.rm=T), TONP = sum(TONP,na.rm=T),
#             TMILE = sum(TMILE,na.rm=T), TMILEP = sum(TMILEP,na.rm=T))
# 
# 
# df_origs_match = as.data.frame(df_origs_match)
# 
# cat("Fifth")
# 
# df_origs_reg = df
# df_origs_reg[,9:14] = df_origs_match[match(paste(df_origs_reg$orig_region, df_origs_reg$destin_region,
#                                                  df_origs_reg$DMODE.display.label),
#                                            paste(df_origs_match$orig_region,
#                                                  df_origs_match$destin_region,
#                                                  df_origs_match$DMODE.display.label)),4:9]
# 
# 
# 
# df_or = df_origs_reg
# 
# df_dest_or = inner_join(df_or, states, by = c("GEO.display.label" = "region"))
# rm(df_or)
# 
# df_dest_or$state_int = df_dest_or$destin_region
# df_dest_or$not_int = df_dest_or$orig_region
# 
# cat("Sixth")
# 
# df_or_2 = df_origs_reg
# 
# df_origs_or = inner_join(df_or_2, states, by = c("DDESTGEO.display.label" = "region"))
# 
# rm(df_or_2)
# 
# df_origs_or$state_int = df_origs_or$orig_region
# df_origs_or$not_int = df_origs_or$destin_region
# 
# cat("Seventh")
# 
# # Net Inflows
# outr = df_origs_reg[match(paste(df_origs_reg$GEO.display.label, df_origs_reg$DDESTGEO.display.label),
#                             paste(df_origs_reg$DDESTGEO.display.label, df_origs_reg$GEO.display.label)),]
# 
# df_outr_st = df_origs_reg
# df_outr_st[,9:14] = -(df_outr_st[,9:14]) + outr[,9:14]
# 
# rm(outr)
# rm(df_origs_reg)
# 
# 
# df_outr_st2 = inner_join(df_outr_st, states, by = c("DDESTGEO.display.label" = "region"))
# 
# cat("Eighth")
# 
# rm(df_outr_st)
# 
# df_outr_st2$state_int = df_outr_st2$orig_region
# df_outr_st2$not_int = df_outr_st2$destin_region

#df_dest_or = df_dest_or[!duplicated(df_dest_or[c("state_int","not_int","long","lat","group","order")]),]
#df_origs_or = df_origs_or[!duplicated(df_origs_or[c("state_int","not_int","long","lat","group","order")]),]
#df_outr_st2 = df_outr_st2[!duplicated(df_outr_st2[c("state_int","not_int","long","lat","group","order")]),]

#cat("Ninth")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
  # State
  values2 <- reactiveValues(df_data = subset(df_origs, state_int == "alabama" & DMODE.display.label == "All modes"),
                            flowing = "Outflow", exclusion = FALSE)
  
  # State
  observeEvent(input$state2, {
    cat("Bye")
    if(values2$flowing == "Inflow"){values2$df_data <- subset(df_dest, state_int == input$state2 & DMODE.display.label == input$modes)}
    else if(values2$flowing == "Outflow"){values2$df_data <- subset(df_origs, state_int == input$state2 & DMODE.display.label == input$modes)}
    else{values2$df_data <- subset(df_net, state_int == input$state2 & DMODE.display.label == input$modes)}
    
    if(values2$exclusion){values2$df_data <- subset(values2$df_data, not_int != input$state2)}
    
  })
  
  # State
  observeEvent(input$flow2, {
    cat("Or")
    values2$flowing <- input$flow2
    
    if(values2$flowing == "Inflow"){values2$df_data <- subset(df_dest, state_int == input$state2 & DMODE.display.label == input$modes)}
    else if(values2$flowing == "Outflow"){values2$df_data <- subset(df_origs, state_int == input$state2 & DMODE.display.label == input$modes)}
    else{values2$df_data <- subset(df_net, state_int == input$state2 & DMODE.display.label == input$modes)}
    
    if(values2$exclusion){values2$df_data <- subset(values2$df_data, not_int != input$state2)}
  })
  
  # State
  observeEvent(input$exclude2, {
    values2$exclusion <- input$exclude2
    
    if(values2$exclusion){values2$df_data <- subset(values2$df_data, not_int != input$state2)}
    else{
      if(values2$flowing == "Inflow"){values2$df_data <- subset(df_dest, state_int == input$state2 & DMODE.display.label == input$modes)}
      else if(values2$flowing == "Outflow"){values2$df_data <- subset(df_origs, state_int == input$state2 & DMODE.display.label == input$modes)}
      else{values2$df_data <- subset(df_net, state_int == input$state2 & DMODE.display.label == input$modes)}
    }
  })
  
  # State
  observeEvent(input$modes, {
    cat("Or")
    
    if(values2$flowing == "Inflow"){values2$df_data <- subset(df_dest, state_int == input$state2 & DMODE.display.label == input$modes)}
    else if(values2$flowing == "Outflow"){values2$df_data <- subset(df_origs, state_int == input$state2 & DMODE.display.label == input$modes)}
    else{values2$df_data <- subset(df_net, state_int == input$state2 & DMODE.display.label == input$modes)}
    
    if(values2$exclusion){values2$df_data <- subset(values2$df_data, not_int != input$state2)}
  })
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  
  # States
  formulaText2 <- reactive({
    paste0("Trade ", input$flow2, ": ", str_to_title(input$state2), " (", input$year2, ")")
  })
  
  # Return the formula text for printing as a caption
  output$caption2 <- renderText({
    formulaText2()
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  
  # State
  output$map2 <- renderPlot({
    cat("Plotting")
    ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) + 
      geom_polygon(color = "black", fill = "gray") +
      geom_polygon(data = values2$df_data, 
                   aes(fill = values2$df_data[,(which(input$units == valus)+8)])) +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() + 
      ditch_the_axes + 
      scale_fill_gradientn(colours = rev(rainbow(7))) + 
      labs(fill="Trade Amount")
  })
  
  # States
  # Reactive expression to compose a data frame containing all of the values
  sliderValues2 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$year2)), 
      stringsAsFactors=FALSE)
  }) 
  
  # States
  # Show the values using an HTML table
  output$values2 <- renderTable({
    sliderValues2()
  })
  
  # Help
  # Return the formula text for printing as a caption
  output$caption4 <- renderUI({
    str1 <- "Checking \"Exclude State of Interest\" will gray out the selected area for which 
    inflows, outflows, or net inflows are desired.<br/>"
    str2 <- "All values are total number of trade flows without any trasnformations.<br/>"
    str3 <- "Light grey means there were no flows in 2007 between
    the specified areas in the given direction.<br/>"
    str4 <- "If you chose \"Percent of Total\" for the type of units, this will give the percent
    of trade flows coming from the specified transportation method. This number is meaningless
    for net inflows.<br/>"
    str5 <- "Trade Amount is in millions of US dollars.<br/>"
    str6 <- "Cite: I used  the US Census Bureau's Commodity Flow Survey for 2007."
    HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
  })
  
})




