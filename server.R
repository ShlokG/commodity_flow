library(shiny)
library(datasets)
library(readxl)
library(ggmap)
library(ggplot2)
library(dplyr)
library(stringr)

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
df$orig_region = state.division[match(df$GEO.display.label, tolower(state.name))]
df$destin_region = state.division[match(df$DDESTGEO.display.label, tolower(state.name))]


df_origs_match = df %>%
  group_by(orig_region, destin_region, DMODE.display.label) %>%
  summarize(VAL = sum(VAL,na.rm=T), VALP = sum(VALP,na.rm=T),
            TON = sum(TON,na.rm=T), TONP = sum(TONP,na.rm=T),
            TMILE = sum(TMILE,na.rm=T), TMILEP = sum(TMILEP,na.rm=T))


df_origs_match = as.data.frame(df_origs_match)

cat("Fifth")

df_origs_reg = df
df_origs_reg[,9:14] = df_origs_match[match(paste(df_origs_reg$orig_region, df_origs_reg$destin_region,
                                                 df_origs_reg$DMODE.display.label),
                                           paste(df_origs_match$orig_region,
                                                 df_origs_match$destin_region,
                                                 df_origs_match$DMODE.display.label)),4:9]



df_or = df_origs_reg

df_dest_or = inner_join(df_or, states, by = c("GEO.display.label" = "region"))
rm(df_or)

df_dest_or$state_int = df_dest_or$destin_region
df_dest_or$not_int = df_dest_or$orig_region

cat("Sixth")

df_or_2 = df_origs_reg

df_origs_or = inner_join(df_or_2, states, by = c("DDESTGEO.display.label" = "region"))

rm(df_or_2)

df_origs_or$state_int = df_origs_or$orig_region
df_origs_or$not_int = df_origs_or$destin_region

cat("Seventh")

# Net Inflows
outr = df_origs_reg[match(paste(df_origs_reg$GEO.display.label, df_origs_reg$DDESTGEO.display.label),
                            paste(df_origs_reg$DDESTGEO.display.label, df_origs_reg$GEO.display.label)),]

df_outr_st = df_origs_reg
df_outr_st[,9:14] = -(df_outr_st[,9:14]) + outr[,9:14]

rm(outr)
rm(df_origs_reg)


df_outr_st2 = inner_join(df_outr_st, states, by = c("DDESTGEO.display.label" = "region"))

cat("Eighth")

rm(df_outr_st)

df_outr_st2$state_int = df_outr_st2$orig_region
df_outr_st2$not_int = df_outr_st2$destin_region

#df_dest_or = df_dest_or[!duplicated(df_dest_or[c("state_int","not_int","long","lat","group","order")]),]
#df_origs_or = df_origs_or[!duplicated(df_origs_or[c("state_int","not_int","long","lat","group","order")]),]
#df_outr_st2 = df_outr_st2[!duplicated(df_outr_st2[c("state_int","not_int","long","lat","group","order")]),]

cat("Ninth")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
  # State
  values2 <- reactiveValues(df_data = subset(df_origs, state_int == "alabama" & DMODE.display.label == "All modes"),
                           flowing = "Outflow", exclusion = FALSE)
  
  # Region
  values3 <- reactiveValues(df_data = subset(df_origs_or, state_int == "East South Central" & DMODE.display.label == "All modes"),
                            flowing = "Outflow", exclusion = FALSE)
  
  # State
  observeEvent(input$state2, {
    cat("Bye")
    if(values2$flowing == "Inflow"){values2$df_data <- subset(df_dest, state_int == input$state2 & DMODE.display.label == input$modes)}
    else if(values2$flowing == "Outflow"){values2$df_data <- subset(df_origs, state_int == input$state2 & DMODE.display.label == input$modes)}
    else{values2$df_data <- subset(df_net, state_int == input$state2 & DMODE.display.label == input$modes)}
    
    if(values2$exclusion){values2$df_data <- subset(values2$df_data, not_int != input$state2)}
    
  })
  
  # Region
  observeEvent(input$region, {
    cat("Hello")
    if(values3$flowing == "Inflow"){values3$df_data <- subset(df_dest_or, state_int == input$region & DMODE.display.label == input$modes2)}
    else if(values3$flowing == "Outflow"){values3$df_data <- subset(df_origs_or, state_int == input$region & DMODE.display.label == input$modes2)}
    else{values3$df_data <- subset(df_outr_st2, state_int == input$region & DMODE.display.label == input$modes2)}
    
    if(values3$exclusion){values3$df_data <- subset(values3$df_data, not_int != input$region)}
    
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
  
  # Region
  observeEvent(input$flow3, {
    cat("See?")
    values3$flowing <- input$flow3
    
    if(values3$flowing == "Inflow"){values3$df_data <- subset(df_dest_or, state_int == input$region & DMODE.display.label == input$modes2)}
    else if(values3$flowing == "Outflow"){values3$df_data <- subset(df_origs_or, state_int == input$region & DMODE.display.label == input$modes2)}
    else{values3$df_data <- subset(df_outr_st2, state_int == input$region & DMODE.display.label == input$modes2)}
    
    if(values3$exclusion){values3$df_data <- subset(values3$df_data, not_int != input$region)}
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
  
  # Region
  observeEvent(input$exclude3, {
    values3$exclusion <- input$exclude3
    
    if(values3$exclusion){values3$df_data <- subset(values3$df_data, not_int != input$region)}
    else{
      if(values3$flowing == "Inflow"){values3$df_data <- subset(df_dest_or, state_int == input$region & DMODE.display.label == input$modes2)}
      else if(values3$flowing == "Outflow"){values3$df_data <- subset(df_origs_or, state_int == input$region & DMODE.display.label == input$modes2)}
      else{values3$df_data <- subset(df_outr_st2, state_int == input$region & DMODE.display.label == input$modes2)}
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
  
  # Region
  observeEvent(input$modes2, {
    cat("See?")
    values3$flowing <- input$flow3
    
    if(values3$flowing == "Inflow"){values3$df_data <- subset(df_dest_or, state_int == input$region & DMODE.display.label == input$modes2)}
    else if(values3$flowing == "Outflow"){values3$df_data <- subset(df_origs_or, state_int == input$region & DMODE.display.label == input$modes2)}
    else{values3$df_data <- subset(df_outr_st2, state_int == input$region & DMODE.display.label == input$modes2)}
    
    if(values3$exclusion){values3$df_data <- subset(values3$df_data, not_int != input$region)}
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
  
  # Region
  formulaText3 <- reactive({
    paste0("Trade ", input$flow3, ": ", str_to_title(input$region), " (", input$year3, ")")
  })
  
  # Return the formula text for printing as a caption
  output$caption3 <- renderText({
    formulaText3()
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
    
    # State
    output$map3 <- renderPlot({
      ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
        coord_fixed(1.3) + 
        geom_polygon(color = "black", fill = "gray") +
        geom_polygon(data = values3$df_data, 
                     aes(fill = values3$df_data[,(which(input$units2 == valus)+8)])) +
        geom_polygon(color = "black", fill = NA) +
        theme_bw() + 
        ditch_the_axes + 
        scale_fill_gradientn(colours = rev(rainbow(7))) + 
        labs(fill="Trade Amount")
    })
  
    # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$year)), 
      stringsAsFactors=FALSE)
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
  
  # Region
  # Reactive expression to compose a data frame containing all of the values
  sliderValues3 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$year3)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
  
  # States
  # Show the values using an HTML table
  output$values2 <- renderTable({
    sliderValues2()
  })
  
  # Region
  # Show the values using an HTML table
  output$values3 <- renderTable({
    sliderValues3()
  })
  
  # Help
  # Return the formula text for printing as a caption
  output$caption4 <- renderUI({
    str1 <- "Checking \"Exclude County of Interest\" or the corresponding mark for 
    State and Region will gray out the selected area for which 
    inflows, outflows, or net inflows are desired.<br/>"
    str2 <- "Note that the values for Net Inflow are log base-10 of (inflows/outflows))
    while for Outflow and Inflow, it's the total number of migrations<br/>"
    str3 <- "Lastly, dark grey means that there were no flows in the specified year but there were
    flows in other years. Light grey means there were no flows between 1990 and 2010 between
    the specified areas in the given direction.<br/>"
    str4 <- "Cite: I used  Matthew Hauer and James Byar's 2019 paper, \"IRS county-to-county migration data, 1990-2010\"
    for the migration data, which they originally obtained from the IRS dataset."
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  })
  
})


