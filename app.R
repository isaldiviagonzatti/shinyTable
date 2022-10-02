# M&E Systems Dashboard

library(shiny)
library(tidyverse)
library(rsconnect)
library(dplyr)
library(DT)
library(shinyWidgets)
library(highcharter)


fullData = read_csv("finalListGIS.csv")
finalList= fullData[, c("Country", "Implementation","Type",'Number of Sectors', "Number of Indicators")]

finalList[ , 'Source'] <- NA
finalList[ , 'Details'] <- NA

finalList['Source'] <- paste0("<a href=' ",fullData$Source,"' target='_blank'>","<img src=\'",fullData$flagURL, "\' height=\"30\" ></img>","</a>","<a style=color:darkblue; href='",fullData$Source,"' target='_blank'>","<p>",fullData$`Primary Source`,"</p>","</a>")

finalList['Details'] <- paste0(
  '<p>Institution in Charge: <strong>',fullData$Institution,'</strong></p>',
    '<p>Last Communication: <strong>',fullData$Year, '</strong></p>',
    '<p>Communication Frequency: <strong>', fullData$Frequency, ' </strong></p>'
)

finalList=finalList[ ,c('Source', 'Country', 'Implementation',
                    'Type','Details', "Number of Sectors",
                    "Number of Indicators")]

#inventorySankey <- read.csv("finalListGIS.csv")

#for selector 
countryList=c(finalList[,"Country"])

#reorder columns
finalList=finalList[ ,c('Source', 'Country', 'Implementation',
                        'Type','Details', "Number of Sectors",
                        "Number of Indicators")]



#prepare the Sankey data
df <- subset(fullData, select = c("Present", "Implementation", "Type")) # Remove unnecessary columns

data <- data.frame(append(df, c(Countries = "Countries"), after = 0)) # Make first column "Countries" to group all together initial starting point on chart

# Change Yes in Present to "Present" for visualisation
data$Present[data$Present == "Yes"] <- "Present"
# Only Present countries
data_pres <- subset(data, Present == "Present")


# Add percentages to all labels
data1 <- data %>%
  group_by(Present) %>%
  tally()%>%
  mutate(perc = n/sum(n))%>%
  mutate(PresentNew = paste0(Present, ' (',round(perc* 100,1) , '%)'))%>%
  select(-n, - perc)

data_w_stats <- merge(data, data1, by = "Present")

data2 <- data %>%
  group_by(Implementation) %>%
  tally()%>%
  mutate(perc = n/sum(n))%>%
  mutate(ImplNew = paste0(Implementation, ' (',round(perc* 100,1) , '%)'))%>%
  select(-n, - perc)

data_w_stats <- merge(data_w_stats, data2, by = "Implementation")

data3 <- data %>%
  group_by(Type) %>%
  tally()%>%
  mutate(perc = n/(sum(n)-2))%>% # -2 for the two countries with no implementation info
  mutate(TypeNew = paste0(Type, ' (',round(perc* 100,1), '%)'))%>%
  select(-n, - perc)

data_w_stats <- merge(data_w_stats, data3, by = "Type")

# Create final table for Sankey chart

dataSankey <- data_w_stats %>%
  select(Countries, PresentNew, ImplNew, TypeNew) %>%
  mutate(Countries = paste0(Countries, ' (',nrow(data),' Countries)'))

# Hide unnecessary information
dataSankey$TypeNew[dataSankey$TypeNew == "NA (79.9%)"] = NA
dataSankey$ImplNew[dataSankey$ImplNew == "NA (79.1%)"] = NA


###For only Present countries chart

# Add percentages to all labels
data1 <- data_pres %>%
  group_by(Present) %>%
  tally()%>%
  mutate(perc = n/sum(n))%>%
  mutate(PresentNew = paste0(Present, ' (',n,' Countries)'))%>%
  select(-n, - perc)

data_w_stats_pres <- merge(data_pres, data1, by = "Present")

data2 <- data_pres %>%
  group_by(Implementation) %>%
  tally()%>%
  mutate(perc = n/sum(n))%>%
  mutate(ImplNew = paste0(Implementation, ' (',round(perc* 100,1) , '%)'))%>%
  select(-n, - perc)

data_w_stats_pres <- merge(data_w_stats_pres, data2, by = "Implementation")

data3 <- data_pres %>%
  group_by(Type) %>%
  tally()%>%
  mutate(perc = n/(sum(n)-2))%>% # -2 for the two countries with no implementation info
  mutate(TypeNew = paste0(Type, ' (',round(perc* 100,1), '%)'))%>%
  select(-n, - perc)

data_w_stats_pres <- merge(data_w_stats_pres, data3, by = "Type")

# Create final table for Sankey chart

dataPresent <- data_w_stats_pres %>%
  select(PresentNew, ImplNew, TypeNew)

# Hide unnecessary information
dataPresent$TypeNew[dataPresent$TypeNew == "NA (78.6%)"] = NA
dataPresent$ImplNew[dataPresent$ImplNew == "NA (78.6%)"] = NA



###Dashboard

ui = fluidPage( 
  theme = bslib::bs_theme(bootswatch = "simplex"),
  tabsetPanel(
    tabPanel( "Inventory",fluid=TRUE,
    titlePanel("Present M&E Systems Comparison"),

  #selectizeInput("country_select", "Select Countries",
  #               choices = finalList[,"COUNTRY"], multiple = TRUE,
  #               selected = c("Austria","Philippines"),
  #               options = list(placeholder = 'Select a country'
  #              )
  # ),
                pickerInput("locInput","Select Countries to Display", 
                  choices=finalList[,"Country"],
                  selected = countryList[["Country"]],
                  options = list(`actions-box` = TRUE),multiple = T),
  
                    fluidRow(
                      column( 
                DT::dataTableOutput("counTable"),width = 12
    
                            ))
    ),
                  tabPanel("Sankey",fluid = TRUE,
                           titlePanel("Sankey Chart"),
                           
                           fluidRow(
                             highchartOutput("hcontainer")
                             )
                          
)

)
)

server = function(input,output, session) {
  
  
  filter_country <- reactive({ 
    filter(finalList, Country %in% input$locInput)
    
  })
  
  output$counTable = renderDT({
    
    datatable(
    filter_country(),
    rownames= FALSE,
    escape = FALSE,
    selection = 'none',
    options = list( dom = 't',pageLength = 50, lengthChange= FALSE,
                    columnDefs = list(list( width = '30%', targets = list(4)), 
                                      list(className = 'dt-center', targets = list(0))  
                    ))  
    
     ) %>%
      formatStyle('Implementation', backgroundColor = styleEqual(c("Implemented","Planned"), c('steelblue', 'lightblue'),c('bold'))) %>%
     formatStyle("Number of Indicators",
                   background = styleColorBar(range(finalList[ "Number of Indicators"]), 'lightblue'),
                   backgroundSize = '98% 88%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center')   %>% 
          formatStyle("Number of Sectors",
                  background = styleColorBar(range(finalList["Number of Sectors"]), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')                                                                        

})        



  output$hcontainer  = renderHighchart ({
    highchart() %>%
      hc_chart(type='sankey') %>%
        hc_add_series(data = data_to_sankey(dataPresent), "sankey", name = "Global Inventory",
                    hcaes(from = from, to = to, weight = weight),
                    nodes = list(list(id = "Present (41 Countries)", color = "#56B4E9"), #sky blue
                                 list(id = "Implemented (58.5%)", color = "#D55E00"), #orange
                                 list(id = "Planned (36.6%)", color = "#CC79A7"), #pink
                                 list(id = "Unknown (4.9%)", color = "black"),
                                 list(id = "Type 1 & 3 (5.1%)", color = "#009E73"), #turquoise
                                 list(id = "Type 1 (12.8%)", color = "#0072B2"), #blue
                                 list(id = "Type 2 (12.8%)", color = "#F0E442"), #yellow-green
                                 list(id = "Type 3 (61.5%)", color = "#56B4E9"),
                                 list(id = "Unknown Type (12.8%)", color = "#E69F00") #pale orange
                    )) %>%
        hc_title(text= "Global Inventory of National Adaptation Monitoring Systems") %>%
        hc_subtitle(text= "Sankey Diagram")  %>%
        hc_caption(text = "<b>This plot is for visualisation purposes only. Data is gathered from the inventory.<b>")%>%
        hc_add_theme(hc_theme_elementary()) %>%
        hc_plotOptions(series = list(enabled = TRUE,
                                     dataLabels = list(style = list(fontSize = "15px",
                                                                    color = "black"),
                                                       connectorAllowed = TRUE,
                                                       allowOverlap = FALSE))) %>%
        hc_yAxis(labels = list(distance = 100)) %>%
        hc_exporting(enabled = TRUE,
                     filename = "present_cb")
    
    
})
  

  
}



shinyApp(ui=ui, server=server)



#deploy table
#rsconnect::setAccountInfo(name='ignaciosaldiviagonzatti', token='ABB651A1AC6E95790C6858D8A79E5157', secret='//QNWTLftmbPXMIR1S5bFUwk+CJi/UozOVmq27Ed')
#rsconnect::deployApp('C:/Users/isaldiviagonzatti/Downloads/WUR/ACT/StoryMap/appTable')
