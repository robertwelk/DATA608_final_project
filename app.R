
#packages
library(shiny)
library(dataRetrieval)
library(tidyverse)
library(plotly)
library(stringr)
library(leaflet)
library(shinythemes)



# Dataframe for stations - ID numbers and station names
station_df <- tibble('site_no'=c('01311143','01302600','01302845','01304057','01304200','01304562',
                                 '01304746','01304920','01305575','01306402','01309225','01310521',
                                 '01310740','01311145','01311850','01311875','01376562','01302250'),
                
                'site_nm'=c('Hog Island Channel','West Pond','Frost Creek','Flax Pond','Orient Harbor','Peconic River',
                            'Shinnecock Bay','Moriches Bay','Watch Hill', 'Sayville','Lindenhurst','Freeport',
                            'Reynolds Channel', 'East Rockaway','Jamaica Bay','Rockaway Inlet','Great Kills Harbor','East Creek'
                ))

# Retrieves site information from NWIS database including lat/long coordinates 
site.info <-readNWISsite(station_df$site_no) %>%
                as_tibble() %>%
                select(site_no,
                       'station_name'=station_nm,
                       'latitude'=dec_lat_va,
                       'longitude'=dec_long_va)

# Dataframe of collected parameters - name, USGS parameter code, and units of measurement
parameters <- tibble('parm_nm'= c("Elevation [ft]", "Salinity [ppt]", "Dissolved Oxygen [mg/L]", "pH", "Temperature [C]", "Chlorophyll [ug/L]", "Turbidity [FNU]"),
                     'parm_cd'= c('62619', '90860',"00300","00400","00010","62361","63680"),
                     'units'=c('feet', 'parts per thousand', 'milligrams per Liter', 'pH', 'degrees Celcius', 'microgram/Liter', 'FNU'))
 
# Named vectors used for user input of parameters and stations 
parm_choices = setNames(parameters$parm_cd, parameters$parm_nm)
site_choices = setNames(station_df$site_no, station_df$site_nm)

### The following three tables were made in the script 'DATA608_final_project.R'
# Read in dataframe of yearly means 
yearly_means <- 
  read.csv('yearly_means.csv', sep=",") %>% 
    as_tibble()
yearly_means$parameter <- as.character(yearly_means$parameter) %>% str_pad(5, side="left", pad="0")
yearly_means$site_no <- as.character(yearly_means$site_no) %>% str_pad(8, side="left", pad="0")    

# Read in dataframe of site means
site_means <-   read.csv('site_means.csv') %>% 
  as_tibble() 
site_means$parameter <- as.character(site_means$parameter) %>% str_pad(5, side="left", pad="0")
site_means$site_no <- as.character(site_means$site_no) %>%str_pad(8, side="left", pad="0")

# Read in dataframe of Endpoint rates
endpoint_rates <- read.csv('endpoint_rates.csv', sep=",") %>% as_tibble()
endpoint_rates$parameter <- as.character(endpoint_rates$parameter) %>% str_pad(5, side="left", pad="0")
endpoint_rates$site_no <- as.character(endpoint_rates$site_no) %>% str_pad(8, side="left", pad="0") 
                 


ui <- fluidPage(theme= shinytheme("slate"),

  titlePanel(
    h1("Real-Time Coastal Water Quality Monitoring in Long Island, New York", align="center")),
  
  sidebarLayout(
    
      sidebarPanel(width=3,
      
        dataTableOutput('table'),
      
        # input 1 - select parameter: only 1 can be chosen 
        radioButtons(inputId="input1",label= h4("1. Select Parameter"), parm_choices),
      
        # input 2 - choices get updated after parameter chosen - multiple stations can be selected
        checkboxGroupInput(inputId ='input2',
                     label=h4('2. Select Site(s)'), ""),
      
        # input 3 - choose output type
        radioButtons(inputId="input3", label=h4("3. Select Output Type"), choices=c("Current Conditions", "Long-Term Trends"))
    ), 
    
  mainPanel(
        leafletOutput("map"),
        br(),
        plotOutput("plot1")
    ) 
  ) 
) 



server <- function(input, output,session){
 
  #  Reactive object - Pull instantaneous values from NWIS database
  ## There is a conditional statement that handles the turbidity parameter differently
  data <- reactive({
    if(input$input1 == '63680'){
    readNWISuv(station_df$site_no, input$input1, Sys.Date()-5, Sys.Date()) %>%
      as_tibble() %>%
      select(2,3,'parameter'=6) %>% 
      left_join(station_df, by= "site_no")
    }
    else{
      readNWISuv(station_df$site_no, input$input1, Sys.Date()-5, Sys.Date()) %>%
        as_tibble() %>%
        select(2,3,'parameter'=4) %>% 
        left_join(station_df, by= "site_no")
    }
  })
  
  #  Reactive object -  that updates Site Selection input choices based on parameter chosen
  ## Not all sites have the same parameters, so list of available site reacts to this
  ## output of this reactive element is a named vector of site numbers 
  outVar = reactive({
      outvar_df <- whatNWISsites(sites=station_df$site_no, 
                               parameterCd=input$input1,
                               hasDataTypeCd="uv") %>% 
      left_join(station_df, by="site_no")
      outvar_vec <- pull(outvar_df,site_no)
      names(outvar_vec) <- outvar_df$site_nm
      outvar_vec
  })
  
  # This is the observer for the previous reactive object 
  ## The selections available for site selection are passed in by outVar()
  observe({
      updateCheckboxGroupInput(session, "input2", choices=outVar(),selected = site_choices[13])
  })
  
  #Map output# 
  #there are two map types: 1) for current condtions; 2) for long-term trends
  # map output type is copntrolled by the conditional statement 
  output$map <- renderLeaflet({
    
    if(input$input3=="Current Conditions"){ 
      
      # Retrieves from NWIS which sites are set up to collect given parameter
      uv_sites <-whatNWISsites(sites=station_df$site_no, 
                               parameterCd=input$input1,
                               hasDataTypeCd="uv")  
  
      ## adds a variable to the site.info df indicating if the site is set up to collect the parameter 
      site.info$has_parm <- match(site.info$site_no, uv_sites$site_no)
      
      # find data with real values - ie eliminate data that is not currently reporting (has NA or a dummy value)
      is_reporting <- data() %>%
        filter( !is.na(parameter)) %>% 
        filter(parameter != -999999)

      ## Find all sites that have reported real values today   
      is_reporting$Date <- as.Date(is_reporting$dateTime, format = "%m/%d/%Y %I:%M:%S %p") 
      is_reporting <- is_reporting %>% 
        filter(Date==Sys.Date()) %>% 
        distinct(site_no) 
      
      ## add previous step to site.info df
      site.info$is_reporting <- match(site.info$site_no, is_reporting$site_no)
      
      ### add a vector to the mapping df - will symbolize the current status 
      ### based on the previous vectors 'has_parm' and 'is_reporting'
      ### three levels of symbols 
      site.info$symbol <- factor(
        ifelse(is.na(site.info$has_parm), "Not collected", 
               ifelse(is.na(site.info$is_reporting), "Temporarily unavailable", "Available")), 
        levels=c("Available", "Temporarily unavailable", "Not collected")
      )
      
      ### assigns a color to each symbol level
      levels(site.info$symbol)
      pal <- colorFactor(palette=c("#f7cb44ff", "#cc6a70ff", "#593d9cff"), domain=site.info$symbol)

      
      # create the leaflet map 
      leaflet(data=site.info) %>% 
        
        addProviderTiles(providers$Esri.WorldImagery) %>% # imagery basemap
        addCircleMarkers(~longitude, 
                         ~latitude,
                         color= ~pal(symbol),
                         popup = ~station_name, 
                         label=~station_name, 
                         stroke = FALSE, 
                         fillOpacity = 0.90) %>%
        addLegend("bottomright", pal = pal, values = ~symbol,
                  title = "Parameter Status",
                  opacity = 1)
    
    }
    # if the long-term trends botton is selcted, a different map get outputed
    else{
      
      ## make a legend label for the map based on the parameter selected
      matchid <- input$input1 %>% match(parameters$parm_cd)
      legend_label <- paste(parameters$units[matchid], "/year")
      
      ## filter end-point rates table for the selected parameter
      map_ep <-  endpoint_rates %>%
        filter(parameter==input$input1) 
      
      # create a color palette 
      pal <- colorNumeric("plasma", map_ep$rate, n=5)
      
      # creat the map
      map_ep %>% 
        leaflet() %>% 
        addProviderTiles(providers$Esri.WorldImagery ) %>%
        addCircleMarkers(~longitude, 
                         ~latitude,
                         color= ~pal(rate),
                         popup = ~rate, 
                         label=~station_name,
                         stroke = FALSE, 
                         fillOpacity = 0.95) %>% 
        addLegend("bottomright", pal = pal, values = ~rate,
                  title = paste('End-point rate:', legend_label),
                  opacity = 1)   
    }
  })



  #PLOT OUTPUT#  
  ## Two types: 1) current conditions instantaneous values time series; 2) long-term trends yearly means time series  
  ## type of output determined by conditional statement/input type selected
  output$plot1 <- renderPlot({
    if(input$input3=="Current Conditions"){
        
        # create a label for y axis based on parameter selected
        matchid <- input$input1 %>% match(parameters$parm_cd)
        ylab <- parameters$parm_nm[matchid]
        
        # create a reference line based on parameter selected
        ref_line <- site_means %>% filter(parameter==input$input1) %>% select(site_no, mean)
        
        # create a legend
        vars <- c("instantaneous value"="steelblue", "station global mean"="grey10")
      
      # make the plot    
      data() %>%
        filter(site_no==input$input2) %>% 
        left_join(ref_line, by="site_no")  %>% 
        ggplot(aes(x=dateTime, y=parameter, color="instantaneous value")) + 
          geom_line(size=2) +
          geom_hline(aes(yintercept=mean, color="station global mean"), linetype="dashed")+
          labs(x='', y=ylab) +
          scale_colour_manual(name="", values=vars)+
          theme(legend.position="top") +
          facet_wrap(~site_nm, nrow=2)+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) 
        
    }
    else{ 
        # y label based on parameter selected 
        matchid <- input$input1 %>% match(parameters$parm_cd)
        ylab <- parameters$parm_nm[matchid]
        
        # make the plot       
        yearly_means %>%
          left_join(station_df, by="site_no") %>% 
          filter(parameter==input$input1, site_no %in% input$input2) %>% 
          ggplot(aes(x=year,y=val)) +
            geom_point(color='black', size=3) +
            labs(x='', y=ylab) +
            #geom_line(stat="smooth",method = "lm", alpha = 0.65, size=1.5, color='steelblue' ) +
            geom_smooth(method='lm', se=FALSE,size=1.5, color='steelblue')+
            facet_wrap(~site_nm,nrow=2)+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
  })
}

shinyApp(ui = ui, server = server) 