#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(scales)
library(leaflet.providers)
library(mapview)


part_1_il <- function(s_il_plants) {
    s_il_plants <- subset(s_il_plants, s_il_plants$Plant.state.abbreviation == "IL")
    m_il <- leaflet(s_il_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= s_il_plants$Plant.name, weight = 6, 
                                                                     radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources")
    return(m_il)
}
#func
part_2_years <- function(plants_year, input,zone){
    
    if(input$radio == "connected"){
        zone <- "1"
    }
    
    if(zone == "1"){
        if("All" %in% input$z1_energy){
            return(plants_year)
        }
        else if("Renewable" %in% input$z1_energy){
            print("in tha renew")
            s_renew <- subset(plants_year, primary_category == "HYDRO" | primary_category == "BIOMASS" | 
                                  primary_category == "WIND" | primary_category == "SOLAR" | primary_category == "GEOTHERMAL")
            return(s_renew)
            
        }
        else if("Non-Renewable" %in% input$z1_energy){
            s_non_renew <- subset(plants_year, primary_category == "COAL" | primary_category == "OIL" | 
                                      primary_category == "GAS" | primary_category == "NUCLEAR" | primary_category == "OTHER")
            return(s_non_renew)
        }
        else{
            s_energies <- subset(plants_year, primary_category %in% input$z1_energy)
            return(s_energies)
        }
    }
    else{
        if("All" %in% input$z2_energy){
            return(plants_year)
        }
        else if("Renewable" %in% input$z2_energy){
            print("in tha renew")
            s_renew <- subset(plants_year, primary_category == "HYDRO" | primary_category == "BIOMASS" | 
                                  primary_category == "WIND" | primary_category == "SOLAR" | primary_category == "GEOTHERMAL")
            return(s_renew)
            
        }
        else if("Non-Renewable" %in% input$z2_energy){
            s_non_renew <- subset(plants_year, primary_category == "COAL" | primary_category == "OIL" | 
                                      primary_category == "GAS" | primary_category == "NUCLEAR" | primary_category == "OTHER")
            return(s_non_renew)
        }
        else{
            s_energies <- subset(plants_year, primary_category %in% input$z2_energy)
            return(s_energies)
        }
    }
    
}

part_3_energies <- function(plants_year, input){
    if("None" %in% input$p3_energy){
        s <- subset(plants_year, primary_category == "hurrdurr")
        return(s)
    }
    else if("All" %in% input$p3_energy){
        return(plants_year)
    }
    else if("Renewable" %in% input$p3_energy){
        s_renew <- subset(plants_year, primary_category == "HYDRO" | primary_category == "BIOMASS" | 
                              primary_category == "WIND" | primary_category == "SOLAR" | primary_category == "GEOTHERMAL")
        return(s_renew)
        
    }
    else if("Non-Renewable" %in% input$p3_energy){
        s_non_renew <- subset(plants_year, primary_category == "COAL" | primary_category == "OIL" | 
                                  primary_category == "GAS" | primary_category == "NUCLEAR" | primary_category == "OTHER")
        return(s_non_renew)
    }
    else{
        s_energies <- subset(plants_year, primary_category %in% input$p3_energy)
        return(s_energies)
    }
}

all_plants <- read.csv("updated_2018_test.csv")
all_plants_2000 <- read.csv("2000_updated.csv")
all_plants_2010 <- read.csv("2010_updated.csv")


# 2018 pre-processing ****************************** ****************************** ******************************
names(all_plants)[5] <- "latitude"
names(all_plants)[6] <- "longitude"
all_plants <- all_plants[-1,]
all_plants$longitude <- sapply(all_plants$longitude, as.numeric)
all_plants$latitude <- sapply(all_plants$latitude, as.numeric)
all_plants <- subset(all_plants,is.na(all_plants$latitude) == FALSE)

energy_col <- c("Coal","Oil","Gas","Nuclear","Hydro","Biomass","Wind","Solar","Other_1",
                "Other_2","total_non_renewable","total_renewable","total_non-hydro_renewable",
                "total_combustion","total_non-combustion")
energy_col_2010 <- c("Coal","Oil","Gas","Nuclear","Hydro","Biomass","Wind","Solar","Other_1",
                "Other_2","total_non_renewable","total_renewable","total_generation")
names(all_plants)[7] <- "Coal"
names(all_plants)[8] <- "Oil"
names(all_plants)[9] <- "Gas"
names(all_plants)[10] <- "Nuclear"
names(all_plants)[11] <- "Hydro"
names(all_plants)[12] <- "Biomass"
names(all_plants)[13] <- "Wind"
names(all_plants)[14] <- "Solar"
names(all_plants)[15] <- "Geothermal"
names(all_plants)[16] <- "Other_1"
names(all_plants)[17] <- "Other_2"
names(all_plants)[18] <- "total_non_renewable"
names(all_plants)[19] <- "total_renewable"
names(all_plants)[20] <- "total_non-hydro_renewable"
names(all_plants)[21] <- "total_combustion"
names(all_plants)[22] <- "total_non-combustion"
names(all_plants)[23] <- "Coal_percent"
names(all_plants)[24] <- "Oil_percent"
names(all_plants)[25] <- "Gas_percent"
names(all_plants)[26] <- "Nuclear_percent"
names(all_plants)[27] <- "Hydro_percent"
names(all_plants)[28] <- "Biomass_percent"
names(all_plants)[29] <- "Wind_percent"
names(all_plants)[30] <- "Solar_percent"
names(all_plants)[31] <- "Geothermal_percent"
names(all_plants)[32] <- "other1_percent"
names(all_plants)[33] <- "other2_percent"
names(all_plants)[34] <- "non_renewable_percent"
names(all_plants)[35] <- "renewable_percent"
names(all_plants)[39] <- "primary_category"
all_plants[energy_col] <- lapply(all_plants[energy_col], gsub, pattern = ",", replacement = "")
all_plants[,7:22] <- sapply(all_plants[,7:22], as.numeric)
all_plants$total_generation = all_plants$total_non_renewable + all_plants$total_renewable
#all_plants$total_generation <- sapply(all_plants$total_generation, as.numeric)

all_plants$primary_category[all_plants$primary_category == "OTHF"] <- "OTHER"
all_plants$primary_category[all_plants$primary_category == "OFSL"] <- "OTHER"
#View(all_plants)
all_plants_new <- all_plants %>%
    mutate(type = case_when(
        Coal > 0 ~ 'Coal',
        Oil > 0 ~ 'Oil',
        Gas > 0 ~ 'Gas',
        Nuclear > 0 ~ 'Nuclear',
        Hydro > 0 ~ 'Hydro',
        Biomass > 0 ~ 'Biomass',
        Wind > 0 ~ 'Wind',
        Solar > 0 ~ 'Solar',
        Geothermal > 0 ~ 'Geothermal',
        total_renewable > 0 ~ 'Renewable'
    ))

all_plants_new_n <- all_plants_new %>%
    mutate(r_type = case_when(
        total_renewable > 0 ~ 'Renewable',
        total_non_renewable > 0 ~ 'Non-Renewable'
    ))
test_a_p <- all_plants_new_n
test_a_p <- subset(test_a_p, test_a_p$total_generation > 500 & test_a_p$total_generation < 1000)
#View(test_a_p)
print("max of 2018")
print(max(all_plants$total_generation, na.rm = TRUE))
# 2000 pre-processing ****************************** ****************************** ******************************

names(all_plants_2000)[3] <- "State"
names(all_plants_2000)[4] <- "Plant.name"
names(all_plants_2000)[5] <- "latitude"
names(all_plants_2000)[6] <- "longitude"
names(all_plants_2000)[7] <- "primary_category"
names(all_plants_2000)[8] <- "primary_fuel_category"
names(all_plants_2000)[9] <- "total_generation"
names(all_plants_2000)[10] <- "Coal"
names(all_plants_2000)[11] <- "Oil"
names(all_plants_2000)[12] <- "Gas"
names(all_plants_2000)[13] <- "Nuclear"
names(all_plants_2000)[14] <- "Hydro"
names(all_plants_2000)[15] <- "Biomass"
names(all_plants_2000)[16] <- "Wind"
names(all_plants_2000)[17] <- "Solar"
names(all_plants_2000)[18] <- "Geothermal"
names(all_plants_2000)[19] <- "Other_1"
names(all_plants_2000)[20] <- "Other_2"
names(all_plants_2000)[21] <- "total_non_renewable"
names(all_plants_2000)[22] <- "total_renewable"

names(all_plants_2000)[24] <- "Coal_percent"
names(all_plants_2000)[25] <- "Oil_percent"
names(all_plants_2000)[26] <- "Gas_percent"
names(all_plants_2000)[27] <- "Nuclear_percent"
names(all_plants_2000)[28] <- "Hydro_percent"
names(all_plants_2000)[29] <- "Biomass_percent"
names(all_plants_2000)[30] <- "Wind_percent"
names(all_plants_2000)[31] <- "Solar_percent"
names(all_plants_2000)[32] <- "Geothermal_percent"
names(all_plants_2000)[33] <- "other1_percent"
names(all_plants_2000)[34] <- "other2_percent"
names(all_plants_2000)[35] <- "non_renewable_percent"
names(all_plants_2000)[36] <- "renewable_percent"
all_plants_2000 <- all_plants_2000[-1,]
all_plants_2000 <- all_plants_2000[-1,]
all_plants_2000 <- all_plants_2000[-1,]

all_plants_2000$longitude <- sapply(all_plants_2000$longitude, as.numeric)
all_plants_2000$latitude <- sapply(all_plants_2000$latitude, as.numeric)
all_plants_2000$total_generation <- sapply(all_plants_2000$total_generation, as.numeric)
all_plants_2000 <- subset(all_plants_2000,is.na(all_plants_2000$latitude) == FALSE)
all_plants_2000[,9:23] <- sapply(all_plants_2000[,9:23], as.numeric)

all_plants_new_2000 <- all_plants_2000 %>%
    mutate(type = case_when(
        primary_category == 'BIT' | primary_category == 'COL'|primary_category == 'LIG' | primary_category == 'SUB'|primary_category == 'BFG' | primary_category == 'WOC'|primary_category == 'EF' ~ 'COAL',
        primary_category == 'OIL' | primary_category == 'DFO'|primary_category == 'PC' | primary_category == 'KER'|primary_category == 'RFO' | primary_category == 'OO'|primary_category == 'JF' ~ 'OIL',
        primary_category == 'GAS' | primary_category == 'NG' ~ 'GAS',
        primary_category == 'UR' |primary_category == 'NUC'  ~ 'NUCLEAR',
        primary_category == 'WT'|primary_category == 'WAT'  ~ 'HYDRO',
        primary_category == 'BL' | primary_category == 'OBG' | primary_category == 'WT' | primary_category == 'WDS' |primary_category == 'LFG' | primary_category == 'MWC' |primary_category == 'AB' |primary_category == 'WDL' |primary_category == 'BIOMASS' ~ 'BIOMASS',
        primary_category == 'WND' |primary_category == 'WN'  ~ 'WIND',
        primary_category == 'SL' | primary_category == 'SUN' ~ 'SOLAR',
        primary_category == 'GEO' |primary_category == 'GE'  ~ 'GEOTHERMAL',
        primary_category == 'WH' |primary_category == 'MSW' |primary_category == 'OTG'|primary_category == 'OG'  ~ 'OTHER'
    ))

names(all_plants_new_2000)[7] <- "oldie_prime"
names(all_plants_new_2000)[37] <- "primary_category"

all_plants_new_n_2000 <- all_plants_new_2000 %>%
    mutate(r_type = case_when(
        total_renewable > 0 ~ 'Renewable',
        total_non_renewable > 0 ~ 'Non-Renewable'
    ))
#View(all_plants_new_n_2000)
all_plants_new_n_2000$longitude = all_plants_new_n_2000$longitude*(-1)
#View(all_plants_new_n_2000)
print("max of 2000")
print(max(all_plants_2000$total_generation, na.rm = TRUE))
# 2010 pre-processing  ****************************** ****************************** ******************************
names(all_plants_2010)[2] <- "State"
names(all_plants_2010)[3] <- "Plant.name"
names(all_plants_2010)[8] <- "latitude"
names(all_plants_2010)[9] <- "longitude"
names(all_plants_2010)[10] <- "primary_category"
names(all_plants_2010)[11] <- "total_generation"
names(all_plants_2010)[12] <- "Coal"
names(all_plants_2010)[13] <- "Oil"
names(all_plants_2010)[14] <- "Gas"
names(all_plants_2010)[15] <- "Nuclear"
names(all_plants_2010)[16] <- "Hydro"
names(all_plants_2010)[17] <- "Biomass"
names(all_plants_2010)[18] <- "Wind"
names(all_plants_2010)[19] <- "Solar"
names(all_plants_2010)[20] <- "Geothermal"
names(all_plants_2010)[21] <- "Other_1"
names(all_plants_2010)[22] <- "Other_2"
names(all_plants_2010)[23] <- "total_non_renewable"
names(all_plants_2010)[24] <- "total_renewable"

names(all_plants_2010)[26] <- "Coal_percent"
names(all_plants_2010)[27] <- "Oil_percent"
names(all_plants_2010)[28] <- "Gas_percent"
names(all_plants_2010)[29] <- "Nuclear_percent"
names(all_plants_2010)[30] <- "Hydro_percent"
names(all_plants_2010)[31] <- "Biomass_percent"
names(all_plants_2010)[32] <- "Wind_percent"
names(all_plants_2010)[33] <- "Solar_percent"
names(all_plants_2010)[34] <- "Geothermal_percent"
names(all_plants_2010)[35] <- "other1_percent"
names(all_plants_2010)[36] <- "other2_percent"
names(all_plants_2010)[37] <- "non_renewable_percent"
names(all_plants_2010)[38] <- "renewable_percent"

all_plants_2010 <- all_plants_2010[-1,]
all_plants_2010 <- all_plants_2010[-1,]
all_plants_2010 <- all_plants_2010[-1,]
all_plants_2010 <- all_plants_2010[-1,]

all_plants_2010$longitude <- sapply(all_plants_2010$longitude, as.numeric)
all_plants_2010$latitude <- sapply(all_plants_2010$latitude, as.numeric)
all_plants_2010 <- subset(all_plants_2010,is.na(all_plants_2010$latitude) == FALSE)
all_plants_2010[energy_col_2010] <- lapply(all_plants_2010[energy_col_2010], gsub, pattern = ",", replacement = "")
all_plants_2010[,11:24] <- sapply(all_plants_2010[,11:24], as.numeric)
all_plants_2010$primary_category[all_plants_2010$primary_category == "OTHRFOSL"] <- "OTHER"
all_plants_2010$primary_category[all_plants_2010$primary_category == "WSTHTOTPUR"] <- "OTHER"
#View(all_plants_2010)

print("max of 2010")
print(max(all_plants_2010$total_generation))

cof_types <- colorFactor(c("#1b9e77","#d95f02","#7570b3","#E0141B","#1f78b4","#b2df8a",
                           "#66c2a5","#fc8d62","#8da0cb","#353848"), 
                         levels = c("COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND",
                                    "SOLAR","GEOTHERMAL","OTHER"))
only_energies <-  c("COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND",
                    "SOLAR","GEOTHERMAL","OTHER")
energy_colors <- c("#1b9e77","#d95f02","#7570b3","#E0141B","#1f78b4","#b2df8a",
                   "#66c2a5","#fc8d62","#8da0cb","#353848")
il_plants <- subset(all_plants_new_n, all_plants_new_n$Plant.state.abbreviation == "IL")
map_il <- leaflet(il_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= il_plants$Plant.name, weight = 6, 
                                                              radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources")

#il_plants_2000 <- subset(all_plants_new_n_2000, all_plants_new_n_2000$State == "IL")
#View(il_plants_2000)
#View(all_plants_new_n)
print(unique(all_plants_new_n$primary_category))
#print(lapply(all_plants_new, class))
s <- unique(all_plants_new_n$Plant.state.abbreviation)
p3_state_list <- c("No States",s)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "United States Energy Sources"),
    dashboardSidebar(
        disable = FALSE,
        collapsed = FALSE,
        checkboxGroupInput(
            "z1_energy",
            "Energy Options",
            c(
                "All",
                "COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND",
                "SOLAR","GEOTHERMAL","OTHER",
                "Non-Renewable",
                "Renewable"
            ),
            inline = TRUE,
            selected = c("All")
        ),
        radioButtons("radio","Checkboxes connection",choices = c("connected","not connected"), selected = c("not connected")),
        checkboxGroupInput(
            "z2_energy",
            "Energy Options",
            c(
                "All",
                "COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND",
                "SOLAR","GEOTHERMAL","OTHER",
                "Non-Renewable",
                "Renewable"
            ),
            inline = TRUE,
            selected = c("All")
        ),
        "Zone 1",
        selectInput(
            "z1State",
            "Select State",
            choices = unique(all_plants_new_n$Plant.state.abbreviation),
            selected = "IL"
        ),selectInput("z1_year", "Select Year", choices = (c("2018","2000","2010")), width = '70%', selected = '2000'),
        selectInput("z1_map_type", "Select Map Type", choices = (c("Map 1","Map 2","Map 3")), width = '70%'),
        
        "Zone 2",
        selectInput(
            "z2State",
            "Select State",
            choices = unique(all_plants_new_n_2000$State),
            selected = "IL"
        ), 
        selectInput("z2_year", "Select Year", choices = (c("2018","2000","2010")), width = '70%', selected = '2018'),
        selectInput("z2_map_type", "Select Map Type", choices = (c("Map 1","Map 2","Map 3")), width = '70%')
    ),
    dashboardBody(
        tabsetPanel(
            type = "tabs",
            tabPanel(
                "Part 1",
                leafletOutput("ilMap"),
                actionButton("reset_button", "Reset the Map!")),
            tabPanel("Part 2",
                     h3("Zone 1"),
                     
                     leafletOutput("map_state_1"),
                     actionButton("z1_reset_button", "Reset the Zone 1 Map!"),
                     
                     h3("Zone 2"),
                     leafletOutput("map_state_2"),
                     actionButton("z2_reset_button", "Reset the Map!")),
            tabPanel("Part 3",
                     
                     actionButton("reset_button_p3", "Reset the Map!"),
                     leafletOutput("usMap"),
                     selectInput("p3_state","Select State",choices = p3_state_list, width = '70%' ,selected = 'No States'),
                     selectInput("p3_year", "Select Year", choices = (c("2018","2000","2010")), width = '70%'),
                     splitLayout(sliderInput("min_int", "Minimum Generation:",
                                             min = 0, max = 32000000,
                                             value = 0),
                                 sliderInput("max_int", "Maximum Generation:",
                                             min = 0, max = 32000000,
                                             value = 32000000)
                                
                     ),
                     checkboxGroupInput(
                         "p3_energy",
                         "Energy Options",
                         c(
                             "None","All",
                             "COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND",
                             "SOLAR","GEOTHERMAL","OTHER",
                             "Non-Renewable",
                             "Renewable"
                         ),
                         inline = TRUE,
                         selected = c("None")
                     )
                     ),
           
             tabPanel(
                "About Page",
                mainPanel(
                    HTML(
                        paste(
                            h3("Thank you for using this app!"),'<br/>',
                            h4("This app contains the numbers of the various different types of energy sources categorized by State and US-Total.
                     It is written by Syed Raza and is Project 1 for the CS 424 class at UIC"),'<br/>',
                            h4("The data can be found through this link: https://www.eia.gov/electricity/data/state/")
                        )
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    observeEvent(input$reset_button_p3,{
        updateSliderInput(session, 'min_int',value = 0)
        updateSliderInput(session, "max_int",value = 32000000)
        updateSelectInput(session, "p3_state",selected = "No States")
        updateSelectInput(session, "p3_year",selected = "2018")
        updateCheckboxGroupInput(session, "p3_energy",selected = "None")
    })
    
    observeEvent(input$z1_reset_button,{
        updateSelectInput(session, "z1_year", selected = "2000")
        updateSelectInput(session, "z1State",selected = "IL")
        updateCheckboxGroupInput(session, "z1_energy",selected = "All")
    })
    
    observeEvent(input$z2_reset_button,{
        updateSelectInput(session, "z2_year", selected = "2018")
        updateSelectInput(session, "z2State",selected = "IL")
        updateCheckboxGroupInput(session, "z2_energy",selected = "All")
    })
    energyReactive <-
        reactive({
            subset(all_plants_new_n, all_plants_new_n$primary_category %in% input$z1_energy)
        })
    renew_energyReactive <-
        reactive({
            subset(all_plants_new_n, all_plants_new_n$primary_category == "HYDRO" | all_plants_new_n$primary_category == "BIOMASS" | 
                       all_plants_new_n$primary_category == "WIND" | all_plants_new_n$primary_category == "SOLAR" | all_plants_new_n$primary_category == "GEOTHERMAL")
        })
    non_renew_energyReactive <-
        reactive({
            subset(all_plants_new_n, all_plants_new_n$primary_category == "COAL" | all_plants_new_n$primary_category == "OIL" | 
                       all_plants_new_n$primary_category == "GAS" | all_plants_new_n$primary_category == "NUCLEAR" | all_plants_new_n$primary_category == "OTHER")
        })
    z1_stateReactive_2018 <-
        reactive({
            subset(all_plants_new_n,all_plants_new_n$Plant.state.abbreviation == input$z1State)
        })
    z1_stateReactive_2000 <-
        reactive({
            subset(all_plants_new_n_2000, all_plants_new_n_2000$State == input$z1State)
        })
    z1_stateReactive_2010 <-
        reactive({
            subset(all_plants_2010, all_plants_2010$State == input$z1State)
        })
    z2_stateReactive_2018 <-
        reactive({
            subset(all_plants_new_n,all_plants_new_n$Plant.state.abbreviation == input$z2State)
        })
    z2_stateReactive_2000 <-
        reactive({
            subset(all_plants_new_n_2000, all_plants_new_n_2000$State == input$z2State)
        })
    z2_stateReactive_2010 <-
        reactive({
            subset(all_plants_2010, all_plants_2010$State == input$z2State)
        })
    p3_sliderReactive_2018 <- reactive({
        subset(all_plants_new_n, all_plants_new_n$total_generation > input$min_int & all_plants_new_n$total_generation < input$max_int )
    })
    p3_sliderReactive_2000 <- reactive({
        subset(all_plants_new_n_2000, total_generation > input$min_int & total_generation < input$max_int )
    })
    p3_sliderReactive_2010 <- reactive({
        subset(all_plants_2010, total_generation > input$min_int & total_generation < input$max_int )
    })

    output$ilMap <- renderLeaflet({
        if("All" %in% input$z1_energy){
            #View(il_plants)
            m_il <- leaflet(il_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= il_plants$Plant.name, weight = 6, 
                                                                            radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources")
            m_il}
        else if("Renewable" %in% input$z1_energy){
            s_il_plants <- renew_energyReactive()
            m_il <- part_1_il(s_il_plants)
            m_il
            }
        else if("Non-Renewable" %in% input$z1_energy){
            s_il_plants <- non_renew_energyReactive()
            m_il <- part_1_il(s_il_plants)
            m_il}
        else{
            s_il_plants <- energyReactive()
            m_il <- part_1_il(s_il_plants)
            m_il
            }
        if(input$reset_button%%1 == 0){
            m_il
        }
    })
    
    
    #func
    z1_fullReactive <- reactive({
        if(input$z1_year == "2018"){
            print("in the if statement")
            plants_2018 <- z1_stateReactive_2018()
            k <- part_2_years(plants_2018, input,"1")
            return(k)
        } 
        else if (input$z1_year == "2000"){
            plants_2000 <- z1_stateReactive_2000()
            k <- part_2_years(plants_2000, input,"1")
            return(k)
        }
        else if (input$z1_year == "2010"){
            plants_2010 <- z1_stateReactive_2010()
            k <- part_2_years(plants_2010, input,"1")
            return(k)
        }
    })
    
    z2_fullReactive <- reactive({
        if(input$z2_year == "2018"){
            plants_2018 <- z2_stateReactive_2018()
            k <- part_2_years(plants_2018, input,"2")
            return(k)
        } 
        else if (input$z2_year == "2000"){
            plants_2000 <- z2_stateReactive_2000()
            k <- part_2_years(plants_2000, input,"2")
            return(k)
        }
        else if (input$z2_year == "2010"){
            plants_2010 <- z2_stateReactive_2010()
            k <- part_2_years(plants_2010, input,"2")
            return(k)
        }
    })
    
    p3_fullReactive <- reactive({
        if(input$p3_year == "2018"){
            plants_2018 <- p3_sliderReactive_2018()
            if(input$p3_state != "No States"){
                plants_2018 <- subset(plants_2018,plants_2018$Plant.state.abbreviation == input$p3_state)
            }
            plants_2018 <- part_3_energies(plants_2018,input)
            return(plants_2018)
        } 
        else if (input$p3_year == "2000"){
            plants_2000 <- p3_sliderReactive_2000()
            if(input$p3_state != "No States"){
                plants_2000 <- subset(plants_2000,plants_2000$State == input$p3_state)
            }
            plants_2000 <- part_3_energies(plants_2000,input)
            return(plants_2000)
        }
        else if (input$p3_year == "2010"){
            plants_2010 <- p3_sliderReactive_2010()
            if(input$p3_state != "No States"){
                plants_2010 <- subset(plants_2010,plants_2010$State == input$p3_state)
            }
            plants_2010 <- part_3_energies(plants_2010,input)
            return(plants_2010)
        }
    })
    
    
    
    output$map_state_1 <- renderLeaflet({
            z1_all_plants <- z1_fullReactive()
            if(input$z1_map_type == "Map 2"){
                maps <- leaflet(z1_all_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= ~Plant.name, weight = 6, 
                                                                                   radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources") %>% addProviderTiles(providers$CartoDB.Positron)

            }
            else if(input$z1_map_type == "Map 3"){
                maps <- leaflet(z1_all_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= ~Plant.name, weight = 6, 
                                                                                   radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources") %>% addProviderTiles(providers$Stamen.Toner)

            }
            else{
                maps <- leaflet(z1_all_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= ~Plant.name, weight = 6, 
                                                                                   radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources")
            }
            maps
    })

    output$map_state_2 <- renderLeaflet({
        
        z2_all_plants <- z2_fullReactive()
        if(input$z2_map_type == "Map 2"){
            maps <- leaflet(z2_all_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= ~Plant.name, weight = 6, 
                                                                               radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources") %>% addProviderTiles(providers$CartoDB.Positron)
            
        }
        else if(input$z2_map_type == "Map 3"){
            maps <- leaflet(z2_all_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= ~Plant.name, weight = 6, 
                                                                               radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources") %>% addProviderTiles(providers$Stamen.Toner)
            
        }
        else{
            maps <- leaflet(z2_all_plants) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= ~Plant.name, weight = 6, 
                                                                               radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources")
        }
        maps
    })
    
    
    output$usMap <- renderLeaflet({
        map <- p3_fullReactive()
        m_il <- leaflet(map) %>% addTiles() %>% addCircleMarkers(~longitude,~latitude, popup= ~paste("Plant Name: ", Plant.name,"<br>",
                                                                                                          "Generation: ", total_generation,"<br>",
                                                                                                          "Non-Renewable (%): ", non_renewable_percent,"<br>",
                                                                                                          "Renewable (%): ", renewable_percent,"<br>"), weight = 6, 
                                                                      radius= ~rescale(total_generation, to = c(6,12)),color=~cof_types(primary_category), stroke = FALSE, fillOpacity = 0.8) %>% addLegend("bottomright", colors = energy_colors, labels=only_energies, title="Energy Sources") %>% setView(-97.48572,38.96384, zoom = 4)
        m_il

        

        
        if(input$reset_button_p3%%1 == 0){
            m_il
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
