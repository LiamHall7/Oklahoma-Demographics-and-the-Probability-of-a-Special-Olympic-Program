
#OKLAHOMA DEMOGRAPHICS SHINY APP

library(readr)
library(ggforce)
library(ggplot2)
library(tidyverse)
library(ggforce)
library(dplyr)
library(leaflet)
library(shinythemes)
library(dplyr)
library(ipumsr)
library(sf)
library(readxl)
library(tools)

all_location <- read_rds("all.RDS")
all_school <- read_rds("school.RDS")


# Define UI for application that draws a histogram

ui <-   navbarPage("Oklahoma School District Demographics",
                   theme = shinytheme("cosmo"),
                   windowTitle = "Oklahoma School Demographics", 
                   
                   #above section names the app, sets the theme of the app
                   #design, and names the tab on the browser.
                   
  navbarMenu("Demographic Maps by School District",    

             #names the tab that has the drop downs named below.
             
    tabPanel("Racial and Ethnic Groups",
             
             h3(strong("Welcome to my Final Project!")), 
             p("Below is an interactive plot that will map different factors
               and their respective densities throughout the state of
               Oklahoma."),
             
             #sidebar creates the drop down for different variables.
             
                  sidebarLayout(
                    selectInput("group", label = "Group",
                                choices = c("Total Population",
                                            "Native American",
                                            "Native American (Percent)",
                                            "White",
                                            "White (Percent)",
                                            "Black",
                                            "Black (Percent)",
                                            "Asian",
                                            "Asian (Percent)",
                                            "Mixed Race",
                                            "Mixed Race (Percent)",
                                            "Pacific Islander",
                                            "Pacific Islander (Percent)")),
                    
                    mainPanel(
                  leafletOutput("na_map", width = "100%")))),
                 
    tabPanel("Economic Status", 
             
             #This section is the same as above.
             
             h3(strong("Welcome to my Final Project!")), 
             p("Below is an interactive plot that will map different factors
               and their respective densities throughout the state of
               Oklahoma."),
             
                  sidebarLayout(
                    selectInput("econ", label = "Economic",
                                choices = c("Median Household Income",
                                            "Per Capita Income")),
            
            mainPanel(
                leafletOutput("econ_map", width = "100%"))))),
    

    tabPanel("Model",
             fluidPage(
                 h3("Welcome to my Model page!"),
                 
                 #the p() argument is normal text. The different h() arguments
                 #create titles or "headlines" of a certain size, so you can
                 #have multiple h3() functions. The number isn't for listing the 
                 #number of headlines, but to determine the size of those 
                 #headlines.
                 
                 p("My model predicts the likelihood 
                 that a given public school in the state of Oklahoma has a 
                 Special Olympics Unified Champion (UCS) School Program based on the
                 the median household income and per capita income of the
                 school's district as well as the percentage of Native American
                 students in that school. I used a logistic regression model
                 because the presence of Special Olympics in the school is a
                 binary indicator. If you're more interested in how log regressions
                 work, this",
                   
                  #the a() line hyperlinks text. 
                   
                  a("site", href = "https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/"),
                 
                 "was very helpful."),
                
              
                 p("The two graphs below show two potential
                 schools. The", 
                 strong("top plot"),
                 
                 #strong() makes the text bold. em() italicizes the text. This 
                 #is different from R markdown where * around words are used to
                 #get these effects.
                 
                 "predicts the likelihood of UCS in a school
                 with a median household and per capita income of $100,000. The",
                 strong("bottom graph"),
                 "predicts the likelihood of UCS in a school with
                 a median household income and per capita income equivalent to 
                 the state median ($47,344 and $23,985, respectively)."),
                 
                 #the new p() adds a break between the previous text and the new
                 #text
                 
                 p("As you may notice from the y-axis, my model suggests a very
                   small likelihood that the UCS Program will be in any school
                   in Oklahoma. Moreover, there is a clear negative
                   relationship between likelihood of a UCS Program and the 
                   proportion of Native Americans in that school district, 
                   regardless of the economic attributes of the school. The
                   model suggests that the likelihood of a UCS Program in a 
                   school decreases as the percentage of Native Americans within
                   the school increases."),
                 
                 
                 #watch the parentheses, make sure they line up and close around
                 #the right sections or things won't run, or won't run properly
                 
                 mainPanel(
                     plotOutput("hundkmodel"),
                     br(),
                    
                     #the br() adds a break between lines or plots, etc.
                     
                     plotOutput("medianmodel")
             ))),
    
    tabPanel("About",
             
             fluidPage(
                 h3("Project Background and Motivations"),
                 p("Hello lost traveler! Welcome to my final project! I looked
             at how racial demographics, particularly Native American
             populations, correlated with median household income by school 
             district around the state of Oklahoma. All of my data, like the
             ingredients in most environmentally-conscious 
             restaurants, are from local, sustainable sources. 
             And by that I mean the American Community Survey's publicly
             accessible datasets, available through",
                   
               a("NHGIS.", href = "https://data2.nhgis.org/"),
               
             "Because school districts
             are relatively small in
             Oklahoma (and school demographic information was hard to publicly
             access by individual school), I'm examining trends between race and
             income by school district."),
             
             h3("Some Notes About the Data"),
             p("All of my data come from NHGIS, as mentioned above, and the
               datasets from the ACS (gathered via NHGIS) ultimately included only
               513 school districts out of the 
               537 that exist in Oklahoma according to online sources. That
               said, with the 513 school districts, I was still able to map 
               the entire state of Oklahoma by school district without any gaps
               in my map. As I alluded to earlier as well, school data would yield
               a more granular assessment of trends between demographics and 
               economic status, but those data were too difficult to gather and 
               bind effectively."),
             
             
             h3("About Me"),
             p("My name is Liam Hall. I concentrate in Social Studies at
             Harvard. You can reach me at liam_hall@college.harvard.edu.
             This is a link to my", 
              
              
              a("repo.", href = "https://github.com/LiamHall7/final_project"))),

    ))
    
    
    #you can read in different leaflets to show different outputs, or use the
#strategy I used below to substitute inputs.

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$na_map <- renderLeaflet({
        
        if(input$group == "Total Population") {
            x = all_location$total_pop
            y = 1
            w = "Population"
        }
        
        #the first one is if, the others are else if because of how shiny runs
            
        else if(input$group == "Native American") {
            x = all_location$native_amer
            y = 1
            w = "Population"
        }
        
        else if(input$group == "White") {
            x = all_location$white
            y = 1
            w = "Population"
        }
        
        else if(input$group == "Black") {
            x = all_location$black
            y = 1
            w = "Population"
        }
        
        else if(input$group == "Asian") {
            x = all_location$asian
            y = 1
            w = "Population"
        }
       
        else if(input$group == "Mixed Race") {
            x = all_location$two_or_more
            y = 1
            w = "Population"
        }
        
        else if(input$group == "Pacific Islander") {
            x = all_location$pac_islander
            y = 1
            w = "Population"
        }
        
        else if(input$group == "Native American (Percent)") {
            x = all_location$na_percent
            y = 100
            w = "Percent"
        }
        
        else if(input$group == "White (Percent)") {
            x = all_location$white_percent
            y = 100
            w = "Percent"
        }
        
        else if(input$group == "Black (Percent)") {
            x = all_location$black_percent
            y = 100
            w = "Percent"
        }
        
        else if(input$group == "Asian (Percent)") {
            x = all_location$asian_percent
            y = 100
            w = "Percent"
        }
        
        else if(input$group == "Mixed Race (Percent)") {
            x = all_location$mixed_race_percent
            y = 100
            w = "Percent"
     }
        
        else if(input$group == "Pacific Islander (Percent)") {
            x = all_location$pi_percent
            y = 100
            w = "Percent"
     
        }
               
        pal <- colorNumeric("viridis", NULL)
        
        #viridis sets up the color palette (that's what pal is short for).
 
        leaflet(all_location) %>%
            addTiles() %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                        fillColor = ~ pal(x)) %>%
            addLegend(pal = pal, values = ~ x * y, title = w, opacity = 1.0)
    })
        
        
    output$econ_map <- renderLeaflet({
        
        if(input$econ == "Median Household Income") {
            x = all_location$median_house_income
            y = "Income"
        }
        if(input$econ == "Per Capita Income") {
            x = all_location$capita_income
            y = "Income"
        }
        
        pal <- colorNumeric("viridis", NULL)
        case_when
            leaflet(all_location) %>%
            addTiles() %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                        fillColor = ~ pal(x)) %>%
            addLegend(pal = pal, values = ~ x * 10000, title = y,
                      position = "topright",  opacity = 1.0)
                
        
    })
# 
    output$hundkmodel <- renderPlot({
        
        # posterior <-
        #     stan_glm(data = all_school,
        #              formula = sook ~ median_house_income + capita_income + 
        #                        na_percent,
        #              refresh = 0,
        #              family = binomial())
        
        #commented out the posterior above because it isn't used as an input
        #itself in the below code. All that is needed to graph are the values 
        #from the posterior output, which can be done in the gather.rmd. 
        
        tibble(NA_Pct=1:100, Intercept = -2.99636) %>%
            mutate(Probability = exp(Intercept + (NA_Pct * -4.67385) + (.00002 * 10) + (.00001 * 10))/
                       (1 + exp(Intercept + (NA_Pct * -4.67385) + (.00002 * 10) + (.00001 * 10)))) %>% 
            ggplot(aes(NA_Pct, Probability)) + 
            geom_line() +
            theme_classic() +
            scale_y_continuous(labels = scales::percent) +
            xlim(0,10) +
            labs(title = "Predicted Probability of Special Olympic Unified Champion
                 School (UCS) Program Status",
                 x = "Percentage of Native Americans Among students",
                 y = "Probability of Special Olympic UCS") 
        
            })
#     output$table <- renderTable({table})
 
    
    
    output$medianmodel <- renderPlot({
        
        tibble(NA_Pct=1:100, Intercept = -2.99636) %>%
            mutate(Probability = exp(Intercept + (NA_Pct * -4.67385) + (.00002 * 2.3985) + (.00001 * 4.7344))/
                       (1 + exp(Intercept + (NA_Pct * -4.67385) + (.00002 * 2.3985) + (.00001 * 4.7344)))) %>% 
            ggplot(aes(NA_Pct, Probability)) + 
            geom_line() +
            theme_classic() +
            scale_y_continuous(labels = scales::percent) +
            xlim(0,10) +
            labs(title = "Predicted Probability of Special Olympic Unified Champion
                 School (UCS) Program Status",
                 x = "Percentage of Native Americans Among students",
                 y = "Probability of Special Olympic UCS") 
        
        
        
        
    })

}


shinyApp(ui = ui, server = server)
