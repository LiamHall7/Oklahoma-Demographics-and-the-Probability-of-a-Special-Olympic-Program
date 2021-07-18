
#OKLAHOMA DEMOGRAPHICS SHINY APP

library(readr)
library(ggforce)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(leaflet)
library(gridExtra)
library(shinythemes)
library(dplyr)
library(sf)
library(shiny)
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
             
             
             h3(strong("Mapping Oklahoma Racial and Ethnic Demographics"), align = "center"), 
             p("The interactive plot below maps different demographics
               and their respective densities throughout the state of
               Oklahoma.", align = "center"),
             
             #sidebar creates the drop down for different variables.
             
                         sidebarLayout(
                           sidebarPanel( 
                                    selectInput("group", label = "Select a Group",
                                        choices = c("Total Population",
                                                    "Native American",
                                                    "Native American (Percent)",
                                                    "White",
                                                    "White (Percent)",
                                                    "Black",
                                                    "Black (Percent)",
                                                    "Hispanic",
                                                    "Hispanic (Percent)",
                                                    "Asian",
                                                    "Asian (Percent)",
                                                    "Mixed Race",
                                                    "Mixed Race (Percent)",
                                                    "Pacific Islander",
                                                    "Pacific Islander (Percent)"))),
                            
                            mainPanel(align = "center",
                  leafletOutput("na_map", width = "100%")))),
                 
    tabPanel("Economic Status", 
             
             #This section is the same as above.
             
             h3(strong("Mapping Oklahoma Economic Demographics"), align = "center"), 
             p("The interactive plot below maps different demographics
               and their respective densities throughout the state of
               Oklahoma.", align = "center"),
             
                  sidebarLayout(
                    sidebarPanel(
                    selectInput("econ", label = "Select an Index",
                                choices = c("Median Household Income",
                                            "Per Capita Income"))),
            
            mainPanel(align = "center",
                leafletOutput("econ_map", width = "100%"))))),
  
     tabPanel("Special Olympics Schools", position = "center",
                h3(strong("Special Olympics UCS Presence in Oklahoma"), align = "center"),
              
              
               
                  p("The interactive plot below shows the school districts with (and without)
                     at least one Unified Champion School (UCS) program.", align = "center"),
              
#              mainPanel(align = "center",
                   fluidRow(align = "center",     
                   
                leafletOutput("sook_map", width = "90%"))),
     

      tabPanel("Models by Income",
                 h2("Predicted Probability of a Unified Champion School (UCS) program by Income", align = "center"),
               br(),

                 #the p() argument is normal text. The different h() arguments
                 #create titles or "headlines" of a certain size, so you can
                 #have multiple h3() functions. The number isn't for listing the
                 #number of headlines, but to determine the size of those
                 #headlines.

                 p("The graphs below show the predicted probability that a given
                 school district in the state of Oklahoma has a Special Olympics Unified
                 Champion School (UCS) program, given the district's percentage
                 of Native American students, at two different income levels.
                 The graph on the left assumes the school district has an income level
                 of $100,000, while the graph on the right assumes the district
                 has an income level equal to the state median. (The median 
                 household income in Oklahoma is $47,344, and the median per capita
                 income is $23,985.)", align = "center"),

                 #the new p() adds a break between the previous text and the new
                 #text

                 p("Paying close attention to the y-axis labels on both graphs, you may notice
                 that the model predicts that there is a very small chance that
                 a school district with even a very small number of Native American
                 students also has a UCS program. By comparing the two graphs,
                 we can see that income level is not a highly influential indicator
                 of whether a school with Native American students has a UCS program.
                 That said, there is a clear negative relationship between probability
                 of a UCS program and percentage of students that are Native American,
                 meaning that as the percentage of Native American
                 students increases, the chance that the school district has a UCS program
                 decreases.", align = "center"),
               br(),


                 #watch the parentheses, make sure they line up and close around
                 #the right sections or things won't run, or won't run properly

               h4(strong("Predicted Probability of Special Olympic Unified Champion
                 School (UCS) program by Percent of Native American Students"), align = "center"),
               
                 plotOutput("econ_models", width = "100%", height = "400px")),
                  

      tabPanel("Models by Race",
               
                     h2("Predicted Probability of Unified Champion School (UCS) program by Race"),
               
                     br(),
               
                     p("The plots below show the predicted likelihood
                     that a given school district in the state of Oklahoma has a
                     Special Olympics Unified Champion School (UCS) program,
                     based on the median household and per capita income, as
                     well as the racial demographics, of that
                     school district."),

                     br(),

                     h4(strong("Predicted Probability of Special Olympic Unified Champion
                     School (UCS) program By Student Race")), align = "center",
               
               #the align = "center" line above makes the whole tab centered
               
                     plotOutput("three_model", width = "100%", height = "300px"),
                    
                     br(),
                     br(),
                     
                     plotOutput("two_model", width = "67%", height = "300px")
               ),
               
                     # h4("Native American"),
                     # br(),
                     #         
                     # plotOutput("nativemodel"),
                     # br(),
                     # 
                     # h4("White"),
                     # plotOutput("whitemodel"),
                     # br(),
                     # 
                     # h4("Hispanic"),
                     # plotOutput("hispmodel"),
                     # br(),
                     # 
                     # h4("Black"),
                     # plotOutput("blackmodel"),
                     # br(),
                     # 
                     # h4("Asian"),
                     # plotOutput("asianmodel")),

    tabPanel("About",
             h3("Project Focus"),
             
             p("This project looks at how economic and racial demographics by
                    school district correlate with the presence of Special
                    Olympics Unified Champion School (UCS) programs. In other words,
                    this project examines what racial and economic demographics
                    are most likely to have a UCS program in their school district.
                    Because the majority of school districts in Oklahoma include only a handful
                    (or, in some cases, just a single) school, this project examines
                    trends between race and income at the school district level."),
             
             h3("About the Models"),
             
             p("I used a logistic regression model to make the predictions included
               in this project, due to the model's statistical ability to accurate predict binary outcomes
               (which, in this case, was whether or not there was a UCS program
               in the school district or not). This",
             
             a("site", href = "https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/"),
             
             "offers an overview of how logistic regressions work and how I
             calculated the log odds ratios to interpret these models."),
             
             h3("Data"),
                 
             p("All of my data come from the American Community Survey's publicly accessible datasets,
                available through",

               a("NHGIS", href = "https://data2.nhgis.org/", ),
               
                 "and the list of Special Olympics UCS schools in Oklahoma comes
                  from the the Special Olympics",
               
               a("UCS website.", href = "https://resources.specialolympics.org/community-building/youth-and-school/unified-champion-schools")),

            p("The datasets from the American Community Survey (gathered via NHGIS)
            ultimately included only 513 school districts out of the 537 that exist in Oklahoma,
               according to online sources. That said, with the 513 school districts,
               I was still able to fully map the entire state of Oklahoma by school district
               without any gaps."),
          
            h3("About Me"),
             p("My name is Liam Hall. I concentrate in Social Studies at
             Harvard and work in the Legal Office at Special Olympics International.
             You can reach me at liam_hall@college.harvard.edu
             or at lhall@specialolympics.org. This is a link to my ",


              a("project repository.", href = "https://github.com/LiamHall7/Oklahoma-Demographics-and-the-Probability-of-a-Special-Olympic-Program"))))


    #you can read in different leaflets to show different outputs, or use the
#strategy I used below to substitute inputs.

served <- function(input, output, session) {
    
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
      
      
        else if(input$group == "Hispanic") {
            x = all_location$hispanic
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
        else if(input$group == "Hispanic (Percent)") {
            x = all_location$hisp_percent
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
        else if(input$econ == "Per Capita Income") {
            x = all_location$capita_income
            y = "Income"
        }
        
        pal <- colorNumeric("viridis", NULL)
        # case_when
            leaflet(all_location) %>%
            addTiles() %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                        fillColor = ~ pal(x)) %>%
            addLegend(pal = pal, values = ~ x * 10000, title = y,
                      position = "topright",  opacity = 1.0)
                
        
     })

    output$sook_map <- renderLeaflet({

      factpal <- colorFactor(topo.colors(5), all_location$sook)
      
      labels <- c("No", "Yes")

      leaflet(all_location) %>%
        addTiles() %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                    fillColor = ~ factpal(sook)) %>%
        addLegend(pal = factpal, values = ~ sook, title = "Has UCS",
                  position = "topright",
                  opacity = 1.0,
                  
                  
                  labFormat = function(type, cuts, p){
                    paste0(labels)
                  
                    #this function uses the labels list for the legend labels
                      
                  })

    })

    output$econ_models <- renderPlot({

        # posterior <-
        #     stan_glm(data = all_school,
        #              formula = sook ~ median_house_income + capita_income +
        #                        na_percent,
        #              refresh = 0,
        #              family = binomial())

        #commented out the posterior above because it isn't used as an input
        #itself in the below code. All that is needed to graph are the values
        #from the posterior output, which can be done in the gather.rmd.
      
      
    hundk_model<- 
      
        tibble(NA_Pct = 1:100, Intercept = -2.97848) %>%
            mutate(Probability =

                 exp(Intercept + (NA_Pct * -4.66471) + (.00001 * 10) + (.00002 * 10))/

                 (1 + exp(Intercept + (NA_Pct * -4.66471) + (.00001 * 10) + (.00002 * 10)))) %>%

            ggplot(aes(NA_Pct, Probability)) +
            geom_line() +
            theme_classic() +
            scale_y_continuous(labels = scales::percent) +
            xlim(0,10) +
            labs(x = "Percentage of Native American Students",
                 y = "Probability of a UCS program",
                 title = "100k Income")  +
      
      theme(plot.title = element_text(hjust = .5, face = "bold"))

        # tibble(NA_Pct=1:100, Intercept = -2.99636) %>%
        #     mutate(Probability = exp(Intercept + (NA_Pct * -4.67385) + (.00002 * 10) + (.00001 * 10))/
        #                (1 + exp(Intercept + (NA_Pct * -4.67385) + (.00002 * 10) + (.00001 * 10)))) %>%
        #     ggplot(aes(NA_Pct, Probability)) +
        #     geom_line() +
        #     theme_classic() +
        #     scale_y_continuous(labels = scales::percent) +
        #     xlim(0,10) +
        #     labs(title = "Predicted Probability of Special Olympic Unified Champion
        #          School (UCS) Program Status",
        #          x = "Percentage of Native Americans Among students",
        #          y = "Probability of Special Olympic UCS")

        
#     output$table <- renderTable({table})

      median_model <- 
        tibble(NA_Pct=1:100, Intercept = -2.99636) %>%
            mutate(Probability = 
                     
                     exp(Intercept + (NA_Pct * -4.66471) + (.00001 * 2.4748) + (.00002 * 4.9380))/
                     
                     (1 + exp(Intercept + (NA_Pct * -4.66471) + (.00001 * 2.4748) + (.00002 * 4.9380)))) %>%
        
            ggplot(aes(NA_Pct, Probability)) +
            geom_line() +
            theme_classic() +
            scale_y_continuous(labels = scales::percent) +
            xlim(0,10) +
            labs(x = "Percentage of Native American Students",
                 y = "Probability of a UCS program", 
                 title = "Median Income")  +
        
        theme(plot.title = element_text(hjust = .5, face = "bold"))

      
      grid.arrange(hundk_model, median_model, ncol = 2)

     })

  output$three_model <- renderPlot({

     native_model <- 
       
          tibble(NA_Pct=1:100, Intercept = -2.75999) %>%
            mutate(Probability = exp(Intercept + (NA_Pct * -2.72479) + (.00003 * 2.4748) + (-.00005 * 4.9380) + (.00006 * 7637))/
                     (1 + exp(Intercept + (NA_Pct * -2.72479) + (.00003 * 2.4748) + (-.00005 * 4.9380) + (.00006 * 7637)))) %>%
            ggplot(aes(NA_Pct, Probability)) +
            geom_line() +
            theme_classic() +
            scale_y_continuous(labels = scales::percent) +
          
            scale_x_continuous(limits = c(0, 10),
                               labels = function(NA_Pct) {
                                 
                                 paste0(NA_Pct, "%")
                                 
                               }) +
            
            labs(x = "Native American student Percentage",
                 title = "Native American") +
       
       theme(plot.title = element_text(hjust = .5, face = "bold"))
     
     
                 #y = "Probability of Special Olympic UCS")
        
    
    white_model <-
      
          tibble(W_Pct = 1:100, Intercept = -3.39546) %>%
            mutate(Probability =
    
                     exp(Intercept + (W_Pct * -.00433) + (.00003 * 2.4748) + (-.00005 * 4.9380) + (.00006 * 7637))/
    
                     (1 + exp(Intercept + (W_Pct * -.00433) + (.00003 * 2.4748) + (-.00005 * 4.9380) + (.00006 * 7637)))) %>%
    
            ggplot(aes(W_Pct, Probability)) +
            geom_line() +
            theme_classic() +
    
            scale_y_continuous(limits = c(0, .08),
                               labels = scales::percent) +
    
            scale_x_continuous(limits = c(0, 10),
                               labels = function(NA_Pct) {
                                 
                                 paste0(NA_Pct, "%")
                                 
                               }) +
            
            labs(x = "White Student Percentage", 
                 title = "White") +
      
      theme(plot.title = element_text(hjust = .5, face = "bold"))
                 #y = "Probability of Special Olympic UCS")
        
    
    black_model <-
      
          tibble(B_Pct = 1:100, Intercept = -3.54074) %>%
            mutate(Probability =
    
                     exp(Intercept + (B_Pct * 2.81166) + (.00003 * 2.4748) + (-.00004 * 4.9380) + (.00006 * 7637))/
    
                     (1 + exp(Intercept + (B_Pct * 2.81166) + (.00003 * 2.4748) + (-.00004 * 4.9380) + (.00006 * 7637)))) %>%
    
            ggplot(aes(B_Pct, Probability)) +
            geom_line() +
            theme_classic() +
    
            scale_y_continuous(limits = c(0, 1),
                               labels = scales::percent) +
    
            scale_x_continuous(limits = c(0, 10),
                               labels = function(NA_Pct) {
                                 
                                 paste0(NA_Pct, "%")
                                 
                               }) +
            
            labs(x = "Black Student Percentage",
                 title = "Black") +
      
      theme(plot.title = element_text(hjust = .5, face = "bold"))
    
                 #y = "Probability of Special Olympic UCS")
    
    grid.arrange(native_model, white_model, black_model,
                ncol = 3)
    
  })
    
  output$two_model <- renderPlot({
    
    hisp_model <- 
      
          tibble(H_Pct = 1:100, Intercept = -3.22811) %>%
            mutate(Probability =
    
                     exp(Intercept + (H_Pct * -1.82599) + (.00003 * 2.4748) + (-.00005 * 4.9380) + (.00006 * 7637))/
    
                     (1 + exp(Intercept + (H_Pct * -1.82599) + (.00003 * 2.4748) + (-.00005 * 4.9380) + (.00006 * 7637)))) %>%
    
            ggplot(aes(H_Pct, Probability)) +
            geom_line() +
            theme_classic() +
    
            scale_y_continuous(limits = c(0, .15),
                               labels = scales::percent) +
    
            scale_x_continuous(limits = c(0, 10),
                               labels = function(NA_Pct) {
                                 
                                 paste0(NA_Pct, "%")
                                 
                               }) +
            
            labs(x = "Hispanic Student Percentage",
                 title = "Hispanic") +
      
      theme(plot.title = element_text(hjust = .5, face = "bold"))
    
                 #y = "Probability of Special Olympic UCS")

    asian_model <- 
      
          tibble(A_Pct = 1:100, Intercept = -3.22332) %>%
            mutate(Probability =
    
                     exp(Intercept + (A_Pct * 21.06048) + (.00002 * 2.4748) + (-.00004 * 4.9380) + (.00005 * 7637))/
    
                     (1 + exp(Intercept + (A_Pct * 21.06048) + (.00002 * 2.4748) + (-.00004 * 4.9380) + (.00005 * 7637)))) %>%
    
            ggplot(aes(A_Pct, Probability)) +
            geom_line() +
            theme_classic() +
    
            scale_y_continuous(limits = c(0, 1),
              labels = scales::percent) +
            
            scale_x_continuous(limits = c(0, 10),
                               labels = function(NA_Pct) {
                                 
                                 paste0(NA_Pct, "%")
                                 
                               }) +
            
            labs(x = "Asian Student Percentage",
                 title = "Asian") +
      
      theme(plot.title = element_text(hjust = .5, face = "bold"))
    
                 #y = "Probability of Special Olympic UCS")

    
    grid.arrange(hisp_model, asian_model,
                 nrow = 1)
    

    
        })
  

     }

shinyApp(ui = ui, server = served)
