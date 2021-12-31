#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Covid SIR rates"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("vaccinated",
                        "Vaccination rate in percent:",
                        min = 0,
                        max = 100,
                        value = 73),
        
            selectInput(
                "variant", "Variant of covid to model",list("SARS-COV-2", "SARS-COV-2 Delta Variant")
            )
        ),
        sliderInput("timeperiod",
                    "Time in days to simulate:",
                    min = 1,
                    max = 365,
                    value = 30)
                ), 
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("hee")
            
        )
    )
)
