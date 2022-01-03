
library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Covid SIR rates"),
    
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
        
        
        
        mainPanel(
            plotOutput("hee")
            
        )
    )
)
