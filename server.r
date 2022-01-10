library(shiny)
library(ggplot2)
library(deSolve)
library(reshape2)
shinyServer (function(input, output) {
    output$intro=renderText({
        print("The SIR model is a compartmental model used to model epidemics, developed in 1956. S represent the suceptible, those who can catch the disease. In this case, this is the whole population of Hawaii. The next group is the I group, the infected. Last is the recovered or removed group, which accounts for those who are recovered with immunity or are dead.")
    }
    )
    output$hoo= renderText({
        # Model inputs
        statedata=read.csv("statedata.csv")
        population=1421771
        delta=465:612
        covid=0:464
        vaccinated=input$vaccinated
        statedata[is.na(statedata)]=0
        deltacases=sum(tail(statedata$New.Cases[delta],n=30))
        covidcases=sum(tail(statedata$New.Confirmed.Cases[covid]+statedata$New.Cases[covid],n=30))
        covidrecovered=sum(statedata$Deaths_New[covid])
        deltarecovered=sum(statedata$Deaths_New[delta])
        deltainit=c(S=((1-vaccinated/100)*(population-deltacases-deltarecovered)),I=deltacases,R=(vaccinated/100)*(population+deltarecovered-deltacases))
        covidinit=c(S=((1-vaccinated/100)*(population-covidcases-covidrecovered)),I=covidcases,R=(vaccinated/100)*(population+covidrecovered-covidcases))
        covidparameters = c(beta=0.279, gamma=0.1)
        deltaparameters = c(beta = 0.508, gamma = 0.1)
        # Time points
        
        time=seq(from=0,to=input$timeperiod,by=1)
        
        # SIR model function 
        
        sir_model = function(time,state,parameters){
            with(as.list(c(state,parameters)),{
                N=S+I+R
                lambda=beta*(I/N) 
                dS=-lambda*S
                dI=lambda*S-gamma*I
                dR=gamma*I
                
                return(list(c(dS,dI,dR)))
            }
            )
        }
        
        
        #Solving the differential equations
        if(input$variant=="SARS-COV-2"){
            output=as.data.frame(ode(y=covidinit,func = sir_model,parms=covidparameters,times = time))
            
            
            out_long=melt(output,id="time")
            # To plot the proportion of susceptible, infected and recovered individuals over time
            
            
            
            
            
                   deathstent<-round(round(max(output$I))*.5464*0.391)
                   sprintf("%s is the estimated amount of deaths caused by COVID in this simulation.", deathstent)
        }else if(input$variant=="SARS-COV-2 Delta Variant"){
            output=as.data.frame(ode(y=deltainit,func = sir_model,parms=deltaparameters,times = time))
            
            
            out_long=melt(output,id="time")
            # To plot the proportion of susceptible, infected and recovered individuals over time
            
            
            
            
            
           deathstent<-round(round(max(output$I))*0.391*.5464)
           sprintf("%s is the estimated amount of deaths caused by COVID in this simulation.", deathstent)        }
    })
    output$hee= renderPlot({
        # Model inputs
          statedata=read.csv("statedata.csv")
        population=1421771
        delta=465:612
        covid=0:464
        vaccinated=input$vaccinated
        statedata[is.na(statedata)]=0
        deltacases=sum(tail(statedata$New.Cases[delta],n=30))
        covidcases=sum(tail(statedata$New.Confirmed.Cases[covid]+statedata$New.Cases[covid],n=30))
        covidrecovered=sum(statedata$Deaths_New[covid])
        deltarecovered=sum(statedata$Deaths_New[delta])
        deltainit=c(S=((1-vaccinated/100)*(population-deltacases-deltarecovered)),I=deltacases,R=(vaccinated/100)*(population+deltarecovered-deltacases))
        covidinit=c(S=((1-vaccinated/100)*(population-covidcases-covidrecovered)),I=covidcases,R=(vaccinated/100)*(population+covidrecovered-covidcases))
        covidparameters = c(beta=0.279, gamma=0.1)
        deltaparameters = c(beta = 0.508, gamma = 0.1)
        # Time points
        
        time=seq(from=0,to=input$timeperiod,by=1)
        
        # SIR model function 
        
        sir_model = function(time,state,parameters){
            with(as.list(c(state,parameters)),{
                N=S+I+R
                lambda=beta*(I/N) 
                dS=-lambda*S
                dI=lambda*S-gamma*I
                dR=gamma*I
                
                return(list(c(dS,dI,dR)))
            }
            )
        }
        
        
        #Solving the differential equations
        if(input$variant=="SARS-COV-2"){
            output=as.data.frame(ode(y=covidinit,func = sir_model,parms=covidparameters,times = time))
            
            
            out_long=melt(output,id="time")
            # To plot the proportion of susceptible, infected and recovered individuals over time
            
            
            
            
            
            ggplot(data = out_long,          
                   aes(x = time, y = value/population, colour = variable, group = variable)) +  
                geom_line() +xlab("Time (days)")+ylab("Proportion of the population")+scale_color_discrete(name="State")
        }else if(input$variant=="SARS-COV-2 Delta Variant"){
            output=as.data.frame(ode(y=deltainit,func = sir_model,parms=deltaparameters,times = time))
            
            
            out_long=melt(output,id="time")
            # To plot the proportion of susceptible, infected and recovered individuals over time
            
            
            
            
            
            ggplot(data = out_long,          
                   aes(x = time, y = value/population, colour = variable, group = variable)) +  
                geom_line() +xlab("Time (days)")+ylab("Proportion of the population")+scale_color_discrete(name="State")
        }
    })
})
