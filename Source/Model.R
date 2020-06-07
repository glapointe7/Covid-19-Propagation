


SIR <- function(S, I, R, D, alpha, beta, gamma)
{
    susceptibles.delta <- -beta * S * I
    recovered.delta <- gamma * I
    dead.delta <- alpha * I
    infected.delta <- -susceptibles.delta - recovered.delta - dead.delta
    
    return(list("Susceptibles.Delta" = susceptibles.delta,
                "Infected.Delta" = infected.delta,
                "Recovered.Delta" = recovered.delta,
                "Deaths.Delta" = dead.delta))
}


BuildDatasetFromSIRModel <- function(days, initial.date, initial.SIR, alpha, beta, gamma)
{
    dataset <- data.frame(Date = initial.date,
                          Susceptibles = initial.SIR[["Population"]] - initial.SIR[["Infected"]] - initial.SIR[["Recovered"]] - initial.SIR[["Deaths"]],
                          Infected = initial.SIR["Infected"],
                          Recovered = initial.SIR["Recovered"],
                          Deaths = initial.SIR["Deaths"])
    
    for(i in seq(1:days)) 
    {
        model <- SIR(S = dataset$Susceptibles[i],
                     I = dataset$Infected[i],
                     R = dataset$Recovered[i],
                     D = dataset$Deaths[i],
                     alpha = alpha,
                     beta = beta,
                     gamma = gamma)
        
        dataset <- rbind(dataset, list(Date = dataset$Date[i] + 1,
                                       Susceptibles = model$Susceptibles.Delta + dataset$Susceptibles[i],
                                       Infected = model$Infected.Delta + dataset$Infected[i],
                                       Recovered = model$Recovered.Delta + dataset$Recovered[i],
                                       Deaths = model$Deaths.Delta + dataset$Deaths[i]))
    }
    
    return(dataset)
}


S <- function(t, N, beta, S0)
{
    return(N / (1 + exp(N * (beta * t + S0))))
}


Ic <- function(t, N, beta, I0)
{
    return(N / (1 - exp(-N * (beta * t + I0))))
}


R <- function(t, N, beta, gamma, I0) 
{
    
}