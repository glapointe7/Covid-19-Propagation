library(reshape2)
library(dplyr)
library(data.table)

ReadDatasetFromURL <- function(url, feature_name)
{
    dataset <- read.csv(url, header = TRUE)
    colnames(dataset) = gsub("X", "0", colnames(dataset))
    
    dataset <- melt(dataset, id.vars=c("Country.Region", "Province.State", "Lat", "Long"),
                    var.name="Date",
                    value.name=feature_name)
    colnames(dataset) = gsub("variable", "Date", colnames(dataset))
    dataset$Date <- strptime(as.character(dataset$Date), "%m.%d.%y")
    dataset$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")

    return(dataset)
}


GetPreparedDataset <- function()
{
    dataset <- ReadDatasetFromURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                          "Confirmed")
    dataset_deaths <- ReadDatasetFromURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                                 "Deaths")
    dataset_recovered <- ReadDatasetFromURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                                    "Recovered")
    
    dataset <- merge(dataset, dataset_deaths, by=c("Country.Region", "Province.State", "Lat", "Long", "Date"), all = TRUE)
    dataset <- merge(dataset, dataset_recovered, by=c("Country.Region", "Province.State", "Lat", "Long", "Date"), all = TRUE)
    
    dataset <- dataset[order(dataset$Country.Region, dataset$Province.State), ]
    
    dataset[is.na(dataset)] <- 0
    
    return(dataset)
}


AddPopulationToDataset <- function(dataset)
{
    populations <- fread("Dataset/PopulationByCountry.csv", 
                         header = TRUE, 
                         select = c("Country Name", "Country Code", "2018"))
    colnames(populations) <- c("Country.Region", "Country.Code", "Population")
    
    return(merge(dataset, populations, by="Country.Region"))
}


GetTotalPerCountryState <- function(dataset)
{
    return(
        dataset %>%
            group_by(Country.Region, Province.State, Long, Lat) %>%
            summarise(Total.Infected = last(Confirmed),
                      Total.Deaths = last(Deaths),
                      Total.Recovered = last(Recovered)) %>%
            arrange(desc(Total.Infected))
    )
}


GetTotalPerCountry <- function(dataset)
{
    return(
        dataset %>%
            group_by(Country.Region) %>%
            summarise(Total.Infected = sum(Total.Infected),
                      Total.Deaths = sum(Total.Deaths),
                      Total.Recovered = sum(Total.Recovered),
                      Total.Infected.Active = sum(Total.Infected) -  sum(Total.Deaths) - sum(Total.Recovered)) %>%
            arrange(desc(Total.Infected))
    )
}


GetWorldProgression <- function(dataset)
{
    return(
        dataset %>%
            group_by(Date) %>%
            summarise(Total.Infected = sum(Confirmed),
                      Total.Deaths = sum(Deaths),
                      Total.Recovered = sum(Recovered),
                      Active.Infected = sum(Confirmed) - sum(Deaths) - sum(Recovered))
    )
}


GetCountryProgression <- function(dataset, country)
{
    return(
        dataset %>%
            filter(Country.Region == !!country) %>%
            group_by(Date) %>%
            summarise(Total.Infected = sum(Confirmed),
                      Total.Deaths = sum(Deaths),
                      Total.Recovered = sum(Recovered))
    )
}


GetCountryFeatureDeltaBetweenDays <- function(feature) 
{
    return(ave(feature, FUN=function(x) c(0, diff(x))))
}


GetDateOfFirstConfirmedInCountry <- function(dataset, country)
{
    country.dataset <- GetCountryProgression(dataset, country = "Canada")
    
    return(head(country.dataset$Date[country.dataset$Total.Infected > 0], n=1))
}


GetPercentageOf <- function(dataset)
{
    return(data.frame(Country.Region = dataset$Country.Region,
                      Population = dataset$Population,
                      Percent.Infected = dataset$Total.Infected / dataset$Population * 100,
                      Percent.Deaths = dataset$Total.Deaths / dataset$Population * 100))
}


GetCountriesWithWorstPercentageOfDeathsOverInfected <- function(dataset)
{
    return(dataset %>%
               mutate(Percent.Deaths = Total.Deaths / Total.Infected * 100) %>%
               arrange(desc(Percent.Deaths)))
}


GetCountriesWithBestPercentageOfRecoveredOverInfected <- function(dataset)
{
    return(dataset %>%
               mutate(Percent.Recovered = Total.Recovered / Total.Infected * 100) %>%
               arrange(desc(Percent.Recovered), desc(Total.Infected)))
}
