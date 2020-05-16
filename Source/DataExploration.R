library(ggplot2)
library(maps)
library(diagram)


BarChart.ShowTotalByCountry <- function(dataset)
{
    data <- melt(dataset, id_vars="Country.Region")
    ggplot(data, aes(x = Country.Region, y = value, fill = variable)) +
        geom_bar(stat = "identity", position='dodge') +
        ggtitle("Top 20 of the Number of People Infected by Country") +
        labs(x = "Country", y = "Number of Infected People") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


Scatter.ShowProgression <- function(dataset)
{
    data <- melt(dataset, id="Date")
    ggplot(data, aes(x = Date, y = value, colour = variable)) +
        geom_line() +
        geom_point() +
        ggtitle("Daily Progression of Infected, Deaths and Recovered People") +
        labs(x = "Date", y = "Number of People") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


Scatter.ShowCountryDeltaProgression <- function(dataset)
{
    ggplot(dataset, aes(x = Date, y = infected.delta)) +
        geom_bar(stat = "identity") +
        ggtitle("Daily increase of infected people") +
        labs(x = "Date", y = "Infected Delta") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


BarChart.ShowPercentOverPopulation <- function(dataset)
{
    data <- melt(dataset, id_vars="Country")
    ggplot(data, aes(x = Country, y = value, fill = variable)) +
        geom_bar(stat = "identity", position='dodge') +
        #ggtitle("Top 20 of the Number of People Infected by Country") +
        labs(x = "Country", y = "Percentage") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


TransitionMatrix.ShowDiagram <- function(states.name, matrix.transition)
{
    row.names(matrix.transition) <- states.name
    colnames(matrix.transition) <-states.name
    
    plotmat(matrix.transition,
            pos = c(1, 3), 
            curve = 0.26,
            lwd = 2, 
            box.lwd = 2, 
            cex.txt = 0.8, 
            box.size = 0.06, 
            box.col = c("green", "yellow", "blue", "red"),
            box.cex = 0.7,
            shadow.size = 0,
            dtext = 0.6,
            arr.length = 0.2,
            arr.width = 0.2,
            self.cex = 0.4,
            main = "COVID-19 Markov Chain State Diagram")
}


ShowConfirmedByCountryStateMap <- function(dataset)
{
    # world<-map_data('world')
    # 
    # p <- ggplot(legend=FALSE) +
    #     geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
    #     theme(panel.background = element_blank()) +
    #     theme(panel.grid.major = element_blank()) +
    #     theme(panel.grid.minor = element_blank()) +
    #     theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
    #     theme(axis.ticks = element_blank()) +
    #     xlab("") + 
    #     ylab("")
    # 
    # p <- p + geom_point(data=dataset,aes(x = Long, y = Lat), colour="red", size=3)
    # p
}