library(ggplot2)
library(maps)
library(diagram)
library(latex2exp)


PieChart.ShowPercentageOverCumulativeInfected <- function(dataset, grand_total)
{
    ggplot(dataset, aes(x = "", y = Total, fill = State.Names)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y", start = 0) +
        geom_text(aes(label = paste0(round(Total / grand_total * 100, 2), "%")), position = position_stack(vjust = 0.5)) +
        labs(x = NULL, y = NULL) +
        ggtitle("Pie Chart of the COVID-19 Propagation States Percentage") +
        theme_classic() + 
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, color = "#666666"))
}

BarChart.ShowTotalByCountry <- function(dataset)
{
    data <- melt(dataset, id_vars="Country.Region")
    ggplot(data, aes(x = Country.Region, y = value, fill = variable)) +
        geom_bar(stat = "identity") +
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


Scatter.ShowCountryDeltaProgression <- function(dataset, feature)
{
    variable <- ifelse(feature == "infected.delta", "Velocity", "Acceleration")
    title <- paste("Daily Propagation", variable, "of Infected People")
    
    ggplot(dataset, aes_string(x = "Date", y = feature)) +
        geom_bar(stat = "identity") +
        ggtitle(title) +
        labs(x = "Date", y = variable) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


BarChart.ShowPercentOverPopulation <- function(dataset)
{
    feature <- colnames(dataset)[2]
    variable <- unlist(strsplit(feature, split='.', fixed=TRUE))[2]
    title <- paste("Top 20 of the Percentage of People", variable, "by Country")
    y <- paste("Percentage of People", variable)
    
    ggplot(dataset, aes_string(x = "Country.Region", y = feature)) +
        geom_bar(stat = "identity") +
        ggtitle(title) +
        labs(x = "Country", y = y) +
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