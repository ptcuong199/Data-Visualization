install.packages("reshape2")
library(reshape2)
# About to use melt() but it didn't work

install.packages("dslabs")
library(dslabs)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

graph <- function(y) {
  # Number of children aged 0-4 died from measles  
  measles_data <- read.csv(paste(y, "Children Mortality due to Measles.csv", sep = "_"))
  measles_data <- measles_data[,-1]
  colnames(measles_data) <- as.numeric(c(1:length(names(measles_data))))
  measles_data <- as.data.frame(t(measles_data))
  colnames(measles_data) <- c("Age", "Number_of_deaths")
  year <- sort(rep(seq(2017, 2000, -1),3), decreasing = TRUE)
  measles_data <- measles_data %>% mutate(Year = year)
  measles_data$Number_of_deaths <- as.numeric(paste(measles_data$Number_of_deaths)) # When passing a single vector, paste work like as.character.
  measles_data <- measles_data %>% filter(Age == "0-4 years") %>% select(Year, Number_of_deaths)

  # Measles 1 Coverage - The first dose is generally given to children around 9 to 15 months of age
  measles_1 <- read.csv(paste(y, "Measles 1 Coverage.csv", sep = "_"))
  measles_1 <- measles_1[,-1]
  colnames(measles_1) <- as.numeric(c(1:length(names(measles_1))))
  measles_1 <- as.data.frame(t(measles_1))
  colnames(measles_1) <- c("Year", "MCV1_Coverage")
  measles_1 <- measles_1 %>% filter(Year %in% seq(2017,2000,-1))

  # Measles 2 Coverage - The second dose is given to children from 4 to 6 years of age, with at least 4 weeks between the doses.
  measles_2 <- read.csv(paste(y, "Measles 2 Coverage.csv", sep = "_"))
  measles_2 <- measles_2[,-1]
  colnames(measles_2) <- as.numeric(c(1:length(names(measles_2))))
  measles_2 <- as.data.frame(t(measles_2))
  colnames(measles_2) <- c("Year", "MCV2_Coverage")

  # Merge data frame
  measles_data <- merge(measles_data, merge(measles_1, measles_2))

  # Graph
  upper_lim <- max(measles_data$Number_of_deaths)+20
  graph <- measles_data %>% ggplot() + geom_bar(aes(Year, Number_of_deaths, fill = "Number of deaths"), stat = "identity", alpha = 1.0) + 
    geom_line(aes(Year, MCV1_Coverage*(upper_lim/100), col = "First dose"), size = 1.0) +
    geom_line(aes(Year, MCV2_Coverage*(upper_lim/100), col = "Second dose"), size = 1.0) +
    scale_y_continuous(limits = c(0, upper_lim), sec.axis = sec_axis(~./(upper_lim/100), name = "Coverage")) +
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = "black") +
    labs(fill = "") +
    labs(color = "Legends\n\nMeasles containing vaccine") +
  ### Instead of stating what color we use, we use col = as a way to create our legend. Therefore, we put col = "name of the variable".
  # Similarly, we put the name of the barplot under fill, fill = "name of the variable". 
  # After that, we use scale_color_manual to change the color of whichever use col =, and scale_fill_manual to change the color of whichever use fill =.
    ylab("Number of deaths") + 
    ggtitle(paste("A Comparison between Prevalence of child mortality due to Measles and Measles Immunization Coverage by year", y, sep = " in "))
  return(graph)
  ### To create the second y-axis, first we need to scale the values of the second variable.
  # Since the maximum of the primary variable is, for example, 3000 and the maximum of the secondary variable is, for example, 100, we need to multiply the values
  # of the secondary variable by 30 so that the scales of both sides are compatible on the same graph.
  # Lastly, the number a in sec.axis(~./a) is similar to the scaling factor, which is 30. 
}
