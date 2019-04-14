install.packages("readxl")
library(readxl)

graph_case <- function(y) {
  # Number of children aged 0-4 died from measles  
  measles_data <- as.data.frame(read_excel(paste(y, "Measles Cases.xlsx", sep = "_")))
  
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
  
  # Graph
  upper_lim <- max(measles_data$Total)+20
  graph <- ggplot() + geom_bar(data = measles_data, aes(Year, Total, fill = "Number of cases"), stat = "identity", alpha = 1.0) + 
    geom_line(data = measles_1, aes(Year, MCV1_Coverage*(upper_lim/100), col = "First dose"), size = 1.0) +
    geom_line(data = measles_2, aes(Year, MCV2_Coverage*(upper_lim/100), col = "Second dose"), size = 1.0) +
    scale_y_continuous(limits = c(0, upper_lim), sec.axis = sec_axis(~./(upper_lim/100), name = "Coverage")) +
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = "black") +
    labs(fill = "") +
    labs(color = "Legends\n\nMeasles containing vaccine") +
    ylab("Number of cases") + 
    ggtitle(paste("A Comparison between the Number of Measles Cases and Measles Immunization Coverage by year", y, sep = " in "))
  return(graph)
}
