# set working directory. 
# setwd(" INCLUDE YOUR PATH  ")

library(tidyr)
library(trend)
library(patchwork)
library(strucchange)
library(stargazer)
library(Kendall)
library(ggplot2)
library(lattice)
library(mclust)
library("texreg")
library(nnet)
library("foreign")
library(Hmisc)
library(psych)
library(plyr)
library(RCurl)
library(sandwich)
library(ggcorrplot)
library(data.table)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

library(forcats)
library(ggrepel)
library(ggplot2)
library(gganimate)
library(dplyr)
library(igraph)
library(stringr)

#CODE TO GENERATE FIGURE 2-----------------------------------------------------------------------------------------

data<-read.table(paste(getwd(),"/dataNetworks.csv", sep =''),header=TRUE,sep=",")
data2<-read.table(paste(getwd(),"/ActionsNetworks.csv", sep=''),header=TRUE,sep=",")
data3<-read.table(paste(getwd(),"/NetworksChoices.csv", sep=''),header=TRUE,sep=",")
#REMOVE PRATICE ROUND
data<-subset(data,RoundCode>1)

data<-merge(data,data2, by=c("RoundCode","SessionCode"))
#REMOVE UNANALYZED NETWORKS (children who could not be assigned a group of 6 and played separately on a group of 4 with research assistants)
data<-subset(data,SizeNetwork!=4)

data<-merge(data,data3, by=c("RoundCode","SessionCode"))

# VARIABLES FOR LINK NUMBER AND COLOR NUMBER BY ROUND
data$NumberLinks[data$RoundCode==2]<-3
data$NumberLinks[data$RoundCode==3]<-3
data$NumberLinks[data$RoundCode==4]<-2
data$NumberLinks[data$RoundCode==5]<-2
data$NumberLinks[data$RoundCode==6]<-3
data$NumberLinks[data$RoundCode==7]<-3
data$NumberLinks[data$RoundCode==8]<-2
data$NumberLinks[data$RoundCode==9]<-2
data$NumberLinks[data$RoundCode==10]<-2
data$NumberLinks[data$RoundCode==11]<-2
data$NumberLinks[data$RoundCode==12]<-3
data$NumberLinks[data$RoundCode==13]<-3
data$NumberLinks[data$RoundCode==14]<-2
data$NumberLinks[data$RoundCode==15]<-2
data$NumberLinks[data$RoundCode==16]<-3
data$NumberLinks[data$RoundCode==17]<-3

data$NumberColors[data$RoundCode==2]<-2
data$NumberColors[data$RoundCode==3]<-2
data$NumberColors[data$RoundCode==4]<-2
data$NumberColors[data$RoundCode==5]<-2
data$NumberColors[data$RoundCode==6]<-4
data$NumberColors[data$RoundCode==7]<-4
data$NumberColors[data$RoundCode==8]<-4
data$NumberColors[data$RoundCode==9]<-4
data$NumberColors[data$RoundCode==10]<-2
data$NumberColors[data$RoundCode==11]<-2
data$NumberColors[data$RoundCode==12]<-2
data$NumberColors[data$RoundCode==13]<-2
data$NumberColors[data$RoundCode==14]<-4
data$NumberColors[data$RoundCode==15]<-4
data$NumberColors[data$RoundCode==16]<-4
data$NumberColors[data$RoundCode==17]<-4

data$ColLinks<-paste("C",data$NumberColors,"L",data$NumberLinks)
Convergence<-data.frame(prop.table(table(data$Convergence,data$NumberColors),2))
colnames(Convergence)<-c("Conv","Colors","Freq")



Convergence1 <- data.frame(
  prop.table(
    table(subset(data, ColLinks=="C 2 L 2")$Convergence,
          subset(data, ColLinks=="C 2 L 2")$Grade),
    2
  )
)

Convergence1$N <- NA_integer_
Convergence1$StdErr <- NA_real_

for (i in 1:nrow(Convergence1)) {
  subset_count <- nrow(subset(data,
                              ColLinks == "C 2 L 2" &
                                Grade == Convergence1$Var2[i]))
  Convergence1$N[i] <- subset_count
  Convergence1$StdErr[i] <- sqrt(Convergence1$Freq[i] * (1 - Convergence1$Freq[i]) / subset_count)
}

colnames(Convergence1) <- c("Conv", "Grade", "Freq", "N", "StdErr")
Convergence1$ColLinks <- "C2N2"









Convergence2 <- data.frame(
  prop.table(
    table(subset(data, ColLinks=="C 2 L 3")$Convergence,
          subset(data, ColLinks=="C 2 L 3")$Grade),
    2
  )
)

Convergence2$N <- NA_integer_
Convergence2$StdErr <- NA_real_

for (i in 1:nrow(Convergence2)) {
  subset_count <- nrow(subset(data,
                              ColLinks == "C 2 L 3" &
                                Grade == Convergence2$Var2[i]))
  Convergence2$N[i] <- subset_count
  Convergence2$StdErr[i] <- sqrt(Convergence2$Freq[i] * (1 - Convergence2$Freq[i]) / subset_count)
}

colnames(Convergence2) <- c("Conv", "Grade", "Freq", "N", "StdErr")
Convergence2$ColLinks <- "C2N3"










Convergence3 <- data.frame(
  prop.table(
    table(subset(data, ColLinks=="C 4 L 2")$Convergence,
          subset(data, ColLinks=="C 4 L 2")$Grade),
    2
  )
)

Convergence3$N <- NA_integer_
Convergence3$StdErr <- NA_real_

for (i in 1:nrow(Convergence3)) {
  subset_count <- nrow(subset(data,
                              ColLinks == "C 4 L 2" &
                                Grade == Convergence3$Var2[i]))
  Convergence3$N[i] <- subset_count
  Convergence3$StdErr[i] <- sqrt(Convergence3$Freq[i] * (1 - Convergence3$Freq[i]) / subset_count)
}

colnames(Convergence3) <- c("Conv", "Grade", "Freq", "N", "StdErr")
Convergence3$ColLinks <- "C4N2"













Convergence4 <- data.frame(
  prop.table(
    table(subset(data, ColLinks=="C 4 L 3")$Convergence,
          subset(data, ColLinks=="C 4 L 3")$Grade),
    2
  )
)

Convergence4$N <- NA_integer_
Convergence4$StdErr <- NA_real_

for (i in 1:nrow(Convergence4)) {
  subset_count <- nrow(subset(data,
                              ColLinks == "C 4 L 3" &
                                Grade == Convergence4$Var2[i]))
  Convergence4$N[i] <- subset_count
  Convergence4$StdErr[i] <- sqrt(Convergence4$Freq[i] * (1 - Convergence4$Freq[i]) / subset_count)
}

colnames(Convergence4) <- c("Conv", "Grade", "Freq", "N", "StdErr")
Convergence4$ColLinks <- "C4N3"







Convergence<-rbind(Convergence1,Convergence2,Convergence3,Convergence4)

Convergence$ColLinks<-factor(Convergence$ColLinks, levels=c("C2N2","C4N2","C2N3","C4N3"))
levels(Convergence$ColLinks)<-c("C2N2","C4N2","C2N3","C4N3")

#ALGORITHM CODE AND CALCULATIONS
#BECAUSE THE SIMULATION RUNS EVERYTIME, THE AL RESULTS ARE SLIGHTLY DIFFERENT EACH TIME

# Function to create a graph where each node has 2 links
create_custom_graph_2 <- function(n) {
  g <- make_ring(n)
  return(g)
}  


# create graph with 3 links

# Function to create a graph where each node has 3 links
create_custom_graph_3 <- function(n) {
  # Define the edges such that each node connects to 3 others
  edges <- c(1,2, 1,4, 1,6,  # Node 1 connects to Nodes 2, 3, and 6
             2,3, 2,5,        # Node 2 connects to Nodes 3 and 4 (already connected to 1)
             3,4, 3,6,        # Node 3 connects to Nodes 4 and 5 (already connected to 2)
             4,5,      # Node 4 connects to Nodes 5 and 6 (already connected to 1 and 3)
             5,6)            # Node 5 connects to Node 6 (already connected to 2 and 4)
  # Create the graph
  g <- graph(edges, directed = FALSE)
  return(g)
}
# create a graph with 4 links

# Function to create a graph where each node has 4 links
create_custom_graph_4 <- function(n) {
  # Define the edges such that each node connects to 4 others
  edges <- c(1,2, 1,3, 1,6, 1,5,  # Node 1 connects to Nodes 2, 3, 5, and 6
             2,3, 2,4, 2,6,       # Node 2 connects to Nodes 3, 4, 6 (already connected to 1)
             3,4, 3,5,       # Node 3 connects to Nodes 4, 5 (already connected to 1, 2)
             4,5, 4,6,            # Node 4 connects to Nodes 5, 6 (already connected to 2, 3)
             5,6)                 # Node 5 connects to Node 6 (already connected to 1, 3, 4)
  # Create the graph
  g <- graph(edges, directed = FALSE)
  return(g)
}

# Update network colors with strategy A

update_network_colorsA <- function(g, p) {
  node_ids <- V(g)
  for (node_id in sample(node_ids[c(-disabled)], 1, replace = FALSE)) { 
    #sample only 1 value from a vector without the 'disabled' value from the global variable
    neighbor_ids <- neighbors(g, node_id)
    neighbor_colors <- V(g)$color[neighbor_ids]
    colors <- c(V(g)$color[node_id], neighbor_colors) #A and C strategies include counting your own color
    color_table <- table(colors)
    disabled <<- node_id #change the global variable disabled to whatever value was just dplyr::selected
    
    # Determine if there's a clear majority color
    sorted_colors <- sort(color_table, decreasing = TRUE)
    is_tie <- length(sorted_colors) > 1 && sorted_colors[1] == sorted_colors[2]
    
    if (is_tie) {
      # Tie situation - Decide to keep the same color with probability q
      current_color <- V(g)$color[node_id]  # Current Color
      other_colors <- unique(colors[(! colors %in% current_color)])
      new_color <- ifelse(runif(1) < 1-p, current_color, sample(other_colors, 1))
    } else {
      # No tie: proceed as before
      majority_color <- names(which.max(color_table))
      non_majority_colors <- unique(colors)
      non_majority_colors <- setdiff(non_majority_colors, majority_color)
      if (length(non_majority_colors) == 0) {
        new_color <- majority_color} else {
          new_color <- ifelse(runif(1) < p, majority_color, sample(non_majority_colors, 1))}}
    
    
    V(g)$color[node_id] <- new_color
  }
  return(g)
}

# Perform a single simulation

# Function to perform a single simulation and return convergence time
perform_simulation <- function(p,x) {
  n <- 6  # Number of nodes
  colorvector
  
  if( s != "A") { #if using sequential strategy initialize graph through this method
    if(l==2){
      g <- create_custom_graph_2(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      g <- initialize_network_colorsAs(g,x)
    }
    if(l==3){
      g <- create_custom_graph_3(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      g <- initialize_network_colorsAs(g,x)
    }}
  else{
    if(l==2){
      g <- create_custom_graph_2(n)
      V(g)$color <- sample(colorvector, n, replace = TRUE)
    }else{
      
      if (l==3) {g <- create_custom_graph_3(n)
      V(g)$color <- sample(colorvector, n, replace = TRUE)
      }else{g <- create_custom_graph_4(n)
      V(g)$color <- sample(colorvector, n, replace = TRUE)}
    } }
  for (i in 1:30) {
    if (i == 1) {
      if (length(unique(V(g)$color)) == 1) {
        return(0) # return 0 in the case where everyone chose the same color through random chance
      }
    }
    if (s=="A") {g <- update_network_colorsA(g, p)}
    else {g <- update_network_colorsA(g, p)}
    
    # Check for convergence
    if (length(unique(V(g)$color)) == 1) {
      return(i)  # Return the iteration at which convergence occurred
    }
  }
  return(NA)  # Return NA if no convergence within 30 iterations
}

p <- 1 # Probability of choosing the majority color
l <- 2 #number links
s <- "A"
x <- 1
colorvector<-c("red","blue") #, "yellow", "brown")
disabled <- 8 # create the global variable to disable colors in previous rounds; initially not 1-6

num_simulations <- 1000 # number simulations

# Run simulations and collect results

convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C2N2AL <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))
c1<-c("yes","AL",C2N2AL/num_simulations,NA,NA, "C2N2") 

colorvector<-c("red","blue", "yellow", "brown")
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C4N2AL <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))
c2<-c("yes","AL",C4N2AL/num_simulations,NA,NA, "C4N2") 

colorvector<-c("red","blue")
l <- 3
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C2N3AL <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))
c3<-c("yes","AL",C2N3AL/num_simulations,NA,NA, "C2N3") 

colorvector<-c("red","blue", "yellow", "brown")
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C4N3AL <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))
c4<-c("yes","AL",C4N3AL/num_simulations,NA,NA, "C4N3") 


c<-rbind(c1,c2,c3,c4)
colnames(c)<-c("Conv","Grade","Freq","N","StdErr", "ColLinks")
Convergence<-rbind(Convergence,c)
Convergence$Freq<-as.numeric(as.character(Convergence$Freq))

Convergence$ColLinks <- factor(
  Convergence$ColLinks,
  levels = c("C2N3", "C4N3", "C2N2", "C4N2")
)



g <- ggplot(subset(Convergence, Conv == "yes"), aes(x = Grade, y = Freq, fill = Grade)) + 
  facet_grid(cols = vars(ColLinks)) +
  geom_bar(stat = "identity", width = 0.9, color = "white", 
           position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Freq - as.numeric(StdErr), 
                    ymax = Freq + as.numeric(StdErr)), 
                width = .4, colour = "black", alpha = 0.5, size = .3, position = position_dodge(width = .9)) +
  ggtitle(" ") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        plot.title = element_text(size = 15, face = "bold"),
        legend.position = "none",  # Remove the legend
        axis.text = element_text(size = 8),
        axis.title.x=element_blank(),
        axis.title = element_text(size = 10, face = "bold"),
        aspect.ratio = 1.2) +
  scale_fill_manual(values = c("springgreen3", "dodgerblue3", "gold1", "grey")) +
  scale_x_discrete(labels = c("K", "1", "2", "AL")) +
  geom_text(aes(label = round(Freq, 2)), vjust = 2.5 , size = 3, position = position_dodge(width = 0.9))

g

g <- g +
  geom_text(
    data = subset(Convergence, !is.na(N)),
    aes(x = Grade, y = 0.05, label = paste0("n=", N)),
    vjust = -0.2, size = 3,
    position = position_dodge(width = 0.9),
    inherit.aes = FALSE
  )
g


#CODE TO GENERATE FIGURE 3 -> ALGORITHM IS SAME AS FIGURE 2----------------------------------------------------------------
#BECAUSE THE SIMULATION RUNS EVERYTIME, THE RESULTS ARE SLIGHTLY DIFFERENT EACH TIME
#Cleaning the Population Data
tableC4L2<-as.data.frame(prop.table(table(subset(data,ColLinks == "C 4 L 2")$Convergence,subset(data,ColLinks == "C 4 L 2")$NumberChoices)))
tableC4L2$ColLinks<-"C4N2"
tableC4L2$sum <- sum(subset(tableC4L2, Var1 == "yes")$Freq)

tableC4L3<-as.data.frame(prop.table(table(subset(data,ColLinks == "C 4 L 3")$Convergence,subset(data,ColLinks == "C 4 L 3")$NumberChoices)))
tableC4L3$ColLinks<-"C4N3"
tableC4L3$sum <- sum(subset(tableC4L3, Var1 == "yes")$Freq)

tableC2L2<-as.data.frame(prop.table(table(subset(data,ColLinks == "C 2 L 2")$Convergence,subset(data,ColLinks == "C 2 L 2")$NumberChoices)))
tableC2L2$ColLinks<-"C2N2"
tableC2L2$sum <- sum(subset(tableC2L2, Var1 == "yes")$Freq)

tableC2L3<-as.data.frame(prop.table(table(subset(data,ColLinks == "C 2 L 3")$Convergence,subset(data,ColLinks == "C 2 L 3")$NumberChoices)))
tableC2L3$ColLinks<-"C2N3"
tableC2L3$sum <- sum(subset(tableC2L3, Var1 == "yes")$Freq)

tableCL<-rbind(tableC2L2, tableC2L3, tableC4L2, tableC4L3)
tableCL$source <- "Population"

tableCL$Var2 <- as.numeric(as.character(tableCL$Var2))

l <- 2 #number links
x <- 1
colorvector<-c("red","blue", "yellow", "brown")
disabled <- 8 

#C4L2
convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC4N2 <- na.omit(convergence_times) +6 

# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)


tableC4L2<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC4L2$ColLinks<-"C4N2"
tableC4L2$source <- "Algorithm"
tableC4L2$Var1 <- ifelse(tableC4L2$Var1 == TRUE, "yes", "no")
tableC4L2$sum <- sum(subset(tableC4L2, Var1 == "yes")$Freq)

#Change for C4L3
l <- 3

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC4N3 <- na.omit(convergence_times) +6 
# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)


tableC4L3<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC4L3$ColLinks<-"C4N3"
tableC4L3$source <- "Algorithm"
tableC4L3$Var1 <- ifelse(tableC4L3$Var1 == TRUE, "yes", "no")
tableC4L3$sum <- sum(subset(tableC4L3, Var1 == "yes")$Freq)

#Change for C2L3

colorvector<-c("red","blue")

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC2N3 <- na.omit(convergence_times) +6 
# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)

tableC2L3<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC2L3$ColLinks<-"C2N3"
tableC2L3$source <- "Algorithm"
tableC2L3$Var1 <- ifelse(tableC2L3$Var1 == TRUE, "yes", "no")
tableC2L3$sum <- sum(subset(tableC2L3, Var1 == "yes")$Freq)


#Change for C2L2
l<- 2

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)
convergence_timesC2N2 <- na.omit(convergence_times) +6 
tableC2L2<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC2L2$ColLinks<-"C2N2"
tableC2L2$source <- "Algorithm"
tableC2L2$Var1 <- ifelse(tableC2L2$Var1 == TRUE, "yes", "no")
tableC2L2$sum <- sum(subset(tableC2L2, Var1 == "yes")$Freq)


#Combine Everything into one dataframe
tableCLT <- rbind(tableC2L2, tableC4L2, tableC2L3, tableC4L3)
tableCLT$Var2 <-as.numeric(as.character(tableCLT$Var2)) +6
tableCL$Var2 <- as.numeric(as.character(tableCL$Var2))
tableCLK <- rbind(tableCL, tableCLT)
# Convert Var2 to numeric if it is not already
tableCLK$Var2 <- as.numeric(as.character(tableCLK$Var2))

# Subset the data with the desired conditions
subset_data <- subset(tableCLK, Var1 == "yes" & Var2 <37)

# Subset the data with the desired conditions
subset_data <- subset(tableCLK, Var1 == "yes" & Var2 > 5 & Var2 < 37)
subset_data$ColLinks <- factor(subset_data$ColLinks, levels = c("C2N2", "C4N2", "C2N3", "C4N3"))

subset_data$FreqAdjusted <- subset_data$Freq/subset_data$sum

population_means <- subset(subset_data, source == "Population") %>%
  group_by(ColLinks) %>%
  summarize(
    sum_var2_freqadjusted = sum(Var2 * FreqAdjusted, na.rm = TRUE),
    sum_freqadjusted = sum(FreqAdjusted, na.rm = TRUE),
    weighted_mean_var2_freqadjusted = sum_var2_freqadjusted / sum_freqadjusted
  )

alg_means <- subset(subset_data, source == "Algorithm") %>%
  group_by(ColLinks) %>%
  summarize(
    sum_var2_freqadjusted = sum(Var2 * FreqAdjusted, na.rm = TRUE),
    sum_freqadjusted = sum(FreqAdjusted, na.rm = TRUE),
    weighted_mean_var2_freqadjusted = sum_var2_freqadjusted / sum_freqadjusted
  )


plot <- ggplot(subset_data, aes(x = Var2, y = FreqAdjusted, fill = source)) +
  facet_grid(cols = vars(ColLinks)) +
  geom_bar(data = subset(subset_data, source == "Population"), 
           width = 0.7, stat = "identity", position = "identity", alpha = 0.5, color = "blue") +
  geom_bar(data = subset(subset_data, source == "Algorithm"), 
           width = 0.7, stat = "identity", position = "identity", alpha = 0.5, color = "red") +
  labs(x = "Number Choices", y = "Frequency") +
  theme_minimal() +

geom_vline(data = population_means, aes(xintercept = sum_var2_freqadjusted), linetype = "dashed", color = "blue", size = 0.5) +
  geom_vline(data = alg_means, aes(xintercept = sum_var2_freqadjusted), linetype = "dashed", color = "red", size = 0.5) +
  scale_x_continuous(breaks = seq(6, 36, by = 3)) +  # Set y-axis limits
  scale_fill_manual(values = c("Population" = "blue", "Algorithm" = "red"),
                    labels = c("Population" = "Data", "Algorithm" = "AL")) +  # Modify legend labels
  theme(
    axis.title.x = element_text(size = 75),
    axis.title.y = element_text(size = 75),
    legend.title = element_blank(),
    legend.text = element_text(size = 50),
    strip.text = element_text(size = 62.5),  # Increase the font size of facet labels
    axis.text.x = element_text(size = 30),  # Increase the font size of x axis text
    axis.text.y = element_text(size = 30)   # Increase the font size of y axis text
  )

plot

#CODE TO GENERATE FIGURE 4-------------------------------------------------------------------------------------------------

data<-read.table(paste(getwd(),"/Probabilities.csv", sep = ''),header=TRUE,sep=",")


# Age, gender, grade information


dataI<-read.table(paste(getwd(),"/dataNetworksIndiv.csv", sep = ''),header=TRUE,sep=",")
dataI<-subset(dataI,RoundCode>1)
dataI<-subset(dataI,SizeNetwork!=4)

dataI$MoB[dataI$BirthMonth==1]<-12
dataI$MoB[dataI$BirthMonth==2]<-11
dataI$MoB[dataI$BirthMonth==3]<-10
dataI$MoB[dataI$BirthMonth==4]<-9
dataI$MoB[dataI$BirthMonth==5]<-8
dataI$MoB[dataI$BirthMonth==6]<-7
dataI$MoB[dataI$BirthMonth==7]<-6
dataI$MoB[dataI$BirthMonth==8]<-5
dataI$MoB[dataI$BirthMonth==9]<-4
dataI$MoB[dataI$BirthMonth==10]<-3
dataI$MoB[dataI$BirthMonth==11]<-2
dataI$MoB[dataI$BirthMonth==12]<-1

dataI$Age<-(2023-dataI$BirthYear)*12+5+dataI$MoB

dataKeep<-subset(dataI, RoundCode==2)
dataKeep<-dataKeep[c("ParticipantID","SessionCode","Grade","Age","Gender")]
datagroup<-read.table(paste(getwd(),"/dataNetworks.csv", sep =''),header=TRUE,sep=",")


data<-merge(data,dataKeep, by=c("ParticipantID","SessionCode"))
data$Grade[data$Grade == '0'] <- 'K'
data$Grade <- factor(data$Grade, levels = c("K", "1", "2"))

avdata<-read.table(paste(getwd(),"/AverageActionsIndiv.csv", sep = ''),header=TRUE,sep=",")
avdata$leader <- ifelse(avdata$Rank < 3, 1, 0)
avdata2<-avdata[c("ParticipantID", "Rank","ExtraClicks", "NumberChoices")]
data<-merge(data,avdata2, by=c("ParticipantID"))
data$leader <- ifelse(data$Rank < 3, 1, 0)
data$GenderM <- ifelse(data$Gender == "M", 1, 0)


data$Grade <- factor(data$Grade, levels = c("K", "1", "2"))
vline_positions <- data.frame(
  Grade = factor(c("K", "1", "2"), levels = c("K", "1", "2")),
  vline = c(0.7, 0.7, 0.7),
  label = c("hat(p) == .73", "hat(p) == .77", "hat(p) == .77")
)

vline_positions <- data.frame(
  Grade = factor(c("K", "1", "2"), levels = c("K", "1", "2")),
  vline = c(0.7, 0.7, 0.7),
  label = c("hat(p) == .73", "hat(p) == .77", "hat(p) == .77")
)

plot <- ggplot(data, aes(x = Probability)) + 
  facet_grid(cols = vars(Grade), scales = "free_y") +
  geom_histogram(aes(y = ..count.. / tapply(..count.., ..PANEL.., sum)[..PANEL..]), binwidth = 0.05, fill = "blue", color = "black") +
  labs(x = "p", y = "Proportion") +  # Set y-axis label to 'Proportion'
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 60),  # Increase x-axis label size
    axis.title.y = element_text(size = 60),  # Increase y-axis label size
    axis.text.x = element_text(size = 40),   # Increase x-axis text size
    axis.text.y = element_text(size = 40),   # Increase y-axis text size
    strip.text = element_text(size = 60)     # Increase the font size of facet labels
  )

print(plot)

#CODE TO GENERATE FIGURE 5 -> USES ALGORITHM FROM FIGURE 2 & DATAFRAME FROM FIGURE 4 IN LINE BELOW----------------------------------------------
#RUNS NEW SIMULATIONS EVERY TIME SO RESULTS WILL BE SLIGHTLY DIFFERENT EACH TIME
vertical_p <- mean(data$Probability)

dataI<-read.table(paste(getwd(),"/dataNetworksIndiv.csv", sep = ''),header=TRUE,sep=",")
dataI<-subset(dataI,RoundCode>1)
dataI<-subset(dataI,SizeNetwork!=4)


dataAI<-read.table(paste(getwd(),"/ActionsIndiv.csv", sep =''),header=TRUE,sep=",")
dataI<-merge(dataI,dataAI, by=c("SessionCode","ParticipantID","RoundCode"))

avdata<-read.table(paste(getwd(),"/AverageActionsIndiv.csv", sep = ''),header=TRUE,sep=",")

dataI$MoB[dataI$BirthMonth==1]<-12
dataI$MoB[dataI$BirthMonth==2]<-11
dataI$MoB[dataI$BirthMonth==3]<-10
dataI$MoB[dataI$BirthMonth==4]<-9
dataI$MoB[dataI$BirthMonth==5]<-8
dataI$MoB[dataI$BirthMonth==6]<-7
dataI$MoB[dataI$BirthMonth==7]<-6
dataI$MoB[dataI$BirthMonth==8]<-5
dataI$MoB[dataI$BirthMonth==9]<-4
dataI$MoB[dataI$BirthMonth==10]<-3
dataI$MoB[dataI$BirthMonth==11]<-2
dataI$MoB[dataI$BirthMonth==12]<-1

dataI$Age<-(2023-dataI$BirthYear)*12+5+dataI$MoB

dataI$NumberLinks[dataI$RoundCode==2]<-3
dataI$NumberLinks[dataI$RoundCode==3]<-3
dataI$NumberLinks[dataI$RoundCode==4]<-2
dataI$NumberLinks[dataI$RoundCode==5]<-2
dataI$NumberLinks[dataI$RoundCode==6]<-3
dataI$NumberLinks[dataI$RoundCode==7]<-3
dataI$NumberLinks[dataI$RoundCode==8]<-2
dataI$NumberLinks[dataI$RoundCode==9]<-2
dataI$NumberLinks[dataI$RoundCode==10]<-2
dataI$NumberLinks[dataI$RoundCode==11]<-2
dataI$NumberLinks[dataI$RoundCode==12]<-3
dataI$NumberLinks[dataI$RoundCode==13]<-3
dataI$NumberLinks[dataI$RoundCode==14]<-2
dataI$NumberLinks[dataI$RoundCode==15]<-2
dataI$NumberLinks[dataI$RoundCode==16]<-3
dataI$NumberLinks[dataI$RoundCode==17]<-3

dataI$NumberColors[dataI$RoundCode==2]<-2
dataI$NumberColors[dataI$RoundCode==3]<-2
dataI$NumberColors[dataI$RoundCode==4]<-2
dataI$NumberColors[dataI$RoundCode==5]<-2
dataI$NumberColors[dataI$RoundCode==6]<-4
dataI$NumberColors[dataI$RoundCode==7]<-4
dataI$NumberColors[dataI$RoundCode==8]<-4
dataI$NumberColors[dataI$RoundCode==9]<-4
dataI$NumberColors[dataI$RoundCode==10]<-2
dataI$NumberColors[dataI$RoundCode==11]<-2
dataI$NumberColors[dataI$RoundCode==12]<-2
dataI$NumberColors[dataI$RoundCode==13]<-2
dataI$NumberColors[dataI$RoundCode==14]<-4
dataI$NumberColors[dataI$RoundCode==15]<-4
dataI$NumberColors[dataI$RoundCode==16]<-4
dataI$NumberColors[dataI$RoundCode==17]<-4

datatest<-subset(dataI,RoundCode==2)[c("SessionCode","ParticipantID","Grade","Gender","Age")]

avdata<-merge(avdata,datatest,by=c("SessionCode","ParticipantID"))

# Function to run multiple simulations across different p values
run_simulations <- function() {
  results <- data.frame(p = numeric(), AverageTime = numeric(), ConvergenceLikelihood = numeric())
  
  for (p in seq(0.5, 1, by = 0.02)) {
    convergence_times <- replicate(num_simulations, perform_simulation(p,x))
    converged <- !is.na(convergence_times)
    average_time <- mean(convergence_times[converged], na.rm = TRUE)
    likelihood_of_convergence <- mean(converged)
    
    
    results <- rbind(results, data.frame(p = p, AverageTime = average_time, ConvergenceLikelihood = likelihood_of_convergence))
  }
  
  return(results)
}

l <- 2    # Number of links
s <- "A"
colorvector<-c("red","blue") #,"yellow", "brown")
disabled <- 8 # create the global variable to disable colors in previous rounds; initially not 1-6
num_simulations <- 1000 # number simulations

# Running simulations for C2N2
simulation_resultsC2N2 <- run_simulations()

# Running Simulations for C4N2
colorvector<-c("red","blue","yellow", "brown")
simulation_resultsC4N2 <- run_simulations()

# Running Simulations for C2N3
colorvector<-c("red","blue") #,"yellow", "brown")
l <- 3
simulation_resultsC2N3 <- run_simulations()

# Running Simulations for C4N3
colorvector<-c("red","blue","yellow", "brown")
simulation_resultsC4N3 <- run_simulations()

#Graphs
#Combine the 4 dataframes to get the whole picture

# Convert dataframes to data.tables
dt1 <- as.data.table(simulation_resultsC2N2)
dt2 <- as.data.table(simulation_resultsC4N2)
dt3 <- as.data.table(simulation_resultsC2N3)
dt4 <- as.data.table(simulation_resultsC4N3)

# Combine data.tables and calculate the average
combinedgraph <- rbindlist(list(dt1, dt2, dt3, dt4))[, .(
  AverageTime = mean(AverageTime, na.rm = TRUE),
  ConvergenceLikelihood = mean(ConvergenceLikelihood, na.rm = TRUE)
), by = p]

#grab the closest p to the population average that was tested (.755)
specific_p_data <- combinedgraph %>%
  filter(p == .76) %>%
  slice(1) %>%
  dplyr::select(p, ConvergenceLikelihood) %>%
  unlist()

# Extract that p and ConvergenceLikelihood at that p for plotting
specific_p <- specific_p_data["p"]
specific_likelihood <- specific_p_data["ConvergenceLikelihood"]

specific_p_data <- combinedgraph %>%
  filter(ConvergenceLikelihood == max(ConvergenceLikelihood, na.rm = TRUE)) %>%
  slice(1) %>%
  dplyr::select(p, ConvergenceLikelihood) %>%
  unlist()

# Extract p and ConvergenceLikelihood for plotting
p_star <- specific_p_data["p"]
p_star_specific_likelihood <- specific_p_data["ConvergenceLikelihood"]


#4 Lines, 1 Graph, with p* values for all
# Combine the dataframes with category labels
combined_df <- bind_rows(
  mutate(simulation_resultsC2N2, Category = "C2N2"),
  mutate(simulation_resultsC4N2, Category = "C4N2"),
  mutate(simulation_resultsC2N3, Category = "C2N3"),
  mutate(simulation_resultsC4N3, Category = "C4N3")
)

# Specific p value for the vertical dashed line
vertical_p <- round(vertical_p,3)

plot <- ggplot(combined_df, aes(x = p, y = ConvergenceLikelihood, color = Category)) +
  geom_line(size = 3) +
  coord_cartesian(xlim = c(0.5, 1), ylim = c(0, 1)) +
  geom_vline(xintercept = p_star, linetype = "dashed", color = "yellow", size = 2) +
  geom_vline(xintercept = vertical_p, linetype = "dashed", color = "grey", size = 2) +
  labs(x = "p", y = "Likelihood of Convergence", color = "Configuration") +
  theme_minimal() +
  scale_color_manual(values = c("C2N2"="red","C4N2"="lightblue","C2N3"="orange","C4N3"="purple")) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold")
  ) +
  annotate("text", x = vertical_p, y = 0.1,
           label = paste0("hat(p)==", signif(vertical_p, 4)),
           parse = TRUE, vjust = 1.5, size = 8, color = "black", fontface = "bold") +
  annotate("text", x = p_star, y = 0.1,
           label = paste0("p^\"*\"==", signif(p_star, 4)),
           parse = TRUE, vjust = 1.5, size = 8, color = "black", fontface = "bold") +
  annotate("text", x = 0.53, y = 0.65, label = "C2N2", color = "red",    size = 8, hjust = 1) +
  annotate("text", x = 0.53, y = 0.47, label = "C4N2", color = "lightblue",  size = 8, hjust = 1) +
  annotate("text", x = 0.53, y = 0.555,label = "C2N3", color = "orange", size = 8, hjust = 1) +
  annotate("text", x = 0.53, y = 0.375,label = "C4N3", color = "purple", size = 8, hjust = 1)

plot






#CODE FOR FIGURE 6-----------------------------------------------------------------------------------------------------
#RUNS NEW SIMULATIONS EVERY TIME SO RESULTS WILL BE SLIGHTLY DIFFERENT EACH TIME

dataI<-read.table(paste(getwd(),"/dataNetworksIndiv.csv", sep = ''),header=TRUE,sep=",")
dataI<-subset(dataI,RoundCode>1)
#test<-subset(dataI,SizeNetwork==4)
dataI<-subset(dataI,SizeNetwork!=4)


dataAI<-read.table(paste(getwd(),"/ActionsIndiv.csv", sep =''),header=TRUE,sep=",")
dataI<-merge(dataI,dataAI, by=c("SessionCode","ParticipantID","RoundCode"))
avdata<-read.table(paste(getwd(),"/AverageActionsIndiv.csv", sep = ''),header=TRUE,sep=",")


dataI$MoB[dataI$BirthMonth==1]<-12
dataI$MoB[dataI$BirthMonth==2]<-11
dataI$MoB[dataI$BirthMonth==3]<-10
dataI$MoB[dataI$BirthMonth==4]<-9
dataI$MoB[dataI$BirthMonth==5]<-8
dataI$MoB[dataI$BirthMonth==6]<-7
dataI$MoB[dataI$BirthMonth==7]<-6
dataI$MoB[dataI$BirthMonth==8]<-5
dataI$MoB[dataI$BirthMonth==9]<-4
dataI$MoB[dataI$BirthMonth==10]<-3
dataI$MoB[dataI$BirthMonth==11]<-2
dataI$MoB[dataI$BirthMonth==12]<-1

dataI$Age<-(2023-dataI$BirthYear)*12+5+dataI$MoB



dataI$NumberLinks[dataI$RoundCode==2]<-3
dataI$NumberLinks[dataI$RoundCode==3]<-3
dataI$NumberLinks[dataI$RoundCode==4]<-2
dataI$NumberLinks[dataI$RoundCode==5]<-2
dataI$NumberLinks[dataI$RoundCode==6]<-3
dataI$NumberLinks[dataI$RoundCode==7]<-3
dataI$NumberLinks[dataI$RoundCode==8]<-2
dataI$NumberLinks[dataI$RoundCode==9]<-2
dataI$NumberLinks[dataI$RoundCode==10]<-2
dataI$NumberLinks[dataI$RoundCode==11]<-2
dataI$NumberLinks[dataI$RoundCode==12]<-3
dataI$NumberLinks[dataI$RoundCode==13]<-3
dataI$NumberLinks[dataI$RoundCode==14]<-2
dataI$NumberLinks[dataI$RoundCode==15]<-2
dataI$NumberLinks[dataI$RoundCode==16]<-3
dataI$NumberLinks[dataI$RoundCode==17]<-3

dataI$NumberColors[dataI$RoundCode==2]<-2
dataI$NumberColors[dataI$RoundCode==3]<-2
dataI$NumberColors[dataI$RoundCode==4]<-2
dataI$NumberColors[dataI$RoundCode==5]<-2
dataI$NumberColors[dataI$RoundCode==6]<-4
dataI$NumberColors[dataI$RoundCode==7]<-4
dataI$NumberColors[dataI$RoundCode==8]<-4
dataI$NumberColors[dataI$RoundCode==9]<-4
dataI$NumberColors[dataI$RoundCode==10]<-2
dataI$NumberColors[dataI$RoundCode==11]<-2
dataI$NumberColors[dataI$RoundCode==12]<-2
dataI$NumberColors[dataI$RoundCode==13]<-2
dataI$NumberColors[dataI$RoundCode==14]<-4
dataI$NumberColors[dataI$RoundCode==15]<-4
dataI$NumberColors[dataI$RoundCode==16]<-4
dataI$NumberColors[dataI$RoundCode==17]<-4

datatest<-subset(dataI,RoundCode==2)[c("SessionCode","ParticipantID","Grade","Gender","Age")]

avdata<-merge(avdata,datatest,by=c("SessionCode","ParticipantID"))


# Define the grades
grades <- unique(avdata$Grade)
n_bins <- 10
# Create a list to store the plots
plot_list <- list()

for (grade in grades) {
  # Simplified title: K, 1, or 2
  grade_title <- ifelse(grade == 0, "K", as.character(grade)) 
  
  # Filter the data for the current grade
  grade_data <- avdata[avdata$Grade == grade, ]
  
  # Calculate bin width
  bin_width <- (max(grade_data$Rank) - min(grade_data$Rank)) / n_bins
  
  # Simulate the averages
  set.seed(626)
  n_simulations <- 1000
  simulated_averages <- replicate(n_simulations, mean(sample(1:6, 16, replace = TRUE)))
  
  # Perform MLE to find the optimal weight
  neg_log_likelihood <- function(weight, actual_data, sim_data) {
    density_sim <- density(sim_data, bw = "nrd0", from = min(actual_data), to = max(actual_data))
    density_sim_values <- approx(density_sim$x, density_sim$y, xout = actual_data)$y
    density_uniform_values <- dunif(actual_data, min = min(actual_data), max = max(actual_data))
    
    mix_density <- weight * density_sim_values + (1 - weight) * density_uniform_values
    mix_density[mix_density < 1e-10] <- 1e-10  # Avoid log(0)
    
    -sum(log(mix_density))
  }
  
  mle_result <- optim(par = 0.5, 
                      fn = neg_log_likelihood, 
                      actual_data = grade_data$Rank, 
                      sim_data = simulated_averages, 
                      method = "Brent", 
                      lower = 0, 
                      upper = 1)
  
  optimal_weight <- mle_result$par
  
  # Generate the mixture density
  density_actual <- density(grade_data$Rank, bw = "nrd0", from = min(grade_data$Rank), to = max(grade_data$Rank))
  density_sim <- density(simulated_averages, bw = "nrd0", from = min(grade_data$Rank), to = max(grade_data$Rank))
  x_points <- density_actual$x
  
  optimal_mix_density <- optimal_weight * approx(density_sim$x, density_sim$y, xout = x_points)$y + 
    (1 - optimal_weight) * dunif(x_points, min = min(grade_data$Rank), max = max(grade_data$Rank))
  
  # Create a data frame for plotting the optimal mixture density
  optimal_mix_data <- data.frame(x = x_points, y = optimal_mix_density)
  
  # Create the plot
  plot <- ggplot(grade_data, aes(x = Rank)) +
    geom_histogram(aes(y = ..count.. / sum(..count..), fill = "Empirical"), binwidth = bin_width, color = "black") +
    labs(x = "Rank", y = "Density", title = grade_title) +
    theme_minimal(base_size = 14) + ylim(0, 0.5) +
    scale_x_continuous(breaks = 1:6) +  # Set consistent x-axis labels
    geom_hline(aes(yintercept = 1 / 11, color = "Expected Density\nfor Perfect Correlation"), linetype = "dashed", size = 0.75) +
    stat_density(data = data.frame(Rank = simulated_averages), aes(x = Rank, y = ..density.. * bin_width, color = "Simulated Density\nfor No Correlation"), size = 0.75, bw = "nrd0", geom = "line") +
    geom_line(data = optimal_mix_data, aes(x = x, y = y * bin_width, color = "Optimal Mixture\nDensity"), size = 1.5) +
    scale_fill_manual(values = c("Empirical" = "blue"), labels = c("Empirical" = "Empirical")) +
    scale_color_manual(values = c("Expected Density\nfor Perfect Correlation" = "red", "Simulated Density\nfor No Correlation" = "green", "Optimal Mixture\nDensity" = "black"),
                       labels = c("Expected Density\nfor Perfect Correlation" = "Perfect Correlation",
                                  "Simulated Density\nfor No Correlation" = "No Correlation",
                                  "Optimal Mixture\nDensity" = "Optimal Mixture Density")) +
    guides(fill = guide_legend(order = 1, title = NULL), 
           color = guide_legend(order = 2, title = NULL)) +
    theme(
      legend.text = element_text(size = 15),
      plot.title = element_text(face = "bold", size = 24, hjust = 0.5),  # Center titles
      axis.title.y = element_text(size = 20)  # Set consistent y-axis title size
    )
  
  # Add the plot to the list
  plot_list[[as.character(grade)]] <- plot
}

# Reorder the plot list according to the desired order
ordered_plot_list <- plot_list[c("0", "1", "2")]  # Ensure order: "K", "1", "2"
# Ensure consistent x-axis labels and ranges for all plots
# Ensure consistent x-axis labels and ranges for all plots
for (i in seq_along(ordered_plot_list)) {
  ordered_plot_list[[i]] <- ordered_plot_list[[i]] +
    scale_x_continuous(breaks = 1:6, limits = c(1, 6)) +  # Ensure consistent x-axis labels and range
    theme(
      strip.text = element_text(size = 24, face = "bold", hjust = 0.5),  # Center K, 1, 2
      axis.title.y = if (i == 1) element_text(size = 20) else element_blank()  # Y-axis title only on the leftmost plot
    )
}

# Combine the plots side by side using patchwork
final_plot <- wrap_plots(ordered_plot_list, ncol = 3, guides = "collect") &
  theme(
    legend.position = "bottom",                  # Move legend to the bottom
    legend.text = element_text(size = 20),       # Increase legend text size
    legend.title = element_blank(),              # Remove legend title
    legend.key.size = unit(1, "cm"),             # Increase legend key size
    axis.text = element_text(size = 18),         # Increase axis tick label size
    axis.title.x = element_text(size = 20),      # X-axis title size consistent
    strip.text = element_text(size = 24, face = "bold", hjust = 0.5)  # Center K, 1, 2
  )

# Display the combined plot
print(final_plot)





#CODE FOR FIGURE 7-------------------------------------------------------------------------------------------------------
#RUNS NEW SIMULATIONS EVERY TIME SO RESULTS WILL BE SLIGHTLY DIFFERENT EACH TIME
# Optimal Q
#IN THIS CODE, X is the same thing as Q; it was called X in an earlier iteration
bin_width <- (max(avdata$Rank) - min(avdata$Rank)) / n_bins

# Calculate the mean and standard deviation of the data
mean_rank <- mean(avdata$Rank)
sd_rank <- sd(avdata$Rank)

set.seed(626) # For reproducibility
n_simulations <- 1000
simulated_averages <- replicate(n_simulations, mean(sample(1:6, 16, replace = TRUE)))

# Define the negative log-likelihood function
neg_log_likelihood <- function(weight, actual_data, sim_data) {
  density_sim <- density(sim_data, bw = "nrd0", from = min(actual_data), to = max(actual_data))
  density_sim_values <- approx(density_sim$x, density_sim$y, xout = actual_data)$y
  density_uniform_values <- dunif(actual_data, min = min(actual_data), max = max(actual_data))
  
  mix_density <- weight * density_sim_values + (1 - weight) * density_uniform_values
  # Avoid log(0) by setting a minimum threshold
  mix_density[mix_density < 1e-10] <- 1e-10
  
  -sum(log(mix_density))
}

# Perform MLE to find the optimal weight
mle_result <- optim(par = 0.5, 
                    fn = neg_log_likelihood, 
                    actual_data = avdata$Rank, 
                    sim_data = simulated_averages, 
                    method = "Brent", 
                    lower = 0, 
                    upper = 1)

vertical_q <- mle_result$par

p <- vertical_p

initialize_network_colorsAs <- function(g, x) {
  node_ids <- V(g)
  while (length(node_ids) > 0) { #while loop that ensures this continues until all 6 nodes are "sampled"
    for (node_id in sample(node_ids, 1, replace = TRUE)) {
      if (length(node_ids) == 1) { #Quirk of sample function requires me to manually set the final node
        node_id <- node_ids[1]
      }
      neighbor_ids <- neighbors(g, node_id)
      neighbor_colors <- V(g)$color[neighbor_ids]
      colors <- c(V(g)$color[node_id], neighbor_colors) #A and C strategies include counting your own color
      colors <- colors[(! colors %in% c("white"))] #remove white colors from table
      color_table <- table(colors)
      
      
      # Determine if there's a clear majority color
      sorted_colors <- sort(color_table, decreasing = TRUE)
      is_tie <- length(sorted_colors) > 1 && sorted_colors[1] == sorted_colors[2]
      
      draw <- runif(1, 0, 1) #draw random number between 0 and 1
      if (draw < x) { #compare draw to x to determine what path is taken
        new_color <- sample(colorvector, 1, replace = TRUE)} # draw < x means random sample
      else{ if (nrow(color_table) == 0) { # draw > x but no neighbors have drawn a color, so random
        new_color <- sample(colorvector, 1, replace = TRUE)
      }
        else{
          if (is_tie) {
            # Tie situation - Decide to keep the same color with probability
            new_color_choices <- names(sorted_colors)  # All colors tied for majority
            new_color <- sample(new_color_choices, 1) # random dplyr::selection when two colors are tied
          }   else {
            # No tie: proceed as before
            majority_color <- names(which.max(color_table))
            non_majority_colors <- setdiff(colorvector, majority_color)
            new_color <- ifelse(runif(1) < 1, majority_color, sample(non_majority_colors, 1))}}}
      # p changed to 1 to ensure the majority color is ALWAYS FOLLOWED
      
      V(g)$color[node_id] <- new_color
      node_ids <- node_ids[(!node_ids %in% node_id)] #remove chosen node_id from dplyr::selection in next rounds
    }}
  return(g)
}


# Function to run multiple simulations across different q values
run_simulations <- function(p) {
  results <- data.frame(x = numeric(), AverageTime = numeric(), ConvergenceLikelihood = numeric())
  
  for (x in seq(0, 1, by = .1)) {
    convergence_times <- replicate(num_simulations, perform_simulation(p,x))
    converged <- !is.na(convergence_times)
    average_time <- mean(convergence_times[converged], na.rm = TRUE)
    likelihood_of_convergence <- mean(converged)
    
    
    results <- rbind(results, data.frame(x = x, AverageTime = average_time, ConvergenceLikelihood = likelihood_of_convergence))
  }
  
  return(results)
}

# Parameters For C2N2
l <- 2    # Number of links
s <- "As"
colorvector<-c("red","blue") #,"yellow", "brown")
disabled <- 8 # create the global variable to disable colors in previous rounds; initially not 1-6
num_simulations <- 1000 # number simulations

# Running simulations for C2N2
simulation_resultsC2N2 <- run_simulations(p)

# Running Simulations for C4N2
colorvector<-c("red","blue","yellow", "brown")
simulation_resultsC4N2 <- run_simulations(p)

# Running Simulations for C2N3
colorvector<-c("red","blue") #,"yellow", "brown")
l <- 3
simulation_resultsC2N3 <- run_simulations(p)

# Running Simulations for C4N3
colorvector<-c("red","blue","yellow", "brown")
simulation_resultsC4N3 <- run_simulations(p)

#Graphs
#Combine the 4 dataframes to get the whole picture

# Convert dataframes to data.tables
dt1 <- as.data.table(simulation_resultsC2N2)
dt2 <- as.data.table(simulation_resultsC4N2)
dt3 <- as.data.table(simulation_resultsC2N3)
dt4 <- as.data.table(simulation_resultsC4N3)

# Combine data.tables and calculate the average
combinedgraph <- rbindlist(list(dt1, dt2, dt3, dt4))[, .(
  AverageTime = mean(AverageTime, na.rm = TRUE),
  ConvergenceLikelihood = mean(ConvergenceLikelihood, na.rm = TRUE)
), by = x]

#grab the highest q of the combined graphs
specific_x_data <- combinedgraph %>%
  filter(ConvergenceLikelihood == max(ConvergenceLikelihood, na.rm = TRUE)) %>%
  slice(1) %>%
  dplyr::select(x, ConvergenceLikelihood) %>%
  unlist()

# Extract that q and ConvergenceLikelihood for plotting
specific_q <- specific_x_data["x"]
specific_likelihood <- specific_x_data["ConvergenceLikelihood"]

# Extract max q and ConvergenceLikelihood for C2N2
max_likelihood_xC2N2 <- simulation_resultsC2N2 %>%
  filter(ConvergenceLikelihood == max(ConvergenceLikelihood, na.rm = TRUE)) %>%
  slice(1) %>%
  dplyr::select(x, ConvergenceLikelihood) %>%
  unlist()

# Combine the dataframes with category labels
combined_df <- bind_rows(
  mutate(simulation_resultsC2N2, Category = "C2N2"),
  mutate(simulation_resultsC4N2, Category = "C4N2"),
  mutate(simulation_resultsC2N3, Category = "C2N3"),
  mutate(simulation_resultsC4N3, Category = "C4N3")
)


plot <- ggplot(combined_df, aes(x = x, y = ConvergenceLikelihood, color = Category)) +
  geom_line(size = 3) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  geom_vline(xintercept = specific_q, linetype = "dashed", color = "grey",   size = 2) +
  geom_vline(xintercept = vertical_q, linetype = "dashed", color = "yellow", size = 2) +
  theme_minimal() +
  labs(x = "q", y = "Likelihood of Convergence", color = "Configuration") +
  scale_color_manual(values = c("C2N2"="red","C4N2"="lightblue","C2N3"="orange","C4N3"="purple")) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 22),
    axis.text  = element_text(size = 20),
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold")
  ) +
  annotate("text", x = 0.2, y = 0.1,
           label = paste0("hat(q)==", signif(vertical_q, 4)),
           parse = TRUE, vjust = 1.5, size = 8, color = "black", fontface = "bold") +
  annotate("text", x = 0.05, y = 0.2,
           label = paste0("q^\"*\"==", signif(specific_q, 4)),
           parse = TRUE, vjust = 1.5, size = 8, color = "black", fontface = "bold") +
  annotate("text", x = 0.9, y = 0.625, label = "C2N2", color = "red",    size = 8, hjust = 1) +
  annotate("text", x = 0.9, y = 0.425, label = "C4N2", color = "lightblue",  size = 8, hjust = 1) +
  annotate("text", x = 0.9, y = 0.925, label = "C2N3", color = "orange", size = 8, hjust = 1) +
  annotate("text", x = 0.9, y = 0.795, label = "C4N3", color = "purple", size = 8, hjust = 1)

plot



#CODE FOR TABLE 1 USES INFO FROM FIGURE 2----------------------------------------------------------------------------------
#RUNS NEW SIMULATIONS EVERY TIME SO RESULTS WILL BE SLIGHTLY DIFFERENT EACH TIME
PopulationConvergences <- dplyr::select(Convergence,c("Conv","Grade","Freq", "ColLinks"))
PC <- subset(PopulationConvergences, Conv == "yes")

p <- vertical_p # Probability of choosing the majority color
l <- 2 #number links
s <- "A"
colorvector<-c("red","blue") #, "yellow", "brown")
disabled <- 8 # create the global variable to disable colors in previous rounds; initially not 1-6

num_simulations <- 1000 # number simulations
x<-1
# Run simulations and collect results

convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C2N2ALp <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))

colorvector<-c("red","blue", "yellow", "brown")
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C4N2ALp <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))

colorvector<-c("red","blue")
l <- 3
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C2N3ALp <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))

colorvector<-c("red","blue", "yellow", "brown")
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C4N3ALp <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))

s <- "As"
colorvector<-c("red","blue") #, "yellow", "brown")
disabled <- 8 # create the global variable to disable colors in previous rounds; initially not 1-6
x<-vertical_q
l<-2
# Run simulations and collect results

convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C2N2ALpq <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))

colorvector<-c("red","blue", "yellow", "brown")
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C4N2ALpq <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))

colorvector<-c("red","blue")
l <- 3
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C2N3ALpq <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))

colorvector<-c("red","blue", "yellow", "brown")
convergence_times <- replicate(num_simulations, perform_simulation(p,x))
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)
C4N3ALpq <- nrow(subset(convergence_df, !is.na(ConvergenceTime)))

C2N2pop <- c(PC[1,3],PC[2,3],PC[3,3],PC[13,3],C2N2ALp/num_simulations,C2N2ALpq/num_simulations)
C4N2pop <- c(PC[7,3],PC[8,3],PC[9,3],PC[14,3],C4N2ALp/num_simulations,C4N2ALpq/num_simulations)
C2N3pop <- c(PC[4,3],PC[5,3],PC[6,3],PC[15,3],C2N3ALp/num_simulations,C2N3ALpq/num_simulations)
C4N3pop <- c(PC[10,3],PC[11,3],PC[12,3],PC[16,3],C4N3ALp/num_simulations,C4N3ALpq/num_simulations)

table <- rbind(C2N2pop, C4N2pop, C2N3pop, C4N3pop)
table <- as.data.frame(table)
colnames(table) <- c("K", "1", "2", "AL", "ALp", "ALpq")
rownames(table) <- c("C2N2","C4N2", "C2N3", "C4N3")
print(table)

#CODE FOR TABLE SI1 -------------------------------------------------------------------------------------------------------------
data<-read.table(paste(getwd(),"/Probabilities.csv", sep = ''),header=TRUE,sep=",")


# Age, gender, grade information

dataI<-read.table(paste(getwd(),"/dataNetworksIndiv.csv", sep = ''),header=TRUE,sep=",")
dataI<-subset(dataI,RoundCode>1)
dataI<-subset(dataI,SizeNetwork!=4)

dataI$MoB[dataI$BirthMonth==1]<-12
dataI$MoB[dataI$BirthMonth==2]<-11
dataI$MoB[dataI$BirthMonth==3]<-10
dataI$MoB[dataI$BirthMonth==4]<-9
dataI$MoB[dataI$BirthMonth==5]<-8
dataI$MoB[dataI$BirthMonth==6]<-7
dataI$MoB[dataI$BirthMonth==7]<-6
dataI$MoB[dataI$BirthMonth==8]<-5
dataI$MoB[dataI$BirthMonth==9]<-4
dataI$MoB[dataI$BirthMonth==10]<-3
dataI$MoB[dataI$BirthMonth==11]<-2
dataI$MoB[dataI$BirthMonth==12]<-1

dataI$Age<-(2023-dataI$BirthYear)*12+5+dataI$MoB

dataKeep<-subset(dataI, RoundCode==2)
dataKeep<-dataKeep[c("ParticipantID","SessionCode","Grade","Age","Gender")]
datagroup<-read.table(paste(getwd(),"/dataNetworks.csv", sep =''),header=TRUE,sep=",")


data<-merge(data,dataKeep, by=c("ParticipantID","SessionCode"))
data$Grade[data$Grade == '0'] <- 'K'
data$Grade <- factor(data$Grade, levels = c("K", "1", "2"))

avdata<-read.table(paste(getwd(),"/AverageActionsIndiv.csv", sep = ''),header=TRUE,sep=",")
avdata$leader <- ifelse(avdata$Rank < 3, 1, 0)
avdata2<-avdata[c("ParticipantID", "Rank","ExtraClicks", "NumberChoices")]
data<-merge(data,avdata2, by=c("ParticipantID"))
data$leader <- ifelse(data$Rank < 3, 1, 0)
data$GenderM <- ifelse(data$Gender == "M", 1, 0)



#Using dataNetworks to Calculate P of Convergence for each group

datagroup<-subset(datagroup,RoundCode>1)
datagroup$PConvergence <- ifelse(datagroup$Convergence == "yes", 1,0)

rankdiff <- ddply(avdata, c("SessionCode"), summarise,
                  Nleader = sum(leader)
)

preg <- ddply(data, c("SessionCode"), summarise,
              NumM = sum(Gender == "M"),
              Age = mean(Age),
              meanP = mean(Probability),
              sdP = sd(Probability),
              minP = min(Probability),
              R2 = sum(X2Total_CorrectDec)/sum(X2Total_NumberOpp),
              R3 = sum(X3Total_CorrectDec)/sum(X3Total_NumberOpp),
              R4 = sum(X4Total_CorrectDec)/sum(X4Total_NumberOpp),
              R5 = sum(X5Total_CorrectDec)/sum(X5Total_NumberOpp),
              R6 = sum(X6Total_CorrectDec)/sum(X6Total_NumberOpp),
              R7 = sum(X7Total_CorrectDec)/sum(X7Total_NumberOpp),
              R8 = sum(X8Total_CorrectDec)/sum(X8Total_NumberOpp),
              R9 = sum(X9Total_CorrectDec)/sum(X9Total_NumberOpp),
              R10 = sum(X10Total_CorrectDec)/sum(X10Total_NumberOpp),
              R11 = sum(X11Total_CorrectDec)/sum(X11Total_NumberOpp),
              R12 = sum(X12Total_CorrectDec)/sum(X12Total_NumberOpp),
              R13 = sum(X13Total_CorrectDec)/sum(X13Total_NumberOpp),
              R14 = sum(X14Total_CorrectDec)/sum(X14Total_NumberOpp),
              R15 = sum(X15Total_CorrectDec)/sum(X15Total_NumberOpp),
              R16 = sum(X16Total_CorrectDec)/sum(X16Total_NumberOpp),
              R17 = sum(X17Total_CorrectDec)/sum(X17Total_NumberOpp))
#Merge rank dataframe and this 
preg <- merge(preg, rankdiff, by = c("SessionCode"))

#Flipping columns if their column name starts with R(aka every round column)
preg <- preg %>%
  pivot_longer(cols = starts_with("R"), 
               names_to = "RoundCode", 
               values_to = "PRound")

#Changing Round Code names 
preg$RoundCode[preg$RoundCode=="R2"] <- 2
preg$RoundCode[preg$RoundCode=="R3"] <- 3
preg$RoundCode[preg$RoundCode=="R4"] <- 4
preg$RoundCode[preg$RoundCode=="R5"] <- 5
preg$RoundCode[preg$RoundCode=="R6"] <- 6
preg$RoundCode[preg$RoundCode=="R7"] <- 7
preg$RoundCode[preg$RoundCode=="R8"] <- 8
preg$RoundCode[preg$RoundCode=="R9"] <- 9
preg$RoundCode[preg$RoundCode=="R10"] <- 10
preg$RoundCode[preg$RoundCode=="R11"] <- 11
preg$RoundCode[preg$RoundCode=="R12"] <- 12
preg$RoundCode[preg$RoundCode=="R13"] <- 13
preg$RoundCode[preg$RoundCode=="R14"] <- 14
preg$RoundCode[preg$RoundCode=="R15"] <- 15
preg$RoundCode[preg$RoundCode=="R16"] <- 16
preg$RoundCode[preg$RoundCode=="R17"] <- 17

#Adding links and color numbers
preg$NumberLinks[preg$RoundCode==2]<-3
preg$NumberLinks[preg$RoundCode==3]<-3
preg$NumberLinks[preg$RoundCode==4]<-2
preg$NumberLinks[preg$RoundCode==5]<-2
preg$NumberLinks[preg$RoundCode==6]<-3
preg$NumberLinks[preg$RoundCode==7]<-3
preg$NumberLinks[preg$RoundCode==8]<-2
preg$NumberLinks[preg$RoundCode==9]<-2
preg$NumberLinks[preg$RoundCode==10]<-2
preg$NumberLinks[preg$RoundCode==11]<-2
preg$NumberLinks[preg$RoundCode==12]<-3
preg$NumberLinks[preg$RoundCode==13]<-3
preg$NumberLinks[preg$RoundCode==14]<-2
preg$NumberLinks[preg$RoundCode==15]<-2
preg$NumberLinks[preg$RoundCode==16]<-3
preg$NumberLinks[preg$RoundCode==17]<-3

preg$NumberColors[preg$RoundCode==2]<-2
preg$NumberColors[preg$RoundCode==3]<-2
preg$NumberColors[preg$RoundCode==4]<-2
preg$NumberColors[preg$RoundCode==5]<-2
preg$NumberColors[preg$RoundCode==6]<-4
preg$NumberColors[preg$RoundCode==7]<-4
preg$NumberColors[preg$RoundCode==8]<-4
preg$NumberColors[preg$RoundCode==9]<-4
preg$NumberColors[preg$RoundCode==10]<-2
preg$NumberColors[preg$RoundCode==11]<-2
preg$NumberColors[preg$RoundCode==12]<-2
preg$NumberColors[preg$RoundCode==13]<-2
preg$NumberColors[preg$RoundCode==14]<-4
preg$NumberColors[preg$RoundCode==15]<-4
preg$NumberColors[preg$RoundCode==16]<-4
preg$NumberColors[preg$RoundCode==17]<-4

#Again using Data Networks to find convergences by round and binding them
datagroup<- datagroup[c("SessionCode","RoundCode","Grade","PConvergence")]
preg <- merge(preg, datagroup, by = c("SessionCode", "RoundCode"))

#Add categorical variable to see if there is a difference between the first
#and second half when it comes to converging probability

preg$secondhalf <- ifelse(as.numeric(preg$RoundCode) < 10, 0, 1)

preg$secondtrial<- ifelse(as.numeric(preg$RoundCode) %% 2 == 1, 1, 0)

myprobit1<-glm(PConvergence ~ relevel(as.factor(Grade),ref="1")+NumM +  
                 as.factor(secondhalf) +as.factor(secondtrial)+ as.factor(NumberColors) + relevel(as.factor(NumberLinks),ref="3"),
               family = binomial(link = "probit"), data = preg)

summary(myprobit1)

#CODE FOR GRAPH SI3 (LEFT) -------------------------------------------------------------------------------
#RUNS NEW SIMULATIONS EVERY TIME SO RESULTS WILL BE SLIGHTLY DIFFERENT EACH TIME
# import data
data<-read.table(paste(getwd(),"/dataNetworks.csv", sep =''),header=TRUE,sep=",")
data2<-read.table(paste(getwd(),"/ActionsNetworks.csv", sep=''),header=TRUE,sep=",")
data3<-read.table(paste(getwd(),"/NetworksChoices.csv", sep=''),header=TRUE,sep=",")

data<-subset(data,RoundCode>1)

data<-merge(data,data2, by=c("RoundCode","SessionCode"))

data<-subset(data,SizeNetwork!=4)

data<-merge(data,data3, by=c("RoundCode","SessionCode"))

data$NumberLinks[data$RoundCode==2]<-3
data$NumberLinks[data$RoundCode==3]<-3
data$NumberLinks[data$RoundCode==4]<-2
data$NumberLinks[data$RoundCode==5]<-2
data$NumberLinks[data$RoundCode==6]<-3
data$NumberLinks[data$RoundCode==7]<-3
data$NumberLinks[data$RoundCode==8]<-2
data$NumberLinks[data$RoundCode==9]<-2
data$NumberLinks[data$RoundCode==10]<-2
data$NumberLinks[data$RoundCode==11]<-2
data$NumberLinks[data$RoundCode==12]<-3
data$NumberLinks[data$RoundCode==13]<-3
data$NumberLinks[data$RoundCode==14]<-2
data$NumberLinks[data$RoundCode==15]<-2
data$NumberLinks[data$RoundCode==16]<-3
data$NumberLinks[data$RoundCode==17]<-3

data$NumberColors[data$RoundCode==2]<-2
data$NumberColors[data$RoundCode==3]<-2
data$NumberColors[data$RoundCode==4]<-2
data$NumberColors[data$RoundCode==5]<-2
data$NumberColors[data$RoundCode==6]<-4
data$NumberColors[data$RoundCode==7]<-4
data$NumberColors[data$RoundCode==8]<-4
data$NumberColors[data$RoundCode==9]<-4
data$NumberColors[data$RoundCode==10]<-2
data$NumberColors[data$RoundCode==11]<-2
data$NumberColors[data$RoundCode==12]<-2
data$NumberColors[data$RoundCode==13]<-2
data$NumberColors[data$RoundCode==14]<-4
data$NumberColors[data$RoundCode==15]<-4
data$NumberColors[data$RoundCode==16]<-4
data$NumberColors[data$RoundCode==17]<-4

data$ColLinks<-paste("C",data$NumberColors,"L",data$NumberLinks)
# Convergence by color

Convergence<-data.frame(prop.table(table(data$Convergence,data$NumberColors),2))
colnames(Convergence)<-c("Conv","Colors","Freq")

#Variables for initial run. ADJUST AS NEEDED
p <- vertical_p # Probability of choosing the majority color
l <- 2 #number links
s <- "As"
x <- 1
colorvector<-c("red","blue", "yellow", "brown")
disabled <- 8 

#C4L2
convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC4N2 <- na.omit(convergence_times) +6 

# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)


tableC4L2<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC4L2$ColLinks<-"C4N2"
tableC4L2$source <- "Algorithm"
tableC4L2$Var1 <- ifelse(tableC4L2$Var1 == TRUE, "yes", "no")
tableC4L2$sum <- sum(subset(tableC4L2, Var1 == "yes")$Freq)

#Change for C4L3
l <- 3

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC4N3 <- na.omit(convergence_times) +6 
# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)


tableC4L3<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC4L3$ColLinks<-"C4N3"
tableC4L3$source <- "Algorithm"
tableC4L3$Var1 <- ifelse(tableC4L3$Var1 == TRUE, "yes", "no")
tableC4L3$sum <- sum(subset(tableC4L3, Var1 == "yes")$Freq)

#Change for C2L3

colorvector<-c("red","blue")

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC2N3 <- na.omit(convergence_times) +6 
# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)

tableC2L3<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC2L3$ColLinks<-"C2N3"
tableC2L3$source <- "Algorithm"
tableC2L3$Var1 <- ifelse(tableC2L3$Var1 == TRUE, "yes", "no")
tableC2L3$sum <- sum(subset(tableC2L3, Var1 == "yes")$Freq)


#Change for C2L2
l<- 2

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)
convergence_timesC2N2 <- na.omit(convergence_times) +6 
tableC2L2<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC2L2$ColLinks<-"C2N2"
tableC2L2$source <- "Algorithm"
tableC2L2$Var1 <- ifelse(tableC2L2$Var1 == TRUE, "yes", "no")
tableC2L2$sum <- sum(subset(tableC2L2, Var1 == "yes")$Freq)


#Combine Everything into one dataframe
tableCLT <- rbind(tableC2L2, tableC4L2, tableC2L3, tableC4L3)
tableCLT$Var2 <-as.numeric(as.character(tableCLT$Var2)) +6
tableCL$Var2 <- as.numeric(as.character(tableCL$Var2))
tableCLK <- rbind(tableCL, tableCLT)
# Convert Var2 to numeric if it is not already
tableCLK$Var2 <- as.numeric(as.character(tableCLK$Var2))

# Subset the data with the desired conditions
subset_data <- subset(tableCLK, Var1 == "yes" & Var2 <37)

# Subset the data with the desired conditions
subset_data <- subset(tableCLK, Var1 == "yes" & Var2 > 5 & Var2 < 37)
subset_data$ColLinks <- factor(subset_data$ColLinks, levels = c("C2N2", "C4N2", "C2N3", "C4N3"))
subset_data$FreqAdjusted <- subset_data$Freq/subset_data$sum

population_means <- subset(subset_data, source == "Population") %>%
  group_by(ColLinks) %>%
  summarize(
    sum_var2_freqadjusted = sum(Var2 * FreqAdjusted, na.rm = TRUE),
    sum_freqadjusted = sum(FreqAdjusted, na.rm = TRUE),
    weighted_mean_var2_freqadjusted = sum_var2_freqadjusted / sum_freqadjusted
  )

alg_means <- subset(subset_data, source == "Algorithm") %>%
  group_by(ColLinks) %>%
  summarize(
    sum_var2_freqadjusted = sum(Var2 * FreqAdjusted, na.rm = TRUE),
    sum_freqadjusted = sum(FreqAdjusted, na.rm = TRUE),
    weighted_mean_var2_freqadjusted = sum_var2_freqadjusted / sum_freqadjusted
  )


plot <- ggplot(subset_data, aes(x = Var2, y = FreqAdjusted, fill = source)) +
  facet_grid(cols = vars(ColLinks)) +
  geom_bar(data = subset(subset_data, source == "Population"), 
           width = 0.7, stat = "identity", position = "identity", alpha = 0.5, color = "blue") +
  geom_bar(data = subset(subset_data, source == "Algorithm"), 
           width = 0.7, stat = "identity", position = "identity", alpha = 0.5, color = "red") +
  labs(x = "Number Choices", y = "Frequency") +
  theme_minimal() +
  ylim(0,.6)+
geom_vline(data = population_means, aes(xintercept = sum_var2_freqadjusted), linetype = "dashed", color = "blue", size = 0.5) +
  geom_vline(data = alg_means, aes(xintercept = sum_var2_freqadjusted), linetype = "dashed", color = "red", size = 0.5) +
  scale_x_continuous(breaks = seq(6, 36, by = 3)) +  # Set y-axis limits
  scale_fill_manual(values = c("Population" = "blue", "Algorithm" = "red"),
                    labels = c("Population" = "Data", "Algorithm" = expression('AL'[hat(p)]))) +  # Modify legend labels
  theme(
    axis.title.x = element_text(size = 75),
    axis.title.y = element_text(size = 75),
    legend.title = element_blank(),
    legend.text = element_text(size = 50),
    strip.text = element_text(size = 62.5),  # Increase the font size of facet labels
    axis.text.x = element_text(size = 30),  # Increase the font size of x axis text
    axis.text.y = element_text(size = 30)   # Increase the font size of y axis text
  )

plot

#CODE FOR GRAPH SI3 (RIGHT) -------------------------------------------------------------------------------
#RUNS NEW SIMULATIONS EVERY TIME SO RESULTS WILL BE SLIGHTLY DIFFERENT EACH TIME
x <- vertical_q
l <- 2 #number links
s <- "As"
colorvector<-c("red","blue", "yellow", "brown")
disabled <- 8 

#C4L2
convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC4N2 <- na.omit(convergence_times) +6 

# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)
tableC4L2<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC4L2$ColLinks<-"C4N2"
tableC4L2$source <- "Algorithm"
tableC4L2$Var1 <- ifelse(tableC4L2$Var1 == TRUE, "yes", "no")
tableC4L2$sum <- sum(subset(tableC4L2, Var1 == "yes")$Freq)

#Change for C4L3
l <- 3

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC4N3 <- na.omit(convergence_times) +6 
# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)


tableC4L3<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC4L3$ColLinks<-"C4N3"
tableC4L3$source <- "Algorithm"
tableC4L3$Var1 <- ifelse(tableC4L3$Var1 == TRUE, "yes", "no")
tableC4L3$sum <- sum(subset(tableC4L3, Var1 == "yes")$Freq)

#Change for C2L3

colorvector<-c("red","blue")

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_timesC2N3 <- na.omit(convergence_times) +6 
# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)

tableC2L3<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC2L3$ColLinks<-"C2N3"
tableC2L3$source <- "Algorithm"
tableC2L3$Var1 <- ifelse(tableC2L3$Var1 == TRUE, "yes", "no")
tableC2L3$sum <- sum(subset(tableC2L3, Var1 == "yes")$Freq)


#Change for C2L2
l<- 2

convergence_times <- replicate(1000, perform_simulation(p,x))
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)
convergence_timesC2N2 <- na.omit(convergence_times) +6 
tableC2L2<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC2L2$ColLinks<-"C2N2"
tableC2L2$source <- "Algorithm"
tableC2L2$Var1 <- ifelse(tableC2L2$Var1 == TRUE, "yes", "no")
tableC2L2$sum <- sum(subset(tableC2L2, Var1 == "yes")$Freq)


#Combine Everything into one dataframe
tableCLT <- rbind(tableC2L2, tableC4L2, tableC2L3, tableC4L3)
tableCLT$Var2 <-as.numeric(as.character(tableCLT$Var2)) +6
tableCL$Var2 <- as.numeric(as.character(tableCL$Var2))
tableCLK <- rbind(tableCL, tableCLT)
# Convert Var2 to numeric if it is not already
tableCLK$Var2 <- as.numeric(as.character(tableCLK$Var2))

# Subset the data with the desired conditions
subset_data <- subset(tableCLK, Var1 == "yes" & Var2 <37)

# Subset the data with the desired conditions
subset_data <- subset(tableCLK, Var1 == "yes" & Var2 > 5 & Var2 < 37)
subset_data$ColLinks <- factor(subset_data$ColLinks, levels = c("C2N2", "C4N2", "C2N3", "C4N3"))
subset_data$FreqAdjusted <- subset_data$Freq/subset_data$sum

population_means <- subset(subset_data, source == "Population") %>%
  group_by(ColLinks) %>%
  summarize(
    sum_var2_freqadjusted = sum(Var2 * FreqAdjusted, na.rm = TRUE),
    sum_freqadjusted = sum(FreqAdjusted, na.rm = TRUE),
    weighted_mean_var2_freqadjusted = sum_var2_freqadjusted / sum_freqadjusted
  )

alg_means <- subset(subset_data, source == "Algorithm") %>%
  group_by(ColLinks) %>%
  summarize(
    sum_var2_freqadjusted = sum(Var2 * FreqAdjusted, na.rm = TRUE),
    sum_freqadjusted = sum(FreqAdjusted, na.rm = TRUE),
    weighted_mean_var2_freqadjusted = sum_var2_freqadjusted / sum_freqadjusted
  )

plot <- ggplot(subset_data, aes(x = Var2, y = FreqAdjusted, fill = source)) +
  facet_grid(cols = vars(ColLinks)) +
  geom_bar(data = subset(subset_data, source == "Population"), 
           width = 0.7, stat = "identity", position = "identity", alpha = 0.5, color = "blue") +
  geom_bar(data = subset(subset_data, source == "Algorithm"), 
           width = 0.7, stat = "identity", position = "identity", alpha = 0.5, color = "red") +
  labs(x = "Number Choices", y = "Frequency") +
  theme_minimal() +
  ylim(0,.6)+
  geom_vline(data = population_means, aes(xintercept = sum_var2_freqadjusted), linetype = "dashed", color = "blue", size = 0.5) +
  geom_vline(data = alg_means, aes(xintercept = sum_var2_freqadjusted), linetype = "dashed", color = "red", size = 0.5) +
  scale_x_continuous(breaks = seq(6, 36, by = 3)) +  # Set y-axis limits
  scale_fill_manual(values = c("Population" = "blue", "Algorithm" = "red"),
                    labels = c("Population" = "Data", "Algorithm" = expression('AL'[hat(p)]*','[hat(q)]))) +  # Modify legend labels
  theme(
    axis.title.x = element_text(size = 75),
    axis.title.y = element_text(size = 75),
    legend.title = element_blank(),
    legend.text = element_text(size = 50),
    strip.text = element_text(size = 62.5),  # Increase the font size of facet labels
    axis.text.x = element_text(size = 30),  # Increase the font size of x axis text
    axis.text.y = element_text(size = 30)   # Increase the font size of y axis text
  )

plot

#CODE FOR TABLE SI2-----------------------------------------------------------------------------------------

data<-read.table(paste(getwd(),"/Probabilities.csv", sep = ''),header=TRUE,sep=",")
dataI<-read.table(paste(getwd(),"/dataNetworksIndiv.csv", sep = ''),header=TRUE,sep=",")
dataI<-subset(dataI,RoundCode>1)
dataI<-subset(dataI,SizeNetwork!=4)
dataI$MoB[dataI$BirthMonth==1]<-12
dataI$MoB[dataI$BirthMonth==2]<-11
dataI$MoB[dataI$BirthMonth==3]<-10
dataI$MoB[dataI$BirthMonth==4]<-9
dataI$MoB[dataI$BirthMonth==5]<-8
dataI$MoB[dataI$BirthMonth==6]<-7
dataI$MoB[dataI$BirthMonth==7]<-6
dataI$MoB[dataI$BirthMonth==8]<-5
dataI$MoB[dataI$BirthMonth==9]<-4
dataI$MoB[dataI$BirthMonth==10]<-3
dataI$MoB[dataI$BirthMonth==11]<-2
dataI$MoB[dataI$BirthMonth==12]<-1

dataI$Age<-(2023-dataI$BirthYear)*12+5+dataI$MoB

dataKeep<-subset(dataI, RoundCode==2)
dataKeep<-dataKeep[c("ParticipantID","SessionCode","Grade","Age","Gender")]
datagroup<-read.table(paste(getwd(),"/dataNetworks.csv", sep =''),header=TRUE,sep=",")


data<-merge(data,dataKeep, by=c("ParticipantID","SessionCode"))
data$Grade[data$Grade == '0'] <- 'K'
data$Grade <- factor(data$Grade, levels = c("K", "1", "2"))

avdata<-read.table(paste(getwd(),"/AverageActionsIndiv.csv", sep = ''),header=TRUE,sep=",")
avdata<-subset(avdata,SessionCode !="d38z20jj")
avdata$leader <- ifelse(avdata$Rank < 3, 1, 0)
avdata2<-avdata[c("ParticipantID", "Rank","ExtraClicks", "NumberChoices")]
data<-merge(data,avdata2, by=c("ParticipantID"))
data$leader <- ifelse(data$Rank < 3, 1, 0)
data$GenderM <- ifelse(data$Gender == "M", 1, 0)
indiv<-merge(avdata,data[c("ParticipantID","SessionCode","Probability","Grade","Age","Gender")], by=c("ParticipantID", "SessionCode"))

conv<-ddply(preg, c("SessionCode"), summarise,
            AvConv = sum(PConvergence))

indiv<-merge(indiv,conv, by=c("SessionCode"))
dataI_reduced <- dataI %>%
  distinct(SessionCode, ParticipantID, id_in_session)

# Step 2: Merge the reduced dataI with indiv, keeping the original 400 rows from indiv
indiv <- merge(indiv, dataI_reduced, by = c("SessionCode", "ParticipantID"))

model5<-lm(Probability ~ Age+Gender+Rank+ExtraClicks,data = indiv)
summary(model5)

#CODE FOR TABLE SI3-------------------------------------------------------------------------------------------------

# import data
data<-read.table(paste(getwd(),"/dataNetworks.csv", sep =''),header=TRUE,sep=",")
data2<-read.table(paste(getwd(),"/ActionsNetworks.csv", sep=''),header=TRUE,sep=",")
data3<-read.table(paste(getwd(),"/NetworksChoices.csv", sep=''),header=TRUE,sep=",")

data<-subset(data,RoundCode>1)

data<-merge(data,data2, by=c("RoundCode","SessionCode"))

data<-subset(data,SizeNetwork!=4)

data<-merge(data,data3, by=c("RoundCode","SessionCode"))

data$NumberLinks[data$RoundCode==2]<-3
data$NumberLinks[data$RoundCode==3]<-3
data$NumberLinks[data$RoundCode==4]<-2
data$NumberLinks[data$RoundCode==5]<-2
data$NumberLinks[data$RoundCode==6]<-3
data$NumberLinks[data$RoundCode==7]<-3
data$NumberLinks[data$RoundCode==8]<-2
data$NumberLinks[data$RoundCode==9]<-2
data$NumberLinks[data$RoundCode==10]<-2
data$NumberLinks[data$RoundCode==11]<-2
data$NumberLinks[data$RoundCode==12]<-3
data$NumberLinks[data$RoundCode==13]<-3
data$NumberLinks[data$RoundCode==14]<-2
data$NumberLinks[data$RoundCode==15]<-2
data$NumberLinks[data$RoundCode==16]<-3
data$NumberLinks[data$RoundCode==17]<-3

data$NumberColors[data$RoundCode==2]<-2
data$NumberColors[data$RoundCode==3]<-2
data$NumberColors[data$RoundCode==4]<-2
data$NumberColors[data$RoundCode==5]<-2
data$NumberColors[data$RoundCode==6]<-4
data$NumberColors[data$RoundCode==7]<-4
data$NumberColors[data$RoundCode==8]<-4
data$NumberColors[data$RoundCode==9]<-4
data$NumberColors[data$RoundCode==10]<-2
data$NumberColors[data$RoundCode==11]<-2
data$NumberColors[data$RoundCode==12]<-2
data$NumberColors[data$RoundCode==13]<-2
data$NumberColors[data$RoundCode==14]<-4
data$NumberColors[data$RoundCode==15]<-4
data$NumberColors[data$RoundCode==16]<-4
data$NumberColors[data$RoundCode==17]<-4

data$ColLinks<-paste("C",data$NumberColors,"L",data$NumberLinks)
# Convergence by color

Convergence<-data.frame(prop.table(table(data$Convergence,data$NumberColors),2))
colnames(Convergence)<-c("Conv","Colors","Freq")


data$Convergence <- ifelse(data$Convergence == "yes", 1, 0)

#Time1move etc. are based on the id_in_session, so they need to be sorted to get the first fastest movement etc.
data$sorted_moves <- apply(data[, c("Time1move", "Time2move", "Time3move", "Time4move", "Time5move", "Time6move")], 
                           1,  # Apply function row-wise
                           function(x) sort(x, na.last = NA))

data$fastestmove <- sapply(data$sorted_moves, function(x) x[1])
data$secondmove <- sapply(data$sorted_moves, function(x) x[2])
data$thirdmove <- sapply(data$sorted_moves, function(x) x[3])
data$fourthmove <- sapply(data$sorted_moves, function(x) x[4])
data$fifthmove <- sapply(data$sorted_moves, function(x) x[5])
data$sixthmove <- sapply(data$sorted_moves, function(x) x[6])

#Creation of variable to store whether a player made their first move near network convergence
data6 <- subset(data, !is.na(sixthmove))
data6$online <- ifelse(abs(data6$RT - data6$sixthmove) <= 0.1 & data6$RT!=0 &data6$NumberChoices !=6, TRUE, FALSE)


ByColorsGrade <- subset(data6, NumberChoices != 6 & RT != 0) %>% #RT ==0 when the network did not converge
  group_by(Grade, NumberColors) %>%
  summarise(pass = sum(online),
            fail = n()-sum(online))

ByColorsGrade$proportion <- ByColorsGrade$pass/(ByColorsGrade$pass + ByColorsGrade$fail)
GradeK <- c(ByColorsGrade[1,5],ByColorsGrade[2,5])
Grade1 <- c(ByColorsGrade[3,5],ByColorsGrade[4,5])
Grade2 <- c(ByColorsGrade[5,5],ByColorsGrade[6,5])
table <- rbind(GradeK, Grade1,Grade2)
colnames(table) <- c("C2","C4")
rownames(table) <- c("K","1","2")

print(table)


#CODE FOR FIGURE SI4-------------------------------------------------------------------------------------------------


sniperhist <- data6 %>%
  group_by(SessionCode, Grade) %>%
  summarise(Num_snipe = sum(online)) %>%  # Summing the Freq for each Bracket
  ungroup()

# Assuming your data is in a dataframe named 'data'
# Ensure the Grade column is a factor with the correct levels
sniperhist$Grade <- factor(sniperhist$Grade, levels = c("0", "1", "2"))

# Replace "0" with "K" in the labels but retain the original factor levels for consistency
ggplot(sniperhist, aes(x = Num_snipe, fill = factor(Grade))) +
  geom_histogram(binwidth = 1, color = "black", position = "stack", alpha = 0.8) +  # Add transparency to fill
  scale_fill_manual(
    values = c("0" = "#66c2a5", "1" = "#fc8d62", "2" = "#8da0cb"),  # Ensure colors match desired output
    labels = c("K", "1", "2"),                                      # Replace "0" with "K" in the legend
    name = "Grade"
  ) +
  scale_x_continuous(breaks = 0:9) +  # Adjust x-axis breaks
  labs(
    x = "Number of Times",
    y = "Number of Networks",
    title = "Histogram of Number of Times Each Network was Sniped"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = 0:6) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center and bold title
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),   # Add space above x-axis label
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),   # Add space beside y-axis label
    legend.position = "top",                                           # Move legend to the top
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )


#CODE FOR TABLE SI4 -------------------------------------------------------------------------------------------------------------------------------------
data6<-read.table(paste(getwd(),"/dataNetworks.csv", sep =''),header=TRUE,sep=",")
data2<-read.table(paste(getwd(),"/ActionsNetworks.csv", sep=''),header=TRUE,sep=",")
data3<-read.table(paste(getwd(),"/NetworksChoices.csv", sep=''),header=TRUE,sep=",")
#
data6<-subset(data6,RoundCode>1)
#
data6<-merge(data6,data2, by=c("RoundCode","SessionCode"))
#
data6<-subset(data6,SizeNetwork!=4)
#
data6<-merge(data6,data3, by=c("RoundCode","SessionCode"))
#
#Time1move etc. are based on the id_in_session, so they need to be sorted to get the first fastest movement etc.
data6$sorted_moves <- apply(data6[, c("Time1move", "Time2move", "Time3move", "Time4move", "Time5move", "Time6move")],
                            1,  # Apply function row-wise
                            function(x) sort(x, na.last = NA))

data6$fastestmove <- sapply(data6$sorted_moves, function(x) x[1])
data6$secondmove <- sapply(data6$sorted_moves, function(x) x[2])
data6$thirdmove <- sapply(data6$sorted_moves, function(x) x[3])
data6$fourthmove <- sapply(data6$sorted_moves, function(x) x[4])
data6$fifthmove <- sapply(data6$sorted_moves, function(x) x[5])
data6$sixthmove <- sapply(data6$sorted_moves, function(x) x[6])
data6$gap61 <- ifelse(data6$NumberChoices >= 6, data6$sixthmove - data6$fastestmove,NA)
data6<- data6[c("SessionCode", "RoundCode", "gap61","NumberChoices", "RT", "fastestmove")]
lol <- merge(preg, data6, by= c("SessionCode", "RoundCode"))

lol$secondtrial<- ifelse(as.numeric(lol$RoundCode) %% 2 == 1, 1, 0)

myprobit1<-glm(PConvergence ~ relevel(as.factor(Grade),ref="1")+NumM + as.factor(secondhalf) +
                 as.factor(secondtrial)+fastestmove+as.factor(NumberColors) + relevel(as.factor(NumberLinks),ref="3"),
               family = binomial(link = "probit"), data = lol)

summary(myprobit1)










#CODE FOR TABLE SI5 -------------------------------------------------------------------------------------------------------------------------------------

# IT RERUNS THE SIMULATION EACH TIME, SO NUMBERS ARE BOUND TO CHANGE SLIGHTLY
#SET PARAMETERS HERE AND RUN THE FOLLOWING CODE TO GET THE FUNCTION TO PRODUCE A CHOSEN RESULT IN TABLE SI5

# Parameters
p <- .754  #.754 for p-hat and 1 for 1
l <-  3 #number links
colorvector<-c("red","blue") #, "yellow", "brown") #FOR 4 COLORS, REMOVE # and parenthesis
num_rounds <- 5 #NUMBER OF SUPER ROUNDS


s <- "A" # DO NOT CHANGE
x <- 1# DO NOT CHANGE
num_simulations <- 1000 # DO NOT CHANGE
 
##create graph with 2 links 
# Function to create a graph where each node has 2 links
create_custom_graph_2 <- function(n) {
  g <- make_ring(n)
  return(g)
}  


# create graph with 3 links

# Function to create a graph where each node has 3 links
create_custom_graph_3 <- function(n) {
  # Define the edges such that each node connects to 3 others
  edges <- c(1,2, 1,4, 1,6,  # Node 1 connects to Nodes 2, 3, and 6
             2,3, 2,5,        # Node 2 connects to Nodes 3 and 4 (already connected to 1)
             3,4, 3,6,        # Node 3 connects to Nodes 4 and 5 (already connected to 2)
             4,5,      # Node 4 connects to Nodes 5 and 6 (already connected to 1 and 3)
             5,6)            # Node 5 connects to Node 6 (already connected to 2 and 4)
  # Create the graph
  g <- graph(edges, directed = FALSE)
  return(g)
}
# create a graph with 4 links

# Function to create a graph where each node has 4 links
create_custom_graph_4 <- function(n) {
  # Define the edges such that each node connects to 4 others
  edges <- c(1,2, 1,3, 1,6, 1,5,  # Node 1 connects to Nodes 2, 3, 5, and 6
             2,3, 2,4, 2,6,       # Node 2 connects to Nodes 3, 4, 6 (already connected to 1)
             3,4, 3,5,       # Node 3 connects to Nodes 4, 5 (already connected to 1, 2)
             4,5, 4,6,            # Node 4 connects to Nodes 5, 6 (already connected to 2, 3)
             5,6)                 # Node 5 connects to Node 6 (already connected to 1, 3, 4)
  # Create the graph
  g <- graph(edges, directed = FALSE)
  return(g)
}

# Update network colors with strategy A

update_network_colorsA <- function(g, p) {
  node_ids <- V(g)
  for (node_id in sample(node_ids[c(-disabled)], 1, replace = FALSE)) { 
    if (length(node_ids[c(-disabled)]) == 1) { #Quirk of sample function requires me to manually set the final node
      node_id <- node_ids[c(-disabled)][1]
      #print(cat("The final node is: ", node_id))
    }
    #sample only 1 value from a vector without the 'disabled' value from the global variable
    neighbor_ids <- neighbors(g, node_id)
    neighbor_colors <- V(g)$color[neighbor_ids]
    colors <- c(V(g)$color[node_id], neighbor_colors) #A and C strategies include counting your own color
    color_table <- table(colors)
    #print(cat("The color_table used for ", node_id," is "))
    #print(color_table)
    disabled <<- append(disabled, node_id) #change the global variable disabled to whatever value was just dplyr::selected
    #print(cat("The disabled nodes for the following round are: ", disabled))
    # Determine if there's a clear majority color
    sorted_colors <- sort(color_table, decreasing = TRUE)
    is_tie <- length(sorted_colors) > 1 && sorted_colors[1] == sorted_colors[2]
    
    if (is_tie) {
      current_color <- V(g)$color[node_id]  # Current Color
      other_colors <- unique(colors[(! colors %in% current_color)])
      new_color <- ifelse(runif(1) < 1-p, current_color, sample(other_colors, 1))
    } else {
      # No tie: proceed as before
      majority_color <- names(which.max(color_table))
      non_majority_colors <- unique(colors)
      non_majority_colors <- setdiff(non_majority_colors, majority_color)
      if (length(non_majority_colors) == 0) {
        new_color <- majority_color} else {
          new_color <- ifelse(runif(1) < p, majority_color, sample(non_majority_colors, 1))}}
    
    # V(g)$color[node_id] <- new_color #save this to a global variable, then every 6 rounds attach the changes
    #print(cat("I am adding change ", new_color, " to node_id ", node_id))
    change_log[node_id] <<- new_color
    #print(cat("To confirm, while the current color is ", V(g)$color[node_id], "change log has color ", change_log[node_id], " can be the same"))
  }
  return(g)
}

# Perform a single simulation
# Function to perform a single simulation and return convergence time
perform_simulation <- function(p) {
  n <- 6  # Number of nodes
  colorvector
  
  if( s != "A") { #if using sequential strategy initialize graph through this method
    if(l==2){
      g <- create_custom_graph_2(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      g <- initialize_network_colorsAs(g,x)
    }
    if(l==3){
      g <- create_custom_graph_3(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      g <- initialize_network_colorsAs(g,x)
    }}
  else{
    if(l==2){
      g <- create_custom_graph_2(n)
      V(g)$color <- sample(colorvector, n, replace = TRUE)
    }else{
      
      if (l==3) {g <- create_custom_graph_3(n)
      #V(g)$color <- c("red", "blue", "red", "blue", "red", "blue")
      V(g)$color <- sample(colorvector, n, replace = TRUE)
      }else{g <- create_custom_graph_4(n)
      V(g)$color <- sample(colorvector, n, replace = TRUE)}
    } }
  print(V(g)$color)
  for (i in 1:num_rounds*6) {
    if (i == 1) {
      if (length(unique(V(g)$color)) == 1) {
        return(0) # return 0 in the case where everyone chose the same color through random chance
      }
    }
    if (s=="A") {g <- update_network_colorsA(g, p)}
    else {g <- update_network_colorsA(g, p)}
    if (length(change_log) == 6 && all(!is.na(change_log))){
      #print(cat("The colors were", V(g)$color))
      V(g)$color <- change_log
      #print(cat("Now they are", V(g)$color))
      change_log <<- c()
      disabled <<- c(8)
      #print(cat("Change_Log empty again:", change_log))
      #print(cat("disabled only includes 8", disabled))
    } else {
      #print(cat("The change_log was not full, so I didn't apply any changes. Proof is here: ", change_log))
    }
    # Check for convergence
    if (length(unique(V(g)$color)) == 1) {
      return(i)  # Return the iteration at which convergence occurred
    }
  }
  return(NA)  # Return NA if no convergence within 30 iterations
}



# Parameters and Graph



xstore <- x
disabled <- c(8) # create the global variable to disable colors in previous rounds; initially not 1-6
change_log <- c()

# Run simulations and collect results

convergence_times <- replicate(num_simulations, perform_simulation(p))

# Prepare data for plotting
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)

# Compute average convergence time, excluding simulations that didn't converge
average_convergence_time <- mean(convergence_times, na.rm = TRUE)
convergence_times.matrix = matrix(data = convergence_times, ncol = 1, byrow = TRUE)
convergence_times_number.df = as.data.frame(convergence_times.matrix)
number_of_convergences <- colSums(!is.na(convergence_times_number.df))
x <- na.omit(as.data.frame(convergence_times_number.df[ convergence_times_number.df[,1] == 0, ]))
number_of_instant <- nrow(x)

# Display results
cat("For p is", p ,", l is", l,"and the number of colors is", length(colorvector), ", running", num_simulations, "simulations, the network 
    converges", number_of_convergences, "times, the number of instant convergences is", number_of_instant, "and the average convergence time is:", 
    average_convergence_time, "\n")

# Generate a ggplot graph
plot <- ggplot(convergence_df, aes(x = ConvergenceTime)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + ylim(0,500) + xlim(0, 35) +
  labs(title = paste("Distribution of Convergence Times For simulations, \n strategy",s,",p =", p ,", l =", l,
                     "and # of colors =", length(colorvector),  ", no. of 
convergences is", number_of_convergences, "average time is", average_convergence_time,",if 
relevant, value of x is", xstore, "and no. of instant convergences is", number_of_instant, ""),
       x = "Number of Steps to Converge",
       y = "Frequency") +
  theme_minimal()

print(plot)


###CODE FIGURE SI5-------------------------------------------------------------------------------------------------------------------------

### ITS A SIMULATION SO RESULTS WILL SLIGHTLY CHANGE EACH TIME
### RUN with p = 1 to get the Left graph and p = .754 to get the right graph
p <- 1

# create graph with 2 links

# Function to create a graph where each node has 2 links
create_custom_graph_2 <- function(n) {
  g <- make_ring(n)
  return(g)
}  


# create graph with 3 links
# Function to create a graph where each node has 3 links
create_custom_graph_3 <- function(n) {
  # Define the edges such that each node connects to 3 others
  edges <- c(1,2, 1,4, 1,6,  # Node 1 connects to Nodes 2, 3, and 6
             2,3, 2,5,        # Node 2 connects to Nodes 3 and 4 (already connected to 1)
             3,4, 3,6,        # Node 3 connects to Nodes 4 and 5 (already connected to 2)
             4,5,      # Node 4 connects to Nodes 5 and 6 (already connected to 1 and 3)
             5,6)            # Node 5 connects to Node 6 (already connected to 2 and 4)
  # Create the graph
  g <- graph(edges, directed = FALSE)
  return(g)
}
# create a graph with 4 links

# Function to create a graph where each node has 4 links
create_custom_graph_4 <- function(n) {
  # Define the edges such that each node connects to 4 others
  edges <- c(1,2, 1,3, 1,6, 1,5,  # Node 1 connects to Nodes 2, 3, 5, and 6
             2,3, 2,4, 2,6,       # Node 2 connects to Nodes 3, 4, 6 (already connected to 1)
             3,4, 3,5,       # Node 3 connects to Nodes 4, 5 (already connected to 1, 2)
             4,5, 4,6,            # Node 4 connects to Nodes 5, 6 (already connected to 2, 3)
             5,6)                 # Node 5 connects to Node 6 (already connected to 1, 3, 4)
  # Create the graph
  g <- graph(edges, directed = FALSE)
  return(g)
}


# Update network colors with strategy A

update_network_colorsA <- function(g, p) {
  node_ids <- V(g)
  for (node_id in sample(node_ids[c(-disabled)], 1, replace = FALSE)) { 
    if (length(node_ids[c(-disabled)]) == 1) { #Quirk of sample function requires me to manually set the final node
      node_id <- node_ids[c(-disabled)][1]
      #print(cat("The final node is: ", node_id))
    }
    #sample only 1 value from a vector without the 'disabled' value from the global variable
    neighbor_ids <- neighbors(g, node_id)
    neighbor_colors <- V(g)$color[neighbor_ids]
    colors <- c(V(g)$color[node_id], neighbor_colors) #A and C strategies include counting your own color
    color_table <- table(colors)
    #print(cat("The color_table used for ", node_id," is "))
    #print(color_table)
    disabled <<- append(disabled, node_id) #change the global variable disabled to whatever value was just dplyr::selected
    #print(cat("The disabled nodes for the following round are: ", disabled))
    # Determine if there's a clear majority color
    sorted_colors <- sort(color_table, decreasing = TRUE)
    is_tie <- length(sorted_colors) > 1 && sorted_colors[1] == sorted_colors[2]
    
    if (is_tie) {
      current_color <- V(g)$color[node_id]  # Current Color
      other_colors <- unique(colors[(! colors %in% current_color)])
      new_color <- ifelse(runif(1) < 1-p, current_color, sample(other_colors, 1))
    } else {
      # No tie: proceed as before
      majority_color <- names(which.max(color_table))
      non_majority_colors <- unique(colors)
      non_majority_colors <- setdiff(non_majority_colors, majority_color)
      if (length(non_majority_colors) == 0) {
        new_color <- majority_color} else {
          new_color <- ifelse(runif(1) < p, majority_color, sample(non_majority_colors, 1))}}
    
    # V(g)$color[node_id] <- new_color #save this to a global variable, then every 6 rounds attach the changes
    #print(cat("I am adding change ", new_color, " to node_id ", node_id))
    change_log[node_id] <<- new_color
    #print(cat("To confirm, while the current color is ", V(g)$color[node_id], "change log has color ", change_log[node_id], " can be the same"))
  }
  return(g)
}

# Perform a single simulation

# Function to perform a single simulation and return convergence time
perform_simulation <- function(p) {
  n <- 6  # Number of nodes
  colorvector
  
  if( s != "A") { #if using sequential strategy initialize graph through this method
    if(l==2){
      g <- create_custom_graph_2(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      g <- initialize_network_colorsAs(g,x)
    }
    if(l==3){
      g <- create_custom_graph_3(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      g <- initialize_network_colorsAs(g,x)
    }}
  else{
    if(l==2){
      g <- create_custom_graph_2(n)
      V(g)$color <- sample(colorvector, n, replace = TRUE)
    }else{
      
      if (l==3) {g <- create_custom_graph_3(n)
      #V(g)$color <- c("red", "blue", "red", "blue", "red", "blue")
      V(g)$color <- sample(colorvector, n, replace = TRUE)
      }else{g <- create_custom_graph_4(n)
      V(g)$color <- sample(colorvector, n, replace = TRUE)}
    } }
  print(V(g)$color)
  for (i in 1:120) {
    if (i == 1) {
      if (length(unique(V(g)$color)) == 1) {
        return(0) # return 0 in the case where everyone chose the same color through random chance
      }
    }
    if (s=="A") {g <- update_network_colorsA(g, p)}
    else {g <- update_network_colorsA(g, p)}
    if (length(change_log) == 6 && all(!is.na(change_log))){
      #print(cat("The colors were", V(g)$color))
      V(g)$color <- change_log
      #print(cat("Now they are", V(g)$color))
      change_log <<- c()
      disabled <<- c(8)
      #print(cat("Change_Log empty again:", change_log))
      #print(cat("disabled only includes 8", disabled))
    } else {
      #print(cat("The change_log was not full, so I didn't apply any changes. Proof is here: ", change_log))
    }
    # Check for convergence
    if (length(unique(V(g)$color)) == 1) {
      return(i)  # Return the iteration at which convergence occurred
    }
  }
  return(NA)  # Return NA if no convergence within 30 iterations
}



# Parameters  # Probability of choosing the majority color
l <-  2#number links
s <- "A" # change to As to run a variation that uses q
x <- 1# q value
xstore <- x
colorvector<-c("red","blue", "yellow", "brown")
disabled <- c(8) # create the global variable to disable colors in previous rounds; initially not 1-6
num_simulations <- 1000 # number simulations
change_log <- c()

#C4L2
convergence_times <- replicate(1000, perform_simulation(p))
convergence_timesC4N2 <- na.omit(convergence_times) +6 

# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)


tableC4L2<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC4L2$ColLinks<-"C4N2"
tableC4L2$source <- "Algorithm"
tableC4L2$Var1 <- ifelse(tableC4L2$Var1 == TRUE, "yes", "no")
tableC4L2$sum <- sum(subset(tableC4L2, Var1 == "yes")$Freq)

#Change for C4L3
l <- 3

convergence_times <- replicate(1000, perform_simulation(p))
convergence_timesC4N3 <- na.omit(convergence_times) +6 
# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)


tableC4L3<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC4L3$ColLinks<-"C4N3"
tableC4L3$source <- "Algorithm"
tableC4L3$Var1 <- ifelse(tableC4L3$Var1 == TRUE, "yes", "no")
tableC4L3$sum <- sum(subset(tableC4L3, Var1 == "yes")$Freq)

#Change for C2L3

colorvector<-c("red","blue")

convergence_times <- replicate(1000, perform_simulation(p))
convergence_timesC2N3 <- na.omit(convergence_times) +6 
# Prepare data for plotting
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)

tableC2L3<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC2L3$ColLinks<-"C2N3"
tableC2L3$source <- "Algorithm"
tableC2L3$Var1 <- ifelse(tableC2L3$Var1 == TRUE, "yes", "no")
tableC2L3$sum <- sum(subset(tableC2L3, Var1 == "yes")$Freq)


#Change for C2L2
l<- 2

convergence_times <- replicate(1000, perform_simulation(p))
convergence_df <- data.frame(
  Converged = !is.na(convergence_times),
  ConvergenceTime = convergence_times
)
convergence_timesC2N2 <- na.omit(convergence_times) +6 
tableC2L2<-as.data.frame(prop.table(table(subset(convergence_df)$Converged,subset(convergence_df)$ConvergenceTime)))
tableC2L2$ColLinks<-"C2N2"
tableC2L2$source <- "Algorithm"
tableC2L2$Var1 <- ifelse(tableC2L2$Var1 == TRUE, "yes", "no")
tableC2L2$sum <- sum(subset(tableC2L2, Var1 == "yes")$Freq)


#Combine Everything into one dataframe
tableCLT <- rbind(tableC2L2, tableC4L2, tableC2L3, tableC4L3)
tableCLT$Var2 <-as.numeric(as.character(tableCLT$Var2)) +6
tableCLK <- tableCLT
# Convert Var2 to numeric if it is not already
tableCLK$Var2 <- as.numeric(as.character(tableCLK$Var2))

# Subset the data with the desired conditions
subset_data <- subset(tableCLK, Var1 == "yes" & Var2 <150)

# Subset the data with the desired conditions
subset_data <- subset(tableCLK, Var1 == "yes" & Var2 > 5 & Var2 < 150)
subset_data$ColLinks <- factor(subset_data$ColLinks, levels = c("C2N3", "C4N3", "C2N2", "C4N2"))

subset_data$FreqAdjusted <- subset_data$Freq/subset_data$sum

alg_means <- subset(subset_data, source == "Algorithm") %>%
  group_by(ColLinks) %>%
  summarize(
    sum_var2_freqadjusted = sum(Var2 * FreqAdjusted /6, na.rm = TRUE),
    sum_freqadjusted = sum(FreqAdjusted, na.rm = TRUE),
    weighted_mean_var2_freqadjusted = (sum_var2_freqadjusted / sum_freqadjusted)
  )

plot <- ggplot(subset_data, aes(x = Var2/6, y = FreqAdjusted, fill = source)) +
  facet_grid(cols = vars(ColLinks)) +
  geom_bar(data = subset(subset_data, source == "Algorithm"), 
           width = 0.7, stat = "identity", position = "identity", alpha = 0.5, color = "red") +
  labs(x = "Super Rounds", y = "Frequency") +
  theme_minimal() +
  ylim(0,1) +
  geom_vline(data = alg_means, aes(xintercept = sum_var2_freqadjusted), linetype = "dashed", color = "red", size = 0.5) +
  scale_x_continuous(breaks = seq(4, 20, by = 4), limits = c(0, 21)) + 
  ggtitle(
    if (p == 1) {
      expression(AL[S*","*1])
    } else {
      expression(AL[S*","*hat(p)])
    }
  ) +
  theme(
    axis.title.x = element_text(size = 75),
    plot.title = element_text(size = 75, hjust = .5),
    axis.title.y = element_text(size = 75),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 62.5),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30)
  )
plot

#CODE FOR TABLE SI6---------------------------------------------------------------------------------------------------------
# IT RERUNS THE SIMULATION EACH TIME, SO NUMBERS ARE BOUND TO CHANGE SLIGHTLY
#SET PARAMETERS HERE AND RUN THE FOLLOWING CODE TO GET THE FUNCTION TO PRODUCE A CHOSEN RESULT IN TABLE SI6

# Parameters

l <-  3 #number links
colorvector<-c("red","blue") #, "yellow", "brown") #FOR 4 COLORS, REMOVE # and parenthesis
num_rounds <- 5 #NUMBER OF SUPER ROUNDS
types <- c("N", "N", "N", "N", "N", "N") #S for Stubborn Leader, F for Finisher, and N for Normal

# DO NOT CHANGE
num_simulations <- 1000 # DO NOT CHANGE
p <- 1 # DO NOT CHANGE
s <- "As" # MUST USE As NOT A
x <- .1866 # q value
xstore <- x
disabled <- 8 # create the global variable to disable colors in previous rounds; initially not 1-6
strikes <- c(0,0,0,0,0,0)
strike_num <- 1
same_color_as_first <- 0
first_color <- c()
strike_finished_network <- 0


# create graph with 2 links

# Function to create a graph where each node has 2 links
create_custom_graph_2 <- function(n) {
  g <- make_ring(n)
  return(g)
}  


# create graph with 3 links

# Function to create a graph where each node has 3 links
create_custom_graph_3 <- function(n) {
  # Define the edges such that each node connects to 3 others
  edges <- c(1,2, 1,4, 1,6,  # Node 1 connects to Nodes 2, 3, and 6
             2,3, 2,5,        # Node 2 connects to Nodes 3 and 4 (already connected to 1)
             3,4, 3,6,        # Node 3 connects to Nodes 4 and 5 (already connected to 2)
             4,5,      # Node 4 connects to Nodes 5 and 6 (already connected to 1 and 3)
             5,6)            # Node 5 connects to Node 6 (already connected to 2 and 4)
  # Create the graph
  g <- graph(edges, directed = FALSE)
  return(g)
}
# create a graph with 4 links

# Function to create a graph where each node has 4 links
create_custom_graph_4 <- function(n) {
  # Define the edges such that each node connects to 4 others
  edges <- c(1,2, 1,3, 1,6, 1,5,  # Node 1 connects to Nodes 2, 3, 5, and 6
             2,3, 2,4, 2,6,       # Node 2 connects to Nodes 3, 4, 6 (already connected to 1)
             3,4, 3,5,       # Node 3 connects to Nodes 4, 5 (already connected to 1, 2)
             4,5, 4,6,            # Node 4 connects to Nodes 5, 6 (already connected to 2, 3)
             5,6)                 # Node 5 connects to Node 6 (already connected to 1, 3, 4)
  # Create the graph
  g <- graph(edges, directed = FALSE)
  return(g)
}



initialize_network_colorsAs <- function(g, x, archetype) {
  node_ids <- V(g)
  while (length(node_ids) > 0) { #while loop that ensures this continues until all 6 nodes are "sampled"
    for (node_id in sample(node_ids, 1, replace = TRUE)) {
      if (length(node_ids) == 1) { #Quirk of sample function requires me to manually set the final node
        node_id <- node_ids[1]
      }
      neighbor_ids <- neighbors(g, node_id)
      neighbor_colors <- V(g)$color[neighbor_ids]
      colors <- c(V(g)$color[node_id], neighbor_colors) #A and C strategies include counting your own color
      colors <- colors[(! colors %in% c("white"))] #remove white colors from table
      color_table <- table(colors)
      
      # Determine if there's a clear majority color
      sorted_colors <- sort(color_table, decreasing = TRUE)
      is_tie <- length(sorted_colors) > 1 && sorted_colors[1] == sorted_colors[2]
      
      draw <- runif(1, 0, 1) #draw random number between 0 and 1
      if (draw < x) { #compare draw to x to determine what path is taken
        new_color <- sample(colorvector, 1, replace = TRUE)} # draw < x means random sample
      else{ if (nrow(color_table) == 0) { # draw > x but no neighbors have drawn a color, so random
        new_color <- sample(colorvector, 1, replace = TRUE)
      }
        else{
          if (is_tie) {
            # Tie situation - Decide to keep the same color with probability
            new_color_choices <- names(sorted_colors)  # All colors tied for majority
            new_color <- sample(new_color_choices, 1) # random dplyr::selection when two colors are tied
          }   else {
            # No tie: proceed as before
            majority_color <- names(which.max(color_table))
            non_majority_colors <- setdiff(colorvector, majority_color)
            new_color <- ifelse(runif(1) < 1, majority_color, sample(non_majority_colors, 1))}}}
      # p changed to 1 to ensure the majority color is ALWAYS FOLLOWED
      if (archetype[node_id] == "S") {
        print("The Stubborn was dplyr::selected, but their color remains the same")
        new_color <- V(g)$color[node_id]
        print(cat("The stubborn color is",  V(g)$color[node_id]))
      }
      if (archetype[node_id] == "F") {
        V(g)$color[node_id] <- "white"
      } else {
        V(g)$color[node_id] <- new_color
      }
      
      if (length(node_ids) == 6  && length(which(archetype == "S")) == 0) {
        first_color <<- new_color
        print(cat("The First Color is", new_color))
      }
      node_ids <- node_ids[(!node_ids %in% node_id)] #remove chosen node_id from dplyr::selection in next rounds
    }}
  return(g)
}


# Update network colors with strategy A

update_network_colorsA <- function(g, p, archetype,i, strike_num) {
  node_ids <- V(g)
  for (node_id in sample(node_ids[c(-disabled)], 1, replace = FALSE)) {
    #sample only 1 value from a vector without the 'disabled' value from the global variable
    neighbor_ids <- neighbors(g, node_id)
    neighbor_colors <- V(g)$color[neighbor_ids]
    colors <- c(V(g)$color[node_id], neighbor_colors)
    finisher_count <- sum("white"== colors)
    colors <- colors[(! colors %in% c("white"))]
    fellow_finishers <- max(c(finisher_count - 1,0))
    color_table <- table(colors)
    disabled <<- node_id #change the global variable disabled to whatever value was just dplyr::selected
    
    # Determine if there's a clear majority color
    sorted_colors <- sort(color_table, decreasing = TRUE)
    is_tie <- length(sorted_colors) > 1 && sorted_colors[1] == sorted_colors[2]
    
    if (is_tie) {
      current_color <- V(g)$color[node_id]  # Current Color
      other_colors <- unique(colors[(! colors %in% current_color)])
      new_color <- ifelse(runif(1) < 1-p, current_color, sample(other_colors, 1))
    } else {
      # No tie: proceed as before
      majority_color <- names(which.max(color_table))
      non_majority_colors <- unique(colors)
      non_majority_colors <- setdiff(non_majority_colors, majority_color)
      if (length(non_majority_colors) == 0) {
        new_color <- majority_color} else {
          new_color <- ifelse(runif(1) < p, majority_color, sample(non_majority_colors, 1))}}
    
    if (archetype[node_id] == "S") {
      print(cat("It is round ", i))
      print(cat("I ", node_id, "AM STUBBORN AND WILL NEVER CHANGE"))
      new_color <- V(g)$color[node_id]
      print(cat("MY COLOR WAS ", V(g)$color[node_id], "AND ALWAYS WILL BE ", V(g)$color[node_id]))
    }
    
    
    if (archetype[node_id] == "F") {
      print(cat("I'm a finisher, and my color is ", V(g)$color[node_id]))
      print(cat("there are this many fellow finishers who have not acted I see", fellow_finishers))
      print(cat("The number of strikes is currently less than", strike_num))
      if ((max(table(neighbor_colors))) == l-fellow_finishers) {
        print("All my non-finisher neighbors are in agreement, so strikes increased by 1")
        strikes[node_id] <<- strikes[node_id] + 1
        print(cat("There are now this many strikes: ", strikes[node_id]))
      } else {
        print("No strike this round, reducing consecutive strikes to 0")
        strikes[node_id] <<- 0
      }
      new_color <- V(g)$color[node_id]
      print(cat("Confirming my color remains ", V(g)$color[node_id]))
      
      if (strikes[node_id] == strike_num) { 
        print("We have enough strikes, I am making my move")
        print(table(neighbor_colors))
        majority_color <- names(which.max(color_table))
        new_color <- majority_color
        print(cat("I will dplyr::select ", majority_color))
        old_color <- V(g)$color[node_id]
        V(g)$color[node_id] <- new_color
        strikes[node_id] <<- 0
        if (length(unique(V(g)$color)) == 1 & old_color== "white") {
          strike_finished_network <<- strike_finished_network+1
        }
        
        
      }
      print(strikes)
    }
    V(g)$color[node_id] <- new_color
  }
  return(g)
}

# Perform a single simulation

# Function to perform a single simulation and return convergence time
perform_simulation <- function(p) {
  n <- 6  # Number of nodes
  colorvector
  strikes <<- c(0,0,0,0,0,0)
  archetype <- sample(types, 6, replace = FALSE)
  print(cat("The archetypes are the following:, ", archetype))
  if( s != "A") { #if using sequential strategy initialize graph through this method
    if(l==2){
      g <- create_custom_graph_2(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      
      s_node <- which(archetype == "S")
      if (length(s_node) > 0) {
        print(cat("The Stubborn Node is", s_node))
        V(g)$color[s_node] <- sample(colorvector, 1, replace = TRUE)
        print(cat("The Stubborn Node now has color", V(g)$color[s_node]))
      }
      
      g <- initialize_network_colorsAs(g,xstore,archetype)
      print(V(g)$color)
    }
    if(l==3){
      g <- create_custom_graph_3(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      
      s_node <- which(archetype == "S")
      if (length(s_node) > 0) {
        print(cat("The Stubborn Node is", s_node))
        V(g)$color[s_node] <- sample(colorvector, 1, replace = TRUE)
        print(cat("The Stubborn Node now has color", V(g)$color[s_node]))
      }
      
      g <- initialize_network_colorsAs(g,xstore,archetype)
      print(V(g)$color)
    }}
  else{
    if(l==2){
      g <- create_custom_graph_2(n)
      V(g)$color <- sample(colorvector, n, replace = TRUE)
      for (node_id in seq_along(V(g)$color)){
        if (archetype[node_id]=="F") {
          V(g)$color[node_id] <- "white"
          print(cat("The following node has been assigned a finisher. Color below should be white", node_id))
          print(V(g)$color)
        }
      }}else{
        
        if (l==3) {g <- create_custom_graph_3(n)
        V(g)$color <- sample(colorvector, n, replace = TRUE)
        for (node_id in seq_along(V(g)$color)){
          if (archetype[node_id]=="F") {
            print(cat("The following node has been assigned a finisher. Color below should be white", node_id))
            V(g)$color[node_id] <- "white"
            print(V(g)$color)
          }
        }
        
        }else{g <- create_custom_graph_4(n)
        V(g)$color <- sample(colorvector, n, replace = TRUE)}
      } }
  for (i in 1:30) {
    if (i == 1) {
      if (length(unique(V(g)$color)) == 1) {
        return(0) # return 0 in the case where everyone chose the same color through random chance
      }
    }
    if (s=="A") {g <- update_network_colorsA(g, p, archetype,i, strike_num)}
    else {g <- update_network_colorsA(g, p, archetype,i, strike_num)}
    
    # Check for convergence
    if (length(unique(V(g)$color)) == 1) {
      if (length(which(archetype == "S")) == 0 &&
          length(unique(V(g)$color)) == 1 &&
          unique(V(g)$color) == first_color) {
        print("SAME COLOR")
        same_color_as_first <<- same_color_as_first + 1
      }
      return(i)  # Return the iteration at which convergence occurred
    }
  }
  return(NA)  # Return NA if no convergence within 30 iterations
}


# Run simulations and collect results

convergence_times <- replicate(num_simulations, perform_simulation(p))

# Prepare data for plotting
convergence_df <- data.frame(
  ConvergenceTime = convergence_times,
  Converged = !is.na(convergence_times)
)

# Compute average convergence time, excluding simulations that didn't converge
average_convergence_time <- mean(convergence_times, na.rm = TRUE)
convergence_times.matrix = matrix(data = convergence_times, ncol = 1, byrow = TRUE)
convergence_times_number.df = as.data.frame(convergence_times.matrix)
number_of_convergences <- colSums(!is.na(convergence_times_number.df))
x <- na.omit(as.data.frame(convergence_times_number.df[ convergence_times_number.df[,1] == 0, ]))
number_of_instant <- nrow(x)

# Display results
cat("For p is", p ,", l is", l,"and the number of colors is", length(colorvector), ", running", num_simulations, "simulations, the network 
    converges", number_of_convergences, "times, the number of instant convergences is", number_of_instant, "and the average convergence time is:", 
    average_convergence_time, "\n")

# Generate a ggplot graph
plot <- ggplot(convergence_df, aes(x = ConvergenceTime)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + ylim(0,100) + xlim(0, 35) +
  labs(title = paste("Distribution of Convergence Times For simulations, \n strategy",s,",p =", p ,", l =", l,
                     "and # of colors =", length(colorvector),  ", no. of 
convergences is", number_of_convergences, "average time is", average_convergence_time,",if 
relevant, value of x is", xstore, "and no. of instant convergences is", number_of_instant, ""),
       x = "Number of Steps to Converge",
       y = "Frequency") +
  theme_minimal()

print(plot)

print(cat("The proportion of networks that converged where the 
          first color was the same color as the first chosen color was (Only for when no S exists)"
          , same_color_as_first/number_of_convergences))

print(cat("The proportion of networks that converged where a 
          sniper sniped was"
          , strike_finished_network/number_of_convergences))

################################################################################




# STATISTICS IN MAIN TEXT & ABSTRACT--------

# ## NETWORK OUTCOMES -----------------------------------------------------
data$convBin<-ifelse(data$Convergence=="yes",1,0)

m0 <- glm(convBin ~ Grade, data = d1, family = binomial)
summary(m0)

d1 <- subset(data, ColLinks == "C 2 L 2")
d1$convBin<-ifelse(d1$Convergence=="yes",1,0)
m0 <- glm(convBin ~ 1, data = d1, family = binomial)
m1 <- glm(convBin ~ Grade, data = d1, family = binomial)
anova(m0, m1, test = "Chisq")

d2 <- subset(data, ColLinks == "C 2 L 3")
d2$convBin<-ifelse(d2$Convergence=="yes",1,0)
m0 <- glm(convBin ~ 1, data = d2, family = binomial)
m1 <- glm(convBin ~ Grade, data = d2, family = binomial)
anova(m0, m1, test = "Chisq")


d3 <- subset(data, ColLinks == "C 4 L 2")
d3$convBin<-ifelse(d3$Convergence=="yes",1,0)
m0 <- glm(convBin ~ 1, data = d3, family = binomial)
m1 <- glm(convBin ~ Grade, data = d3, family = binomial)
anova(m0, m1, test = "Chisq")

d4 <- subset(data, ColLinks == "C 4 L 3")
d4$convBin<-ifelse(d4$Convergence=="yes",1,0)
m0 <- glm(convBin ~ 1, data = d4, family = binomial)
m1 <- glm(convBin ~ Grade, data = d4, family = binomial)
anova(m0, m1, test = "Chisq")



p1<-c(0,1000)

t1<-table(subset(data,ColLinks=="C 2 L 2")$Convergence,subset(data,ColLinks=="C 2 L 2")$Grade)
AL1<-c(1000-C2N2AL,C2N2AL)
t1<-cbind(t1,AL1)
t1<-cbind(t1,p1)

pairwise.prop.test(t1[2,],t1[1,]+t1[2,],p.adjust.method = "holm")


t2<-table(subset(data,ColLinks=="C 4 L 2")$Convergence,subset(data,ColLinks=="C 4 L 2")$Grade)
AL2<-c(1000-C4N2AL,C4N2AL)
t2<-cbind(t2,AL2)
t2<-cbind(t2,p1)
pairwise.prop.test(t2[2,],t2[1,]+t2[2,],p.adjust.method = "holm")


t3<-table(subset(data,ColLinks=="C 2 L 3")$Convergence,subset(data,ColLinks=="C 2 L 3")$Grade)
AL3<-c(1000-C2N3AL,C2N3AL)
t3<-cbind(t3,AL3)
t3<-cbind(t3,p1)
pairwise.prop.test(t3[2,],t3[1,]+t3[2,],p.adjust.method = "holm")


t4<-table(subset(data,ColLinks=="C 4 L 3")$Convergence,subset(data,ColLinks=="C 4 L 3")$Grade)
AL4<-c(1000-C4N3AL,C4N3AL)
t4<-cbind(t4,AL4)
t4<-cbind(t4,p1)
pairwise.prop.test(t4[2,],t4[1,]+t4[2,],p.adjust.method = "holm")





x <- t1[2, ]
n <- t1[1, ] + t1[2, ]
groups <- colnames(t1)

pairs <- combn(seq_along(x), 2)

raw_p <- numeric(ncol(pairs))

for(k in 1:ncol(pairs)){
  test <- prop.test(x[pairs[,k]], n[pairs[,k]], correct=FALSE)
  raw_p[k] <- test$p.value
}

p_adj <- p.adjust(raw_p, method="holm")

sink(paste(getwd(),"/Pairwise_C2N2.txt", sep=''))

for(k in 1:ncol(pairs)){
  
  i <- pairs[1,k]
  j <- pairs[2,k]
  
  test <- prop.test(x[c(i,j)], n[c(i,j)], correct=FALSE)
  
  p1 <- x[i]/n[i]
  p2 <- x[j]/n[j]
  diff <- p1 - p2
  
  tab2x2 <- matrix(c(x[i], n[i]-x[i],
                     x[j], n[j]-x[j]),
                   nrow=2, byrow=TRUE)
  
  chi <- chisq.test(tab2x2, correct=FALSE)
  phi <- sqrt(unname(chi$statistic)/sum(tab2x2))
  
  cat(groups[i], "vs", groups[j], "\n")
  cat("p1 =", p1, "p2 =", p2, "\n")
  cat("Difference =", diff, "\n")
  cat("Chi-square(1) =", unname(test$statistic), "\n")
  cat("Raw p =", test$p.value, "\n")
  cat("Holm p =", p_adj[k], "\n")
  cat("95% CI [", test$conf.int[1], ",", test$conf.int[2], "]\n")
  cat("Phi =", phi, "\n\n")
}

sink()


x <- t1[2, ]
n <- t1[1, ] + t1[2, ]
groups <- colnames(t1)

pairs <- combn(seq_along(x), 2)

raw_p <- numeric(ncol(pairs))

for(k in 1:ncol(pairs)){
  test <- prop.test(x[pairs[,k]], n[pairs[,k]], correct=FALSE)
  raw_p[k] <- test$p.value
}

p_adj <- p.adjust(raw_p, method="holm")

sink(paste(getwd(),"/Pairwise_C2N2.txt", sep=''))

for(k in 1:ncol(pairs)){
  
  i <- pairs[1,k]
  j <- pairs[2,k]
  
  test <- prop.test(x[c(i,j)], n[c(i,j)], correct=FALSE)
  
  p1 <- x[i]/n[i]
  p2 <- x[j]/n[j]
  diff <- p1 - p2
  
  tab2x2 <- matrix(c(x[i], n[i]-x[i],
                     x[j], n[j]-x[j]),
                   nrow=2, byrow=TRUE)
  
  chi <- chisq.test(tab2x2, correct=FALSE)
  phi <- sqrt(unname(chi$statistic)/sum(tab2x2))
  
  cat(groups[i], "vs", groups[j], "\n")
  cat("p1 =", p1, "p2 =", p2, "\n")
  cat("Difference =", diff, "\n")
  cat("Chi-square(1) =", unname(test$statistic), "\n")
  cat("Raw p =", test$p.value, "\n")
  cat("Holm p =", p_adj[k], "\n")
  cat("95% CI [", test$conf.int[1], ",", test$conf.int[2], "]\n")
  cat("Phi =", phi, "\n\n")
}

sink()




x <- t2[2, ]
n <- t2[1, ] + t2[2, ]
groups <- colnames(t2)

pairs <- combn(seq_along(x), 2)

raw_p <- numeric(ncol(pairs))

for(k in 1:ncol(pairs)){
  test <- prop.test(x[pairs[,k]], n[pairs[,k]], correct=FALSE)
  raw_p[k] <- test$p.value
}

p_adj <- p.adjust(raw_p, method="holm")

sink(paste(getwd(),"/Pairwise_C4N2.txt", sep=''))

for(k in 1:ncol(pairs)){
  
  i <- pairs[1,k]
  j <- pairs[2,k]
  
  test <- prop.test(x[c(i,j)], n[c(i,j)], correct=FALSE)
  
  p1 <- x[i]/n[i]
  p2 <- x[j]/n[j]
  diff <- p1 - p2
  
  tab2x2 <- matrix(c(x[i], n[i]-x[i],
                     x[j], n[j]-x[j]),
                   nrow=2, byrow=TRUE)
  
  chi <- chisq.test(tab2x2, correct=FALSE)
  phi <- sqrt(unname(chi$statistic)/sum(tab2x2))
  
  cat(groups[i], "vs", groups[j], "\n")
  cat("p1 =", p1, "p2 =", p2, "\n")
  cat("Difference =", diff, "\n")
  cat("Chi-square(1) =", unname(test$statistic), "\n")
  cat("Raw p =", test$p.value, "\n")
  cat("Holm p =", p_adj[k], "\n")
  cat("95% CI [", test$conf.int[1], ",", test$conf.int[2], "]\n")
  cat("Phi =", phi, "\n\n")
}

sink()






x <- t3[2, ]
n <- t3[1, ] + t3[2, ]
groups <- colnames(t3)

pairs <- combn(seq_along(x), 2)

raw_p <- numeric(ncol(pairs))

for(k in 1:ncol(pairs)){
  test <- prop.test(x[pairs[,k]], n[pairs[,k]], correct=FALSE)
  raw_p[k] <- test$p.value
}

p_adj <- p.adjust(raw_p, method="holm")

sink(paste(getwd(),"/Pairwise_C2N3.txt", sep=''))

for(k in 1:ncol(pairs)){
  
  i <- pairs[1,k]
  j <- pairs[2,k]
  
  test <- prop.test(x[c(i,j)], n[c(i,j)], correct=FALSE)
  
  p1 <- x[i]/n[i]
  p2 <- x[j]/n[j]
  diff <- p1 - p2
  
  tab2x2 <- matrix(c(x[i], n[i]-x[i],
                     x[j], n[j]-x[j]),
                   nrow=2, byrow=TRUE)
  
  chi <- chisq.test(tab2x2, correct=FALSE)
  phi <- sqrt(unname(chi$statistic)/sum(tab2x2))
  
  cat(groups[i], "vs", groups[j], "\n")
  cat("p1 =", p1, "p2 =", p2, "\n")
  cat("Difference =", diff, "\n")
  cat("Chi-square(1) =", unname(test$statistic), "\n")
  cat("Raw p =", test$p.value, "\n")
  cat("Holm p =", p_adj[k], "\n")
  cat("95% CI [", test$conf.int[1], ",", test$conf.int[2], "]\n")
  cat("Phi =", phi, "\n\n")
}

sink()





x <- t4[2, ]
n <- t4[1, ] + t4[2, ]
groups <- colnames(t4)

pairs <- combn(seq_along(x), 2)

raw_p <- numeric(ncol(pairs))

for(k in 1:ncol(pairs)){
  test <- prop.test(x[pairs[,k]], n[pairs[,k]], correct=FALSE)
  raw_p[k] <- test$p.value
}

p_adj <- p.adjust(raw_p, method="holm")

sink(paste(getwd(),"/Pairwise_C4N3.txt", sep=''))

for(k in 1:ncol(pairs)){
  
  i <- pairs[1,k]
  j <- pairs[2,k]
  
  test <- prop.test(x[c(i,j)], n[c(i,j)], correct=FALSE)
  
  p1 <- x[i]/n[i]
  p2 <- x[j]/n[j]
  diff <- p1 - p2
  
  tab2x2 <- matrix(c(x[i], n[i]-x[i],
                     x[j], n[j]-x[j]),
                   nrow=2, byrow=TRUE)
  
  chi <- chisq.test(tab2x2, correct=FALSE)
  phi <- sqrt(unname(chi$statistic)/sum(tab2x2))
  
  cat(groups[i], "vs", groups[j], "\n")
  cat("p1 =", p1, "p2 =", p2, "\n")
  cat("Difference =", diff, "\n")
  cat("Chi-square(1) =", unname(test$statistic), "\n")
  cat("Raw p =", test$p.value, "\n")
  cat("Holm p =", p_adj[k], "\n")
  cat("95% CI [", test$conf.int[1], ",", test$conf.int[2], "]\n")
  cat("Phi =", phi, "\n\n")
}

sink()

sim <- data.frame(
  condition = c("C2N3", "C4N3", "C2N2", "C4N2"),
  n = c(10000, 10000, 10000, 10000),
  prop = c(1.00, 0.91, 0.46, 0.24)
)

sim$success <- sim$n * sim$prop
sim$failure <- sim$n - sim$success
sim

m0 <- glm(cbind(success, failure) ~ 1, data = sim, family = binomial)
m1 <- glm(cbind(success, failure) ~ condition, data = sim, family = binomial)
anova(m0, m1, test = "Chisq")

sim$type <- ifelse(sim$condition %in% c("C2N3", "C4N3"), "N3", "N2")
sim$type <- factor(sim$type, levels = c("N2", "N3"))

m_type0 <- glm(cbind(success, failure) ~ 1, data = sim, family = binomial)
m_type1 <- glm(cbind(success, failure) ~ type, data = sim, family = binomial)

anova(m_type0, m_type1, test = "Chisq")
summary(m_type1)


table(subset(data,(ColLinks=="C 2 L 2" |ColLinks=="C 4 L 2"))$convBin)
prop.test(
  x = c(148, 71),
  n = c(200, 200),
  correct = FALSE
)      

# ## SPEED OF CONVERGENCE ------------------------------------------

conds <- c("C 2 L 2", "C 2 L 3", "C 4 L 2", "C 4 L 3")

omnibus_results <- lapply(conds, function(cc) {
  d <- subset(data, ColLinks == cc)
  d$Grade <- factor(d$Grade, levels = c(0, 1, 2))
  a <- anova(lm(NumberChoices ~ Grade, data = d))
  
  data.frame(
    ColLinks = cc,
    df1 = a$Df[1],
    df2 = a$Df[2],
    F = a$`F value`[1],
    p = a$`Pr(>F)`[1]
  )
})

do.call(rbind, omnibus_results)


combined <- subset(data,Convergence==1)
pairwise.t.test(combined$NumberChoices, combined$Grade)
pairwise.t.test(subset(combined, ColLinks == "C 2 L 2")$NumberChoices, subset(combined, ColLinks == "C 2 L 2")$Grade)
pairwise.t.test(subset(combined, ColLinks == "C 2 L 3")$NumberChoices, subset(combined, ColLinks == "C 2 L 3")$Grade)
pairwise.t.test(subset(combined, ColLinks == "C 4 L 2")$NumberChoices, subset(combined, ColLinks == "C 4 L 2")$Grade)
pairwise.t.test(subset(combined, ColLinks == "C 4 L 3")$NumberChoices, subset(combined, ColLinks == "C 4 L 3")$Grade)


library(effsize)  # for Cohens d

# Output file
output_file <- "Pairwise_NumberChoices_Holm.txt"
sink(output_file)

# Define all networks
conditions <- c("C 2 L 2", "C 2 L 3", "C 4 L 2", "C 4 L 3")

for(cond in conditions){
  cat("=== Network:", cond, "===\n\n")
  
  subset_data <- subset(combined, ColLinks == cond)
  grades <- sort(unique(subset_data$Grade))
  
  # Generate all pairs
  pairs <- combn(grades, 2)
  
  raw_p <- numeric(ncol(pairs))  # store raw p-values temporarily
  t_list <- vector("list", ncol(pairs))  # store test results for each pair
  
  # First pass: compute raw p-values and store test objects
  for(k in 1:ncol(pairs)){
    g1 <- pairs[1,k]
    g2 <- pairs[2,k]
    
    x1 <- subset_data$NumberChoices[subset_data$Grade == g1]
    x2 <- subset_data$NumberChoices[subset_data$Grade == g2]
    
    ttest <- t.test(x1, x2, var.equal = FALSE)  # Welch t-test
    d <- cohen.d(x1, x2, pooled = TRUE)$estimate
    
    # Store raw p and results
    raw_p[k] <- ttest$p.value
    t_list[[k]] <- list(g1 = g1, g2 = g2, ttest = ttest, d = d)
  }
  
  # Holm adjustment across all pairwise comparisons in this network
  p_adj <- p.adjust(raw_p, method = "holm")
  
  # Print results
  for(k in 1:ncol(pairs)){
    res <- t_list[[k]]
    ttest <- res$ttest
    cat("Comparison:", res$g1, "vs", res$g2, "\n")
    cat("t(", round(ttest$parameter,1), ") = ", round(ttest$statistic,2),
        ", raw p = ", signif(ttest$p.value,3),
        ", Holm-adjusted p = ", signif(p_adj[k],3),
        ", Cohen's d = ", round(res$d,2),
        ", 95% CI [", round(ttest$conf.int[1],2), ",", round(ttest$conf.int[2],2), "]\n\n")
  }
  
}

sink()


#KS TESTS

# Open sink to save output
sink("TestKsNetwork.txt")

# Map each network to its correct convergence vector
network_vectors <- list(
  "C 2 L 2" = "convergence_timesC2N2",
  "C 2 L 3" = "convergence_timesC2N3",
  "C 4 L 2" = "convergence_timesC4N2",
  "C 4 L 3" = "convergence_timesC4N3"
)

for(cond in names(network_vectors)){
  cat("=== KS Test for network:", cond, "===\n")
  
  # Get observed data
  x <- subset(data, ColLinks == cond & Convergence == 1)$NumberChoices
  
  # Get simulation/convergence vector
  y <- get(network_vectors[[cond]]) + 6  # keep +6 if needed
  
  # Run KS test
  ks_res <- ks.test(x, y)
  
  # Save only KS statistic (D) and p-value
  D <- ks_res$statistic
  pval <- ks_res$p.value
  
  # Print in manuscript-ready format
  cat("KS statistic (D) = ", round(D, 3),
      ", p = ", signif(pval, 3), "\n\n")
}

sink()


#CODE COMPARING theoretical Proportions to actual
# Open sink to save output
sink("Proportion_TTests.txt")

# --- NumberColors == 2 ---
subset_success2 <- subset(data, Convergence == 1 & NumberColors == 2 & NumberChoices == 6)
subset_total2   <- subset(data, Convergence == 1 & NumberColors == 2)

prop2 <- nrow(subset_success2) / nrow(subset_total2)
x2 <- ifelse(subset_total2$NumberChoices == 6, 1, 0)

t_res2 <- t.test(x2, mu = 1/64)

d2 <- (mean(x2) - 1/64) / sd(x2)

cat("NumberColors = 2, expected proportion = 1/64\n")
cat("Observed proportion = ", round(prop2, 5), "\n")
cat("t(", t_res2$parameter, ") = ", round(t_res2$statistic, 3),
    ", p = ", signif(t_res2$p.value, 3),
    ", Cohen's d = ", round(d2, 3),
    ", 95% CI [", round(t_res2$conf.int[1], 5), ", ",
    round(t_res2$conf.int[2], 5), "]\n\n")


# --- NumberColors == 4 ---
subset_success4 <- subset(data, Convergence == 1 & NumberColors == 4 & NumberChoices == 6)
subset_total4   <- subset(data, Convergence == 1 & NumberColors == 4)

prop4 <- nrow(subset_success4) / nrow(subset_total4)
x4 <- ifelse(subset_total4$NumberChoices == 6, 1, 0)

t_res4 <- t.test(x4, mu = 1/1024)

d4 <- (mean(x4) - 1/1024) / sd(x4)

cat("NumberColors = 4, expected proportion = 1/1024\n")
cat("Observed proportion = ", round(prop4, 5), "\n")
cat("t(", t_res4$parameter, ") = ", round(t_res4$statistic, 3),
    ", p = ", signif(t_res4$p.value, 3),
    ", Cohen's d = ", round(d4, 3),
    ", 95% CI [", round(t_res4$conf.int[1], 5), ", ",
    round(t_res4$conf.int[2], 5), "]\n")

sink()


# ## Initial Choice Convergence Analysis==============================

# Function to calculate Cohen's h
cohens_h <- function(p_obs, p_null) {
  2 * asin(sqrt(p_obs)) - 2 * asin(sqrt(p_null))
}

# Specify output file
sink("FLchoice_analysis.txt")
dataI<-read.table(paste(getwd(),"/dataNetworksIndiv.csv", sep = ''),header=TRUE,sep=",")
dataI<-subset(dataI,RoundCode>1)
#test<-subset(dataI,SizeNetwork==4)
dataI<-subset(dataI,SizeNetwork!=4)


dataAI<-read.table(paste(getwd(),"/ActionsIndiv.csv", sep =''),header=TRUE,sep=",")
dataI<-merge(dataI,dataAI, by=c("SessionCode","ParticipantID","RoundCode"))


dataI$MoB[dataI$BirthMonth==1]<-12
dataI$MoB[dataI$BirthMonth==2]<-11
dataI$MoB[dataI$BirthMonth==3]<-10
dataI$MoB[dataI$BirthMonth==4]<-9
dataI$MoB[dataI$BirthMonth==5]<-8
dataI$MoB[dataI$BirthMonth==6]<-7
dataI$MoB[dataI$BirthMonth==7]<-6
dataI$MoB[dataI$BirthMonth==8]<-5
dataI$MoB[dataI$BirthMonth==9]<-4
dataI$MoB[dataI$BirthMonth==10]<-3
dataI$MoB[dataI$BirthMonth==11]<-2
dataI$MoB[dataI$BirthMonth==12]<-1

dataI$Age<-(2023-dataI$BirthYear)*12+5+dataI$MoB



dataI$NumberLinks[dataI$RoundCode==2]<-3
dataI$NumberLinks[dataI$RoundCode==3]<-3
dataI$NumberLinks[dataI$RoundCode==4]<-2
dataI$NumberLinks[dataI$RoundCode==5]<-2
dataI$NumberLinks[dataI$RoundCode==6]<-3
dataI$NumberLinks[dataI$RoundCode==7]<-3
dataI$NumberLinks[dataI$RoundCode==8]<-2
dataI$NumberLinks[dataI$RoundCode==9]<-2
dataI$NumberLinks[dataI$RoundCode==10]<-2
dataI$NumberLinks[dataI$RoundCode==11]<-2
dataI$NumberLinks[dataI$RoundCode==12]<-3
dataI$NumberLinks[dataI$RoundCode==13]<-3
dataI$NumberLinks[dataI$RoundCode==14]<-2
dataI$NumberLinks[dataI$RoundCode==15]<-2
dataI$NumberLinks[dataI$RoundCode==16]<-3
dataI$NumberLinks[dataI$RoundCode==17]<-3

dataI$NumberColors[dataI$RoundCode==2]<-2
dataI$NumberColors[dataI$RoundCode==3]<-2
dataI$NumberColors[dataI$RoundCode==4]<-2
dataI$NumberColors[dataI$RoundCode==5]<-2
dataI$NumberColors[dataI$RoundCode==6]<-4
dataI$NumberColors[dataI$RoundCode==7]<-4
dataI$NumberColors[dataI$RoundCode==8]<-4
dataI$NumberColors[dataI$RoundCode==9]<-4
dataI$NumberColors[dataI$RoundCode==10]<-2
dataI$NumberColors[dataI$RoundCode==11]<-2
dataI$NumberColors[dataI$RoundCode==12]<-2
dataI$NumberColors[dataI$RoundCode==13]<-2
dataI$NumberColors[dataI$RoundCode==14]<-4
dataI$NumberColors[dataI$RoundCode==15]<-4
dataI$NumberColors[dataI$RoundCode==16]<-4
dataI$NumberColors[dataI$RoundCode==17]<-4
cat("====================================\n")
cat("First vs Last Choice Analysis\n")
cat("====================================\n\n")

# Create binary indicator: 1 if first choice = last choice
dataI$FLchoice <- ifelse(dataI$Fchoice == dataI$Lchoice, 1, 0)
cat("FLchoice column created: 1 if Fchoice == Lchoice, 0 otherwise\n\n")


# Proportion and test for 2 colors
subset_2 <- subset(dataI, Convergence == "yes" & Rank == 1 & NumberColors == 2)
prop_2 <- mean(subset_2$FLchoice)
n_2 <- nrow(subset_2)
prop_test_2 <- prop.test(sum(subset_2$FLchoice), n_2, p = 0.5)

# Cohen's h
h_2 <- cohens_h(prop_2, 0.5)

cat("Number of Colors = 2\n")
cat(sprintf("Proportion (first=last) = %0.7f\n", prop_2))
cat(sprintf("Chi-squared(%d) = %0.3f, p = %0.3g\n", 
            prop_test_2$parameter, prop_test_2$statistic, prop_test_2$p.value))
cat(sprintf("Effect size (Cohen's h) = %0.3f\n", h_2))
cat(sprintf("95%% CI = %0.7f -- %0.7f\n\n", prop_test_2$conf.int[1], prop_test_2$conf.int[2]))



#Proportion and test for 4 colors
subset_4 <- subset(dataI, Convergence == "yes" & Rank == 1 & NumberColors == 4)
prop_4 <- mean(subset_4$FLchoice)
n_4 <- nrow(subset_4)
prop_test_4 <- prop.test(sum(subset_4$FLchoice), n_4, p = 0.25)

# Cohen's h
h_4 <- cohens_h(prop_4, 0.25)

cat("Number of Colors = 4\n")
cat(sprintf("Proportion (first=last) = %0.7f\n", prop_4))
cat(sprintf("Chi-squared(%d) = %0.3f, p = %0.3g\n", 
            prop_test_4$parameter, prop_test_4$statistic, prop_test_4$p.value))
cat(sprintf("Effect size (Cohen's h) = %0.3f\n", h_4))
cat(sprintf("95%% CI = %0.7f -- %0.7f\n", prop_test_4$conf.int[1], prop_test_4$conf.int[2]))

sink()  # close file



# ## AVERAGE NO. OF ACTIONS OF CLOSERS DEBATERS AND LEADERS ----------------



# Set output file
sink("welch_t_tests_avdata.txt")

cat("Welch's 2-sample t-tests for 'NumberChoices' by Rank groups in avdata\n\n")

# Helper function to run t-test and report nicely
report_ttest <- function(group1, group2, label1, label2) {
  
  test <- t.test(group1, group2)
  
  # Calculate Cohen's d
  d_val <- cohen.d(group1, group2, hedges.correction=TRUE)$estimate
  
  # Format output
  cat(paste0(label1, " vs ", label2, ":\n"))
  cat(sprintf("t(%0.2f) = %0.3f, p = %0.4f, d = %0.3f, 95%% CI = [%0.3f, %0.3f]\n",
              test$parameter,
              test$statistic,
              test$p.value,
              d_val,
              test$conf.int[1],
              test$conf.int[2]))
  cat("\n----------------------------------------\n\n")
}

# 1. Rank < 2 vs Rank 2–5
report_ttest(
  subset(avdata, Rank < 2)$NumberChoices,
  subset(avdata, Rank >= 2 & Rank <= 5)$NumberChoices,
  "Rank < 2",
  "Rank 2–5"
)

# 2. Rank > 5 vs Rank < 2  (Rank > 5 first)
report_ttest(
  subset(avdata, Rank > 5)$NumberChoices,
  subset(avdata, Rank < 2)$NumberChoices,
  "Rank > 5",
  "Rank < 2"
)

# 3. Rank > 5 vs Rank 2–5  (Rank > 5 first)
report_ttest(
  subset(avdata, Rank > 5)$NumberChoices,
  subset(avdata, Rank >= 2 & Rank <= 5)$NumberChoices,
  "Rank > 5",
  "Rank 2–5"
)

sink()

