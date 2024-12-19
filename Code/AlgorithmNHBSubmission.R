library(ggplot2)
library(gganimate)
library(dplyr)
library(igraph)
library(stringr)


# create graph with 2 links -----------------------------------------------

# Function to create a graph where each node has 2 links
create_custom_graph_2 <- function(n) {
  g <- make_ring(n)
  return(g)
}  


# create graph with 3 links -----------------------------------------------

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
# create a graph with 4 links ---------------------------------------------

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
            new_color <- sample(new_color_choices, 1) # random selection when two colors are tied
          }   else {
            # No tie: proceed as before
            majority_color <- names(which.max(color_table))
            non_majority_colors <- setdiff(colorvector, majority_color)
            new_color <- ifelse(runif(1) < 1, majority_color, sample(non_majority_colors, 1))}}}
      # p changed to 1 to ensure the majority color is ALWAYS FOLLOWED
      
      V(g)$color[node_id] <- new_color
      node_ids <- node_ids[(!node_ids %in% node_id)] #remove chosen node_id from selection in next rounds
    }}
  return(g)
}


# Update network colors with strategy A -----------------------------------------------------------------

update_network_colorsA <- function(g, p) {
  node_ids <- V(g)
  for (node_id in sample(node_ids[c(-disabled)], 1, replace = FALSE)) { 
    #sample only 1 value from a vector without the 'disabled' value from the global variable
    neighbor_ids <- neighbors(g, node_id)
    neighbor_colors <- V(g)$color[neighbor_ids]
    colors <- c(V(g)$color[node_id], neighbor_colors) #A and C strategies include counting your own color
    color_table <- table(colors)
    disabled <<- node_id #change the global variable disabled to whatever value was just selected
    
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
    
    
    V(g)$color[node_id] <- new_color
  }
  return(g)
}

# Perform a single simulation ------------------------------------------------------------------

# Function to perform a single simulation and return convergence time
perform_simulation <- function(p) {
  n <- 6  # Number of nodes
  colorvector
  
  if( s != "A") { #if using sequential strategy initialize graph through this method
    if(l==2){
    g <- create_custom_graph_2(n)
    V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
    g <- initialize_network_colorsAs(g)
    }
    if(l==3){
      g <- create_custom_graph_3(n)
      V(g)$color <- sample("white", n, replace = TRUE) #v(g)$colors does not work in function without colors being set
      g <- initialize_network_colorsAs(g)
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



# Parameters and Graph ----------------------------------------------------



# Parameters
p <- .925  # Probability of choosing the majority color
l <- 3 #number links
s <- "A" # change to As to run a variation that uses q
x <- 1# q value
xstore <- x
colorvector<-c("red","blue", "yellow", "brown")
disabled <- 8 # create the global variable to disable colors in previous rounds; initially not 1-6
num_simulations <- 1000 # number simulations

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
