
library(Matrix)
library(igraph)
library(purrr)
library(latex2exp)

Cora_Adj <- as.matrix(readMM("/Users/billnunn/Desktop/Project/Graphs/A_cora.mtx"))
Cora_G <- graph.adjacency(Cora_Adj, mode = "undirected")

Email_Adj <- as.matrix(readMM("/Users/billnunn/Desktop/Project/Graphs/A_emails.mtx"))
Email_G <- graph.adjacency(Email_Adj, mode = "undirected")

Cora_Sym <- as.matrix(read.csv("/Users/billnunn/Desktop/Cora_Sym_Probs.csv")[,-1])

Email_Sym <- as.matrix(read.csv("/Users/billnunn/Desktop/Email_Sym_Probs.csv")[,-1])
  
# Simulation function.
  
Simulated_Adjacency_Logistic <- function(A){
  Simulated_A <- A
  for(i in 1:nrow(A)){
    for(j in i:nrow(A)){
      edge <- as.integer(rbernoulli(1, A[i,j]))
      Simulated_A[i,j] = edge
      Simulated_A[j,i] = edge
    }
  }
  for(i in 1:nrow(A)){
    Simulated_A[i,i] = 0
  }
  return(Simulated_A)
}

# Triangle plots for Cora.

delta = rep(1, max(degree(Cora_G)))

r = nrow(Cora_Adj)
combined = 1:r

for (i in 1:max(degree(Cora_G))){ 
  too_big = c()
  for (j in 1:r){
    if (degree(Cora_G,j) > i){ 
      too_big[length(too_big) + 1] = combined[j]
    } 
  }
  G1 = delete_vertices(Cora_G,too_big)
  delta[i] = sum(count_triangles(G1)) / vcount(Cora_G)
}

plot(c(1:max(degree(Cora_G))),delta,log='xy',type='l', col="blue",
     xlab=TeX(r'(Degree)'), ylab=TeX(r'($\Delta$)'), 
     main=TeX(r'(Degree vs $\Delta$ for Cora Graph)'))

for(i in 1:5){
  delta_spec = rep(1, max(degree(Cora_G)))
  Cora_Sim <- Simulated_Adjacency_Logistic(Cora_Sym)
  G_Sim <- graph.adjacency(Cora_Sim, mode = "undirected")
  for (i in 1:max(degree(Cora_G))){ 
    too_big = c()
    for (j in 1:r){
      if (degree(G_Sim,j) > i){ 
        too_big[length(too_big) + 1] = combined[j]
      } 
    }
    G1 = delete_vertices(G_Sim,too_big)
    delta_spec[i] = sum(count_triangles(G1)) / vcount(G_Sim)
  }
  lines(c(1:max(degree(Cora_G))),delta_spec, col="red")
}

lines(c(1:max(degree(Cora_G))),delta, col="blue")


# Triangle plots for Email.

delta = rep(1, max(degree(Email_G)))

r = nrow(Email_Adj)
combined = 1:r

for (i in 1:max(degree(Email_G))){ 
  too_big = c()
  for (j in 1:r){
    if (degree(Email_G,j) > i){ 
      too_big[length(too_big) + 1] = combined[j]
    } 
  }
  G1 = delete_vertices(Email_G,too_big)
  delta[i] = sum(count_triangles(G1)) / vcount(Email_G)
}

plot(c(1:max(degree(Email_G))),delta,log='xy',type='l', col="blue",
     xlab=TeX(r'(Degree)'), ylab=TeX(r'($\Delta$)'), 
     main=TeX(r'(Degree vs $\Delta$ for Email Graph)'),
     xlim=c(5,400))

for(i in 1:5){
  delta_spec = rep(1, max(degree(Email_G)))
  Email_Sim <- Simulated_Adjacency_Logistic(Email_Sym)
  G_Sim <- graph.adjacency(Email_Sim, mode = "undirected")
  for (i in 1:max(degree(Email_G))){ 
    too_big = c()
    for (j in 1:r){
      if (degree(G_Sim,j) > i){ 
        too_big[length(too_big) + 1] = combined[j]
      } 
    }
    G1 = delete_vertices(G_Sim,too_big)
    delta_spec[i] = sum(count_triangles(G1)) / vcount(G_Sim)
  }
  lines(c(1:max(degree(Email_G))),delta_spec, col="red")
}

lines(c(1:max(degree(Email_G))),delta, col="blue")

# Degree plots for Cora.

temp <- data.frame(table(degree(Cora_G)))
degs <- as.numeric(as.character(temp[,1]))
cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))

plot(cum_freq~degs, log= 'xy', type='l', col="blue",
     xlab="Degree", ylab="Number of Nodes", 
     main="Degree Distribution of Cora Graph")

for(i in 1:5){
  Cora_Sim <- Simulated_Adjacency_Logistic(Cora_Sym)
  G_Sim <- graph.adjacency(Cora_Sim, mode = "undirected")
  
  temp <- data.frame(table(degree(G_Sim)))
  degs <- as.numeric(as.character(temp[,1]))
  cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))
  
  lines(cum_freq~degs, col = "red")
}

temp <- data.frame(table(degree(Cora_G)))
degs <- as.numeric(as.character(temp[,1]))
cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))

lines(cum_freq~degs, col = "blue")

# Degree plots for Email.

temp <- data.frame(table(degree(Email_G)))
degs <- as.numeric(as.character(temp[,1]))
cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))

plot(cum_freq~degs, log= 'xy', type='l', col="blue",
     xlab="Degree", ylab="Number of Nodes", 
     main="Degree Distribution of Email Graph")

for(i in 1:5){
  Cora_Sim <- Simulated_Adjacency_Logistic(Email_Sym)
  G_Sim <- graph.adjacency(Cora_Sim, mode = "undirected")
  
  temp <- data.frame(table(degree(G_Sim)))
  degs <- as.numeric(as.character(temp[,1]))
  cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))
  
  lines(cum_freq~degs, col = "red")
}

temp <- data.frame(table(degree(Email_G)))
degs <- as.numeric(as.character(temp[,1]))
cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))

lines(cum_freq~degs, col = "blue")
