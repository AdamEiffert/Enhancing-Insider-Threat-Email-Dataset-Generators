
library(Matrix)
library(igraph)
library(purrr)
library(wordspace)
library(latex2exp)

A_email <- readMM("/Users/billnunn/Desktop/Project/Graphs/A_emails.mtx")
A_email <- as.matrix(A_email)

indices <- sample(1:986, 986/2, replace = FALSE)

A_email <- A_email[indices, indices]

Email_G <- graph_from_adjacency_matrix(A_email, mode = "undirected")

# Embedding machinery:

initial <- function(n,e){ 
  init <- runif(n * e, -1, 1) 
  dim(init) <- c(n,e) 
  return(init)
}

l <- function(x){
  return(1 / (1 + exp(-x)))
}

l_matrix <- function(X, Y, A){ 
  L_mat <- A * (X %*% t(Y))
  L_mat <- apply(L_mat, c(1,2), l) 
  return(L_mat)
}

loss_and_gradients <- function(X, Y, A){ 
  # First find the useful l_matrix
  L_mat <- l_matrix(X, Y, A)
  # Find the loss
  loss_matrix <- -1 * log(L_mat) 
  loss <- sum(loss_matrix)
  # Now find the gradient matrices 
  M_mat <- A * (L_mat + (-1))
  X_grad <- M_mat %*% Y
  Y_grad <- t(M_mat) %*% X 
  return(list(loss, X_grad, Y_grad))
}

# ADAM optimiser.
initAdam = function(epsilon = 0.01, beta1 = 0.9, beta2 = 0.999, delta = 1E-8){
  epsilon <<- epsilon
  beta1 <<- beta1
  beta2 <<- beta2
  delta<<-delta
  r_x<<-0
  s_x<<-0
  t_x<<-0
  r_y<<-0
  s_y<<-0
  t_y<<-0
}

stepx = function(gradientEst) {
  t_x <<- t_x + 1
  s_x <<- beta1*s_x + (1-beta1)*c(gradientEst)
  r_x <<- beta2*r_x + (1-beta2)*(c(gradientEst)^2) 
  s_hat = s_x / (1-beta1^t_x)
  r_hat = r_x / (1-beta2^t_x)
  inc = - epsilon * s_hat / (sqrt(r_hat) + delta) 
  return(inc)
}

stepy = function(gradientEst) {
  t_y <<- t_y + 1
  s_y <<- beta1*s_y + (1-beta1)*c(gradientEst)
  r_y <<- beta2*r_y + (1-beta2)*(c(gradientEst)^2) 
  s_hat = s_y / (1-beta1^t_y)
  r_hat = r_y / (1-beta2^t_y)
  inc = - epsilon * s_hat / (sqrt(r_hat) + delta) 
  return(inc)
}

tilde <- function(A){ 
  A <- 2 * A - 1 
  return(A)
}

# Code for non regularised gradient descent

A <- tilde(A_email)

X <- initial(nrow(A), 16)
dot <- diag(x = c(1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1),
            16, 16)

Y <- X %*% dot

initAdam(0.1)
for (i in 1:100) {
  loss_list = loss_and_gradients(X,Y,A)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- X %*% dot
  print(i)
  print(paste("Loss: ", loss_list[[1]]))
  print(sum(X*X))
}

initAdam(0.01)
for (i in 1:10) {
  loss_list = loss_and_gradients(X,Y,A)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- X %*% dot
  print(i)
  print(paste("Loss: ", loss_list[[1]]))
  print(sum(X*X))
}

Embedding1 <- X

# Regularisation machinery

reg_loss_and_gradients <- function(X, Y, A, lamb){
  # First find the useful l_matrix
  L_mat <- l_matrix(X, Y, A)
  # Find the loss
  loss_matrix <- -1 * log(L_mat)
  loss <- sum(loss_matrix) + sum(((lamb / 2) * (X * X)))
  # Now find the gradient matrices
  M_mat <- A * (L_mat + (-1))
  X_grad <- M_mat %*% Y + lamb * X
  Y_grad <- t(M_mat) %*% X
  return(list(loss, X_grad, Y_grad))
}

tilde <- function(A, k){ 
  A <- (k+1) * A - 1 
  return(A)
}

# Regularised gradient descent

A <- tilde(A_email, 1)

X <- initial(nrow(A), 16)
dot <- diag(x = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1),
            16, 16)
Y <- X %*% dot

initAdam(0.1)
for (i in 1:80) {
  loss_list = reg_loss_and_gradients(X,Y,A, 5)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- X %*% dot
  print(i)
  print(paste("Loss: ", loss_list[[1]]))
  print(sum(X*X))
}

initAdam(0.01)
for (i in 1:30) {
  loss_list = reg_loss_and_gradients(X,Y,A, 5)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- X %*% dot
  print(i)
  print(paste("Loss: ", loss_list[[1]]))
  print(sum(X*X))
}

Embedding1 <- X
Embedding2 <- X

# New node machinery

indices <- sample(1:986, 986/2, replace = FALSE)

A_1 <- A_email[indices, indices]
A_2 <- A_email[-indices, -indices]

new_nodes <- function(X, number, h){
  for(n in 1:number){
    old_row <- sample(1:nrow(X), 1) 
    new_row <- X[old_row,] + rnorm(ncol(X), mean=0, sd=h)
    X <- rbind(X, new_row)
  }
  return(X)
}

Embedding1 <- new_nodes(X, 493, 0.2)
Embedding2 <- new_nodes(Embedding2, 493, 0.1)

# Simulation function:

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

Probs1 <- apply(Embedding1 %*% dot %*% t(Embedding1), c(1,2), l)
Probs2 <- apply(Embedding2 %*% dot %*% t(Embedding2), c(1,2), l)

A_Sim_1 <- Simulated_Adjacency_Logistic(Probs1)
A_Sim_2 <- Simulated_Adjacency_Logistic(Probs2)

Email_G_1 <- graph_from_adjacency_matrix(A_Sim_1, mode = "undirected")
Email_G_2 <- graph_from_adjacency_matrix(A_Sim_2, mode = "undirected")

# Plotting machinery.

# Triangle counts

delta = rep(1, max(degree(Email_G)))

r = nrow(A_email)
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

delta_1 = rep(1, max(degree(Email_G_1)))

for (i in 1:max(degree(Email_G_1))){ 
  too_big = c()
  for (j in 1:r){
    if (degree(Email_G_1,j) > i){ 
      too_big[length(too_big) + 1] = combined[j]
    } 
  }
  G1 = delete_vertices(Email_G_1,too_big)
  delta_1[i] = sum(count_triangles(G1)) / vcount(Email_G_1)
}

plot(c(1:max(degree(Email_G))),delta,log='xy',type='l', col="blue",
     xlab=TeX(r'($c$)'), ylab=TeX(r'($\Delta$)'), yaxt='n',
     main=TeX(r'(Triangle Counts)'),
     xlim=c(5,400))
lines(c(1:max(degree(Email_G_1))),delta_1, col="red")
lines(c(1:max(degree(Email_G))),delta, col="blue")



# Degree distribution

temp <- data.frame(table(degree(Email_G)))
degs <- as.numeric(as.character(temp[,1]))
cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))

plot(cum_freq~degs, log= 'xy', type='l', col="blue",
     xlab="Degree", ylab="Number of Nodes", 
     main=TeX(r'(Degree Distibution of Email Graph)'))

temp <- data.frame(table(degree(Email_G_1)))
degs <- as.numeric(as.character(temp[,1]))
cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))

lines(cum_freq~degs, col = "red")

temp <- data.frame(table(degree(Email_G)))
degs <- as.numeric(as.character(temp[,1]))
cum_freq <- as.numeric(rev(cumsum(rev(temp[,2]))))

lines(cum_freq~degs, col = "blue")

