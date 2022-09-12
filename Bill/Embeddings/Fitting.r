library(igraph)
library(purrr)
library(wordspace)

tri <- c(0, 1, 1, 1, 0, 1, 1, 1, 0)
dim(tri) <- c(3,3)
tri

tri_cycle <- function(tri_count){
  # Kronecker product to get union of tri_count disjoint triangles 
  A <- diag(tri_count) %x% tri
  # Link them up
  for(j in 1:tri_count){
    A[3*j, (3*j + 1) %% (3*tri_count)] = 1
    A[(3*j + 1) %% (3*tri_count), 3*j] = 1
  }
  return(A)
}

n = 50
toy <- tri_cycle(n)

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

Simulated_Adjacency <- function(A){
  Simulated_A <- apply(A, c(1,2), l)
  for(i in 1:150){
    for(j in i:150){
      edge <- as.integer(rbernoulli(1, A[i,j]))
      Simulated_A[i,j] = edge
      Simulated_A[j,i] = edge
    }
  }
  for(i in 1:150){
    Simulated_A[i,i] = 0
  }
  return(Simulated_A)
}


G <- graph_from_adjacency_matrix(toy)
coords <- layout_in_circle(G, order = V(G))

for(index in 1:n){
  coords[3*index - 1, 1] = 0.8 * coords[3*index - 1, 1]
  coords[3*index - 1, 2] = 0.8 * coords[3*index - 1, 2]
  coords[3*index, 1] = 0.9 * coords[3*index, 1]
  coords[3*index, 2] = 0.9 * coords[3*index, 2]
}

E(G)$arrow.mode="-"
plot(G, layout = coords, vertex.size=3, vertex.label=NA)



# We vary tilde to try and generate graphs which contain new edges!
# Compare the case of 50 to the standard 2.

# Perhaps heuristic is so full graph and empty graph have same loss, derive
# this value for the toy graph!

tilde <- function(A){ 
  A <- 2 * A - 1 
  return(A)
}

A <- tilde(toy)

X <- initial(3 * n, 6)
dot <- diag(x = c(-1, -1, 1, 1, 1, 1),
            6, 6)
Y <- X %*% dot

initAdam(0.1)
for (i in 1:500) {
  loss_list = loss_and_gradients(X,Y,A)
  X <- X + stepx(loss_list[[2]])
  Y <- X %*% dot
  print(i)
  print(paste("Loss: ", loss_list[[1]]))
}

A_SL_10 <- X %*% dot %*% t(X)
Simulated_G_10 <- graph_from_adjacency_matrix(Simulated_Adjacency(A_SL_10))
E(Simulated_G_10)$arrow.mode="-"
plot(Simulated_G_10, layout = coords, vertex.size=3, vertex.label=NA)

A_SL_50 <- X %*% dot %*% t(X)
Simulated_G_50 <- graph_from_adjacency_matrix(Simulated_Adjacency(A_SL_50))
E(Simulated_G_50)$arrow.mode="-"
plot(Simulated_G_50, layout = coords, vertex.size=3, vertex.label=NA)

A_SL_100 <- X %*% dot %*% t(X)
Simulated_G_100 <- graph_from_adjacency_matrix(Simulated_Adjacency(A_SL_100))
E(Simulated_G_100)$arrow.mode="-"
plot(Simulated_G_100, layout = coords, vertex.size=3, vertex.label=NA)

A_SL_200 <- X %*% dot %*% t(X)
Simulated_G_200 <- graph_from_adjacency_matrix(Simulated_Adjacency(A_SL_200))
E(Simulated_G_200)$arrow.mode="-"
plot(Simulated_G_200, layout = coords, vertex.size=3, vertex.label=NA)

par(mfrow=c(2,2))
plot(Simulated_G_10, layout = coords, vertex.size=3, vertex.label=NA,
     main = "10 Steps")
plot(Simulated_G_50, layout = coords, vertex.size=3, vertex.label=NA,
     main = "50 Steps")
plot(Simulated_G_100, layout = coords, vertex.size=3, vertex.label=NA,
     main = "100 Steps")
plot(Simulated_G_200, layout = coords, vertex.size=3, vertex.label=NA,
     main = "200 Steps")

# we now investigate regularisation.

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


A <- tilde(toy, 5)

X <- initial(3 * n, 6)
dot <- diag(x = c(-1, -1, -1, 1, 1, 1),
            6, 6)
Y <- X %*% dot

initAdam(0.1)
for (i in 1:400){
  loss_list = reg_loss_and_gradients(X,Y,A, 0)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- X %*% dot
  print(i)
  print(paste("Loss: ", loss_list[[1]]))
  print(sum(X*X))
  if(i %% 10 == 0){
    A_SL <- X %*% dot %*% t(X)
    Simulated_G <- graph_from_adjacency_matrix(Simulated_Adjacency(A_SL))
    E(Simulated_G)$arrow.mode="-"
    plot(Simulated_G, layout = coords, vertex.size=3, vertex.label=NA)
  }
}

