
library(Matrix)
library(igraph)
library(purrr)
library(wordspace)

A_cora <- readMM("/Users/billnunn/Desktop/Project/Graphs/A_cora.mtx")
A_cora <- readMM("/Users/billnunn/Desktop/Project/Graphs/A_emails.mtx")
A_fb <- readMM("/Users/billnunn/Desktop/Project/Graphs/A_fb.mtx")

# Spectral embedding and simulation.

e <- eigen(A_cora)

largest_eigenvalue_indices <- c()
eigenvalues <- c()
for(i in 1:nrow(A_cora)){
  if(sign(e$values[i]) * e$values[i] > 7.945){ 
    largest_eigenvalue_indices <- c(largest_eigenvalue_indices, i)
    eigenvalues <- c(eigenvalues, e$values[i])
  } 
}

dot <- diag(sign(eigenvalues))
U <- e$vectors[,largest_eigenvalue_indices]
X <- U %*% diag(sqrt(abs(eigenvalues)))

Cora_Spec_Probs <- X %*% dot %*% t(X)

rm(dot, e, U, X, eigenvalues, largest_eigenvalue_indices, i)

# Logistic Embedding Machinery.

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

# Asymmetric case.

A <- tilde(A_cora)

X <- initial(nrow(A), 16)
Y <- initial(nrow(A), 16)

loss_list <- loss_and_gradients(X, Y, A)
print(paste("Initial Loss: ", loss_list[[1]]))

initAdam(0.1)
for (i in 1:20) {
  loss_list = loss_and_gradients(X,Y,A)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- Y + stepy(as.matrix(loss_list[[3]]))
  print(paste("Loss at step ", as.character(i), ":", loss_list[[1]]))
}

initAdam(0.02)
for (i in 1:5) {
  loss_list = loss_and_gradients(X,Y,A)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- Y + stepy(as.matrix(loss_list[[3]]))
  print(paste("Loss at step ", as.character(i), ":", loss_list[[1]]))
}

Cora_Asym_Probs <- apply(X %*% t(Y), c(1,2), l)

# Vanilla symmetric case.

A <- tilde(A_cora)

X <- initial(nrow(A), 16)
dot <- diag(x = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1),
            16, 16)
Y <- X %*% dot

initAdam(0.1)
for (i in 1:20) {
  loss_list = loss_and_gradients(X,Y,A)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- X %*% dot
  print(i)
  print(paste("Loss: ", loss_list[[1]]))
}

initAdam(0.01)
for (i in 1:50) {
  loss_list = loss_and_gradients(X,Y,A)
  X <- X + stepx(as.matrix(loss_list[[2]]))
  Y <- X %*% dot
  print(i)
  print(paste("Loss: ", loss_list[[1]]))
}

Cora_Sym_Probs <- apply(X %*% dot %*% t(X), c(1,2), l)

# Write these matrices to csv.

write.csv(as.data.frame(Cora_Spec_Probs), 
          "/Users/billnunn/Desktop/Email_Spec_Probs.csv")

write.csv(as.data.frame(Cora_Asym_Probs), 
          "/Users/billnunn/Desktop/Email_Asym_Probs.csv")

write.csv(as.data.frame(Cora_Sym_Probs), 
          "/Users/billnunn/Desktop/Email_Sym_Probs.csv")



