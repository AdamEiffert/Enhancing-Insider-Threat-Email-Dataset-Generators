
library(Matrix)
library(igraph)

# Getting the adjacency matrices.

cora <- read.csv("/Users/billnunn/Desktop/Project/Graphs/cora.cites",
                 sep = '\t', header = FALSE)
head(cora)
node_names <- union(cora[,1], cora[,2])
name_dict <- data.frame(new_names = 1:length(node_names),
                        row.names = node_names)
for(i in 1:nrow(cora)){
  cora[i, 1] = name_dict[as.character(cora[i, 1]),1] 
  cora[i, 2] = name_dict[as.character(cora[i, 2]),1]
}
cora <- unique(rbind(cora, data.frame(V1 = cora[,2], V2 = cora[,1])))
A_cora <- get.adjacency(graph_from_edgelist(as.matrix(cora), directed = F)) / 2
rm(cora)

emails <- read.csv("/Users/billnunn/Desktop/Project/Graphs/email-Eu-core.txt",
                 sep = ' ', header = FALSE)
head(emails)
drop_rows <- c()
for(i in 1:nrow(emails)){
  if(emails[i,1] == emails[i,2]){
    drop_rows <- c(drop_rows, i)
  }
}
emails <- emails[-drop_rows,]
node_names <- union(emails[,1], emails[,2])
name_dict <- data.frame(new_names = 1:length(node_names),
                        row.names = node_names)
for(i in 1:nrow(emails)){
  emails[i, 1] = name_dict[as.character(emails[i, 1]),1] 
  emails[i, 2] = name_dict[as.character(emails[i, 2]),1]
}
emails <- unique(rbind(emails, data.frame(V1 = emails[,2], V2 = emails[,1])))
A_emails <- get.adjacency(graph_from_edgelist(as.matrix(emails), directed = F)) / 2
rm(drop_rows, name_dict, node_names, i, emails)

fb <- read.csv("/Users/billnunn/Desktop/Project/Graphs/fb_edges.txt",
               sep = ' ', header = FALSE)
head(fb)

fb <- fb + 1
head(union(fb[,1], fb[,2]))
fb <- unique(rbind(fb, data.frame(V1 = fb[,2], V2 = fb[,1])))

A_fb <- get.adjacency(graph_from_edgelist(as.matrix(fb), directed = F)) / 2
rm(fb)

writeMM(A_cora, "/Users/billnunn/Desktop/Project/Graphs/A_cora.mtx")

writeMM(A_emails, "/Users/billnunn/Desktop/Project/Graphs/A_emails.mtx")

writeMM(A_fb, "/Users/billnunn/Desktop/Project/Graphs/A_fb.mtx")

