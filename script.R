# libs
library(igraph)

# read csv
map <- read.csv("map.csv",header=TRUE,row.names=1,check.names=T)

# prepare data
map[is.na(map)] <- 0
mat <- as.matrix(map)

# create graph from adjacency matrix
g <- graph.adjacency(mat)
plot(g)

# define function that calculates average distance from each city to each city
mean_pandemic <- function(adj_mat) {
  gr <- graph.adjacency(adj_mat)
  short <- data.frame(shortest.paths(gr, algorithm = "dijkstra"))
  mean <-mean(rowMeans(short)) 
  ssd <- sum(Matrix::rowSums(short ** 2))
  return(paste("Mean: ", mean, ", SSD: ", ssd))
}
print(mean_pandemic(mat))

# calculate new means for first research center
for (i in 1:48) {
  if (i!=2) {
    mat_tmp <- mat
    mat_tmp[2, i] = 1
    mat_tmp[i, 2] = 1
    print(paste(rownames(mat)[i],": ", mean_pandemic(mat_tmp)))
  }
}

# set first research center to Kairo
frst = 16
mat[2, frst] = 1
mat[frst, 2] = 1

# calculate new means for second research center
for (i in 1:48) {
  if (i!=2 && i!=frst) {
    mat_tmp <- mat
    mat_tmp[2, i] = 1
    mat_tmp[i, 2] = 1
    mat_tmp[frst, i] = 1
    mat_tmp[i, frst] = 1
    print(paste(rownames(mat)[i],": ", mean_pandemic(mat_tmp)))
  }
}

# set second research center to Hong Kong
snd = 12
mat[2, snd] = 1
mat[snd, 2] = 1

# calculate new means for third research center
for (i in 1:48) {
  if (i!=2 && i!= frst && i!=snd) {
    mat_tmp <- mat
    mat_tmp[2, i] = 1
    mat_tmp[i, 2] = 1
    mat_tmp[frst, i] = 1
    mat_tmp[i, frst] = 1
    mat_tmp[snd, i] = 1
    mat_tmp[i, snd] = 1
    print(paste(rownames(mat)[i],": ", mean_pandemic(mat_tmp)))
  }
}
