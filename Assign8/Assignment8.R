library(igraph)

HIT <- function(adjmatrix,k = 10){
  n <- dim(adjmatrix)[1]
  a <- rep(1/n,n)
  h <- rep(1/n,n)
  for(i in c(1:k)){
    last_a <- a; last_h <- h
    a <- t(adjmatrix) %*% h
    h <-  adjmatrix %*% a
    sum_sq_a <- sum(a*a) 
    sum_sq_h <- sum(h*h) 
    a <- a/sqrt(sum_sq_a) 
    h <- h/sqrt(sum_sq_h)
    diff.h <- sum(abs(last_h - h))
    diff.a <- sum(abs(last_a - a))
  }
  re.da <- data.frame(list(h,a))
  colnames(re.da) <- c("Hubs","Authorities")
  return(re.da)
}

#Q1
#
from <- c(1,2,2,2,2,4,4,4,5,5,6,6,6,7,11,12,12,12,13,14,15,15,15)
to <- c(5,1,5,10,6,7,8,9,10,8,1,2,7,3,2,10,1,13,3,3,3,4,9)
ed <- data.frame(list(from,to));colnames(ed)<- c("from","to")
gra  <- graph.data.frame(ed,directed = T)

adj <- as.matrix(get.adjacency(gra))
adj <- adj[c(1,2,15,3,4,5,6,13,14,12,7,8,9,10,11),c(1,2,15,3,4,5,6,13,14,12,7,8,9,10,11)]

result <- HIT(adj,100)

#Q2
from.e1 <- c(1,2,3,3,4,4,5)
to.e1 <- c(2,4,1,2,1,5,1)
da.g1 <- data.frame(list(from.e1,to.e1));colnames(da.g1)<- c("from","to")
g1 <- graph.data.frame(da.g1)
g1.adj <- as.matrix(get.adjacency(g1))

g1.resutl <- HIT(g1.adj,k=5)


from.e2 <- c(1,1,2,2,3,3,4)
to.e2 <- c(2,3,3,4,1,4,3)
da.g2 <- data.frame(list(from.e2,to.e2));colnames(da.g2)<- c("from","to")
g2 <- graph.data.frame(da.g2)
g2.adj <- as.matrix(get.adjacency(g2))

g2.result <- HIT(g2.adj,k=50)



