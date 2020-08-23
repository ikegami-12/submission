library(igraph)
library(matrixcalc)
source("gen_utils.R")

args <- commandArgs(trailingOnly=TRUE)

# number of nodes
nn <- 30
# average degree
k_ave <- 2
# number of samples
n_samp <- 300
# difference rate between networks A and B
p.netdiff = 0.2
# number of edges
nl <- round(k_ave * nn / 2)
# number of refill edges
niter = round(nl * p.netdiff, 0)

# generate matirices for network A
objA <- generate_covariance_matrix(nn, k_ave, type.network="sf", positive.ratio = 0.5)
# adjacency matrix of network A
netA_real <- objA[[1]]
# covariance (correlation) matrix for network A
x.cor_netA <- objA[[2]]

# generate network B (case) by rewiring edges in network A
objB <- random.rewiring.correlation.mtx(x.cor_netA, niter, positive.ratio = 0.5)
# adjacency matrix of network B
netB_real <- objB[[1]]
# covariance (correlation) matrix for network B
x.cor_netB <- objB[[2]]

# graph for each network
gA_real <- graph.adjacency(netA_real, mode="undirected")
gB_real <- graph.adjacency(netB_real, mode="undirected")
# edgelist for each graph
el1 <- as_edgelist(gA_real)
el2 <- as_edgelist(gB_real)
# output file
write.table(el1, args[1], quote = F, col.names = F, row.names = F, append = F)
write.table(el2, args[2], quote = F, col.names = F, row.names = F, append = F)
