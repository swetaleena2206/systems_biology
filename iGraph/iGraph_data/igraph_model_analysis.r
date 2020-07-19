###-------------------------------------------------------------------------
## For creating a graph from data frame, use "graph.data.frame()" function in igraph.

## graph.data.frame(d, directed, vertices)

##  Here, d -->  a data frame of data
##        directed = TRUE by dfault.
##        vertices = NULL  or a data frame

##  If vertices=NULL, then first 2 columns of data frame d are taken as edge lists, and any additional columns as edge attributes.

## If vertices=df, where df is a data frame, then the first column of df is taken as symbolic verted names. These names will go as verted names of graph.

##  If this happens, then, first 2 columns of d are connected in graph as edges, and any name in first colimn of df not connected in d will be shown as unconnected nodes inthe graph. Also, symbolic links of d should contain only the verted names listed in df.
##---------------------------------------------------------------------------
## include the libraries
library("igraph")
library("igraphdata")

proteins <- read.csv("protein_list.csv", header=TRUE)

interactions <- read.csv("protein_interactions.csv")

g <- graph_from_data_frame(interactions, directed=TRUE, vertices=proteins)

#g <- graph_from_data_frame(interactions, directed=TRUE, vertices=NULL)

V(g)$weight = interactions$expression

plot(g)

###

print(paste("Edge density = ", edge_density(g)))

print("---------------")

deg = degree(g)
print("Degree : ")
print(deg)

plot(deg, type="h")
X11()
hist(deg)


print (degree(g))

print("--------------------------------")


degree_centrality = as.vector(degree(g))

closeness_centrality = as.vector(closeness(g))

eigenvector_centrality = as.vector(eigen_centrality(g)$vector)

betweenness_centrality = as.vector(betweenness(g))

dcm = data.frame(names(deg),degree_centrality, closeness_centrality, eigenvector_centrality, betweenness_centrality)

write.csv(dcm, "centrality_measures.csv",row.names=FALSE)



