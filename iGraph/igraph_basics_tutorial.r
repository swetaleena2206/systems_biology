## A tutorial to learn the basics of "igraph" package in R

## instal.packages("igraph")

## install.packages("igraphdata")


##########################################################################################

## include the libraries
library("igraph")
library("igraphdata")

## Create a directed graph with nodes

## connects successive pairs. N1->N2, N2->N3, N3->N4, N4->N1, N1->5, N2->N5 with 2 isolates N6, N7
gr1_dir = graph(c("N1","N2","N2","N3","N3","N4","N4","N1","N1","N5","N2","N5"), isolates=c("N6","N7"))

## add attributes to vertices by calling V() function.  Nodes 1 to 5 are proteins, 7,8 are small
##    molecules
V(gr1_dir)$type <- c(rep("proteins",5),rep("small molecues",2))


## add edge weights, if you want a weighted graph, using E() function
E(gr1_dir)$weight <- c(1.5, 2.0, 4.0, 2.5, 1.9, 1.0, 1.0)


plot(gr1_dir)
## we can also tamper with parameters
plot(gr1_dir, vertex.label.cex=0.8,vertex.label.dist=2.6, edge.arrow.size=0.5)

plot (gr1_dir,
vertex.label.cex = 0.8,
vertex.label.family="Verdana",
edge.width=E(gr1_dir)$weight*1.0,
edge.color="red")



## create an undirected graph for same data
gr1_undir = graph(c("N1","N2","N2","N3","N3","N4","N4","N1","N1","N5","N2","N5"), isolates=c("N7","N8"), directed=F)
graph(c("N1","N2","N2","N3","N3","N4","N4","N1","N1","N5","N2","N5"), isolates=c("N7","N8"))
V(gr1_dir)$type <- c(rep("proteins",5),rep("small molecues",2))

## plot the graph
plot(gr1_undir)
plot(gr1_undir, vertex.label.cex=0.8,vertex.label.dist=2.6, edge.arrow.size=0.5)

## Print the igraph objects gr1_dir and gr1_undir
print(gr1_dir)
print(gr1_undir)

## NOTE : In the above printout, "U"->undirected, D-->"Directed", "N"-->named graph (edges named)
##                                 "W"-> weighted graph", "B"-> bipartite, where nodes have names.
## Above graph is "UN", Undirected and Named.
## two numbers 7,6 refert to 7 nodes, 6 edges

## print the vertices(node) and Edges 
print(V(gr1_undir))
print(E(gr1_undir))

print(V(gr1_dir))
print(E(gr1_dir))

## print the sparse network matrix
print(gr1_dir[])
print(gr1_undir[])

# we can also access rows and columns of this matrix
print(gr1_dir[1,])

## we can access attributes
print(  V(gr1_dir)$name )
print(  V(gr1_dir)$type )
print( E(gr1_dir)$weight)

#-----------------------------------------------------------------------------

## Adding new vertices(nodes) and edges to the graph

## add 2 new nodes "N8", "N9" to gr1_undir
gr1_dir <- gr1_dir + vertices(c("N8","N9"), type=c("gene","gene" ) )
print(V(gr1_dir))
print(vertex_attr(gr1_dir))

## adding new edges that covers newly added vertices
gr1_dir <- gr1_dir + edges(c("N1","N8","N3","N9","N9","N8","N7","N1","N8","N7","N9","N7"),weight=c(1.5,2.0,3.0,1.0,1.0,1.0))
print(E(gr1_dir))

# plot the new network
#gr1_dir <- delete_graph_attr(gr1_dir,"gene")
l = layout_on_grid(gr1_dir)
plot (gr1_dir,
vertex.label.cex = 0.8,
vertex.label.family="Verdana",
layout=l,
edge.width=E(gr1_dir)$weight*1.0,
edge.color="lightsteelblue")


###--------------------------------------------------------------------------------------

### Computing the properties of the netwoek


##--------------- degree measures----------------------------

## Edge density = ratio between edges in the graph to all pssoble edges between nodes
print(  edge_density(gr1_dir) )



## degree of a node
deg <- degree(gr1_dir)
print(deg)

deg <- degree(gr1_undir)
print(deg)

## histogram of node degree
hist(deg, breaks=10, main="Histogram of node degree")



##  degree distribution and cumulative degree distribution
deg <- degree(gr1_dir)

deg.dist = degree_distribution(gr1_dir,cumulative=T)

plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=0.8, col="coral",
                        xlab="Degree", ylab="Cumulative Frequency")


## ------------Centrality measures-------------------------------------------

######  1. Degree and Degree centrality

print (degree(gr1_dir))
print(centr_degree(gr1_dir))


###### 2. Closeness and Closeness centrality (not well defined for disconnected graphs)
print( closeness(gr1_dir) )
print( centr_clo(gr1_dir) )

##### 3. Eigenvector centrality
print( eigen_centrality(gr1_dir)$vector )
print( centr_eigen(gr1_dir)$centralization )


###  4. Betweenness centrality
print( betweenness(gr1_dir) )
print( centr_betw(gr1_dir)  )
print( edge_betweenness(gr1_dir) )



##### --------------------------  Distance measures -----------------------------------

## get all the shortest paths in the graph
## gives a matrix of distances between two nodes or two sets of nodes
print( distances(gr1_dir) )
print( distances(gr1_undir) )


## get the average shortest path
print(mean_distance(gr1_dir) )
print(mean_distance(gr1_undir) )


# examine the outout of shortest_path
# single target node
selectedPaths1 <- shortest_paths(gr1_dir,from=V(gr1_dir)[name=="N1"],to = V(gr1_dir)[name=="N4"],
                                    output="both")
print(selectedPaths1)



# multiple target nodes
selectedPaths2 <- shortest_paths(gr1_dir, from=V(gr1_dir)[name=="N1"], to = V(gr1_dir)[type=="genes"],                                          output="both")

print(selectedPaths2)



# all_shortest_paths with weight
all_paths1 <- all_shortest_paths(gr1_dir,V(gr1_dir)[name=="N1"],to=V(gr1_dir)[name=="N5"])
print(all_paths1)



##################### Neighbourhood functions ---------------------------------------

# edges of specific single node:
print (  incident(gr1_dir, V(gr1_dir)$name=="N3") )

# edges from a specific set of nodes
print( incident_edges(gr1_dir, V(gr1_dir)$type=="proteins") )

# neighbouring nodes of a single node
print( neighbors(gr1_dir, V(gr1_dir)$name=="N4") )


# neighbouring nodes of a set of nodes
print( adjacent_vertices(gr1_dir, V(gr1_dir)$type=="proteins") )


## order of the neighbour
# The nodes within a distance of one edge from our node of interest are called first order
# neighbours. The ego function allows us to extract nodes further away from our node, within a
# specific order.

# identify nodes within 2 orders from the node A
print(  ego(gr1_dir, order=2, nodes=V(gr1_dir)$name=="proteins")  )



## We can also select all the edges between two specific set of vertices.
print( E(gr1_dir)[V(gr1_dir)[type=="proteins"] %--% V(gr1_dir)[type=="gene"]] )






