library(igraph)
data <- read.table("stormofswords.csv", sep=",", header = TRUE)


mg=as.matrix(data)
g=graph.edgelist(mg[,1:2],directed=FALSE)
E(g)$weight=as.numeric(mg[,3])
plot(g)

plot(g,layout=layout.fruchterman.reingold,edge.width=E(g)$weight*0.1)

degg<-degree(g,mode='all')
V(g)$size<-degg
plot(g,layout=layout.sphere,edge.width=E(g)$weight*0.2)

V(g)$size<-7
c_fg <- cluster_fast_greedy(g)
plot(c_fg, as.undirected(g), vertex.color=membership(c_fg), vertex.label.color= "black")


c_l <-cluster_louvain(g)
plot(c_l, as.undirected(g))
l <- layout.fruchterman.reingold(g,niter=15000,area=vcount(g)^2tki,repulserad=vcount(g)^2)
plot(c_l, as.undirected(g), vertex.color=membership(c_l), vertex.label.color= "black")

c_eb <- edge.betweenness.community(g, directed=F)
plot(c_eb, as.undirected(g), vertex.color=membership(c_eb), vertex.label.color= "black")

c_wt <- walktrap.community(g, modularity=TRUE)
plot(c_wt, as.undirected(g), vertex.color=membership(c_wt), vertex.label.color= "black")

wmemb <- community.to.membership(g, c_wt$merges,
                                 steps=which.max(c_wt$modularity)-1)


library(linkcomm)
got<-data
lc <- getLinkCommunities(got, hcmethod = "single")

plot(lc, type = "graph", layout = layout.fruchterman.reingold)

plot(lc, type = "graph", shownodesin = 2, node.pies = TRUE)

plot(lc, type = "members")

plot(lc, type = "summary")

cm <- getCommunityConnectedness(lc, conn = "modularity")
plot(lc, type = "commsumm", summary = "modularity")

