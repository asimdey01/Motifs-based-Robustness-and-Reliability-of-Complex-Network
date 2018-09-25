


################################################################################################
# Giant component, APL under degree centrality based and betweennesss centrality based attacks
#################################################################################################

library(igraph) 
library(NetSwan)

data11 <- read.csv("Export_Output22.csv")
#summary(data11)


country<-c("Italy","Germany","Spain","Poland","France")
nc<-length(country)


for (j in 1:nc){ #j=2 
   
  
  #-------------------------------------------------------------------------------------
  
  nodes_G2<-data11[,c(2,3,4)][data11[,5]==country[j],]
  length(nodes_G2$X)
  ID_G2<-data11[,2][data11[,5]==country[j]]
  
  G2<-data11[(data11[,17] %in% ID_G2) | (data11[,18] %in% ID_G2)  , ] # or
  G2_data_all<-G2[,c(16,17,18)]
  
  G2_data_all_no_dup=unique(G2_data_all)
  G2_data<-G2_data_all_no_dup[,c(2,3)]   # Edge
  G2_edge=data.matrix(G2_data)
  G2_network=graph_from_edgelist(G2_edge,directed = F)
  
  ed11<-c(as.vector(G2_edge[,1]),as.vector(G2_edge[,2]))
  unique_ed11<-unique(ed11)
  nodes_index=sort(unique_ed11)
  m1<-max(nodes_index)
  delete_nodes_index=setdiff(c(1:m1),nodes_index)
  V(G2_network)$name=V(G2_network)
  new_G2_network=delete_vertices(G2_network,delete_nodes_index)
  
  #---------------------------------------------------------------------------------
  g<-new_G2_network
  
  node<-length(V(g))#node
  edge<-length(E(g))#edge
  
  c0 = components(g)
  GC_0<-max(c0$csize)
  
  AVPL_0<-average.path.length(g) 
  #---------------------------------------------------------------------
  
  n<-length(V(g))
  
  fr<-numeric(n)
  GC<-numeric(n)
  AVPL<-numeric(n)
  
  
  n1<-numeric(n);n2<-numeric(n)
  #----------------------------------degree/betweenness based attack------------------
  mat<- matrix(ncol=2,nrow=n, 0) 
  mat[,1]<-1:n
  
  deg<-degree(g)         # for degree based attacks
  #deg<-betweenness(g)  # for betweenness based attacks
  mat[,2]<-deg
  matri<-mat[order(mat[,2]),] 
  
  g2<-g
  #-----------------------------------------------------------------------------
  
  for(i in 1:n){       #i=5
    
    v=n+1-i
    g2<-delete_vertices(g2, matri[v,1])
    
    
    c = components(g2)    #largest Connected components/Giant component 
    GC[i]<-max(c$csize) 
    
    AVPL[i]<-average.path.length(g2) # Average pacth length
    
    
    fr[i]<-i/n   # fraction of node is removes
    matri[matri[,1]>matri[v,1],1]<-matri[matri[,1]>matri[v,1],1]-1 
  }
  
  
  
  Network<-rep(country[j],(n+1))
  
  Giant_Comp<-data.frame(c(0,fr),c(GC_0,GC),c(AVPL_0,AVPL),Network)
  colnames(Giant_Comp) <- c("fr", "GC","AVPL","Network") 
  print(Giant_Comp)
  #write.csv(Giant_Comp, paste0("Giant_C_",country[j],".csv"))
  
}



