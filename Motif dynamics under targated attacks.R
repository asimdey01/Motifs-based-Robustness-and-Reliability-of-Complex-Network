
################################################################################################
# Motif dynamics under degree centrality based and betweennesss centrality based attacks
#################################################################################################

#install.packages("NetSwan")
library(igraph) 
library(NetSwan)

data11 <- read.csv("Export_Output22.csv")
#summary(data11) 


country<-c("Italy","Germany","Spain","France")
nc<-length(country)


for (j in 1:nc){ #j=1
  
  
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
  
  #plot(new_G2_network, layout=layout_with_fr, vertex.size=5)
  #---------------------------------------
  g<-new_G2_network
  
  node<-length(V(g))#node
  edge<-length(E(g))#edge
  
  #-------- Motif, size=4 ---------------
  m02<-motifs(g, 4)
  m02[is.na(m02)] <- 0
  n22<-count_motifs(g, 4);n22
  
  V1_0<-m02[5];C_V1_0<-m02[5]/n22; C_V1_0[is.na(C_V1_0)] <-0
  V2_0<-m02[7];C_V2_0<-m02[7]/n22; C_V2_0[is.na(C_V2_0)] <-0
  V3_0<-m02[8];C_V3_0<-m02[8]/n22;C_V3_0[is.na(C_V3_0)] <-0
  V4_0<-m02[9];C_V4_0<-m02[9]/n22;C_V4_0[is.na(C_V4_0)] <-0
  V5_0<-m02[10];C_V5_0<-m02[10]/n22;C_V5_0[is.na(C_V5_0)] <-0
  V6_0<-m02[11];C_V6_0<-m02[11]/n22;C_V6_0[is.na(C_V6_0)] <-0
 
   Tot_V0<-sum(V1_0,V2_0,V3_0,V5_0,V6_0)
  #-----------------------------------------------
  n<-length(V(g))
  noeud<-V(g);  arc<-E(g)
  
  
  fr<-numeric(n)
  V1<-numeric(n);V2<-numeric(n);V3<-numeric(n);V4<-numeric(n)
  V5<-numeric(n);V6<-numeric(n);T1<-numeric(n);T2<-numeric(n)
  Tot_V<-numeric(n)
  
  C_V1<-numeric(n);C_V2<-numeric(n);C_V3<-numeric(n);C_V4<-numeric(n)
  C_V5<-numeric(n);C_V6<-numeric(n);C_T1<-numeric(n);C_T2<-numeric(n)
  
  n1<-numeric(n);n2<-numeric(n)
  #----------------------------------degree/betweenness based attack-------------------------------
  mat<- matrix(ncol=2,nrow=n, 0) 
  mat[,1]<-1:n
  
  deg<-degree(g)         # for degree based attacks
  #deg<-betweenness(g)  # for betweenness based attacks
  mat[,2]<-deg
  matri<-mat[order(mat[,2]),] 
  
  g2<-g
  
  #-------------------------------------------------------------------------------
  
  for(i in 1:n){       #i=5
    
    v=n+1-i
    g2<-delete_vertices(g2, matri[v,1])
    # plot(g2, layout=layout_with_fr, vertex.size=5)
    
    
    #------Motif size=4 ---------------------------------------------------
    m2<-motifs(g2, 4)
    m2[is.na(m2)] <- 0
    
    n02<-count_motifs(g2, 4)
    n2[i]<- n02
    
    V1[i]<-m2[5];C_V1[i]<-m2[5]/n02
    
    V2[i]<-m2[7];C_V2[i]<-m2[7]/n02
    
    V3[i]<-m2[8];C_V3[i]<-m2[8]/n02
    V4[i]<-m2[9];C_V4[i]<-m2[9]/n02
    V5[i]<-m2[10];C_V5[i]<-m2[10]/n02
    V6[i]<-m2[11];C_V6[i]<-m2[11]/n02
    
    Tot_V[i]<-sum(m2[5],m2[7],m2[8],m2[9],m2[10],m2[11])
    #-------------------------------------------------
    fr[i]<-i/n
    
    matri[matri[,1]>matri[v,1],1]<-matri[matri[,1]>matri[v,1],1]-1 
  }
  
  C_V1<-V1/n22;C_V1[is.na(C_V1)] <-0
  C_V2<-V2/n22;C_V2[is.na(C_V2)] <-0
  C_V3<-V3/n22;C_V3[is.na(C_V3)] <-0 
  C_V4<-V4/n22;C_V4[is.na(C_V4)] <-0
  C_V5<-V5/n22;C_V5[is.na(C_V5)] <-0
  
  Time<-0:n 
  Network<-rep(country[j],(n+1))
  
  motif_degree<-data.frame(Time,c(0,fr),c(Tot_V0,Tot_V),c(V1_0,V1),c(V2_0,V2),c(V3_0,V3),c(V4_0,V4),c(V5_0,V5),c(C_V1_0,C_V1),c(C_V2_0,C_V2),c(C_V3_0,C_V3),c(C_V4_0,C_V4),c(C_V5_0,C_V5),Network)
  colnames(motif_degree) <- c("Time","fr","Tot_V", "V1", "V2", "V3", "V4", "V5", "C_V1", "C_V2", "C_V3", "C_V4", "C_V5","Network") 
  
   print(motif_degree)          
  #write.csv(motif_degree, paste0("Concen_Deg_",country[j],".csv"))
  
}








