  library("clValid")
  #set a working directory
  setwd("/Users/amirasarbaev/Downloads/Projects/Electricity Load Consumption/Data")
  
  #load data
  load("/Users/amirasarbaev/Downloads/Projects/Electricity Load Consumption/Data/elecsig.RData")
  
  #==============================preparing dataset=============================================
  weeks=data$days
  signals=data$signals[,1:48]
  df=cbind(weeks,signals)  
  #=============================created euclidean distance function============================================================
  euclidean_distance <- function(p,q){                
    sqrt(sum((p - q)^2))
  }
  #=============================================================================================
  
  
  #=============================k-means plus plus============================================
  kmc<-function (data,k){
    #initializing==============================================================================
      c<-1                                                #choose 1st sample as a 1st centroid
      cent=matrix(0,nrow = 0,ncol = ncol(signals))        #centroid values
      dist=matrix(0,nrow = nrow(signals),ncol = k)             #create matrix for distance values
      prob=matrix(0,nrow = nrow(signals),ncol = k)             #create matrix with centroids probability values 
      
    #k-means++ part of calculation mean of centroids===============================================
      df_cent=signals
      for(j in 1:k){
          cent=rbind(cent,df_cent[c,])                        #add new centroid 
          df_cent=df_cent[-c,]                                 #delete sample which was identified as centorid
          for(i in 1:nrow(df_cent)){                             
            dist[i,j]=euclidean_distance(signals[i,],cent[j,])    #calculate and save in dist matrix a distance between found centroids and data samples 
          }
          ss<-colSums(dist)                                    #sum all distances to calculate centroids probability values
          for(m in 1:nrow(df_cent)){
            prob[m,j]=dist[m,j]/ss[j]                          #calcualte and save in prob matrix centroids probability values
          }
          
          c=which.max(prob[,j])                                #find the maximum probability of the next centroid
                                        
      }
      
    #ordinary k-means========================================================================
      
    distkms=matrix(0,nrow=nrow(df),ncol = k)
    centc=cent
    eps=1e-5
    repeat{
      #=========================================================
      clustvec=matrix(0,nrow=nrow(df),ncol=1)
      for (j in 1:k){
        for (i in 1:nrow(signals)){
          distkms[i,j]=euclidean_distance(signals[i,],cent[j,])
        }
      }
      #=========================================================
      for (i in 1:nrow(df)){
        clustvec[i]=which.min(distkms[i,])
      }
      #=========================================================
      cs=matrix(0,nrow=0,ncol=ncol(signals))
      
      for (i in 1:k){
        cs=rbind(cs,colSums(signals[which(clustvec %in% i),]))
        
      }
      #=========================================================  
      for(i in 1:k){
        for(j in 1:ncol(signals)){
          centc[i,j]=cs[i,j]/length(which(clustvec %in% i))
        }
      }
      
      
      RsNew=rowSums(centc) 
      RsOld=rowSums(cent)
      print(cent[,1])
      print(centc[,1])
      print(RsOld)
      print(RsNew)
      
      
      if(sum(setdiff(RsNew,RsOld))<=eps){
        return(list("clusters"=clustvec,"centroids"=centc))
        break
      }else{cent=centc}
    } 
  }
  require("clusterSim")
  #=============================my kmeans++===============================
  z=kmc(signals,6)
  
  centroids1=rowMeans(z$centroids)
  sign=rowMeans(signals)
  classf<-list("class1"=sign[which(z$clusters %in% 1)],"class2"=sign[which(z$clusters %in% 2)],"class3"=sign[which(z$clusters %in% 3)],"class4"=sign[which(z$clusters %in% 4)],"class5"=sign[which(z$clusters %in% 5)],"class6"=sign[which(z$clusters %in% 5)],"centroids"=centroids1)
  stripchart(classf,
             main="Multiple stripchart for comparision",
             xlab="Means",
             ylab="y",
             method="jitter",
             col=c("orange","red","green","yellow","blue","purple","black"),
             pch=16
  )
  x6=index.DB(signals,z$clusters,d=NULL,centrotypes="centroids", p=2, q=2)
  o6=dunn(distance = NULL, z$cluster, Data = signals, method = "euclidean")
  #=============================my kmeans++=============================================
  z=kmc(signals,5)
  
  centroids1=rowMeans(z$centroids)
  sign=rowMeans(signals)
  classf<-list("class1"=sign[which(z$clusters %in% 1)],"class2"=sign[which(z$clusters %in% 2)],"class3"=sign[which(z$clusters %in% 3)],"class4"=sign[which(z$clusters %in% 4)],"class5"=sign[which(z$clusters %in% 5)],"centroids"=centroids1)
  stripchart(classf,
             main="Multiple stripchart for comparision",
             xlab="Means",
             ylab="y",
             method="jitter",
             col=c("orange","red","green","yellow","blue","black"),
             pch=16
  )
  x5=index.DB(signals,z$clusters,d=NULL,centrotypes="centroids", p=2, q=2)
  o5=dunn(distance = NULL, z$cluster, Data = signals, method = "euclidean")
  #=================================my kmeans++=========================================
  z=kmc(signals,4)
  
  centroids1=rowMeans(z$centroids)
  sign=rowMeans(signals)
  classf<-list("class1"=sign[which(z$clusters %in% 1)],"class2"=sign[which(z$clusters %in% 2)],"class3"=sign[which(z$clusters %in% 3)],"class3"=sign[which(z$clusters %in% 4)],"centroids"=centroids1)
  stripchart(classf,
             main="Multiple stripchart for comparision",
             xlab="Means",
             ylab="y",
             method="jitter",
             col=c("orange","red","green","black"),
             pch=16
  )
  x4=index.DB(signals,z$clusters,d=NULL,centrotypes="centroids", p=2, q=2)
  o4=dunn(distance = NULL, z$cluster, Data = signals, method = "euclidean")
  #=================================my kmeans++=========================================
  z=kmc(signals,3)
  
  centroids1=rowMeans(z$centroids)
  sign=rowMeans(signals)
  classf<-list("class1"=sign[which(z$clusters %in% 1)],"class2"=sign[which(z$clusters %in% 2)],"class3"=sign[which(z$clusters %in% 3)],"centroids"=centroids1)
  stripchart(classf,
             main="Multiple stripchart for comparision",
             xlab="Means",
             ylab="y",
             method="jitter",
             col=c("orange","red","green","black"),
             pch=16
  )
  x3=index.DB(signals,z$clusters,d=NULL,centrotypes="centroids", p=2, q=2)
  o3=dunn(distance = NULL, z$cluster, Data = signals, method = "euclidean")
  #=================================my kmeans++=========================================
  z=kmc(signals,2)
  
  centroids1=rowMeans(z$centroids)
  sign=rowMeans(signals)
  classf<-list("class1"=sign[which(z$clusters %in% 1)],"class2"=sign[which(z$clusters %in% 2)],"centroids"=centroids1)
  stripchart(classf,
             main="Multiple stripchart for comparision",
             xlab="Means",
             ylab="y",
             method="jitter",
             col=c("orange","red","black"),
             pch=16
  )
  matplot(t(signals), type ='l', col=z$clusters)
  x2=index.DB(signals,z$clusters,d=NULL,centrotypes="centroids", p=2, q=2)
  o2=dunn(distance = NULL, z$cluster, Data = signals, method = "euclidean")
  #===============================k-means by CRAN===========================================
  
  z1=kmeans(signals,2)
  y6=index.DB(signals,z$cluster,d=NULL,centrotypes="centroids", p=2, q=2)
  s7=dunn(distance = NULL, z$cluster, Data = signals, method = "euclidean")
  #============================Dependence between numb of centroids and DB indexes==============================================
  xCent=c(1,2,3,4,5,6)
  yDB=c(0,x2$DB,x3$DB,x4$DB,x5$DB,y6$DB)
  plot(xCent,yDB,type="b")
  #============================Dependence between numb of centroids and Dunn indexes==============================================
  xCent=c(1,2,3,4,5,6)
  yDunn=c(0,o2,o3,o4,o5,o6)
  plot(xCent,yDunn,type="b")
  
  xCent=c(1,2,3,4,5,6)
  yDunn=c(0,s2,s3,s4,s5,s6)
  plot(xCent,yDunn,type="b")
  #============================BIC method===============================
  library(ClusterR)
  opt_gmm = Optimal_Clusters_GMM(signals, max_clusters = 10, criterion = "BIC", 
                                 
                                 dist_mode = "eucl_dist", seed_mode = "random_subset",
                                 
                                 km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                                 
                                 plot_data = T)
  #====================================================================================
  install.packages("vegan")
  require(vegan)
  fit<-cascadeKM(signals,1,10,iter=1000)
  #============================GAP statistic==================================
  library(cluster)
  gskmn <- clusGap(signals, FUN = kmeans, nstart = 20, K.max = 8, B = 60)
  gskmn #-> its print() method
  plot(gskmn, main = "clusGap(., FUN = kmeans, n.start=20, B= 60)")
  
  #============================Kalinskiy==================
  install.packages("vegan")
  require(vegan)
  fit <- cascadeKM(signals, 1, 10, iter = 1000)
  head(fit$results)
  plot(fit, sortg = TRUE, grpmts.plot = TRUE)
  calinski.best <- as.numeric(which.max(fit$results[2,]))
  cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
  
  #============================affinity propogation======
  install.packages("apcluster")
  library(apcluster)
  d.apclus <- apcluster(negDistMat(r=2), signals)
  cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
  
  
  install.packages("fpc")
  library(fpc)
  
  pamk.best <- pamk(signals)
  cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
  plot(pam(Y, pamk.best$nc))
  pamk.best$nc
  
  weeks <- (weeks + 3)%%7
  weeks[weeks==0]<- 7
  
  library(ggplot2)
  hm<-data.frame(cluster=as.factor(z$clusters),day=weeks)
  ggplot(hm, aes(day,fill=cluster)) + 
    geom_histogram() + 
    facet_wrap(~ cluster)+
    scale_x_continuous(breaks=1:7)
  
  hm2<-data.frame(cluster=as.factor(z$clusters),day=weeks, value = sign, t=1:70)
  ggplot(hm2, aes(x=t,y=value,col=cluster)) + 
    geom_point() + 
    facet_wrap(~ day)
  
  EMcluster<-c( 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1 )
  nonKmeans<-c( 1, 2, 1, 2, 1, 2, 1, 1, 2, 2, 2, 2, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 1, 2, 2, 2 )
  hierarchy<-c( 1, 2, 1, 2, 1, 2, 1, 1, 2, 2, 2, 2, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 1, 2, 2, 2 )
  nonKmeans=(nonKmeans+1)%%2
  nonKmeans[nonKmeans==0]<-2
  library("mclust")
  require("mclust")
  adjustedRandIndex(hierarchy, z$clusters)
  