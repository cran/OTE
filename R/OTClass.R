OTClass <-
function(XTraining,YTraining,method=c("oob+independent","oob","sub-sampling"),
         p=0.1,t.initial=NULL, nf=NULL,ns=NULL,info=TRUE){

	t.initial <- ifelse(is.null(t.initial),1000,t.initial)
	YTraining <- as.factor(as.numeric(YTraining))
  ntr <- nrow(XTraining)   #Number of observations in the training data provided
  if(method=="oob+independent"){

  training2 <- sample(1:ntr,0.05*ntr) ## Sample of indices of training observations for second phase selection

  Xtraining1 <- XTraining[-training2,]

  Ytraining1 <- YTraining[-training2]


  rff   <-list()
  rf.use <-list()
  er    <-c()
  if(info==TRUE)
  cat("Assessing trees for individual performance..............\n")
  for (j in 1:t.initial){

    rff[[j]] <- randomForest(x=Xtraining1,y=Ytraining1, ntree=1, keep.forest=TRUE,
                             norm.votes=FALSE,mtry =ifelse(is.null(nf),round(sqrt(length(Xtraining1))),nf),
                             nodesize=ifelse(is.null(ns),1,ns),replace=T,sampsize = nrow(Xtraining1))#sqrt(length(Xtraining1)))
                             er[j]   <-  rff[[j]]$err.rate[[1]]

  }
  order1 <-order(er)   #order the error vector in increasing order
  rff    <-rff[order1]  #order trees in according to the order of errors
  rf.all <- rff[[1]]
  RF.ALL <- rff[[1]]    # initialize the forest best trees from the best single tree
  if(info==TRUE)
  cat("Assessing trees for collective performance..............\n")
  for (k in 1:(p*length(rff)-1))
  {
    p1 <- predict(rf.all, XTraining[training2, ],type='prob')[,2]
    bs1 <- sum((as.numeric(YTraining[training2])-as.vector(p1))^2)

    rf.all<-combine(rf.all,rff[[k+1]])
    p2 <- predict(rf.all, XTraining[training2, ],type='prob')[,2]
    bs2 <- sum((as.numeric(YTraining[training2])-as.vector(p2))^2)

    if(bs1>bs2)
      RF.ALL<-combine(RF.ALL,rff[[k+1]])
  }
  if(info==TRUE)
    cat("Number of trees selected............................  =  ",RF.ALL$ntree,"\n")
  results <- list("t.object" = RF.ALL, "selected trees" = RF.ALL$ntree,"method used"="oob+independent")
}




t.initial <- ifelse(is.null(t.initial),1000,t.initial)

ntr <- nrow(XTraining)   #Number of observations in the training data provided*

X <- XTraining

Y <- YTraining

rff   <-list()
er    <-c()
  if(method=="oob"){
  oob <- list()
  if(info==TRUE)
    cat("Assessing trees for individual performance..............\n")
  for (j in 1:t.initial){
    inbag <- sample(1:ntr,ntr, replace=T)
    oob[[j]] <- (1:ntr)[-inbag]

    if(all(Y[inbag]==1)|all(Y[inbag]==0)){
      inbag <- sample(1:ntr,length(ntr), replace=T)
      oob[[j]] <- (1:ntr)[-inbag]
    }
      rff[[j]] <- randomForest(X[inbag, ],y=Y[inbag], ntree=1, keep.forest=TRUE,norm.votes=FALSE,
                             replace = FALSE,sampsize=length(inbag))

    pred <- predict(rff[[j]],X[oob[[j]],])
    er[j]   <-  1-sum(diag(table(pred, Y[oob[[j]]])))/sum(table(pred, Y[oob[[j]]]))

  }
  order1 <-order(er)
  rff    <-rff[order1]
  oob <- oob[order1]
  rf.all <- rff[[1]]
  RF.ALL <- rff[[1]]
  if(info==TRUE)
    cat("Assessing trees for collective performance..............\n")
  for (k in 1:(p*length(rff)-1))
  {
    p1 <- predict(rf.all, X[oob[[k]], ],type='prob')[,2]
    bs1 <- sum((as.numeric(Y[oob[[k]]])-as.vector(p1))^2)

    rf.all<-combine(rf.all,rff[[k+1]])
    p2 <- predict(rf.all, X[oob[[k]], ],type='prob')[,2]
    bs2 <- sum((as.numeric(Y[oob[[k]]])-as.vector(p2))^2)

    if(bs1>bs2)
      RF.ALL<-combine(RF.ALL,rff[[k+1]])
  }
  if(info==TRUE)
    cat("Number of trees selected............................  =  ",RF.ALL$ntree,"\n")
  results <- list("t.object" = RF.ALL, "selected trees" = RF.ALL$ntree,"method used"="oob")
  }

#####
 if(method=="sub-sampling"){
  oob <- list()
  if(info==TRUE)
    cat("Assessing trees for individual performance..............\n")
  for (j in 1:1500){
    inbag <- sample(1:ntr,0.9*ntr, replace=FALSE)
    oob[[j]] <- (1:ntr)[-inbag]

    if(all(Y[inbag]==1)|all(Y[inbag]==0)){
      inbag <- sample(1:ntr,0.9*ntr, replace=FALSE)
      oob[[j]] <- (1:ntr)[-inbag]
    }
      rff[[j]] <- randomForest(X[inbag, ],y=Y[inbag], ntree=1, keep.forest=TRUE,norm.votes=FALSE,
                             replace = FALSE,sampsize=length(inbag))

    pred <- predict(rff[[j]],X[oob[[j]],])
    er[j]   <-  1-sum(diag(table(pred, Y[oob[[j]]])))/sum(table(pred, Y[oob[[j]]]))
    #p.test=predict(rff[[i]], X[testing, ])
    #er[[i]]  <- sum((Y[testing]- p.test)^2)/sum((Y[testing]- mean(Y[testing]))^2)
    #er[[i]]  <- sum((predict(rff[[i]])- rff[[i]]$y)^2)/sum((rff[[i]]$y-mean(rff[[i]]$y))^2)
  }
  order1 <-order(er)
  rff    <-rff[order1]
  oob <- oob[order1]
  rf.all <- rff[[1]]
  RF.ALL <- rff[[1]]
  if(info==TRUE)
    cat("Assessing trees for collective performance..............\n")
  for (k in 1:(p*length(rff)-1))
  {
    p1 <- predict(rf.all, X[oob[[k]], ],type='prob')[,2]
    bs1 <- sum((as.numeric(Y[oob[[k]]])-as.vector(p1))^2)

    rf.all<-combine(rf.all,rff[[k+1]])
    p2 <- predict(rf.all, X[oob[[k]], ],type='prob')[,2]
    bs2 <- sum((as.numeric(Y[oob[[k]]])-as.vector(p2))^2)

    if(bs1>bs2)
      RF.ALL<-combine(RF.ALL,rff[[k+1]])
  }
  if(info==TRUE)
    cat("Number of trees selected............................  =  ",RF.ALL$ntree,"\n")
  results <- list("t.object" = RF.ALL, "selected trees" = RF.ALL$ntree,"method used"="sub_sampling")
 }
return((results))
}
