get_by_idx <- function(ts, idx){
  sel_ts <- ts[,idx]
  return(sel_ts)
}

get_feature_by_idx <- function(featurelist, idx){
  feature <- featurelist[idx,2]
  return(feature)
}

plot_hist_for_feature <- function(i,featcl1,featcl2, groups,name){
  nfeatcl1 <- map(featcl1,get_feature_by_idx,i)
  nfeatcl2 <- map(featcl2,get_feature_by_idx,i)
  nfeatcl1 <- as.numeric(nfeatcl1)
  nfeatcl2 <- as.numeric(nfeatcl2)
  df <- data.frame(value = c(nfeatcl1, nfeatcl2),group=groups )
  plot <- ggplot(df, aes(x = value, fill = group)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "Histogram of Two Groups",
       x = "Value",
       y = "Frequency") 
  ggsave( paste(paste("feature",paste(i,name,sep=""),sep=""),".png",sep=""), plot, width = 8, height = 6, dpi = 300) 
}

plot_hist_for_feature_svg <- function(i,featcl1,featcl2, groups,name){
  nfeatcl1 <- map(featcl1,get_feature_by_idx,i)
  nfeatcl2 <- map(featcl2,get_feature_by_idx,i)
  nfeatcl1 <- as.numeric(nfeatcl1)
  nfeatcl2 <- as.numeric(nfeatcl2)
  df <- data.frame(value = c(nfeatcl1, nfeatcl2),group=groups )
  plot <- ggplot(df, aes(x = value, fill = group)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "Histogram of Two Groups",
       x = "Value",
       y = "Frequency") 
  ggsave( paste(paste("feature",paste(i,name,sep=""),sep=""),".svg",sep=""), plot, width = 8, height = 6) 
}

plot_hist_for_feature_noidx_svg <- function(i,featcl1,featcl2, groups,name){
  nfeatcl1 <- as.numeric(featcl1)
  nfeatcl2 <- as.numeric(featcl2)
  df <- data.frame(value = c(nfeatcl1, nfeatcl2),group=groups )
  plot <- ggplot(df, aes(x = value, fill = group)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "Histogram of Two Groups",
       x = "Value",
       y = "Frequency") 
  ggsave( paste(paste("FULLfeature",paste(i,name,sep=""),sep=""),".svg",sep=""), plot, width = 8, height = 6) 
}

plot_hist_for_feature_noidx_svg_wunit <- function(i,featcl1,featcl2, groups,name, unit){
  nfeatcl1 <- as.numeric(featcl1)
  nfeatcl2 <- as.numeric(featcl2)
  df <- data.frame(value = c(nfeatcl1, nfeatcl2),class=groups )
  plot <- ggplot(df, aes(x = value, fill = class)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "",
       x = unit,
       y = "Frequency") 
  ggsave( paste(paste("FULLfeature",paste(i,name,sep=""),sep=""),".svg",sep=""), plot, width = 8, height = 6) 
}

plot_hist_for_feature_noidx_png <- function(i,featcl1,featcl2, groups,name){
  nfeatcl1 <- as.numeric(featcl1)
  nfeatcl2 <- as.numeric(featcl2)
  df <- data.frame(value = c(nfeatcl1, nfeatcl2),group=groups )
  plot <- ggplot(df, aes(x = value, fill = group)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "Histogram of Two Groups",
       x = "Value",
       y = "Frequency") 
  ggsave( paste(paste("FULLfeature",paste(i,name,sep=""),sep=""),".png",sep=""), plot, width = 8, height = 6) 
}

no_legendplot_hist_for_feature_noidx_png_wunit <- function(i,featcl1,featcl2, groups,name,yylim){
  nfeatcl1 <- as.numeric(featcl1)
  nfeatcl2 <- as.numeric(featcl2)
  df <- data.frame(value = c(nfeatcl1, nfeatcl2),class=groups )
  plot <- ggplot(df, aes(x = value, fill = class)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "",
       x = "",
       y = "") + theme(legend.position="none", axis.text.y=element_blank()) + ylim(0, yylim)
  ggsave( paste(paste("NoLegendMatrixfeature",paste(i,name,sep=""),sep=""),".png",sep=""), plot, width = 8, height = 6) 

}

no_legendplot_hist_for_feature_noidx_png_wunit_nolims <- function(i,featcl1,featcl2, groups,name){
  nfeatcl1 <- as.numeric(featcl1)
  nfeatcl2 <- as.numeric(featcl2)
  df <- data.frame(value = c(nfeatcl1, nfeatcl2),class=groups )
  plot <- ggplot(df, aes(x = value, fill = class)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "",
       x = "",
       y = "") + theme(legend.position="none", axis.text.x=element_blank()) 
  ggsave( paste(paste("NoLegendMatrixfeature",paste(i,name,sep=""),sep=""),".png",sep=""), plot, width = 8, height = 6) 
}

plot_hist_for_feature_noidx_png_wunit <- function(i,featcl1,featcl2, groups,name,unit){
  nfeatcl1 <- as.numeric(featcl1)
  nfeatcl2 <- as.numeric(featcl2)
  df <- data.frame(value = c(nfeatcl1, nfeatcl2),class=groups )
  plot <- ggplot(df, aes(x = value, fill = class)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "",
       x = unit,
       y = "Frequency") 
  ggsave( paste(paste("Matrixfeature",paste(i,name,sep=""),sep=""),".png",sep=""), plot, width = 8, height = 6) 
}

plot_hist_for_feature_test <- function(i,featcl1,featcl2, feattest ,groups,groupstest, name){
  nfeatcl1 <- map(featcl1,get_feature_by_idx,i)
  nfeatcl2 <- map(featcl2,get_feature_by_idx,i)
  nfeattest <- map(feattest,get_feature_by_idx,i)
  nfeatcl1 <- as.numeric(nfeatcl1)
  nfeatcl2 <- as.numeric(nfeatcl2)
  nfeattest <- as.numeric(nfeattest)
  df1 <- data.frame(value = c(nfeatcl1, nfeatcl2),group=groups )
  df2 <- data.frame(value = nfeattest,group=groupstest )
  df <- rbind(df1, df2)
  #print(df)
  plot <- ggplot(df, aes(x = value, fill = group)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "Histogram of Two Groups + test",
       x = "Value",
       y = "Frequency") 
  ggsave( paste(paste("TESTfeature",paste(i,name,sep=""),sep=""),".png",sep=""), plot, width = 8, height = 6, dpi = 300) 
}

tsbootstrapover <- function(x,nb){
  res <- tsbootstrap(x,nb=nb,type = "block", b=1200)
  return(res)
}

tsbootstrapover_onemts <- function(x,nb){
  res <- map(matrix2list(x),tsbootstrapover,nb=nb)
  return(res)
}

matrix2list <- function(mat){
  res <- split(mat, rep(1:ncol(mat), each = nrow(mat)))
  return(res)
}

classfun1 <- function(x,q1,q2){
if ((x <= q2) && (q1<=x)){
  return(1) 
}else{

  return(0)
}

}    

classfun2 <- function(x,q1){
if ((q1<=x)){
  return(1) 
}else{

  return(0)
}

}

calc_acc1 <- function(y,q1,q2, ytest){
  res <- sum(as.numeric(map(as.numeric(y),classfun1,q1=q1,q2=q2)==ytest))/length(y)  
  return(res)
}

calc_acc2 <- function(y,q1, ytest){
  res <- sum(as.numeric(map(as.numeric(y),classfun2,q1=q1)==ytest))/length(y)  
  return(res)
}

get_intervals_from_y <- function(input_list, w) {
   n <- length(input_list)
   intervals <- list()

   for (i in seq(1, n, by = w)) {
     if (i + w - 1 <= n) {
       window <- input_list[i:(i + w - 1)]
       intervals[[length(intervals) + 1]] <- c(min(window), max(window))
     }
   }

   return(intervals)
 }

get_moments_from_y <- function(input_list, w) {
   n <- length(input_list)
   moments <- list()

   for (i in seq(1, n, by = w)) {
     if (i + w - 1 <= n) {
       window <- input_list[i:(i + w - 1)]
       moments[[length(moments) + 1]] <- c(mean(window), std(window))
     }
   }

   return(moments)
 }
 
 FC_LocalSimple_mean_stderr <- function(input_list,k){
  ys <- sma(zscore(input_list),k) 
  return(std(ys$residuals))
}

FC_LocalSimple_mean5_stderr <- function(input_list){
  return(FC_LocalSimple_mean_stderr(input_list,5))
}

FC_LocalSimple_mean8_stderr <- function(input_list){
  return(FC_LocalSimple_mean_stderr(input_list,8))

}

FC_LocalSimple_mean11_stderr <- function(input_list){
  return(FC_LocalSimple_mean_stderr(input_list,11))

}

FC_LocalSimple_mean_stderr_struct <- function(input_list){
  struct <- list(mean3 = FC_LocalSimple_mean_stderr(input_list,3), mean5 = FC_LocalSimple_mean_stderr(input_list,5), mean8 = FC_LocalSimple_mean_stderr(input_list,8), mean11 = FC_LocalSimple_mean_stderr(input_list,11), mean15 = FC_LocalSimple_mean_stderr(input_list,15))
  return(struct)
}

FC_LocalSimple_mean_stderr_wrollmedian <- function(input_list,k1,k2){
  input_list <- rollmedian(zscore(input_list),k1)
  return(FC_LocalSimple_mean_stderr(input_list,k2))
}

FC_LocalSimple_mean3_stderr_wrollmedian <- function(input_list){
  return(FC_LocalSimple_mean_stderr_wrollmedian(input_list,11,3))
}

FC_LocalExponential_smooth_stderr <- function(input_list){
  ys <- es(zscore(input_list)) 
  return(std(ys$residuals))
}

FC_LocalAR_AIC_stderr <- function(input_list){
  ys <- ar(zscore(input_list)) 
  return(std(na.omit(ys$resid)))
}

FC_LocalAR_AIC_stderr_nzscore <- function(input_list){
  ys <- ar(input_list)
  return(std(na.omit(ys$resid)))
}

FC_LocalAR_nAIC_stderr <- function(input_list,p){
  ys <- ar(zscore(input_list),aic = FALSE,order.max = p)
  return(std(na.omit(ys$resid)))
}

FC_LocalSimple_median_stderr <- function(input_list,k){
  xs <- zscore(input_list)
  ys <- zoo::rollapplyr( xs, width = k, FUN = median)
  ys_res <- ys - xs[-(1:(k-1))]
  return(std(ys_res))
}

FC_LocalSarima_stderr <- function(input_list,p,d,q){
  xs <- zscore(input_list)
  ys <- sarima(xs,p,d,q)
  ys_res <- resid(ys$fit)
  return(std(ys_res))
}

FC_LocalSarima_stderr_wrollmedian <- function(input_list,k=51,p=1,d=1,q=3){
  input_list <- rollmedian(zscore(input_list),k)
  return(FC_LocalSarima_stderr(input_list,p,d,q))
}

binary_cross_fols_validation_with_acc <- function(X,y, num_folds, IoI){

  fold_indices <- cut(seq_along(y), breaks = num_folds, labels = FALSE)

  xxtrain <- lapply(1:num_folds, function(i) {fold_indices <- which((seq_along(y) - 1) %% num_folds != (i - 1))
  return(fold_indices)})

  xxtest<- lapply(1:num_folds, function(i) {fold_indices <- which((seq_along(y) - 1) %% num_folds == (i - 1))
  return(fold_indices)})
  cv_res <- c()
  cv_res2 <- c()
  cv_auc <- c()
  cv_auc2 <- c()
  for (i in 1:length(xxtrain)){
    print("current step:")
    xtrain <- featdf[xxtrain[[i]]]
    ytrain <- y[xxtrain[[i]]]
    ytest <-  y[xxtest[[i]]]
    tttrain  <- lapply(xtrain,"[", IoI)
    xtrain0 <- tttrain[ytrain==0]
    xtrain1 <- tttrain[ytrain==1]
    xtest <- featdf[xxtest[[i]]]
    tttest  <- lapply(xtest,"[", IoI)
    xtest <- unlist(tttest)
    sumstat1 <- summary(unlist(tttrain[ytrain==1]))

    teststat <- sum(as.numeric(map(as.numeric(xtest),classfun1,q1=sumstat1[2],q2=sumstat1[5]))==ytest)/length(xtest)
    teststat2 <- sum(as.numeric(map(as.numeric(xtest),classfun1,q1=sumstat1[1],q2=sumstat1[6]))==ytest)/length(xtest)

    cv_res <- c(cv_res,teststat)
    cv_res2 <- c(cv_res2,teststat2)

    predicted <- as.numeric(c(as.numeric(!ytest),map(as.numeric(xtest),classfun1,q1=sumstat1[2],q2=sumstat1[5])))
    predicted2 <- as.numeric(c(as.numeric(!ytest),map(as.numeric(xtest),classfun1,q1=sumstat1[1],q2=sumstat1[6])))

    print(predicted)
    resp <- rep(ytest,length(xtest))
    resp <- c(as.numeric(!ytest), resp)
    print(resp)
    v_auc <- auc(resp,predicted)
    v_auc2 <- auc(resp, predicted2)
    cv_auc <- c(cv_auc, v_auc)
    cv_auc2 <- c(cv_auc2,v_auc2)

  }
  Racc <- pmax(cv_res,cv_res2)
  Rauc <- pmax(cv_auc,cv_auc2)

  return(list("acc" = Racc, "auc" = Rauc))
}

colsd <- function(data){apply(data,MARGIN = 2,sd)}



change_period <- function(timeser, per){
tsnew <- rollapplyr(as.zoo(timeser), per, mean, by = per, partial=TRUE)
tsnew <- as.ts(coredata(tsnew))
return (tsnew)
}


filterx <- function(timeser, per, k, ro = FALSE, zeroing=FALSE) {
  tsnew <- hampel(rollmedian(timeser, per), k)
  tsnew <- tsnew$y
  for (i in 1:ncol(tsnew)){
       xts <- tsnew[, i]
       if(ro){
              med_curr<- median(xts)
       xts[xts %in% boxplot.stats(xts)$out] <- med_curr
       #print(i)
       }

       if(zeroing){
              if ((i!=2) | (i!=1)){
              xts[xts < 0] <- 0
       }
       }
       tsnew[, i] <- xts
  }
return(tsnew)
}