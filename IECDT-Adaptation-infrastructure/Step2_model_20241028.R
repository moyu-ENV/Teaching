  rm(list = ls())
  library(dplyr)
  library(randomForest)
  
  wd <- paste0("C:/Users/Y/Desktop/Oxford/Projects/IECDT/") #local
 
  #set up input
  if(TRUE){
  input_raw <- read.csv(paste0(wd,'blackout_withStorm_withSocio.csv'),stringsAsFactors = FALSE)
 
  Y <- 'duration'
  allXs <- c("windmax",'speed',"distToTrack","income")
  
  input_std <- input_raw[,c(Y,allXs)]
  for (col in c(Y,allXs)){
    #col <- 3
    input_std[,col] <- input_std[,col]-mean(input_std[,col],na.rm=TRUE)   
    input_std[,col] <- input_std[,col]/sd(input_std[,col],na.rm=TRUE)   
  }
  
  sd <- 1234
  
  #split training and testing 
  input <- input_std
  if(TRUE){
  set.seed(sd) 
  testRows<- sample(1:nrow(input), round(nrow(input)/10,digits = 0), replace=FALSE)
  input_test <- input[testRows,]
  input_train <- input[-testRows,]
  
  }
  }
  
  accuracy_all <- data.frame()
  varImp_all <- data.frame()
  
  #######################
  #eXtreme Gradient Boosting
  #######################
  if(TRUE){
  library(xgboost)
  mdl <- 'XGB'
  
  formula <- as.formula(paste('prob',"~", paste(allXs, collapse = " + ")))  
  
  #tune
  if(FALSE){  
    hyper_grid_xgb <- expand.grid(
      objective=c("binary:logistic"), #'reg:squarederror', "reg:logistic",#for classification 'binary:logistic', 
      metrics=c('rmse'), #'error','mae',"mape",'logloss','auc','aucpr','merror'
      booster=c('gbtree'), #,'gblinear'
      eta = c(0.1,1), # the learning rate.~[0.01 - 0.3] Lower eta leads to slower computation. It must be supported by increase in nrounds.
      nrounds = c(100), 
      gamma = c(1),#regulation. ~[0,inf]. Start with 0 and check CV error rate. If you see train error >>> test error, bring gamma into action. Higher the gamma, lower the difference in train and test CV.                                
      alpha = c(1), #L1 regularization (equivalent to Lasso regression) on weights.
      lambda = c(1), #L2 regularization (equivalent to Ridge regression) on weights. It is used to avoid overfitting
      max_depth=c(8),#the depth of the tree.Larger the depth, more complex the model; higher chances of overfitting
      min_child_weight=c(6),
      subsample=c(0.8,0.9), #number of samples (observations) supplied to a tree. ~ (0.5-0.8)
      colsample_bytree=c(0.8,0.9), # the number of features (variables) supplied to a tree. [0,1] ~(0.5,0.9)
      colsample_bylevel=c(0.8,0.9), # the number of features (variables) supplied to each level. [0,1] ~(0.5,0.9)
      colsample_bynode=c(0.8,0.9), # the number of features (variables) supplied to each node. [0,1] ~(0.5,0.9)
      trainAccuracy = NA,  
      testAccuracy = NA  
    )
    
    for(i in 1:nrow(hyper_grid_xgb)) {
      #i <-1
      print(paste0(i,'/',nrow(hyper_grid_xgb)))
      set.seed(sd)
      model_xgboost <- xgboost(data = data.matrix(input_train[,allXs]), 
                               label = input_train[,'prob'], 
                               nthread = 1,
                               objective = as.character(hyper_grid_xgb$objective[i]),
                               metrics = as.character(hyper_grid_xgb$metrics[i]),
                               booster=as.character(hyper_grid_xgb$booster[i]),
                               eta= hyper_grid_xgb$eta[i],
                               nrounds=hyper_grid_xgb$nrounds[i],
                               gamma=hyper_grid_xgb$gamma[i],
                               alpha=hyper_grid_xgb$alpha[i],
                               lambda=hyper_grid_xgb$lambda[i],
                               max_depth=hyper_grid_xgb$max_depth[i],
                               min_child_weight=hyper_grid_xgb$min_child_weight[i],
                               subsample=hyper_grid_xgb$subsample[i],
                               colsample_bytree=hyper_grid_xgb$colsample_bytree[i],
                               colsample_bylevel=hyper_grid_xgb$colsample_bylevel[i],
                               colsample_bynode=hyper_grid_xgb$colsample_bynode[i],
                               verbose = TRUE)
      
      pred_train_xgboost <- predict(model_xgboost, newdata = data.matrix(input_train[,allXs]))
      pred_train_xgboost <- ifelse (pred_train_xgboost > 0.5,1,0)
      
      pred_test_xgboost <- predict(model_xgboost, newdata = data.matrix(input_test[,allXs]))
      pred_test_xgboost <- ifelse (pred_test_xgboost > 0.5,1,0)
      
      hyper_grid_xgb$trainAccuracy[i] <- caret::confusionMatrix(as.factor(input_train$prob),as.factor(pred_train_xgboost))$overall[[1]]
      hyper_grid_xgb$testAccuracy[i] <- caret::confusionMatrix(as.factor(input_test$prob),as.factor(pred_test_xgboost))$overall[[1]]
     
  }
  }
  
  #best
  if(TRUE){
    set.seed(sd)
    model_xgboost_bst <- xgboost(data = data.matrix(input_train[,allXs]), 
                                 label = input_train[,'prob'], 
                                 nthread = 1,
                                 objective = 'binary:logistic',
                                 metrics = 'rmse',
                                 booster='gbtree',
                                 eta= 0.1,
                                 nrounds=100,
                                 gamma=1,
                                 alpha=1,
                                 lambda=1,
                                 max_depth=8,
                                 min_child_weight=6,
                                 subsample=0.8,
                                 colsample_bytree=0.8,
                                 colsample_bylevel=0.8,
                                 colsample_bynode=0.8,
                                 verbose = TRUE)
    
    pred_train_xgboost <- predict(model_xgboost_bst, newdata = data.matrix(input_train[,allXs]))
    pred_train_xgboost <- ifelse (pred_train_xgboost > 0.5,1,0)
    
    pred_test_xgboost <- predict(model_xgboost_bst, newdata = data.matrix(input_test[,allXs]))
    pred_test_xgboost <- ifelse (pred_test_xgboost > 0.5,1,0)
    
    trainAccuracy <- caret::confusionMatrix(as.factor(input_train$prob),as.factor(pred_train_xgboost))$overall[[1]]
    testAccuracy <- caret::confusionMatrix(as.factor(input_test$prob),as.factor(pred_test_xgboost))$overall[[1]]
    
    accuracy <- data.frame(train_accuracy=trainAccuracy,test_accuracy=testAccuracy)
    accuracy$model <- mdl
    accuracy_all <- rbind(accuracy_all,accuracy)
        
    tmp<-xgb.importance (feature_names = colnames(input_train[,allXs]),model = model_xgboost_bst)
    varImp<-data.frame(imp=tmp$Gain)
    varImp$var <- tmp$Feature
    varImp$model <- mdl
    varImp_all <- rbind(varImp_all,varImp)
    }
  }

  write.csv(accuracy_all,paste0(wd,'/accuracy.csv'),row.names=FALSE)
  write.csv(varImp_all,paste0(wd,'/varImp.csv'),row.names=FALSE)

