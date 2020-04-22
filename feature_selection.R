data <- read.csv("merged_data.csv", stringsAsFactors = FALSE)

data <- data[data$date == "2019-10-16" , ]  #use only most recent data

library(caret)

subsets <- c(1:5, 10, 20, 40)

ctrl <- rfeControl(functions = rfFuncs,     #lmFuncs, rfFuncs, nbFuncs, treebagFuncs
                   method = "cv",
                   verbose = FALSE)

categories <- unique(data$link_category)

results <- list()

for(i in 1:length(categories)){
    test <- data[data$link_category == categories[i] , c(4, 7, 8, 13, 14, 17:ncol(data))]
    test <- test[ , ! names(test) %in% names(which(apply(test, 2, sd) < 0.01))] #remove zero variance preds
    inds <- findCorrelation(cor(test[ , -1]), cutoff = 0.9, names=TRUE)
    test <- test[ , ! names(test) %in% inds]
    
    preds <- rfe(test[ , -1], test[ , 1], sizes = subsets, rfeControl = ctrl)
    results[[categories[i]]] <- predictors(preds)
}

r <- sort(table(unlist(results)))
r <- data.frame(feature = names(r),
                N_Categories = as.numeric(r))
r$feature <- factor(r$feature, levels = names(sort(table(unlist(results)), decreasing = TRUE)))

ggplot(r, aes(x = feature, y = N_Categories)) + geom_bar(stat="identity") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

