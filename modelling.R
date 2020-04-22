data <- read.csv("merged_data.csv", stringsAsFactors = FALSE)

#use only most recent data and features we built
data <- data[data$date == "2019-10-16" , c(4, 7, 8, 9, 13, 14, 17:ncol(data))] 
app_category <- data$link_category
data$link_category <- NULL

categories <- unique(app_category)

tc <- trainControl(method='cv', number=10)

r2 <- rep(NA_real_, length(categories))

for(i in 1:length(categories)){
    model <- train(rank ~ ., data=data[app_category == categories[i], ], method='rf', 
                   tuneLength=10, metric='Rsquared', trControl=tc)
    r2[i] <- max(model$results$Rsquared)
    
    plot(model)
}

results <- data.frame(r2, categories)

ggplot(results, aes(categories, r2)) + geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))

