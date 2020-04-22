setwd("/home/share/mcook/DataScienceChallange/Bold/analysis")

data <- read.csv("all-scrape-2019-10-17-hour-12.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

reviews1 <- read.csv("review-scrape-2019-09-10-hour-12.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
reviews2 <- read.csv("review-scrape-2019-10-17-hour-12.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
all_reviews <- rbind(reviews1, reviews2)
all_reviews <- unique(all_reviews)

#change nas in number of reviews to 0 rather than na
data$totalReviews[is.na(data$totalReviews)] <- 0

process_link <- function(link){
    s <- strsplit(link, split = "&")[[1]]
    x <- unlist(strsplit(s[2:4], "="))[c(2, 4, 6)]
    s <- s[1]
    s <- strsplit(s, "/")[[1]][4]
    s <- strsplit(s, "\\?")[[1]]
    name <- s[1]
    category <- strsplit(s[2], "=")[[1]][2]
    
    return(c(name, category, x))
}


link_data <- t(sapply(data$link, process_link))
rownames(link_data) <- NULL
colnames(link_data) <- c("link_name", "link_category", "link_page_rank", "link_rank", "link_surafce_type")
link_data <- data.frame(link_data, stringsAsFactors = FALSE)
link_data$link_rank <- as.numeric(link_data$link_rank)
link_data$link_page_rank <- as.numeric(link_data$link_page_rank)
data <- data.frame(data, link_data)
rm(link_data)


#recompute rank
data$rank <- (data$link_page_rank - 1) * 24 + data$link_rank


word_count <- function(text){
    return(length(strsplit(text, " ")[[1]]))
}

data$name_words <- sapply(data$name, word_count)
data$name_chars <- sapply(data$name, nchar)

#add empirical bayes estimates
estBetaParams <- function(mu, var) {
    alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
    beta <- alpha * (1 / mu - 1)
    return(params = list(alpha = alpha, beta = beta))
}

eb <- function(numerator, denominator, mu, var){
    param <- estBetaParams(mu, var)
    a <- param$alpha
    b <- param$beta
    return((numerator + a) / (denominator + a + b))
}

data$d <- data$totalReviews * 5
data$n <- data$aggReviews * data$totalReviews

data$p <- data$n / data$d

data$eb <- eb(data$n, data$d, 
              mean(0.5, na.rm=TRUE), var(data$n / data$d, na.rm=TRUE))  #use prior that maximizes uncertainty

#build set of features that summarize review data
all_reviews$review_words <- sapply(all_reviews$review, word_count)
all_reviews$review_chars <- sapply(all_reviews$review, nchar)

all_reviews$reply_words <- sapply(all_reviews$developer_reply_content, word_count)
all_reviews$reply_chars <- sapply(all_reviews$developer_reply_content, nchar)

count_char <- function(text, char_pattern){
    return(sum(gregexpr(char_pattern, text)[[1]] > 0))
}

all_reviews$review_n_exclamation <- sapply(all_reviews$review, count_char, char_pattern = "\\!")
all_reviews$review_n_period <- sapply(all_reviews$review, count_char, char_pattern = "\\.")
all_reviews$review_n_comma <- sapply(all_reviews$review, count_char, char_pattern = "\\,")
all_reviews$review_n_question <- sapply(all_reviews$review, count_char, char_pattern = "\\?")

all_reviews$reply_n_exclamation <- sapply(all_reviews$developer_reply_content, count_char, char_pattern = "\\!")
all_reviews$reply_n_period <- sapply(all_reviews$developer_reply_content, count_char, char_pattern = "\\.")
all_reviews$reply_n_comma <- sapply(all_reviews$developer_reply_content, count_char, char_pattern = "\\,")
all_reviews$reply_n_question <- sapply(all_reviews$developer_reply_content, count_char, char_pattern = "\\?")

#install.packages("tidytext")
library(tidytext)

sent <- tidytext::sentiments

sent_counts <- function(text){
    features <- unique(sent$sentiment)

    text <- strsplit(tolower(text), " ")[[1]]
    ind <- match(text, sent$word)
    tab <- table(sent$sentiment[ind])
    return(ifelse(is.na(tab["positive"] / sum(tab)), 0, tab["positive"] / sum(tab)))
}

all_reviews$review_sent <- sapply(all_reviews$review, sent_counts)
all_reviews$reply_sent <- sapply(all_reviews$developer_reply_content, sent_counts)
all_reviews$sent_int <- all_reviews$review_sent * all_reviews$reply_sent

#compute length of time to respond
all_reviews$review_date <- as.Date(all_reviews$review_date, format='%B %d, %Y')
all_reviews$developer_reply_date <- as.Date(all_reviews$developer_reply_date, format='%B %d, %Y')
all_reviews$date_diff <- as.numeric(all_reviews$developer_reply_date - all_reviews$review_date)
all_reviews$date_diff[is.na(all_reviews$date_diff)] <- 0
all_reviews$edited <- ifelse(all_reviews$review_date_type == "LAST_EDITED", 1, 0)
all_reviews$dev_reply <- ifelse(all_reviews$developer_reply == "true", 1, 0)

#aggregate review data to main dataset
split_data <- split(all_reviews[ , 11:ncol(all_reviews)], all_reviews$app_name)
vars <- colnames(all_reviews[ , 11:ncol(all_reviews)])
stats_names <- c("min", "fq", "median", "mean", "tq", "max", "sd")

compute_stats <- function(df){
    return(c(sapply(df, function(x){c(summary(x), sd=sd(x))})))
}

stats <- t(sapply(split_data, compute_stats))
stats <- as.data.frame(stats)
colnames(stats) <- apply(expand.grid(stats_names, vars), 1, function(x) paste(x[2], x[1], sep="_")) 
stats$link_name <- rownames(stats)
stats[is.na(stats)] <- 0

merged_data <- merge(data, stats, "link_name")

write.csv(merged_data, "merged_data.csv", row.names = FALSE)

