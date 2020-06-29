better_split <- function(
                          column_of_interest,
                          pct, df) {
                 
                 list.of.packages <- c("broom", "dplyr")
                 new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
                                 if(length(new.packages)) install.packages(new.packages)
                 
                 lapply(list.of.packages, library, character.only = TRUE)

                  i <- 1

                  train <- 0
                  test <- 0

                  unique_vector <- unique(df[, column_of_interest])

                  while (i < length(unique_vector) + 1) {

                    filtered_df <- df %>%
                      filter(!!as.name(column_of_interest) == unique_vector[i])

                      indexes <- sample(seq_len(nrow(filtered_df)),
                                   size = floor(pct * nrow(filtered_df)))

                      trainx <- filtered_df[indexes, ]
                      testx <- filtered_df[-indexes, ]

                      train <- rbind(train, trainx)
                      test <- rbind(test, testx)

                      i <- i + 1

                    }

                    train$split <- "train"
                    train <- train[(2:nrow(train)), ]
                    test$split <- "test"
                    test <- test[(2:nrow(test)), ]
                    df <- rbind(train, test)
                    df
                }
