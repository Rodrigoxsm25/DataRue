qe_boot <- function(iterations,
                          column_of_interest,
                          outcome, predictors,
                          pct, df) {
                 
                 list.of.packages <- c("broom", "dplyr")
                 new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
                                 if(length(new.packages)) install.packages(new.packages)
                 
                 lapply(list.of.packages, library, character.only = TRUE)

                  tidy_results <- data.frame(term = character(),
                                          estimate = numeric(),
                                          std.error = numeric(),
                                          statistic = numeric(),
                                          p.value = numeric(),
                                          UniqueVector = character(),
                                          stringsAsFactors = FALSE)

                  glance_results <- data.frame(r.squared = character(),
                                          adj.r.squared = numeric(),
                                          sigma = numeric(),
                                          statistic = numeric(),
                                          p.value = numeric(),
                                          df.logLik = numeric(),
                                          AIC = numeric(),
                                          BIC = numeric(),
                                          UniqueVector = character(),
                                          stringsAsFactors = FALSE)
                  

                  i <- 1
                  j <- 1
                  k <- iterations

                  unique_vector <- unique(df[, column_of_interest])

                  while (i < length(unique_vector) + 1) {

                    filtered_df <- df %>%
                        filter(!!as.name(column_of_interest) == unique_vector[i])

                    j <- 1

                    while (j < k) {

                      subset_df <- filtered_df[sample(seq_len(nrow(filtered_df)),
                                   size = floor(pct * nrow(filtered_df))), ]
                      subset_df$TreatmentGroup <- 1
                      df$TreatmentGroup <- 0
                      temp_df <- rbind(df,subset_df)
                      
                      model_string <- as.formula(paste(outcome, paste(predictors, collapse = "+"), sep="~"))
                      
                      model <- lm(model_string, data = temp_df)
                      
                      resultst <- tidy(model)
                      resultst$UniqueVector <- unique_vector[i]
                      
                      resultsg <- glance(model)
                      resultsg$UniqueVector <- unique_vector[i]
                      
                      tidy_results <- rbind(resultst, tidy_results)
                      
                      glance_results <- rbind(resultsg, glance_results)
                      
                      j <- j + 1

                    }
                    i <- i + 1
                  }
                  model_results <- list("tidy_results" = tidy_results , "glance_results" = glance_results)
                  model_results
                }
