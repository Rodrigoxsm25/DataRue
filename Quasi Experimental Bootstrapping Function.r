
#bootstrapping(50,"ACT_PrepProgram","CompositeScaleScore",c("Rstar","ECD","SPED","LEP","TreatmentGroup"),df)

bootstrapping <- function(iterations,
                          column_of_interest,
                          outcome, predictors,
                          df) {
                 
                 list.of.packages <- c("broom", "dplyr")
                 new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
                                 if(length(new.packages)) install.packages(new.packages)
                 
                 lapply(list.of.packages, library, character.only = TRUE)

                  results_all <- data.frame(term = character(),
                                          estimate = numeric(),
                                          std.error = numeric(),
                                          statistic = numeric(),
                                          p.value = numeric(),
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
                                   size = floor(.75 * nrow(filtered_df))), ]
                      subset_df$TreatmentGroup <- 1
                      df$TreatmentGroup <- 0
                      temp_df <- rbind(df,subset_df)
                      
                      model_string <- as.formula(paste(outcome, paste(predictors, collapse = "+"), sep="~"))
                      
                      model <- lm(model_string, data = temp_df)
                      
                      results <- tidy(model)
                      results$UniqueVector <- unique_vector[i]
                      
                      results_all <- rbind(results, results_all)
                      j <- j + 1
                      
                      
                     
                    }
                    i <- i + 1
                  }
                  results_all
                }
