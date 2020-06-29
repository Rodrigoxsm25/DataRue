###### Monopoly | 4 companies ###### 

sales_Company_a<-0
sales_Company_b<-0
sales_Company_c<-0
sales_Company_d<-0

population <- 1000

technology_vector_a<-0
technology_vector_b<-0
technology_vector_c<-0
technology_vector_d<-0

tech_a <- .25
tech_b <- .50
tech_c <- .75
tech_d <- 1

i<-1

while(i < 1001){
  
  demand_Company_a <- 0
  supply_Company_a <-1000
  
  demand_Company_b <- 0
  supply_Company_b <-1000
  
  demand_Company_c <- 0
  supply_Company_c <-1000
  
  demand_Company_d <- 0
  supply_Company_d <-1000
  
  agents <- runif(population, min = 0, max = 1)
  
  for(j in agents){
    
    if(j <= tech_a){
      if(supply_Company_a > 0){
        
        
        demand_Company_a <- demand_Company_a + 1
        supply_Company_a <- supply_Company_a - 1
        
      }
      
    } 
    
    else if(j <= tech_b & j > tech_a){
      if(supply_Company_b > 0){
        
        demand_Company_b <- demand_Company_b + 1
        supply_Company_b <- supply_Company_b - 1
        
      }
      
    }
    
    else if(j <= tech_c & j > tech_b){
      if(supply_Company_c > 0){
        
        demand_Company_c <- demand_Company_c + 1
        supply_Company_c <- supply_Company_c - 1
        
      }
      
    }
    
    else if(j <= tech_d & j > tech_c){
      if(supply_Company_d > 0){
        
        demand_Company_d <- demand_Company_d + 1
        supply_Company_d <- supply_Company_d - 1
        
      }
      
    }
    
  }
  
  technology_vector_a[i]<-tech_a
  technology_vector_b[i]<-tech_b - tech_a
  technology_vector_c[i]<-tech_c - tech_b
  technology_vector_d[i]<-tech_d - tech_c
  
  tech_a <- demand_Company_a/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
  tech_b <- demand_Company_b/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d) + tech_a
  tech_c <- demand_Company_c/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d) + tech_b
  tech_d <- demand_Company_d/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d) + tech_c
  
  
  sales_Company_a[i]<-demand_Company_a
  sales_Company_b[i]<-demand_Company_b
  sales_Company_c[i]<-demand_Company_c
  sales_Company_d[i]<-demand_Company_d
  
  i<-i+1
  
}

df_a <- as.data.frame(cbind(sales_Company_a,technology_vector_a))
names(df_a)[1]<-"Sales"
names(df_a)[2]<-"Tech"

df_b <- as.data.frame(cbind(sales_Company_b,technology_vector_b))
names(df_b)[1]<-"Sales"
names(df_b)[2]<-"Tech"

df_c <- as.data.frame(cbind(sales_Company_c,technology_vector_c))
names(df_c)[1]<-"Sales"
names(df_c)[2]<-"Tech"

df_d <- as.data.frame(cbind(sales_Company_d,technology_vector_d))
names(df_d)[1]<-"Sales"
names(df_d)[2]<-"Tech"

df_a$Company <- "Company A"
df_b$Company <- "Company B"
df_c$Company <- "Company C"
df_d$Company <- "Company D"

df<-rbind(df_a,df_b,df_c,df_d)
df$Time<-c(1:1000,1:1000,1:1000,1:1000)

library(ggplot2)

SalesPlot1<-ggplot(df, aes(x=Tech, y=Sales, group=Company)) +
  geom_point(aes(color=Company))

SalesPlot1

SalesPlot_4C<-ggplot(df, aes(x=Time, y=Sales, group=Company)) +
  geom_line(aes(color=Company))+
  geom_point(aes(color=Company))+
  xlim(0,1000)+ylim(0,1000)

SalesPlot_4C

model<-lm(sales_Company_b~technology_vector_b)

plot(technology_vector_b,sales_Company_b)


###### Monopoly | 4 companies | Government Intervention ###### 

reg_factor <- .5

sales_Company_a<-0
sales_Company_b<-0
sales_Company_c<-0
sales_Company_d<-0

population <- 1000

technology_vector_a<-0
technology_vector_b<-0
technology_vector_c<-0
technology_vector_d<-0

tech_a <- .25
tech_b <- .50
tech_c <- .75
tech_d <- 1

i<-1

while(i < 1001){
  
  demand_Company_a <- 0
  supply_Company_a <-1000
  
  demand_Company_b <- 0
  supply_Company_b <-1000
  
  demand_Company_c <- 0
  supply_Company_c <-1000
  
  demand_Company_d <- 0
  supply_Company_d <-1000
  
  agents <- runif(population, min = 0, max = 1)
  
  for(j in agents){
    
    if(j <= tech_a){
      if(supply_Company_a > 0){
        
        
        demand_Company_a <- demand_Company_a + 1
        supply_Company_a <- supply_Company_a - 1
        
      }
      
    } 
    
    else if(j <= tech_b & j > tech_a){
      if(supply_Company_b > 0){
        
        demand_Company_b <- demand_Company_b + 1
        supply_Company_b <- supply_Company_b - 1
        
      }
      
    }
    
    else if(j <= tech_c & j > tech_b){
      if(supply_Company_c > 0){
        
        demand_Company_c <- demand_Company_c + 1
        supply_Company_c <- supply_Company_c - 1
        
      }
      
    }
    
    else if(j <= tech_d & j > tech_c){
      if(supply_Company_d > 0){
        
        demand_Company_d <- demand_Company_d + 1
        supply_Company_d <- supply_Company_d - 1
        
      }
      
    }
    
  }
  
  technology_vector_a[i]<-tech_a
  technology_vector_b[i]<-tech_b - tech_a
  technology_vector_c[i]<-tech_c - tech_b
  technology_vector_d[i]<-tech_d - tech_c
  
  tech_a <- demand_Company_a/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
  tech_b <- demand_Company_b/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
  tech_c <- demand_Company_c/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
  tech_d <- demand_Company_d/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
  
  if(tech_a >= reg_factor | tech_b >= reg_factor | tech_c >= reg_factor | tech_d >= reg_factor){
    tempdf <- as.data.frame(rbind(tech_a,tech_b,tech_c,tech_d))
    tempdf$adjv1 <- ifelse(tempdf$V1 >= reg_factor, 0, tempdf$V1)
    tempdf$adjv2 <- ifelse(tempdf$V1 >= reg_factor, 1, 0)
    tempdf$adjv3 <- ifelse(sum(tempdf$adjv2) == 1, 1 - reg_factor,
                           ifelse(sum(tempdf$adjv2) == 2, 1 -(2*reg_factor), 0))
    tempdf$adjv4 <- ifelse(tempdf$adjv2 == 1, reg_factor, tempdf$V1 )
    tempdf$adjv5 <- ifelse(1-sum(tempdf$adjv4)>0, 1-sum(tempdf$adjv4),0)
    tempdf$adjv6 <- nrow(tempdf[(tempdf$adjv4==0),])
    tempdf$final <- ifelse(tempdf$adjv4 == 0, tempdf$adjv5/tempdf$adjv6, tempdf$adjv4)
    
    tech_a <- tempdf$final[1]
    tech_b <- tempdf$final[2] + tech_a
    tech_c <- tempdf$final[3] + tech_b
    tech_d <- tempdf$final[4] + tech_c
  } else {
    
    tech_a <- tech_a
    tech_b <- tech_a + tech_b
    tech_c <- tech_b + tech_c
    tech_d <- 1}
  
  sales_Company_a[i]<-demand_Company_a
  sales_Company_b[i]<-demand_Company_b
  sales_Company_c[i]<-demand_Company_c
  sales_Company_d[i]<-demand_Company_d
  
  i<-i+1
  
}

df_a <- as.data.frame(cbind(sales_Company_a,technology_vector_a))
names(df_a)[1]<-"Sales"
names(df_a)[2]<-"Tech"

df_b <- as.data.frame(cbind(sales_Company_b,technology_vector_b))
names(df_b)[1]<-"Sales"
names(df_b)[2]<-"Tech"

df_c <- as.data.frame(cbind(sales_Company_c,technology_vector_c))
names(df_c)[1]<-"Sales"
names(df_c)[2]<-"Tech"

df_d <- as.data.frame(cbind(sales_Company_d,technology_vector_d))
names(df_d)[1]<-"Sales"
names(df_d)[2]<-"Tech"

df_a$Company <- "Company A"
df_b$Company <- "Company B"
df_c$Company <- "Company C"
df_d$Company <- "Company D"

df<-rbind(df_a,df_b,df_c,df_d)
df$Time<-c(1:1000,1:1000,1:1000,1:1000)

library(ggplot2)

SalesPlot1<-ggplot(df, aes(x=Tech, y=Sales, group=Company)) +
  geom_point(aes(color=Company))

SalesPlot1

SalesPlot_4C_GovInt<-ggplot(df, aes(x=Time, y=Sales, group=Company)) +
  geom_line(aes(color=Company))+
  geom_point(aes(color=Company))+
  xlim(0,1000)+ylim(0,1000)

SalesPlot_4C_GovInt

model<-lm(sales_Company_b~technology_vector_b)

plot(technology_vector_b,sales_Company_b)

techdf <- cbind(technology_vector_a,technology_vector_b,technology_vector_c,technology_vector_d)

salesdf <- cbind(sales_Company_a,sales_Company_b,sales_Company_c,sales_Company_d)

###### Monopoly | 4 companies | NO Government Intervention | All want to buy | Wage | Optimizing Price ###### 

sum_mean <- 0
####for people's income use weibull dist instead of 
#income <- function(x){rnorm(1, 10, sd=1)}
income <- function(x){(rweibull(x, shape=1.5, scale = 2)*10)}

#price_a <- c(8.09, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9)
#price_b <- c(8.10, 8.41, 8.51, 8.61, 8.71, 8.81, 8.91)
#price_c <- c(8.11, 8.42, 8.52, 8.62, 8.72, 8.82, 8.92)
#price_d <- c(8.12, 8.43, 8.53, 8.63, 8.73, 8.83, 8.93)
df_sum_mean<-data.frame(Sales=numeric(),
                        Price=numeric(), 
                        Company=character(),
                        Time=numeric(),
                        FactorReg=numeric(), 
                        stringsAsFactors=FALSE) 



price_a <- rep(5,100)
price_b <- rep(7,100)
price_c <- rep(10,100)
price_d <- rep(13,100)

f <- 1

reg_factor <- c(.3,.4,.5,.6,.7,.8,.9,1.1)

while(f<length(reg_factor)+1){
  
  k<-1
  
  totsales_a<-0
  totsales_b<-0
  totsales_c<-0
  totsales_d<-0
  
  while(k<length(price_a)+1){
    #round(rnorm(1, .5, sd=.1),2)
    # money <- rnorm(1, 10, sd=1) > 9.5
    
    sales_Company_a<-0
    sales_Company_b<-0
    sales_Company_c<-0
    sales_Company_d<-0
    
    population <- 1000
    
    technology_vector_a<-0
    technology_vector_b<-0
    technology_vector_c<-0
    technology_vector_d<-0
    
    tech_a <- .25
    tech_b <- .50
    tech_c <- .75
    tech_d <- 1
    
    i<-1
    
    while(i < 1001){
      
      demand_Company_a <- 0
      supply_Company_a <-1000
      
      demand_Company_b <- 0
      supply_Company_b <-1000
      
      demand_Company_c <- 0
      supply_Company_c <-1000
      
      demand_Company_d <- 0
      supply_Company_d <-1000
      
      agents <- runif(population, min = 0, max = 1)
      #decides_to_buy <- ifelse(runif(population, min = 0, max = 1) >= .5, 1, NA)
      #agents_buy <- agents * decides_to_buy
      
      for(j in agents){
        
        if(is.na(j)){
          
          
        } 
        
        else if((j <= tech_a) & (income(1) > price_a[k])){
          if(supply_Company_a > 0){
            
            
            demand_Company_a <- demand_Company_a + 1
            supply_Company_a <- supply_Company_a - 1
            
          }
          
        } 
        
        else if((j <= tech_b) & (j > tech_a) & (income(1) > price_b[k])){
          if(supply_Company_b > 0){
            
            demand_Company_b <- demand_Company_b + 1
            supply_Company_b <- supply_Company_b - 1
            
          }
          
        }
        
        else if((j <= tech_c) & (j > tech_b) & (income(1) > price_c[k])){
          if(supply_Company_c > 0){
            
            demand_Company_c <- demand_Company_c + 1
            supply_Company_c <- supply_Company_c - 1
            
          }
          
        }
        
        else if((j <= tech_d) & (j > tech_c) & (income(1) > price_d[k])){
          if(supply_Company_d > 0){
            
            demand_Company_d <- demand_Company_d + 1
            supply_Company_d <- supply_Company_d - 1
            
          }
          
        }  
        
      }
      
      technology_vector_a[i]<-tech_a
      technology_vector_b[i]<-tech_b - tech_a
      technology_vector_c[i]<-tech_c - tech_b
      technology_vector_d[i]<-tech_d - tech_c
      
      tech_a <- demand_Company_a/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
      tech_b <- demand_Company_b/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
      tech_c <- demand_Company_c/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
      tech_d <- demand_Company_d/(demand_Company_a + demand_Company_b + demand_Company_c + demand_Company_d)
      
      if(tech_a >= reg_factor[f] | tech_b >= reg_factor[f] | tech_c >= reg_factor[f] | tech_d >= reg_factor[f]){
        tempdf <- as.data.frame(rbind(tech_a,tech_b,tech_c,tech_d))
        tempdf$adjv1 <- ifelse(tempdf$V1 >= reg_factor[f], 0, tempdf$V1)
        tempdf$adjv2 <- ifelse(tempdf$V1 >= reg_factor[f], 1, 0)
        tempdf$adjv3 <- ifelse(sum(tempdf$adjv2) == 1, 1 - reg_factor[f],
                               ifelse(sum(tempdf$adjv2) == 2, 1 -(2*reg_factor[f]), 0))
        tempdf$adjv4 <- ifelse(tempdf$adjv2 == 1, reg_factor[f], tempdf$V1 )
        tempdf$adjv5 <- ifelse(1-sum(tempdf$adjv4)>0, 1-sum(tempdf$adjv4),0)
        tempdf$adjv6 <- nrow(tempdf[(tempdf$adjv4==0),])
        # 4 because we have four companies
        tempdf$adjv7 <- 4-nrow(tempdf[(tempdf$V1>reg_factor[f]),])
        tempdf$final <- ifelse(tempdf$adjv4 == 0, tempdf$adjv5/tempdf$adjv6,
                               ifelse(tempdf$adjv6==0, (tempdf$adjv5/tempdf$adjv7)+tempdf$adjv4,tempdf$adjv4))
        
        tech_a <- tempdf$final[1]
        tech_b <- tempdf$final[2] + tech_a
        tech_c <- tempdf$final[3] + tech_b
        tech_d <- tempdf$final[4] + tech_c
      } else {
        
        tech_a <- tech_a
        tech_b <- tech_a + tech_b
        tech_c <- tech_b + tech_c
        tech_d <- 1}
      
      sales_Company_a[i]<-demand_Company_a
      sales_Company_b[i]<-demand_Company_b
      sales_Company_c[i]<-demand_Company_c
      sales_Company_d[i]<-demand_Company_d
      
      
      i<-i+1
      
    }
    
    totsales_a[k]<-sum(sales_Company_a)*price_a[k]
    totsales_b[k]<-sum(sales_Company_b)*price_b[k]
    totsales_c[k]<-sum(sales_Company_c)*price_c[k]
    totsales_d[k]<-sum(sales_Company_d)*price_d[k]
    
    
    df_a <- as.data.frame(cbind(totsales_a,price_a))
    names(df_a)[1]<-"Sales"
    names(df_a)[2]<-"Price"
    
    df_b <- as.data.frame(cbind(totsales_b,price_b))
    names(df_b)[1]<-"Sales"
    names(df_b)[2]<-"Price"
    
    df_c <- as.data.frame(cbind(totsales_c,price_c))
    names(df_c)[1]<-"Sales"
    names(df_c)[2]<-"Price"
    
    df_d <- as.data.frame(cbind(totsales_d,price_d))
    names(df_d)[1]<-"Sales"
    names(df_d)[2]<-"Price"
    
    df_a$Company <- "Company A"
    df_b$Company <- "Company B"
    df_c$Company <- "Company C"
    df_d$Company <- "Company D"
    
    df<-rbind(df_a,df_b,df_c,df_d)
    df$Time<-c(1:length(price_a),1:length(price_b),1:length(price_c),1:length(price_d))
    
    
    k<-k+1
    
    
  }
  
  df$FactorReg <- reg_factor[f]
  df_sum_mean <- rbind(df,df_sum_mean)
  
  sum_mean[f]<-sum(mean(totsales_a)+mean(totsales_b)+mean(totsales_c)+mean(totsales_d))
  
  f<-f+1
  
}

df_sum_mean$Quantity <- df_sum_mean$Sales/df_sum_mean$Price

library(ggplot2)

SalesPlot_4C_GovIntFactor_Income<-ggplot(df_sum_mean, aes(x=Time, y=Sales/1000000, colour=Company)) +
  #geom_line()+
  geom_point()+ xlim(0,length(price_a))+ylim(0,5)+ 
  #geom_text(aes(label = round(Price, 2)),
  #        vjust = "inward", hjust = "inward",
  #        show.legend = FALSE)+
  facet_wrap(vars(FactorReg))+
  scale_color_discrete(name="Company", labels=c("Company A","Company B","Company C","Company D"))+
  labs(x = "Time", y = "Sales (Millions)", 
       title = "Sales by Price Point") 

SalesPlot_4C_GovIntFactor_Income


QuantityPlot_4C_GovIntFactor_Income<-ggplot(df_sum_mean, aes(x=Time, y=Quantity/1000000, colour=Company)) +
  #geom_line()+
  geom_point()+ xlim(0,length(price_a))+ylim(0,1.1)+ 
  #geom_text(aes(label = round(Price, 2)),
  #        vjust = "inward", hjust = "inward",
  #        show.legend = FALSE)+
  facet_wrap(vars(FactorReg))+
  scale_color_discrete(name="Company", labels=c("Company A","Company B","Company C","Company D"))+
  labs(x = "Time", y = "Demand (Millions)", 
       title = "Quantity Sold by Price Point") 

QuantityPlot_4C_GovIntFactor_Income

df_sum_mean$Paste <- paste(df_sum_mean$Company,df_sum_mean$Price,sep="__")


library(dplyr)

group_df <- df_sum_mean %>%
  group_by(Company, FactorReg) %>% 
  summarise(quantity = mean(Quantity), sales = mean(Sales))


Agg_plot_4C_GovIntFactor_Income<-ggplot(group_df, aes(x=FactorReg, y=sales/1000000, colour=Company)) +
  geom_line()+
  geom_point()+ xlim(.25,1.25)+ylim(0,6)+ 
  #geom_text(aes(label = round(Price, 2)),
  #        vjust = "inward", hjust = "inward",
  #        show.legend = FALSE)+
  #facet_wrap(vars(FactorReg))+
  scale_color_discrete(name="Company", labels=c("Company A","Company B","Company C","Company D"))+
  labs(x = "Regulation Factor", y = "Sales (Millions)", 
       title = "Sales by Company") 

Agg_plot_4C_GovIntFactor_Income

hist(rweibull(1000, shape=1.5, scale = 2)*10)

