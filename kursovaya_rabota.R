library(readr)
library(ggplot2)
library(glmnet)
library(dplyr)
library(tidyverse)
set.seed(2000)

#Импортирование данных
wd <- getwd()
filename <- "edmonton_housing_data_Feb15_2022.csv"
fullpath <- file.path(wd, filename)
prevdat <- as.data.frame(read_csv(fullpath))
head(prevdat, 10)
dat <- prevdat[, -which(names(prevdat) == "House Description")]
#Предобработка данных
class(dat$Price)
dat$Price = gsub(",", "", dat$Price)
dat$Price = gsub(",", "", dat$Price)
dat$Price = gsub("\\$", "", dat$Price)
dat$Price <- as.numeric(dat$Price)
colSums(is.na(dat)) 
dat <- dat %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))
dat <- dat %>% mutate(across(where(is.character), ~replace_na(., names(which.max(table(.))))))
colSums(is.na(dat)) 

graph <- ggplot(data = dat, aes(x = dat$'Square Footage', y = dat$'Price'))
graph + geom_point() + xlab("Площадь квартиры") + ylab("Стоимость квартиры")

detect_outlier <- function(x) {
  lower_bound <- quantile(x, 0.01)
  upper_bound <- quantile(x, 0.99)
  x < lower_bound | x > upper_bound
}
columns <- colnames(select_if(dat, is.numeric))
columns
for(column in columns){
  dat <- dat[!detect_outlier(dat[[column]]), ]
}

#Визуализация распределения стоимости квартир относительно размера жилплощади
graph <- ggplot(data = dat, aes(x = dat$'Square Footage', y = dat$'Price'))
graph + geom_point() + xlab("Площадь квартиры") + ylab("Стоимость квартиры")


#Подготовка данных к моделированию
num_of_rows = length(dat$`Price`)
num_of_columns = length(dat)
x <- dat %>% select(-'Price') %>% as.matrix()
y <- dat %>% select(Price) %>% scale() %>% as.matrix()

train_rows = sample(1:num_of_rows, 0.8*num_of_rows)

x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]
y.train <- y[train_rows, ]
y.test <- y[-train_rows, ]

#Моделирование
elasNet<-glmnet(x.train, y.train, alpha=0.5, family = "gaussian")
plot(elasNet, xvar =  "lambda", label = TRUE, lwd = 2)


x <- dat %>% select(c("Bedrooms","Full Baths","Bathrooms","Square Footage",
                      "Parking Spaces", "Bedrooms Above Grade")) %>% as.matrix()
y <- dat %>% select(Price) %>% mutate_if(is.character,as.integer) %>%
  scale() %>% as.matrix()

models <- list()
for (i in 1:99) {
  name <- paste0("alpha", i/100)
  
  
  models[[name]] <-
    cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/100, 
              family="gaussian")
}
results <- data.frame()
for (i in 1:99) {
  name <- paste0("alpha", i/100)
  

  predicted <- predict(models[[name]], 
                       s=models[[name]]$lambda.min, newx=x.test)
  
  mse <- mean((y.test - predicted)^2)
  
  temp <- data.frame(alpha=i/100, mse=mse, name=name)
  results <- rbind(results, temp)
}

plot(results$alpha, results$mse, label = TRUE, xlab = "альфа", ylab = "MSE")

results[which.min(results$mse), ]$name

alpha0.98.fit<- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0.98, 
                         family="gaussian")
plot(alpha0.98.fit, xvar =  "lambda", label = TRUE, lwd = 2)
model <- glmnet(x.train, y.train, type.measure="mse", alpha=0.98, 
                family="gaussian", lambda = exp(-6))
model.predicted <- predict(model, 
                              s=model$lambda.min, newx=x.test)

mean((y.test - model.predicted)^2)
deter_coeff <- sum((mean(y.test)-model.predicted)^2) / sum((mean(y.test)-y.test)^2)
deter_coeff

mu_x <- mean(as.matrix(dat$`Year Built`)[-train_rows, ])
mu_y <- mean(model.predicted)
s_x <- sd(as.matrix(dat$`Year Built`)[-train_rows, ])
s_y <- sd(model.predicted)
r <- cor(as.matrix(dat$`Year Built`)[-train_rows, ], model.predicted)
m <- r * s_y / s_x
b <- mu_y - m*mu_x
print(c(m, b))
graph1 <- ggplot(data = as.data.frame(model.predicted), 
                 aes(x = as.matrix(dat$`Year Built`)[-train_rows, ], 
                     y = model.predicted)) + geom_point() + 
  theme(axis.text.x = element_text(angle=45, vjust = 1)) +
  xlab("Год постройки") +
  ylab("Предсказания модели") +
  geom_abline(intercept = b, slope = m )
mu_x <- mean(as.matrix(dat$`Year Built`)[-train_rows, ])
mu_y <- mean(y.test)
s_x <- sd(as.matrix(dat$`Year Built`)[-train_rows, ])
s_y <- sd(y.test)
r <- cor(as.matrix(dat$`Year Built`)[-train_rows, ], y.test)
m <- r * s_y / s_x
b <- mu_y - m*mu_x
print(c(m, b))
graph2 <- ggplot(data = as.data.frame(y.test), 
                 aes(x = as.matrix(dat$`Year Built`)[-train_rows, ], y = y.test))  + 
  geom_point() +
  theme(axis.text.x = element_text(angle=45, vjust = 1)) +
  xlab("Год постройки") +
  ylab("Фактическая стоимость") +
  geom_abline(intercept = b, slope = m )

library(gridExtra)
grid.arrange(graph1, graph2, ncol = 2)