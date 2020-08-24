rm(list=ls())
library(lmtest)
library(ggplot2)
library(gridExtra)
library(GGally)
library(neuralnet)
options(scipen = 9999)
set.seed(2021)

#once the dataset has been imported manually as element "data"
exch <- as.matrix(data[, -c(1:2)])
log_exch <- as.data.frame(diff(log(exch), lag=1))
#daily % change in exchange rates
log_exch <- na.omit(log_exch)
#we remove NA values due to our sufficiently large sample size
rm(exch)

cad_plot <- ggplot(data, aes(x = as.Date(date), y = cadusd)) +
	geom_line(color = "darkblue")

cad_hist <- ggplot(log_exch, aes(x=cadusd)) +
	geom_histogram() +
	labs(x = "log cadusd")

grid.arrange(cad_plot, cad_hist, ncol=2)

#side-by-side plot of overall and % change of CAD in terms of the USD

dim(log_exch)
n = dim(log_exch)[1]
#n = nrow(log_exch)
K = dim(log_exch) [2]
#K = ncol(log_exch)

#create an empty matrix and populate it with relevant statistics
exch_stats <- matrix(nrow = K, ncol =4)
rownames(exch_stats) <- colnames(log_exch)
colnames(exch_stats) <- c("mean", "std", "med", "var")

for (i in 1:K) {
	exch_stats[i, 1] <- mean(log_exch[, i])}
for (i in 1:K) {
	exch_stats[i, 2] <- sd(log_exch[, i])}
for (i in 1:K) {
	exch_stats[i, 3] <- median(log_exch[, i])}
exch_stats[,4] <- (as.matrix(exch_stats[,2])^2)

ggplot(as.data.frame(exch_stats), aes(x= var, y = med)) +
	geom_point() +
	geom_text(label=rownames(exch_stats))

#we can use this plot to look for interesting outliers

corr_matrix <- matrix(NA, nrow=K, ncol=K)
corr_matrix <- as.data.frame(round(cor(log_exch(, 2))
cad_corr <- as.data.frame(t(corr_matrix["cadusd", ]))
								   
cad_corr_hist <- ggplot(corr_matrix, aes(x=cadusd)) +
	geom_histogram() +
	labs(x = "cadusd corr")

install.packages("ggcorrplot")
library(ggcorrplot)
								
ggcorr(log_exch)
ggcorr(data[,-(1:2)], label=TRUE)

ggcorr(log_exch, geom = "blank", label = TRUE, hjust = 0.75) +
  geom_point(size = 10, aes(color = coefficent >0, alpha = abs(coefficent) >= 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

#positively correlated means the currencies move in the same direction, negative: opposite
					
index1 <- sample(1:n, round(0.8*n))
trains <- log_exch[index1,]
test <- log_exch[-index1,]

#n = dim(log_exch)[1]
#K = dim(log_exch)[2]
					
print( c( ncol(train), ncol(test) ) )
print( c( nrow(train), nrow(test) ) )
print( c( sum( nrow(train), nrow(test)), n) )
			
#we split the data following an 80:20 ratio, the first "trains" our models before "testing" the models on the later dataset
y_train <- train$cadusd
y_test <- test$cadusd
x_train <- train[,-6]								
x_test <- test[,-6]								
		
lm_fit <- lm(cadusd~.,data=train)
#y_train ~ X_train 								   
summary(lm_fit)								   
pr_lm <- predict(lm_fit,test)								   
mse_lm <- sum((pr_lm-test$cadusd)^2)/nrow(test)

#scale datasets
maxs <- apply(log_exch, 2, max)
mins <- apply(log_exch, 2, min)
scaled <- as.data.frame(scale(log_exch, center = mins, scale = maxs - mins))
trains_ <- scaled[index1,]
test_ <- scaled[-index1,]								   
#the most basic scaling technique required before feeding our data into the ANN model
								   
ss <- n
rm(n)
								
n <- names(train_)
								 
f <- as.formula(paste("cadusd ~", paste(n[!n %in% "cadusd"], collapse = " + ")))

nn <- neuralnet(f, data=train_, hidden = 8, linear.output = F)
					
plot(nn)
								 
pr.nn <- compute(nn, test_[, -6])
pr.nn_ <- pr.nn$net.result*(max(log_exch$cadusd)-min(log_exch$cadusd))+min(log_exch$cadusd)
test.r <- (test_$cadusd)*(max(log_exch$cadusd)-min(log_exch$cadusd))+min(log_exch$cadusd)
#effectively undo the scaling technique we performed earlier, enables us to compare with mse_lm (the error from linear reg.)
mse_nn <- sum((test.r - pr.nn_)^2)/nrow(test_)	
								  
print(paste(mse_lm, mse_nn))
								   
#we then experimented with different paramteres in our ANN models
								   
nn <- neuralnet(f, data=train_, hidden = c(6,3), linear.output = F)
								   
plot(nn)
								  
pr.nn <- compute(nn, test_[,-6])
pr.nn_ <- pr.nn$net.result*(max(log_exch$cadusd)-min(log_exch$cadusd))+min(log_exch$cadusd)
test.r <- (test_$cadusd)*(max(log_exch$cadusd)-min(log_exch$cadusd))+min(log_exch$cadusd)
mse_nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
								   
print(paste(mse_lm,mse_nn))
								   
nn <- neuralnet(f, data=train_, hidden = c(6,3), linear.output = T)
								   
plot(nn)								   
								   
pr.nn <- compute(nn,test_[,-6])
pr.nn_ <- pr.nn$net.result*(max(log_exch$cadusd)-min(log_exch$cadusd))+min(log_exch$cadusd)								   
test.r <- (test_$cadusd)*(max(log_exch$cadusd)-min(log_exch$cadusd))+min(log_exch$cadusd)								   
mse_nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(mse_lm,mse_nn))
								   
								   
								   
