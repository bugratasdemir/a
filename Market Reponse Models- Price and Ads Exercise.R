#################################################################
## Marketing analytics                                         ##
## Lecture 2 Exercise in Market Response Models				         ##
## Price Ads Data Exercise                                     ##
##                                                             ## 
#################################################################

# free memory if need be
#rm(list = ls())
#gc()


# Get the working directory. If needed, you can set the working directory to another folder.
getwd()
#setwd("C:/")


# Read the Data files from a directory  
priceAdsData<-read.csv("Priceandads.csv",header=T)


#Show attributes  
attributes(priceAdsData)

# draw Yearly sales
plot(priceAdsData$Price,priceAdsData$Sales, ylab="Sales", xlab="Price")
plot(priceAdsData) 


#Correlation between attributes
cor(priceAdsData$Price,priceAdsData$Sales)
cor(priceAdsData$Ad,priceAdsData$Sales) 

#Correlation for all attributes
cor(priceAdsData[,])


# Fit the Data to the model
model <- lm(Sales ~ Price + Ad, data=priceAdsData)
summary(model)

#Model attributes and coefficients  
attributes(model)
model$coefficients



#Find the sales for Price 10 and ad 4   
SalesP10Ad4 <- model$coefficients[[1]]+model$coefficients[[2]]*10+model$coefficients[[3]]*4

#SalesP10Ad4 = 21075.51
SalesP10Ad4


#A simpler Model without Ad
model <- lm(Sales ~ Price, data=priceAdsData)
summary(model)



# differences between observed values and fitted values
residuals(model) 


# Show the plot of the model
#layout(matrix(1)) # one graph per page 
plot(model)


# Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(model)



# Compare Different Models
model1 <- lm(Sales ~ Price + Ad, data=priceAdsData)
summary(model1)

model2 <- lm(Sales ~ Price , data=priceAdsData)
summary(model2)
