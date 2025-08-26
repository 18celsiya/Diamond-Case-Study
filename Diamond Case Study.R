#################################################
#######Importing the diamond data set############
diamonds = read.csv("diamonds.csv", row.names = 1)

#################################################
########Data wrangling###########################
#################################################

##########Removing duplicate records############
diamonds = diamonds[!duplicated(diamonds),]

################################################
############Data exploration####################
################################################
str(diamonds)
summary(diamonds$carat)

##############################################
#########Data cleaning########################
##############################################

###############check the missing value########
colSums(is.na(diamonds))

############checking the range#############

diamonds$carat = ifelse(diamonds$carat< 0.2,mean(diamonds$carat),diamonds$carat)
diamonds$carat = ifelse(diamonds$carat > 5.01,mean(diamonds$carat),diamonds$carat)
diamonds$x = ifelse(diamonds$x < 0,mean(diamonds$x),diamonds$x)
diamonds$x = ifelse(diamonds$x >10.74,mean(diamonds$x),diamonds$x)
diamonds$y = ifelse(diamonds$y < 0,mean(diamonds$y),diamonds$y)
diamonds$y = ifelse(diamonds$y >58.9,mean(diamonds$y),diamonds$y)
diamonds$z = ifelse(diamonds$z < 0,mean(diamonds$z),diamonds$z)
diamonds$z = ifelse(diamonds$z >58.9,mean(diamonds$z),diamonds$z)


#################################################################
##############Data visualization using ggplot####################
#################################################################

library(ggplot2)

########scatter plot#############
ggplot(data = diamonds,aes(x = price)) + geom_histogram(col = "red")
ggplot(data = diamonds,aes(x = carat,y = price)) + geom_point(col = "black",shape = 21,fill = "white",size = 2.5) + labs(title = "Carat vs Price")
ggplot(data = diamonds,aes(x = x,y = price)) + geom_point(col = "green",shape = 21,fill = "black",size = 2.5) + labs(title = "x(length) vs Price")
ggplot(data = diamonds,aes(x = y,y = price)) + geom_point(col = "maroon",shape = 21,size = 2.1) + labs(title = "y(Width of diamond) vs Price")
ggplot(data = diamonds,aes(x = z,y = price)) + geom_point(col = "red",shape = 21,fill = "green",size = 2.1) + labs(title = "z(Depth of diamond) vs Price")

#######histogram##########

ggplot(data = diamonds,aes(x = price)) + geom_histogram(aes(col = cut),fill = "white",bins = 5) + labs(title = "Distribution of price with cutquality of diamond")
ggplot(data = diamonds,aes(x = price)) + geom_histogram(aes(fill = clarity),bins = 20) + labs(title = "Distribution of price with clarity of diamond")
ggplot(data = diamonds,aes(x = price)) + geom_histogram(aes(fill = color),bins = 10) + labs(title = "Distribution of price with color of diamond")

######scatter plot for cut,clarity,color#######
ggplot(data = diamonds,aes(x = carat,y = price)) + geom_point(aes(col = cut)) 
ggplot(data = diamonds,aes(x = carat,y = price)) + geom_point(aes(col = clarity)) 
ggplot(data = diamonds,aes(x = carat,y = price)) + geom_point(aes(col = color))

#############line plot for depth and table#####
ggplot(data = diamonds,aes(x = depth,y = price)) + geom_line(col = "pink") + labs(title = "Depth vs Price")
ggplot(data = diamonds,aes(x = table,y = price)) + geom_line(col = "orange") + labs(title = "Table vs Price")

#######################################################################