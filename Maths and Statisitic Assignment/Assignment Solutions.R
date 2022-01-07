####################Question 1############################

#Question 1 Cholesky decomposition
#Defining the Matrix
A<-matrix(c(3,-1,0,-1,0,-1,3,-1,3,-1,0,-1,3,1,0,-1,3,1,5,-1,0,-1,0,-1,1), nrow=5)

#defining function
choleskyDecompTrans <- function(m) {
  cRows <- nrow(m)
  cCols <- ncol(m)
  
  L = diag(0, cRows)
  
  for(i in 1:cRows) {
    
    for(k in 1:i) {
      
      sum <- 0
      
      for(j in 1:k)
        sum <- sum + L[j, i] * L[j, k]
      #if not diagonal 
      if(i == k)
        L[k, i] <- sqrt(m[i, i] - sum)
      #if diagonal
      if(k == k)
        L[k, i] <- (m[k, i] - sum) / L[k, k]
    }
  }
  return(t(L))
}

#Runs the function
choleskyDecompTrans(A)

####################Question 2############################

#Question 2 - Singular value Decomposition

#the creation of matrix
Q2Matrix <- matrix(c(1,1,1,1,2,0,0,0),nrow = 4)

#Displaying original matrix
Q2Matrix

#using the svd function to show the singular values
sv <-svd(Q2Matrix)

#Displaying Singular values
sv

####################Question 3############################

#Question 3 Differential equations in R
library(deSolve)
#defining the parameters
Yini<-c(y=0,v=1,v2=-(5/2))

#function solving differential equation
derivs.Yv3 <- function(time,Yv,parms){
  with(as.list(c(Yv,parms)),{
    dY <- v
    dY2 <- v2
    dv <- (exp(-time)-v2+v+y)/3
    list(c(dY,dY2,dv))
    })
}

#How many observations 
times <- seq(from = 0, to = 5, by = 1)

#Solving and plotting
out.Yv3 <- ode(y=Yini,times=times,func = derivs.Yv3,parms = NULL)
plot(out.Yv3[,"time"],out.Yv3[,"y"],type="l",xlab="time",ylab="Y",col="green",lwd=2)

####################Question 4############################

#Question 4 Lotka-Volterra model
library(deSolve)
install.packages("phaseR")

#(a)

#defining the function where the equations are sorted
LotVmod <- function(Time,State,Parm){
  with(as.list(c(State,Parm)),{
    dx = a*x - g *x^2 - b*x*y
    dy = -c*y + d*x*y
    return(list(c(dx,dy)))
  })
}

#defining the parameters, state and time of the equations
Parm <- c(a=5,b=0.01,c=100,d=0.01,g=0.0001)
State <- c(x = 10000, y = 60)
Time <- seq(0,5,by = 1)

#logging the outputs 
out <- ode(func = LotVmod, y = State, parms = Parm, times = Time)

#(b)
#plotting the results on graph
matplot(out,type = "l")

#(c)
#finding the equilibrium points

#x = 10000
#y = 400

#Results have been determined though educated guesses
#From both the plot and result print out.
#Number of times observations took place had to be increased
#in order to get a definite value.

out

#Equilibrium values 
x <- 10000
y <- 400

#Defining the parameters
a=5
b=0.01
c=100
d=0.01
g=0.0001

dx = a*x - g *x^2 - b*x*y
dy = -c*y + d*x*y

#Printing out both the values, 
#When both values = 0 equilibrium of the system has taken place
dx # = 0
dy # = 0

####################Question 5#############################

#Question 5 Use of the Optim Function
#Function plot - Plot the formula below
Question5F<-function(x,y,a,b) (x^2+y-11)^2+(x+y^2-7)^2

#defining the x,y and z of the measurements
x<-seq(-4.5,4.5,length.out = 200)
y<-seq(-4.5,4.5,length.out = 200)
z<-outer(x,y,Question5F,a=1,b=100)

#plotting the graph 
persp(x,y,z,theta =-45,phi=20,expand=0.5,col="yellow",shade =1,ticktype ="detailed",xlab ="x",ylab ="y",zlab="z")

#Writing the function so x,y is a vector
Question5FV<-function(vec) (vec[1]^2+vec[2]-11)^2+(vec[1]+vec[2]^2-7)^2


##Finding the minimums##
#Minimum can be found based on value being 0 or so close to 0
#Based on plot there seems to be 4 dips representing each minimum is extracted here
#From plot minimum seems to be near x=-4,y=-4.
minimum1<- optim(c(-4,-4),Question5FV)$par
minimum1

#From plot minimum seems to be near x = 4, y= -3.5
minimum2 <- optim(c(4,-3.5),Question5FV)$par
minimum2

#From plot minimum seems to be near x = -3.5, y = 3.5
minimum3 <- optim(c(-3.5, 3.5),Question5FV)$par
minimum3

#From plot minimum seems to be near x = 3.5, y = 3.5
minimum4 <- optim(c(3.5,3.5),Question5FV,hessian = TRUE)$par
minimum4

##Finding the Maximum points##
maximum1 <- optim(c(0,0),control=list(fnscale=-1),Question5FV)$par
maximum1

##Finding the saddle points##
#Lets define the partial derivatives by plugging vec[1] and vec[2] instead of x and y
fx <- function(x,y,h=0.001){
  (Question5F(x+h,y) - Question5F(x,y))/h
}
fy <- function(x,y,h=0.001){
  (Question5F(x,y+h)-Question5F(x,y))/h
}


fxb <- function(vec){
  fx(vec[1],vec[2])
}
fyb <- function(vec){
  fy(vec[1],vec[2])
}

#call sumssq the function of the vector vec made of the sum of the squares
#of the two partial derivatives
sumssq <- function(vec){
  fxb(vec)^2+fyb(vec)^2
}

#Now we can find the coordinates of the saddle points through "optim".
#We do exactly the same thing for all three points, just changing the guess of their location. 
#according to the graph. 

saddle_point1 <- optim(c(-3.5,0),sumssq)$par
saddle_point1

saddle_point2 <- optim(c(0,-3.5),sumssq)$par
saddle_point2

saddle_point3 <- optim(c(0,2),sumssq)$par
saddle_point3

#creating a list of minimum points
minimum_list <- list(minimum1,minimum2,minimum3,minimum4)
#List of minimum points
minimum_list

#this represents the only maximum point in graph
maximum1

#list of saddle points
saddle_point_list <- list(saddle_point1,saddle_point2,saddle_point3)
#Print of saddle point list
saddle_point_list

####################Question 7#################################

#Question 7 - Photos of famous actors 
#variable to keep track of matched amount
totalNumberMatched <- 0

#function of pairing actors
pairActor <- function(secNumber, trials){
  #variable to track if match has taken place
  numOfMatches <- 0
  #looping through number of trials = 100,000
  for (t in 1:trials) {
    #variables storing the number of photos present in each sequance
    numberOfActors <- seq(1:secNumber)
    numberOfBabys <- sample(1:secNumber)
    
    #for loop comparing
    for(i in 1:secNumber){
      if(numberOfActors[i] == numberOfBabys[i])
      {
        #If match takes place, log
        numOfMatches = numOfMatches + 1
      }
    }
    #checking if matches have taken place
    if(numOfMatches != 0){
      #if any number of matches has taken place, track total number of matches
      totalNumberMatched = totalNumberMatched +1
      #Number of matches set to zero so tracking can take place
      numOfMatches = 0
    }
  }
 #print out the % of successful matches over the trials
  percentageSuccess <- totalNumberMatched/trials
  return(percentageSuccess)
}

#set the number of pictures present
nPictures <- seq(2,15)
#define the number of times the results will be collected
result <- seq(1,14)

#call the function 14 times storing the results each time,  
for(n in 1:14){
  result[n] <- pairActor(nPictures[n],100000)  
}

#forming a final plot so that it can be plotted against the correct number of photos
finalPlot <- matrix(c(seq(2,15),result), nrow = 14)

#plotting the final results on graph
plot(finalPlot,main = "100,000 tests", xlab = "Num of Photos",ylab = "Estimated probability")


#Commenting done inside of submission document. 
#Discussing the reason probability stabilizes at 5+ photos


####################Question 8 ##################################
library(bmp)

#Question 8 - EigenFaces 
#loading the images
face1.bmp <- read.bmp(file.choose())
face2.bmp <- read.bmp(file.choose())
face3.bmp <- read.bmp(file.choose())

#Working out the averages of the 3 faces and plot an image of it
#defining rotate function
rotate.m <- function(m) t(m)[,nrow(m):1]

#getting the average of the 3 face matrix
averOfFaces <- (face1.bmp+face2.bmp+face3.bmp)/3

#plotting image 1
image(rotate.m(face1.bmp),col = gray((0:255)/255),axes=F)
#plotting image 2
image(rotate.m(face2.bmp),col = gray((0:255)/255),axes=F)
#plotting image 3
image(rotate.m(face3.bmp),col = gray((0:255)/255),axes=F)

#Plotting the average of the 3 images
image(rotate.m(averOfFaces),col = gray((0:255)/255),axes=F)

#Plotting images of difference of each face from the average face on one figure.
#defining the difference between the original faces vs the average face
differnce1 <- face1.bmp - averOfFaces
differnce2 <- face2.bmp - averOfFaces
differnce3 <- face3.bmp - averOfFaces

par(mfrow = c(2,2))
#plotting image 1
image(rotate.m(differnce1),col = gray((0:255)/255),axes=F)
#plotting image 2
image(rotate.m(differnce2),col = gray((0:255)/255),axes=F)
#plotting image 3
image(rotate.m(differnce3),col = gray((0:255)/255),axes=F)

#Plotting the average of the 3 images
image(rotate.m(averOfFaces),col = gray((0:255)/255),axes=F)

#eigenfaces

#turn face matrix into there own vectors

face1Vector <-as.vector(face1.bmp)
face2Vector <-as.vector(face2.bmp)
face3Vector <-as.vector(face3.bmp)

#storing the vectors inside of a matrix
fullFaceMatrix <- MatrixofDiff <- cbind(as.vector(differnce1),as.vector(differnce2),as.vector(differnce3))

sv <- svd(fullFaceMatrix)

eigenFaces <- sv$u%*%diag(sv$d)%*%sv$v

par(mfrow = c(1,3))

#Printing the eigenfaces
image(matrix(eigenFaces[,1],nrow = 51,byrow = T),col = gray((0:255)/255),axes=F) 
image(matrix(eigenFaces[,2],nrow = 51,byrow = T),col = gray((0:255)/255),axes=F)
image(matrix(eigenFaces[,3],nrow = 51,byrow = T),col = gray((0:255)/255),axes=F)

####################Question 9#########################

#Question 9 - Binomial Distribution

#define function that generates data from a binomial distribution
generateMeanData <-function(m,n){
  
  dataSets <- array(0,dim = c(n,n,m))
  
  bdData <- apply(dataSets,3,function(x) rbinom(n,size = 10,prob = 0.01))
  
}
#gets the mean from the generate data
meanOf20 <-apply(generateMeanData(10000,20),2,function(x) c(mean(x)))
meanOf100 <-apply(generateMeanData(10000,100),2,function(x) c(mean(x)))

#plot the histogram
par(mfrow = c(1,2))


hist(meanOf20)
hist(meanOf100)


####################Question 10#########

#Question 10 - t-tests
#define the variables x and y 
x <- rnorm(20,100,4)
y <- 2+x+(rnorm(20,0,1))

#plotting the x and y points with there lines
plot(x,col="blue",ylim = c(min(x)-1,max(x)-1))
abline(h=mean(x),col="blue")
points(y,col="red")
abline(h=mean(y),col="red")

#storing the independent T test
unPariedTest <- t.test(x,y)
#storing the paired test
pairedTests <- t.test(x,y,paired = T)

unPariedTest
pairedTests

####################Question 11#########

#Question 11 - Ridge regression

install.packages("glmnet")
install.packages("ElemStatLearn")
library(glmnet)
library(ElemStatLearn)

#n the data set will have 1000 samples
n <- 1000
p <- 5000 #p = 5000 parameters to estimate
real_p <- 20 #Only 20 of those parameters will help us predict the outcome

#x = a matrix of randomly generated data
x <- matrix(rnorm(n*p),nrow = n,ncol=p)

#now we create a vector of values,called y, that we will try to predict with the data in x.
y <- apply(x[,1:real_p],1,sum)+rnorm(n)

#Defining function using cv.glmnet
ridgeEstimates <- function(x,y){
  
  #So we make a vector of indexes, called train_rows,
  #that contains the row numbers of the rows that will be in the training set. 
  train_rows <- sample(1:n, .66*n)
  
  x.train <- x[train_rows,] #Store the training data of x
  x.test <- x[-train_rows,] #Store the test data of x
  
  y.train <- y[train_rows] #Stores the test data of Y
  y.test <- y[-train_rows] #Stores the test data of Y
  
  #first we need to fit a model to the training data
  alpha0.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0, family = "gaussian")
  #the CV part means we want to use cross validation to obtain the values of lambda.
  
  alpha0.predicted <- predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx=x.test)
  
  #now calculate the mean squared error of the difference between the true values, stored in y.test.
  #and the predicted values, stored in alpha0.predicted.

  #stores the lambda mean
  lambda_mean_value <- mean((y.test - alpha0.predicted)^2)

  #stores all lambda values
  lambda_value <- alpha0.fit$lambda
  print(lambda_value)
  
  print(lambda_mean_value)
  
  plot(alpha0.fit)
}

#runs function and plots ridge estimates
ridgeEstimates(x,y)



####################Question 12###############

#Question 12 - Principle component analysis
install.packages("HSAUR3")
install.packages("ggbiplot")
install.packages("tidyverse")

library(HSAUR3)
library(ggbiplot)
library(tidyverse)

cheese <- read.table(file.choose(), header = T)
head(cheese)

sv <- svd(cheese[,2:45])
#plotting the single values of the data matrix
plot(rev(sv$d),main = "Singular values", xlab = "Index",ylab = "Singular value")
sv$d

#renaming the cheese column
renamedCheese <- rename(cheese,Water = Water_.g.,Ener = Energ_Kcal,Pro = Protein_.g.,Lip = Lipid_Tot_.g.,
                        Ash = Ash_.g., Car = Carbohydrt_.g.,Fib = Fiber_TD_.g.,Sug = Sugar_Tot_.g., Cal = Calcium_.mg.,
                        Iron = Iron_.mg.,Mag = Magnesium_.mg.,Phos = Phosphorus_.mg.,Pot = Potassium_.mg., Sod = Sodium_.mg.,
                        Zinc = Zinc_.mg.,Cop = Copper_.mg., Mang = Manganese_.mg.,Selen = Selenium_..g.,vit_c = Vit_C_.mg.,
                        Thia = Thiamin_.mg.,Ribof = Riboflavin_.mg., Nia = Niacin_.mg.,Panto = Panto_Acid_.mg.,Vit_B = Vit_B6_.mg.,
                        Fol = Folate_Tot_..g.,Food = Food_Folate_..g.,Fola = Folate_DFE_..g.,Chol = Choline_Tot_.mg.,Vit = Vit_B12_..g.,
                        Vit_A = Vit_A_IU,Vit_A_R = Vit_A_RAE,Reti = Retinol_..g., Alph = Alpha_Carot_..g.,BetaC = Beta_Carot_..g.,Beta=Beta_Crypt_..g.,
                        Lut = Lut_Zea_..g., VitE = Vit_E_.mg., VitD = Vit_D_.g, VitDI = Vit_D_IU,VitK = Vit_K_..g.,FAS = FA_Sat_.g.,
                        FA_M = FA_Mono_.g., FAP = FA_Poly_.g.,Chole = Cholestrl_.mg.)

#plotting biplot

#Performing PCA
cheese.pca <- prcomp(renamedCheese[,2:45],center = T, scale. = T)
summary(cheese.pca)
biplot(cheese.pca,main = "Biplot",ylim = c(-0.3,0.3),xlim = c(-0.4,0.4))

#storing the principle component %s 
pcValues <- summary(cheese.pca)$importance[2,]

variation90 <- 0.00 
numberOfPcs <- 0.00

#Add the values to find out how many Pcs contribute to 90%
for (i in 1:length(pcValues)) {
  
  variation90 <- pcValues[i] + variation90
  numberOfPcs <- numberOfPcs + 1
  if (variation90 > 0.90) 
    {
    break
  }

}
#number of PC values needed to contribute to 90% = 10
numberOfPcs

#Storing and printing the PCA scores for Chedder and Edam
chedderPC<-cheese.pca$x[6,1]
edamPC<-cheese.pca$x[15,1]
chedderPC
edamPC
