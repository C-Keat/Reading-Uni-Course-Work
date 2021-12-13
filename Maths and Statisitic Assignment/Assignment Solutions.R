####################Question 1############################
#Question 1
#Defining the Matrix
A<-matrix(c(3,-1,0,-1,0,-1,3,-1,3,-1,0,-1,3,1,0,-1,3,1,5,-1,0,-1,0,-1,1), nrow=5)

#Function that can be passed Matx in order to perform Cholesky Functions
choleskyFunction <- function(Matx){
  #Finds the upper triangular matrix
  L<-chol(Matx)
  #Transposes the matrix to show the lower triangular matrix
  Lt<-t(L)
  #Displays the lower hand matrix
  Lt
}
#Runs the function
choleskyFunction(A)
####################Question 2############################
#Question 2 
#Singular value Decompostion
#the creation of matrix
Q2Matrix <- matrix(c(1,1,1,1,2,0,0,0),nrow = 4)
#Displaying original matrix
Q2Matrix
#using the svd function to show the singular values
sv <-svd(Q2Matrix)
#Displaying Singular values
sv
####################Question 3############################
#Question 3
####################Question 4############################
#Question 4
####################Question 5#############################
#Question 5
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


###################Finding the minimums###########################
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

####################Finding the Maximum points#####################
maximum1 <- optim(c(0,0),control=list(fnscale=-1),Question5FV)$par
maximum1

###################finding the saddle points######################
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
#of the two parial derivatives
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

####################Question 7########################

#number of pictures inside of simulation
npictures <- seq(2,15)
totalNumberMatched <- 0

#function of pairing actors
pairActor <- function(secNumber){
  numberOfActors <- seq(1:secNumber)
  numberOfBabys <- sample(1:secNumber)
  numOfMatches <- 0
  
  #for loop comparing
  for(i in 1:secNumber){
    if(numberOfActors[i] == numberOfBabys[i])
    {
      numOfMatches = numOfMatches + 1
    }
  }
  #checking if matches have taken place
  if(numOfMatches != 0){
    totalNumberMatched = totalNumberMatched +1
    numOfMatches = 0
  }
  
}

pairActor(2)

#if number of trials is greater than 1
  # add to totalNumberofMatched 
  # number of matched zerod 

#figure out if a match took place

#plot the final result on graph












