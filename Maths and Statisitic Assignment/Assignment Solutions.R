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
x<-seq(-4.5,4.5,length.out = 100)
y<-seq(-4.5,4.5,length.out = 100)
z<-outer(x,y,Question5F,a=1,b=100)

#plotting the graph 
persp(x,y,z,theta =-45,phi=20,expand=0.5,col="yellow",shade =1,ticktype ="detailed",xlab ="x",ylab ="y",zlab="z")

#Writing the function so x,y is a vector
Question5FV<-function(vec) (vec[1]^2+vec[2]-11)^2+(vec[1]+vec[2]^2-7)^2

#defining function to find maximum point
nll.binom <- function(data,par){
  return(-log(dbinom(data,size = 20, prob = par)))
}

#Finding the minimums:
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
minimum4 <- optim(c(3.5,3.5),Question5FV)$par
minimum4

#Finding the Maximum points
maximum1 <- optim(c(0,0),control=list(fnscale=-1),Question5FV)$par
maximum1

#finding the saddle points
fx <- function(x,y,h = 0.001) (f(x+h,y)-f(x,y))/h
#this discribes the incremental ration, we shock x by adding to it an abitrarily small value, h(=0.001). 
#In this way, we compute the rate of change (of the function)
#This is the same as y
fy <- function(x,y,h=0.001) (f(x,y+h)-f(x,y))/h

#now re-use the other function to compute the z values corresponding to the partial derivatives.
#Thus zfx and zfy are matrices, 400 rows and 400 columns, with values of fx and fy respectivly.
zfx <- outer(x,y,Question5FV)
zfv <- outer(x,v,Question5FV)


#creating a list of critical points
critical_point_list <- list(minimum1,minimum2,minimum3,minimum4,maximum1)
#List of critical points
critical_point_list
##########################################################
