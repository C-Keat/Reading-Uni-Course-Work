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
#Question 2 #Singular value Decompostion
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

####################Question 7#################################
#Photos of famous actors 

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

#forming a final plot so that it can be plotted againced the correct number of photos
finalPlot <- matrix(c(seq(2,15),result), nrow = 14)

#plotting the final results on graph
plot(finalPlot,main = "100,000 tests", xlab = "Num of Photos",ylab = "Estimated probability")

#Commenting done inside of submission document. 
#Discussing the reason probability stabilizes at 5+ photos


################################## Question 8 ##################################
#EigenFaces 
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
#Binomial Distribution

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


####################Question 10

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
