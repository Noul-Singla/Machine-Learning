#This Program contains the following: - 
#1. Simulated data for both x and y
#2. 1 variable linear regression process
#3. Implementation of batch and stochastic gradient descent

#set a seet to any random value for consistency
set.seed(123)

#uniform distribution variable with 100 values as input variable
ud100 <- runif(100,-2,2)
#y is an output variable derived with forumula y=2+3*(input variable)
y100 <- 2+(3*ud100)
#adding random noise to the output variable
y100 <- y100 + rnorm(1,y100,2)

#using the formula b1 = sum(y*(x-mean(x)))/sum((x-mean(x))^2) 
b1numerator <- sum(y100 * (ud100-mean(ud100)))
b1denom <- sum((ud100-mean(ud100))^2)
b1 <- b1numerator/b1denom

#using the formula b0 = mean(y)-b1*mean(x) since mean(y)= b0+b1*mean(x)
b0 <- mean(y100) - (b1*mean(ud100))


#print the output 
print(c(b0,b1))


#implementation of Batch Gradient Descent (BGD) using various learning rate to compare


#intializing a sequaence of learning rates 
bgdlearning_rate <- seq(0.002,0.009,0.00005)

#creating vector of value 0 of equal length of learning rates for b0 and b1. 
bgdb0 <- (bgdlearning_rate*0)
bgdb1 <- (bgdlearning_rate*0)

# creating another vector of value 0 which will store count of iterations per execution
bgditerations <- (bgdlearning_rate*0)


#Loop for implementation of BGD for various learning rates. 
for(j in 1:length(bgdlearning_rate)){
# Each iteration is trained for 1000 times on all the records to find the global minimum 
    for( i in 1:1000){
    #break condition
      if( (2*bgdlearning_rate[j]* sum(y100-(bgdb0[j]+bgdb1[j]*ud100))) < 0.0001 &
        (2*bgdlearning_rate[j]* sum((y100-(bgdb0[j]+bgdb1[j]*ud100))*ud100)) <  0.0001){
      break;
      }
      #update steps if still scope of improvement
    else{
      bgdb0[j] <- bgdb0[j] + (2*bgdlearning_rate[j]* sum(y100-(bgdb0[j]+bgdb1[j]*ud100)))
      bgdb1[j] <- bgdb1[j] + (2*bgdlearning_rate[j]* sum((y100-(bgdb0[j]+bgdb1[j]*ud100))*ud100))
    }
    
    }
#storing the iterations count  
  bgditerations[j] <- i*length(ud100)
}

#Combining the vectors in a dataframe for side by side comparison
bgd <- as.data.frame(cbind(bgdlearning_rate,bgdb0,bgdb1,bgditerations))

#view the result
bgd


#observations:-
#1. Low learning rate take a lot of iterations to find the minimum
#2. High learning rate takes less interation but is incorrect 
#a balance between the learning rate and computation is needed


#implementation of Stochastic Gradient Descent (SGD) using various learning rate to compare


#intializing a sequaence of learning rates
sgdlearning_rate <- seq(0.002,0.009,0.00005)

#creating vector of value 0 of equal length of learning rates for b0 and b1. 
sgdb0 <- (sgdlearning_rate*0)
sgdb1 <- (sgdlearning_rate*0)


# creating another vector of value 0 which will store count of iterations per execution
sgditerations <- (sgdlearning_rate*0)



#define a break variable
brk <- 0
#Loop for implementation of SGD for various learning rates. 
for(j in 1:length(sgdlearning_rate)){
  # Each iteration of learning rate is trained for 1000 loops
  for(k in 1 :1000){
    # each record is processed one by one to find the global minimum 
    for( i in 1:length(ud100)){
      if( (2*sgdlearning_rate[j]* (y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))) < 0.00001 & (2*sgdlearning_rate[j]* ((y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))*ud100[i])) <  0.00001)
      {
        brk <- 1
        break
      }
      #update steps if still scope of improvement
      else{
        sgdb0[j] <- sgdb0[j] + (2*sgdlearning_rate[j]* (y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i])))
        sgdb1[j] <- sgdb1[j] + (2*sgdlearning_rate[j]* ((y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))*ud100[i]))
      }
    } 
    #using the break variable to break the loop for K as well
    if(brk ==1){
      brk <- 0
      break} 
  }
  #no. of iterations is the number of loops for a learning rate*record processed in each loop
  sgditerations[j] <- ((k-1)*length(ud100)+i)
}

#Combining the vectors in a dataframe for side by side comparison
sgd <- as.data.frame(cbind(sgdlearning_rate,sgdb0,sgdb1,sgditerations))

#view the result
sgd

#Observations:-
#1. a lot of variations in nearby learning rates
#2. no. of executions are a lot lesser as compared to the gradient process
#3. Finding the best fit is not easy with lot of variance in the predictor values
