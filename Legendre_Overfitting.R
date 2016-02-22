library(polynom)
library(manipulate)

#q is degree of polynomial, n is number of examples, s is sigma squared

overfit.plot <- function(q,n,s) {
    #set.seed(127)
    x <- runif(n, min = -1, max = 1)
    
    epsi <- rnorm(n) #noise
    poly <- polynomial(rnorm(n = q+1)) #poly
    y <- predict(poly, x) + sqrt(s)*epsi #values of poly +noise
    
    df <- data.frame(x, y)
    
    model_2 <- lm(y ~ poly(x,2), data = df)
    model_10 <- lm(y ~ poly(x, 10), data = df)
    
    
    test <- runif(2000, min = -1, max = 1)
    testdf <- data.frame(test)
    colnames(testdf)[1]<-"x"
    newy_2 <- predict(model_2, newdata = testdf)
    newy_10 <- predict(model_10, newdata = testdf)
   
    plot(test, predict(poly,test), ylim = c(-3,3), xlab = "x", ylab = "y", pch = 20 )
    points(test, newy_10, col = "red", pch = 20)
    points(test, newy_2, col = "green", pch = 20)
    points(x, y, col = "dark blue")
}

manipulate(overfit.plot(q, n, s),
           q = slider(min = 1, max = 20),
           n = slider(min = 20, max = 200),
           s = slider(min = .01, max = 2))



Eout_2 <- sum
Eout_2 <- sum((predict(poly,test)-newy_2)^2)/1000
Eout_10 <- sum((predict(poly,test)-newy_10)^2)/1000


Overfit <- function(q, n, s) {

    x <- runif(n, min = -1, max = 1)
    
    poly <- polynomial(rnorm(n = q+1)) #poly
    
    epsi <- rnorm(n) #noise
    
    y <- predict(poly, x) + sqrt(s)*epsi #values of poly +noise
    df <- data.frame(x, y)
    
    model_2 <- lm(y ~ poly(x,2), data = df)
    model_10 <- lm(y ~ poly(x, 10), data = df)
    
    
    #now we want E_out
    test <- runif(1000, min = -1, max = 1)
    testdf <- data.frame(test)
    colnames(testdf)[1]<-"x"
    newy_2 <- predict(model_2, newdata = testdf)
    newy_10 <- predict(model_10, newdata = testdf)
    
    Eout_2 <- sum((predict(poly,test)-newy_2)^2)/1000
    
    Eout_10 <- sum((predict(poly,test)-newy_10)^2)/1000
    
    return(c(Eout_2, Eout_10, Eout_10 - Eout_2))
}


mean(replicate(500,Overfit(20, 40, 1)[3]))
median(replicate(500,Overfit(20, 40, 1)[3]))
hist(replicate(500,Overfit(20, 40, 1)[3])) #VERY skewed
range(replicate(500,Overfit(20, 40, 1)[3])) # most are close but overfitting is REALLY terrible 
#on a few occasions - this brings the MSE up but not the median.

Q <- c(5, 10, 15, 20, 25, 30)

N<- c(40, 60, 80, 100, 120, 140, 160)

sigma <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)

#Let's vary N and keep Q = 10, Sigma = .5
trainexamples <- numeric(length(N))
j = 1
for (i in N) { 
    trainexamples[j] <- mean(replicate(500,Overfit(10, i, .5)[3]))
    j = j+1
}

plot( N[2:7] , trainexamples[2:7])


#Let's vary Noise: Keep N = 80, sigma = .5

trainnoise = numeric(length(sigma))
j = 1
for (i in sigma) { 
    trainnoise[j] <- mean(replicate(500,Overfit(10, 80, i)[3]))
    j = j+1
}

plot(sigma, trainnoise)

#Let's do complexity

traincomplexity = numeric(length(Q))
j = 1
for (i in Q) { 
    traincomplexity[j] <- mean(replicate(500,Overfit(i, 80, 1)[3]))
    j = j+1
}

plot(Q, traincomplexity, abline = 0)

abline(h = 0, v = 0)

?plot

#____________________________________


hist(replicate(500,Overfit(20, 40, 1)[1]))
hist(replicate(500,Overfit(20, 80, 1)[3]))


set.seed(124)
mean(replicate(500,Overfit(10, 80, 1)[3]))
median(replicate(500,Overfit(10, 80, 1)[3]))

