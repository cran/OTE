\name{Predict.OTProb}
\alias{Predict.OTProb}

\title{Prediction function for the object returned by \code{OTProb}
}
\description{
This function provides prediction for test data on the trained \code{OTProb} object for class membership probability estimation. 
}
\usage{
Predict.OTProb(Opt.Trees, XTesting, YTesting)
}
\arguments{
  \item{Opt.Trees}{ 
An object of class \code{OptTreesEns}.
}
  \item{XTesting}{  
An \code{m x d} dimensional training data matrix/frame consiting of test observations where \code{m} is the number of observations and \code{d} is the number of features. 
}
  \item{YTesting}{  
Optional. A vector of length \code{m} consisting of class labels for the test data. Should be binary (0,1).
}
}

\value{
A list with values

 \item{Brier.Score }{Brier Score based on the estimated probabilities and true class label in YTesting.}
 \item{Estimated.Probabilities  }{A vector of length \code{m} consisting of the estimated class membership probabilities for the observation in XTesting}
}
\references{
Khan, Z., Gul, A., Perperoglou, A., Miftahuddin, M., Mahmoud, O., Adler, W., & Lausen, B. (2019). Ensemble of optimal trees, random forest and random projection ensemble classification. Advances in Data Analysis and Classification, 1-20.

Liaw, A. and Wiener, M. (2002) ``Classification and regression by random forest'' R news. 2(3). 18--22.
}
\author{
Zardad Khan <zkhan@essex.ac.uk>
}
\seealso{
\code{\link{OTProb}}.
}
\examples{
#load the data

  data(Body)
  data <- Body
  
#Divide the data into training and test parts

  set.seed(9123) 
  n <- nrow(data)
  training <- sample(1:n,round(2*n/3))
  testing <- (1:n)[-training]
  X <- data[,1:24]
  Y <- data[,25]
  
#Train OTClass on the training data

  Opt.Trees <- OTProb(XTraining=X[training,],YTraining = Y[training],t.initial=200)
  
#Predict on test data

  Prediction <- Predict.OTProb(Opt.Trees, X[testing,],YTesting=Y[testing])
  
#Objects returned

  names(Prediction)
  Prediction$Brier.Score
  Prediction$Estimated.Probabilities

  }
\keyword{ Predict.OTProb }
\keyword{ OTE }