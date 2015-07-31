\name{Predict.OTClass}
\alias{Predict.OTClass}
\title{
Prediction function for the object returned by \code{OTClass}
}
\description{
This function provides prediction for test data on the trained \code{OTClass} object for classification. 
}
\usage{
Predict.OTClass(Opt.Trees, XTesting, YTesting)
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

 \item{Error.Rate  }{Error rate of the clssifier for the observations in XTesting.}
 \item{Confusion.Matrix }{Confusion matrix based on the estimated class labels and the true class labels.}
 \item{Estimated.Class  }{A vector of length \code{m} consisting of the estimated class labels for the observations in XTesting.}
 }
\references{
Khan, Z., Gul, A., Perperoglou, A., Mahmoud, O.,Miftahuddin, M., Adler, W. and Lausen, B.(2014) ``An ensemble of optimal trees for classification and regression'' Journal name to appear.

Liaw, A. and Wiener, M. (2002) ``Classification and regression by random forest'' R news. 2(3). 18--22.
}
\author{
Zardad Khan <zkhan@essex.ac.uk>
}
\seealso{
\code{\link{OTClass}}, \code{\link{OTReg}}, \code{\link{OTProb}}
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

  Opt.Trees <- OTClass(XTraining=X[training,],YTraining = Y[training], t.initial=200)
  
#Predict on test data

  Prediction <- Predict.OTClass(Opt.Trees, X[testing,],YTesting=Y[testing])
  
#Objects returned

  names(Prediction)
  Prediction$Confusion.Matrix
  Prediction$Predicted.Class.Labels

}
\keyword{Predict.OTClass}
\keyword{OptTreesEns}