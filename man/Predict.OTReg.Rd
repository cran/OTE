\name{Predict.OTReg}
\alias{Predict.OTReg}

\title{
Prediction function for the object returned by \code{OTReg}
}
\description{
This function provides prediction for test data on the trained \code{OTReg} object for the continuous response variable.
}
\usage{
Predict.OTReg(Opt.Trees, XTesting, YTesting)
}

\arguments{
  \item{Opt.Trees}{ 
An object of class \code{OptTreesEns}.
}
  \item{XTesting}{  
An \code{m x d} dimensional training data matrix/frame consiting of test observations where \code{m} is the number of observations and \emph{d} is the number of features. 
}
  \item{YTesting}{  
Optional. A vector of length \code{m} consisting of the values of the continuous response variable for the test data. 
}
}

\value{
A list with values

 \item{Unexp.Variations }{Unexplained variations based on estimated response and given response.}
 \item{Pr.Values  }{A vector of length \code{m} consisting of the estimated values for the response observations in XTesting}
}
\references{
Khan, Z., Gul, A., Perperoglou, A., Mahmoud, O.,Miftahuddin, M., Adler, W. and Lausen, B.(2014) ``An ensemble of optimal trees for classification and regression'' Journal name to appear.

Liaw, A. and Wiener, M. (2002) ``Classification and regression by random forest'' R news. 2(3). 18--22.
}
\author{
Zardad Khan <zkhan@essex.ac.uk>
}
\seealso{
\code{\link{OTProb}}, \code{\link{OTReg}}, \code{\link{OTClass}}
}
\examples{
# Load the data

  data(Galaxy)
  data <- Galaxy
  
#Divide the data into training and test parts

  set.seed(9123) 
  n <- nrow(data)
  training <- sample(1:n,round(2*n/3))
  testing <- (1:n)[-training]
  X <- data[,1:4]
  Y <- data[,5]
  
#Train oTReg on the training data

  Opt.Trees <- OTReg(XTraining=X[training,],YTraining = Y[training],t.initial=200)
  
#Predict on test data

  Prediction <- Predict.OTReg(Opt.Trees, X[testing,],YTesting=Y[testing])
  
#Objects returned

  names(Prediction)
  Prediction$Unexp.Variations
  Prediction$Pr.Values
  Prediction$Trees.Used
}

\keyword{ Predict.OTReg }
\keyword{ OptTreesEns }