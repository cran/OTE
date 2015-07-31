\name{OTReg}
\alias{OTReg}
\title{
Train the ensemble of optimal trees for regression.
}
\description{
This function selects optimal trees for regression from a total of \code{t.initial} trees grown by random forest. Number of trees in the initial set, \code{t.initial}, is specified by the user. If not specified then the default \code{t.initial = 1000} is used.
}
\usage{
OTReg(XTraining, YTraining, p = 0.2, t.initial = NULL,
      nf = NULL, ns = NULL, info = TRUE)
}
\arguments{
  \item{XTraining}{
An \code{n x d} dimensional training data matrix/frame consiting of traing observation where \code{n} is the number of observations and \code{d} is the number of features. 
}
  \item{YTraining}{
A vector of length \code{n} consisting of the values of the continuous response variable for the training data.
}
  \item{p}{
Percent of the best \code{t.initial} trees to be selected on the basis of performance on out-of-bag observations.
}
  \item{t.initial}{
Size of the initial set of regression trees.
}
  \item{nf}{
Number of features to be sampled for spliting the nodes of the trees. If equal to \code{NULL} then the default \code{sqrt(number of features)} is executed.
}
  \item{ns}{
Node size: Minimal number of samples in the nodes. If equal to \code{NULL} then the default \code{5} is executed.
}
 \item{info}{
If \code{TRUE}, displays processing information.
}
}
\details{
Large values are recommended for \code{t.initial} for better performance as possible under the available computational resources. 
}
\value{
A trained object consisting of the selected trees for regression.
}
\references{
Khan, Z., Gul, A., Perperoglou, A., Mahmoud, O.,Miftahuddin, M., Adler, W. and Lausen, B.(2014) ``An ensemble of optimal trees for classification and regression'' Journal name to appear.

Liaw, A. and Wiener, M. (2002) ``Classification and regression by random forest'' R news. 2(3). 18--22.
}
\author{
Zardad Khan <zkhan@essex.ac.uk>
}
\note{
Prior action needs to be taken in case of missing values as the fuction can not handle them at the current version. 
}

\seealso{
\code{\link{Predict.OTReg}}, \code{\link{OTProb}}, \code{\link{OTClass}}
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
  
#Train OTReg on the training data

  Opt.Trees <- OTReg(XTraining=X[training,],YTraining = Y[training],t.initial=200)
  
#Predict on test data

  Prediction <- Predict.OTReg(Opt.Trees, X[testing,],YTesting=Y[testing])
  
#Objects returned

  names(Prediction)
  Prediction$Unexp.Variations
  Prediction$Pr.Values
  Prediction$Trees.Used
}

\keyword{ OTReg }
\keyword{ OptTreesEns }
