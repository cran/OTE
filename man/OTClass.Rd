\name{OTClass}
\alias{OTClass}
\title{
Train the ensemble of optimal trees for classification.
}
\description{
This function selects optimal trees for classification from a total of \code{t.initial} trees grown by random forest. Number of trees in the initial set, \code{t.initial}, is specified by the user. If not specified then the default \code{t.initial = 1000} is used.
}
\usage{
OTClass(XTraining, YTraining, method=c("oob+independent","oob","sub-sampling"),
p = 0.1,t.initial = NULL,nf = NULL, ns = NULL, info = TRUE)
}
\arguments{
  \item{XTraining}{
An \code{n x d} dimensional training data matrix/frame consiting of traing observation where \code{n} is the number of observations and \code{d} is the number of features.
}
  \item{YTraining}{
A vector of length \code{n} consisting of class labels for the training data. Should be binary (0,1).
}
\item{method}{
 Method used in the selection of optimal trees. \code{method="oob+independent"} used out-of-bag observation from the bootstrap sample taken for growing the individual tree for indidual tree assessment while an independent training data for their collective assessement. \code{method="oob"} use the out-of-bag observations both for individual and collective assessment. \code{method="sub-sampling"} uses a sub-sample of the training data for individual tree assessment as well as its contribution towards the ensemble.
}

  \item{p}{
Percent of the best \code{t.initial} trees to be selected on the basis of performance on out-of-bag observations.
}
  \item{t.initial}{
Size of the initial set of classification trees.
}
  \item{nf}{
Number of features to be sampled for spliting the nodes of the trees. If equal to \code{NULL} then the default \code{sqrt(number of features)} is executed.
}
  \item{ns}{
Node size: Minimal number of samples in the nodes. If equal to \code{NULL} then the default \code{1} is executed.
}
 \item{info}{
If \code{TRUE}, displays processing information.
}
}
\details{
Large values are recommended for \code{t.initial} for better performance as possible under the available computational resources.
}
\value{
A trained object consisting of the selected trees.
}
\references{
Khan, Z., Gul, A., Perperoglou, A., Miftahuddin, M., Mahmoud, O., Adler, W., & Lausen, B. (2019). Ensemble of optimal trees, random forest and random projection ensemble classification. Advances in Data Analysis and Classification, 1-20.

Liaw, A. and Wiener, M. (2002) ``Classification and regression by random forest'' R news. 2(3). 18--22.
}


\author{
Zardad Khan <zkhan@essex.ac.uk>
}
\note{
Prior action needs to be taken in the case of missing values as the fuction can not handle them at the current version.
}

\seealso{
\code{\link{Predict.OTClass}}, \code{\link{OTReg}}, \code{\link{OTProb}}
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

  Opt.Trees <- OTClass(XTraining=X[training,],YTraining = Y[training],
  t.initial=200,method="oob+independent")

#Predict on test data

  Prediction <- Predict.OTClass(Opt.Trees, X[testing,],YTesting=Y[testing])

#Objects returned

  names(Prediction)
  Prediction$Confusion.Matrix
  Prediction$Predicted.Class.Labels

}
\keyword{ OTE }
\keyword{ Optimal }
\keyword{ Trees }% __ONLY ONE__ keyword per line
\keyword{ Ensemble }% __ONLY ONE__ keyword per line
