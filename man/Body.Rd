\name{Body}
\alias{Body}
\docType{data}
\title{ Exploring Relationships in Body Dimensions}
\description{ The Body data set consists of 507 observations on 24 predictor variables including age, weight, hight and 21 body dimensions. All the 507 observations are on individuals, 247 men and 260 women, in the age of twenties and thirties with a small number of old people. The class variable is gender having two categories male and female.
}
\usage{data(Body)}
\format{  A data frame with 507 observations recorded on the following 25 variables.
  \describe{
    \item{\code{  Biacrom }}{ The diameter of Biacrom taken in centimeter.}
    \item{\code{  Biiliac }}{ "Pelvic breadth" measured in centimeter.}
    \item{\code{  Bitro }}{ Bitrochanteric whole diameter measured in centimeter.}
    \item{\code{  ChestDp }}{ The depth of Chest of a person in centimeter between sternum and spine at nipple level.}
    \item{\code{  ChestD  }}{ The diameter of Chest of a person in centimeter at nipple level.}
    \item{\code{  ElbowD  }}{ The sum of diameters of two Elbows in centimeter.}
    \item{\code{  WristD  }}{ Sum of two Wrists diameters in centimeter.}
    \item{\code{  KneeD }}{ The sum of the diameters of two Knees in centimeter.}
    \item{\code{  AnkleD  }}{ The sum of the diameters of two Ankles in centimeter.}
    \item{\code{  ShoulderG }}{ The wideness of shoulder in centimeter.}
    \item{\code{  ChestG  }}{ The circumference of chest centimeter taken at nipple line for males and just above breast tissue for females.}
    \item{\code{  WaistG  }}{ The circumference of Waist in centimeter taken as the average of contracted and relaxed positions at the narrowest part.}
    \item{\code{  AbdG  }}{ Girth of Abdomin in centimeter at umbilicus and iliac crest, where iliac crest is taken as a landmark.}
    \item{\code{  HipG  }}{ Girth of Hip in centimeter at level of bitrochanteric diameter.}
    \item{\code{  ThighG  }}{ Average of left and right Thigh girths in centimeter below gluteal fold.}
    \item{\code{  BicepG  }}{ Average of left and right Bicep girths in centimeter.}
    \item{\code{  ForearmG  }}{ Average of left and right Forearm girths, extended, palm up.}
    \item{\code{  KneeG }}{ Average of left and right Knees girths over patella, slightly flexed position.}
    \item{\code{  CalfG }}{ Average of right and left Calf maximum girths.}
    \item{\code{  AnkleG  }}{Average of right and left Ankle minimum girths.}
    \item{\code{  WristG  }}{ Average of left and right minimum circumferences of Wrists.}
    \item{\code{  Age }}{ Age in years}
    \item{\code{  Weight  }}{ Weight in kilogram}
    \item{\code{  Height  }}{ Height in centimeter}
    \item{\code{  Gender  }}{ Binary response with two categories; 1 - male, 0 - female}
  }
}
\source{
Heinz, G., Peterson, L.J., Johnson, R.W. and Kerk, C.J. (2003), ``Exploring Relationships in Body Dimensions'', Journal of Statistics Education , 11.


}
\references{
Hurley, C. (2012), `` gclus: Clustering Graphics'', R package version 1.3.1, \url{http://CRAN.R-project.org/package=gclus}.
}
\examples{
data(Body)
str(Body)
}
\keyword{datasets}
