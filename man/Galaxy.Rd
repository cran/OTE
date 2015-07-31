\name{Galaxy}
\alias{Galaxy}
\docType{data}
\title{ Radial Velocity of Galaxy NGC7531
}
\description{ This data set is a record of radial velocity of a spiral galaxy that is measured at 323 points in its covered area of the sky. The positions of the measurements, that are in the range of seven slot crossing at the origin, are denoted by 4 variables.
}
\usage{data(Galaxy)}
\format{  A data frame with 324 observations recorded on the following 5 variables.
  \describe{
     \item{\code{ east.west }}{ It is the east-west coordinate where east is taken as negative, west is taken as positive and origin, (0,0), is close to the center of galaxy.
     }
    \item{\code{  north.south }}{  It is the north-south coordinate where south is taken as negative, north is taken as positive and origin, (0,0), is near the center of galaxy.
    }
    \item{\code{  angle }}{  It is the degrees of anti rotation (clockwise) from the slot horizon where the observation lies.
    }
    \item{\code{  radial.position }}{  It is the signed distance from the center, (0,0), which is signed as negative if the east-west coordinate is negative.
    }
    \item{\code{  velocity  }}{ This is the response variable denoting the radial velocity(km/sec) of the galaxy.
    }
  }
}

\source{
Buta, R. (1987), ``The Structure and Dynamics of Ringed Galaxies, III: Surface Photometry and Kinematics of the Ringed Nonbarred Spiral NGC7531'' The Astrophysical J. Supplement Ser. 64. 1--37.
}

\examples{
data(Galaxy)
str(Galaxy)
}
\keyword{datasets}
