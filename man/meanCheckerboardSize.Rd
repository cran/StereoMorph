\name{meanCheckerboardSize}
\alias{meanCheckerboardSize}
\alias{summary.meanCheckerboardSize}
\alias{print.summary.meanCheckerboardSize}
\title{Finds mean checkerboard square size}
\description{
This function finds the mean square size of a checkerboard pattern (in pixels), optionally scaling this to real-world units (e.g. millimeters).
}
\usage{
meanCheckerboardSize(checker.pts, nx, ny, ruler.pts=NULL, 
                     ruler.pt.size=NULL, rep=200)

\method{summary}{meanCheckerboardSize}(object, ...)
}
\arguments{
  \item{checker.pts}{a matrix of internal corners from  a checkerboard pattern.}
  \item{nx}{the number of internal corners in the first dimension along which the checkerboard points are ordered.}
  \item{ny}{the number of internal corners in the second dimension along which the checkerboard points are ordered.}
  \item{ruler.pts}{evenly spaced points digitized along a ruler or comparable standard.}
  \item{ruler.pt.size}{the size of the spacing between points in \code{ruler.pts} in real world units. This can be numeric or alphanumeric including the unit (see "Details").}
  \item{rep}{the number of times to repeat the square size measurement with a new random sample of point pairs.}
  \item{object}{a list of class \code{"meanCheckerboardSize"} (the output of \code{meanCheckerboardSize()}).}
  \item{...}{further arguments passed to other methods.}
}
\details{
The matrix \code{checker.pts} is a matrix of internal corners from a checkerboard pattern. These can be automatically detected using the C++ function \code{findCheckerboardCorners}. For a step-by-step tutorial on how to download, install and use \code{findCheckerboardCorners} see \href{http://stereomorph.blogspot.com/2014/04/auto-detecting-checkerboard-corners.html}{Auto-detecting checkerboard corners}. \code{meanCheckerboardSize()} finds the mean checkerboard size by randomly selecting pairs of points (without re-sampling) and dividing the distance by the expected distance for a unit grid. Since the points are chosen randomly, results will differ slightly from run to run. \code{rep} sets the number of times this is repeated, selecting different point pairs at each iteration. For a large \code{rep}, the mean square size should converge.

If \code{ruler.pts} is \code{NULL}, then only the checkerboard square size in pixels (the mean and a vector of all size measurements) is returned. All other return values are \code{NULL}. If \code{ruler.pts} is non-\code{NULL}, the distance between consecutive ruler points is measured and this is used to scale the checkerboard square size to the units of \code{ruler.pt.size}.

\code{ruler.pt.size} can be numeric or alphanumeric (including the units). For example, \code{'1'}, \code{'1 mm'} and \code{'1.0 mm'} are all possible inputs to \code{ruler.pt.size}. The units are automatically extracted and only used in the summary function to help interpret the function results.

\code{meanCheckerboardSize} also returns the mean real-world size of a pixel. This represents the resolution of the camera at the surface of the checkerboard pattern.

See \href{http://stereomorph.blogspot.com/2014/04/measuring-checkerboard-square-size_4.html}{Measuring checkerboard square size} for a tutorial on this function.

See \href{http://stereomorph.blogspot.com/2014/04/auto-detecting-checkerboard-corners.html}{Auto-detecting checkerboard corners} for a tutorial on how to automatically detect checkerboard corners from an image.
}
\value{
a list of class \code{"meanCheckerboardSize"} with the following elements:
  \item{mean.square.size.px}{the mean checkerboard square size in pixels. This is the length of one side of the square, not the diagonal.}
  \item{mean.square.size.rwu}{the mean checkerboard square size in real-world units. \code{NULL} if \code{ruler.pts} is \code{NULL}.}
  \item{mean.ruler.size.px}{the mean distance between consecutive ruler points in pixels. \code{NULL} if \code{ruler.pts} is \code{NULL}.}
  \item{mean.rwu.per.px}{the mean real-world size of a pixel in the image (the length of one side of the pixel). \code{NULL} if \code{ruler.pts} is \code{NULL}.}
  \item{square.size.px}{a vector of all checkerboard square sizes in pixels.}
  \item{ruler.size.px}{a vector of all distances between consecutive ruler points in pixels. \code{NULL} if \code{ruler.pts} is \code{NULL}.}
  \item{unit}{if \code{ruler.pt.size} includes a unit, the unit. \code{NULL} if \code{ruler.pts} is \code{NULL}.}
}
\author{Aaron Olsen}
\seealso{
\code{\link{drawCheckerboard}}
}
\examples{
## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO CHECKERBOARD POINTS FILE
checker_file <- paste0(fdir, "checker_21_14_200(9).txt")

## NUMBER OF INTERNAL CORNERS IN THE HORIZONTAL DIMENSION
nx <- 21

## NUMBER OF INTERNAL CORNERS IN THE VERTICAL DIMENSION
ny <- 14

## SET FILE PATH TO RULER POINTS FILE
ruler_file <- paste0(fdir, "ruler_21_14_200(9).txt")

## READ POINTS INTO MATRICES
checker.pts <- as.matrix(read.table(checker_file))
ruler.pts <- as.matrix(read.table(ruler_file))

## GET MEAN SQUARE SIZE
mean_sq_size <- meanCheckerboardSize(checker.pts=checker.pts, nx=nx, ny=ny)

## PRINT SUMMARY
summary(mean_sq_size)

## THIS TIME, GET MEAN SQUARE SIZE AND SCALE WITH RULER POINTS
mean_sq_size_scale <- meanCheckerboardSize(checker.pts= checker.pts, nx=nx, ny=ny,
  ruler.pts=ruler.pts, ruler.pt.size='1 mm')

## PRINT SUMMARY
summary(mean_sq_size_scale)
}
\keyword{ grid functions }