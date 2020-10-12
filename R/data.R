#' Nakaya (1997)
#'
#' @description Nakaya, T. (1997) Statistical inferences in bidimensional regression models. Geographical Analysis, 29(2), 169-186.
#'
#' @format A data frame with 19 observations on the following 4 variables:
#' \describe{
#'   \item{depV1, depV2}{numeric vectors, dependent variables}
#'   \item{indepV1, indepV2}{numeric vectors, independent variables}
#' }
"NakayaData"

#' Eye gaze calibration data
#'
#' A dataset containing a monocular eye gaze recording with calibration sequence.
#' Courtesy of \href{https://www.uni-bamberg.de/entwicklungspsychologie/transfer/babyforschung-bambi/}{Bamberger Baby Institut: BamBI}.
#'
#' @source \url{https://www.uni-bamberg.de/entwicklungspsychologie/transfer/babyforschung-bambi/}.
#' @format A data frame with 365 rows and 6 variables:
#' \describe{
#'   \item{time}{sample timestamp, in milliseconds}
#'   \item{x, y}{recorded gaze, in internal eye tracker units}
#'   \item{target_x, target_y}{location of the calibration target on the screen, in pixels}
#'   \item{target}{index of the target within the sequence}
#' }
"EyegazeData"


#' Friedman & Kohler (2003), data set #1
#'
#' @description Data from Friedman, A., & Kohler, B. (2003). Bidimensional regression: Assessing
#' the configural similarity and accuracy of cognitive maps and other two-dimensional data sets.
#' Psychological Methods, 8(4), 468-491.
#'
#' @format A data frame with 4 observations on the following 4 variables:
#' \describe{
#'   \item{depV1, depV2}{numeric vectors, dependent variables}
#'   \item{indepV1, indepV2}{numeric vectors, independent variables}
#' }
"FriedmanKohlerData1"

#' Friedman & Kohler (2003), data set #2
#'
#' @description Data from Friedman, A., & Kohler, B. (2003). Bidimensional regression: Assessing
#' the configural similarity and accuracy of cognitive maps and other two-dimensional data sets.
#' Psychological Methods, 8(4), 468-491.
#'
#' @format A data frame with 4 observations on the following 4 variables:
#' \describe{
#'   \item{depV1, depV2}{numeric vectors, dependent variables}
#'   \item{indepV1, indepV2}{numeric vectors, independent variables}
#' }
"FriedmanKohlerData2"


#' Carbon, C. C. (2013), data set #1
#'
#' @description Example 1 from the domain of aesthetics to show how the method can be utilized
#' for assessing the similarity of two portrayed persons, actually the Mona Lisa in the world
#' famous Louvre version and the only recently re-discovered Prado version.
#'
#' @source \url{http://www.jstatsoft.org/v52/c01/}
#' @format A data frame with 36 observations on the following 4 variables:
#' \describe{
#'   \item{depV1, depV2}{numeric vectors, dependent variables}
#'   \item{indepV1, indepV2}{numeric vectors, independent variables}
#' }
"CarbonExample1Data"

#' Carbon, C. C. (2013), data set #2
#'
#' @description Example 2 originates from the area of geography and inspects the accuracy of
#' different maps of the city of Paris which were created over the last 350 years as compared
#' to a recent map.
#'
#' @source \url{http://www.jstatsoft.org/v52/c01/}
#' @format A data frame with 13 observations on the following 4 variables:
#' \describe{
#'   \item{depV1, depV2}{numeric vectors, dependent variables}
#'   \item{indepV1, indepV2}{numeric vectors, independent variables}
#' }
"CarbonExample2Data"

#' Carbon, C. C. (2013), data set #3
#'
#' @description Example 3 focuses on demonstrating how good a cognitive map recalculated from
#' averaged cognitive distance data fits with a related real map.
#'
#' @source \url{http://www.jstatsoft.org/v52/c01/}
#' @format A data frame with 10 observations on the following 4 variables:
#' \describe{
#'   \item{depV1, depV2}{numeric vectors, dependent variables}
#'   \item{indepV1, indepV2}{numeric vectors, independent variables}
#' }
"CarbonExample3Data"


#' Female face, neutral expression
#'
#' @description Facial landmarks for a female face with neutral expression.
#'
#' @format A data frame with 64 observations on the following 3 variables:
#' \describe{
#'   \item{x, y, z}{numeric vectors, 3D coordinates of factial landmarks.}
#' }
"female_face_neutral"

#' Female face, happy expression
#'
#' @description Facial landmarks for a female face with happoy expression.
#'
#' @format A data frame with 64 observations on the following 3 variables:
#' \describe{
#'   \item{x, y, z}{numeric vectors, 3D coordinates of factial landmarks.}
#' }
"female_face_happy"

#' Male face, neutral expression
#'
#' @description Facial landmarks for a male face with neutral expression.
#'
#' @format A data frame with 64 observations on the following 3 variables:
#' \describe{
#'   \item{x, y, z}{numeric vectors, 3D coordinates of factial landmarks.}
#' }
"male_face_neutral"
