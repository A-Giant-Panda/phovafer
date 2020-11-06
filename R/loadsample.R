#' The half-hourly smart meter readings of 622 households in the middle Scotland.
#'
#' This dataset is the pre-processed data that created by the developers based on the original EDRP dataset provided by the "Analytical Middleware for Informed Distribution Networks (AMIDiNe)" (EPSRC Reference: EP/S030131/1) project. It contains three-month data from March to May.
#'
#' @format A dataframe with 4416 rows and 628 variables:
#' \describe{
#'   \item{date}{(character) Date of the smart meter readings with the form 'yyyy-mm-dd'.}
#'   \item{time}{(character) Timestamps was recorded. Ranges from 00:00:00 to 23:30:00.}
#'   \item{year}{(integer) The year was recorded.}
#'   \item{month}{(character) The month was recorded. These months are March, April and May.}
#'   \item{mdate}{(character) Denote the date and month with the form 'dd-mm'.}
#'   \item{week}{(character) The day-of-the-week was recorded. Ranges from 'Monday' to 'Sunday'.}
#'   \item{`7-628`}{(numeric) Smart meter readings of each household at the given time.}
#' }
#' @source Provided by the project "Analytical Middleware for Informed Distribution Networks (AMIDiNe)" (EPSRC Reference: EP/S030131/1).
#' @examples head(loadsample[,1:10])
"loadsample"

