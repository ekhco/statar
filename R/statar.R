#' Median (IQR)
#'
#' This function returns the median (IQR)
#' @param my_data the data for which you want the median (IQR)
#' @param sig the significant digits in the IQR
#' @keywords median IQR
#' @export
#' @examples
#' data(mtcars) # load the cars dataset
#' med.iqr(mtcars$mpg, sig=2)

med.iqr <- function(my_data,sig){
	median <- median(my_data)
	q1 <- quantile(my_data,0.25)
	q3 <- quantile(my_data,0.75)
	if (sig==0){
		result <- sprintf("%1.0f (%1.0f-%1.0f)", median, q1, q3)
	} else if (sig==1) {
		result <- sprintf("%2.1f (%2.1f-%2.1f)", median, q1, q3)
	} else if (sig==2) {
		result <- sprintf("%3.2f (%3.2f-%3.2f)", median, q1, q3)
	} else {
		result <- sprintf("%4.3f (%4.3f-%4.3f)", median, q1, q3)
	}
	return(result)
}


#' Is Variable Uniquely Identifying
#'
#' whether the data is unique - combinations of variables too!
#' @param data the data for which you want to know if uniquely identifying
#' @keywords identifier unique
#' @export
#' @examples
#' data(mtcars) # load the cars dataset
#' isid(row.names(mtcars))

isid <- function(data) {
	if(nrow(unique(data.frame(data))) == nrow(data.frame(data))) {
		return(TRUE)
	}
	else {
		return(FALSE)
	}
}


#' Stata-like Dates
#'
#' Stata-like date entry
#' @param datein a string of the date, formatted "01Jan1960"
#' @keywords date
#' @export
#' @examples
#' td("26Aug1984")

td <- function (datein) {
	if (is.numeric(datein)) {
		diff <- as.numeric(as.character(datein))
		result <- as.Date("01Jan1960", format="%d%b%Y") + diff
		return(result)
	} else {
		return(as.Date(datein,format="%d%b%Y"))
	}
}


#' As Radians
#'
#' Returns radians from degrees
#' @param degrees the degrees for which you'd like radians
#' @keywords degrees to rad
#' @export
#' @examples
#' as.rad(365)

as.rad <- function(degrees) {
	radians = degrees * pi / 180
	return(radians)
}


#' Arc Miles
#'
#' Returns the arc miles between lat1, long1, and lat2, long2
#' @param lat1 the latitude of the first location
#' @param long1 the longitude of the first location
#' @param lat2 the latitude of the second location
#' @param long2 the longitude of the second location
#' @keywords arc miles
#' @export
#' @examples
#' arc.miles(49.123, -119.33, 49.02, -118.12)

# returns arc (miles) bewteen two lat longs
arc.miles <- function(lat1, long1, lat2, long2) {
	arc_miles <- acos(cos(as.rad(90-lat1)) * cos(as.rad(90-lat2)) + sin(as.rad(90-lat1)) * sin(as.rad(90-lat2)) * cos(as.rad(long1-long2))) * 3958.756
	return(arc_miles)

}


#' Odds Given Probability
#'
#' Returns the odds given the probability (0,1)
#' @param x the probability, between 0...1
#' @keywords odds
#' @export
#' @examples
#' odds(0.95)
odds <- function(x) {
    if (x>=0 && x<1) {
        return(x/(1-x))
    } else {return(NA)}
}


#' Odds Ratio
#'
#' Returns the odds ratio of y relative to x
#' @param x the treatment
#' @param y the controls
#' @keywords odds ratio OR
#' @export
#' @examples
#' OR(5,8)
OR <- function(y,x) {
    return(odds(x)/odds(y))
}
