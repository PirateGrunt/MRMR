PeriodToDifftime <- function(x){
  
  hasYears <- x@year != 0
  hasMonths <- x@month != 0
  hasDays <- x@day != 0
  
  numSlots <- sum(hasYears, hasMonths, hasDays)
  if (numSlots == 0){
    stop("This should only work with years, months and days.")
  }
  
  if (numSlots != 1){
    stop("Too many slots.")
  }
  
  if (hasYears) y <- mondate::as.difftime(x@year, units="years") 
  if (hasMonths) y <- mondate::as.difftime(x@month, units="months")
  if (hasDays) y <- mondate::as.difftime(x@day, units="days")
  
  y

}

DateSequence = function(from, to, by){

  if ("Period" %in% class(by))  by <- PeriodToDifftime(by)
  
  if (!"difftime" %in% class(by)) {
    stop("No difftime given for by argument.")
  }
  
  if (!units(by) %in% c("years", "months")){
    stop ("Must use years or months")
  }
  
  if (units(by) == "years") by <- by*12
  by <- as.numeric(by)

  to <- as.mondate(to)
  from <- as.mondate(from)
  
#   singleUnit <- as.difftime(1, units=units(by))
#   periodsBetween = mondate::difftime(to, from, units=units(by))
  
#   periodsBetween = suppressMessages(daysBetween / (by / days(1)))
#   periodsBetween = floor(as.numeric(periodsBetween))
  
#   retSequence = from + by * (0:(periodsBetween-1))
#   
#   if (retSequence[periodsBetween] < to) retSequence[periodsBetween+1] = retSequence[periodsBetween]+by
  
  retSequence <- seq(from, to, by)

  retSequence <- as.Date(retSequence)

  retSequence
}
