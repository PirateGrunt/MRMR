#
# mySequence = DateSequence(as.Date("2001-01-01"), as.Date("2010-12-31"), as.period(1, "year"))
DateSequence = function(from, to, by){
  daysBetween = difftime(to, from, units="days")
  periodsBetween = suppressMessages(daysBetween / (by / days(1)))
  periodsBetween = floor(as.numeric(periodsBetween))
  
  retSequence = from + by * (0:(periodsBetween-1))
  
  if (retSequence[periodsBetween] < to) retSequence[periodsBetween+1] = retSequence[periodsBetween]+by
  retSequence
}

