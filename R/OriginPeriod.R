#'
#' OriginPeriod class
#' 
#' @doctype class
#' 
#' @seealso \code{\link{OriginPeriodConstructor}}
#' 
#' @name OriginPeriod-class
#' @rdname OriginPeriod-class
#' @exportClass OriginPeriod
#' 
#' @description
#' OriginPeriod is an S4 class used to store information about the time interval which produces claims. 
#' 
#' @details
#' An OriginPeriod may be of any arbitrary length, though years are most common. The "type" slot is a character string which 
#' indicatesthe type of origin period, such as "Accident", "Occurrence", "Report", "Policy", "Lloyds" and so forth. "Accident" 
#' and "Occurrence" are synonymous and indicate the a claim occurred in a particular period of time. "Report" is the interval 
#' during which a claim is reported, such as exists under a claims-made policy. "Policy" or "Underwriting" periods are ones
#' wherein the risk attaches. "Lloyds" refers to a Lloyds year of account. There is no restriction on what type may be used
#' for an OriginPeriod, meaning that there is no language restriction either. So, "Zeichnungsjahr", "Ereignisjahr" and so forth
#' are permitted.
#' 
#' \strong{OriginPeriod construction}
#' \strong{1. Construct an empty OriginPeriod}
#' myOP = OriginPeriod()
#' 
#' startDates = seq(as.Date("2002/01/01"), as.Date("2013/12/31"), by="6 months")
#' x = OriginPeriod(startDates, Type="AccidentYear")

#' endDates = startDates + as.period(6, "months") - days(1)
#' y = MonthsBetween(startDates, endDates)
#' z = MeanMonths(y)

#' x = OriginPeriod(startDates, endDates, Type="AccidentYear")

#===================================
# TODO: ensure that moniker is the same length as the startdate
#===================================

is.OriginPeriod = function(object)
{
  is(object, "OriginPeriod")
}

checkOriginPeriod = function(object)
{
  errors = character()
  if (length(errors) == 0) TRUE else errors
}

setClass("OriginPeriod"
         , representation(StartDate = "Date"
                          , EndDate = "Date"
                          , Moniker = "character"
                          , Length = "Period"
                          , Type = "character")
         , validity = checkOriginPeriod
)

MonthsBetween = function(StartDate, EndDate){
  diff = as.double(EndDate - StartDate)
  diff = round(diff / 30)
  as.period(diff, unit="months")
}

MeanMonths = function(Periods){
  z = month(Periods)
  z = mean(z)
  z = as.period(z, unit="months")
}

DefaultLength = function(){
  years(1)
}
  
OriginPeriod = function(StartDate 
                        , EndDate 
                        , Length
                        , ...){
  stop("You must use a specific constructor method to create an OriginPeriod object. 
       Is it possible you forgot to provide a formal argument name? 
       For example, monikers and type must be explicity named in the function signature:
       OriginPeriod(start, end, Moniker=yourMoniker, Type=yourType")
}

setGeneric("OriginPeriod")

setMethod("OriginPeriod", signature=c(StartDate = "missing", EndDate = "missing", Length = "missing")
          , definition=function(StartDate, EndDate, Moniker, Type){
            
            if (missing(Moniker)) {Moniker = character()}
            if (missing(Type)) {Type = character()}
            
            op = new("OriginPeriod"
                     , StartDate = as.Date("1900/1/1")
                     , EndDate = as.Date("1900/1/1")
                     , Length = period(0)
                     , Moniker = Moniker
                     , Type = Type)
            op
})

setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "missing", Length = "missing")
          , definition=function(StartDate, EndDate, Moniker, Type){
            
            if (missing(Moniker)) {Moniker = character()}
            if (missing(Type)) {Type = character()}
            
            Length = DefaultLength()
            EndDate = StartDate + Length - days(1)
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Length = Length
                     , Moniker = Moniker
                     , Type = Type)
            op
})

setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "Date", Length = "missing")
          , definition=function(StartDate, EndDate, Moniker, Type){
            
            if(length(StartDate) != length(StartDate)){ stop("Start and end dates are not of equal length") }
            if (missing(Moniker)) {Moniker = character()}
            if (missing(Type)) {Type = character()}
            
            Length = MonthsBetween(StartDate, EndDate)
            Length = MeanMonths(Length)
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Length = Length
                     , Moniker = Moniker
                     , Type = Type)
  op
})

setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "missing", Length = "Period")
          , definition=function(StartDate, Length, Moniker, Type){
            
            if (missing(Moniker)) {Moniker = character()}
            if (missing(Type)) {Type = character()}
            
            EndDate = StartDate + Length - days(1)
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Length = Length
                     , Moniker = Moniker
                     , Type = Type)
            op
})

setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "Period", Length = "missing")
          , definition=function(StartDate, EndDate, Moniker, Type){
            
            # This is perverse, but necessary. R will match arguments based on position first, then
            # by name. If a user passes in two un-named arguments, the second of which is class= Period,
            # We have to match by indicating that the second argument- named EndDate in the generic
            # signature- is a period. We must instantly swap Length and EndDate to return to normal.
            if (missing(Moniker)) {Moniker = character()}
            if (missing(Type)) {Type = character()}
            op = OriginPeriod(StartDate, Length=EndDate, Moniker=Moniker, Type=Type)
})

setMethod("OriginPeriod", signature=c(StartDate = "integer", EndDate = "missing", Length = "missing")
          , definition=function(StartDate, Moniker, Type, StartDay, StartMonth){
            
            if (missing(Moniker)) {Moniker = character()}
            if (missing(Type)) {Type = character()}
            
            if(missing(StartMonth)){StartMonth = 1}
            if(missing(StartDay)){ StartDay = 1}
            
            StartDate = ymd(paste(StartDate, StartMonth, StartDay, sep="/"))
            StartDate = as.Date(StartDate)
            Length = DefaultLength()
            EndDate = StartDate + Length - days(1)
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Length = Length
                     , Moniker = Moniker
                     , Type = Type)
            op
})

setGeneric("length")

setMethod("length", signature=c(x="OriginPeriod"), definition=function(x){
  length(x@StartDate)
})

setGeneric("rbind", 
           function(..., deparse.level=1) standardGeneric("rbind"), 
           signature = "...") 

setMethod("rbind", signature="OriginPeriod", definition=function(..., deparse.level=1){
  elements = list(...)
  blnOPs = sapply(elements, is.OriginPeriod)
  elements = elements[blnOPs]
  
  startDates = lapply(elements, slot, "StartDate")
  startDates = do.call("c", startDates)
  monikers = c(unlist(lapply(elements, slot, "Moniker")))
  
  # Length and Type ought to be the same
  length = elements[[1]]@Length
  type = elements[[1]]@Type
  
  x = OriginPeriod(StartDate = startDates, Length=length, Moniker=monikers, Type=type)
  x
})

setMethod("c", signature(x="OriginPeriod"), definition=function(x, ...){
  elements = list(...)
  blnOPs = sapply(elements, is.OriginPeriod)
  elements = elements[blnOPs]
  
  startDates = lapply(elements, slot, "StartDate")
  startDates = do.call("c", startDates)
  startDates = c(x@StartDate, startDates)
  monikers = c(unlist(lapply(elements, slot, "Moniker")))
  monikers = c(x@Moniker, monikers)
  
  # Length and Type ought to be the same
  length = x@Length
  type = x@Type
  
  x = OriginPeriod(StartDate = startDates, Length=length, Moniker=monikers, Type=type)
  x
})
