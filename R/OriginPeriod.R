#'
#' OriginPeriod class
#' 
#' @docType class
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
NULL

#**********************************************************************************************
# 0. Helper functions ====
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

DefaultPeriod = function(){
  years(1)
}

DefaultMoniker = function(startDates){
  as.character(startDates)
}

is.OriginPeriod = function(object)
{
  is(object, "OriginPeriod")
}

#**********************************************************************************************
# 1. Class Definition ====
checkOriginPeriod = function(object)
{
  errors = character()
  
  if (length(object@StartDate) != length(unique(object@StartDate))) {
    errors = c(errors, "Start dates must be unique.")
  }
  
  if (length(object@EndDate) != length(unique(object@EndDate))) {
    errors = c(errors, "End dates must be unique.")
  }
  
  if (length(object@StartDate) != length(object@EndDate)) {
    errors = c(errors, "Start and end date must have the same length.")
  }
  
  if (any(diff(object@StartDate) <= 0)) {
    errors = c(errors, "Start dates must be strictly increasing.")
  }
  
  if (any(object@EndDate <= object@StartDate)) {
    errors = c(errors, "End dates must be strictly greater than start dates.")
  }
  
  if (length(object@Moniker) != length(unique(object@Moniker))) {
    errors = c(errors, "Monikers must be unique.")
  }
  
  if (length(object@Moniker) != length(object@StartDate)){
    errors = c(errors, "Moniker and StartDate don't have equal length.")
  }
  
  if (length(errors) == 0) TRUE else errors
}

setClass("OriginPeriod"
         , representation(StartDate = "Date"
                          , EndDate = "Date"
                          , Moniker = "character"
                          , Period = "Period"
                          , Type = "character")
         , validity = checkOriginPeriod
)

#**********************************************************************************************
# 2. Constructors ====
setGeneric("OriginPeriod", function(StartDate , EndDate , Period, ...) {
  standardGeneric("OriginPeriod")
})

# OriginPeriod = function(StartDate 
#                         , EndDate 
#                         , Period
#                         , ...){
#   stop("You must use a specific constructor method to create an OriginPeriod object. 
#        Is it possible you forgot to provide a formal argument name? 
#        For example, monikers and type must be explicity named in the function signature:
#        OriginPeriod(start, end, Moniker=yourMoniker, Type=yourType")
# }
# 
# setGeneric("OriginPeriod")

#' @export 
setMethod("OriginPeriod", signature=c(StartDate = "missing", EndDate = "missing", Period = "missing")
          , function(StartDate, EndDate, Moniker, Type){
            
            if (missing(Moniker)) {Moniker = character()}
            if (missing(Type)) {Type = character()}
            
            op = new("OriginPeriod"
                     , StartDate = as.Date("1900/1/1")
                     , EndDate = as.Date("1900/1/1")
                     , Period = period(0)
                     , Moniker = Moniker
                     , Type = Type)
            op
})

#' @export 
setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "Date", Period = "Period")
          , function(StartDate, EndDate, Period, Type, Moniker){
            
            if (missing(Type)) {Type = character()}
            
            if (length(StartDate) != 1){
              warning("Calling standard constructor with StartDate and EndDate vectors. Period will be ignored.")
              op = OriginPeriod(StartDate, EndDate)
              return (op)
            }
            
            # Very lazy way of working with periods
            daysBetween = difftime(EndDate, StartDate, units="days")
            periodsBetween = suppressMessages(daysBetween / (Period / days(1)))
            periodsBetween = floor(as.numeric(periodsBetween))
            
            StartDate = StartDate + Period * (0:(periodsBetween-1))
            
            if (StartDate[periodsBetween] < EndDate) StartDate[periodsBetween+1] = StartDate[periodsBetween]+Period
            
            EndDate = StartDate + Period - days(1)
            
            if (missing(Moniker)) {Moniker = DefaultMoniker(StartDate)}
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Period = Period
                     , Moniker = Moniker
                     , Type = Type)
            op
})

#' @export 
setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "missing", Period = "missing")
          , function(StartDate, EndDate, Moniker, Type, Verbose=FALSE){
            
            if (missing(Moniker)) {
              Moniker = DefaultMoniker(StartDate)
              if (Verbose) warning("No Moniker has been specified. Defaulting to a blank Moniker.")
            }
            if (missing(Type)) {Type = character()}
            
            Period = DefaultPeriod()
            EndDate = StartDate + Period - days(1)
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Period = Period
                     , Moniker = Moniker
                     , Type = Type)
            op
})

#' @export 
setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "Date", Period = "missing")
          , definition=function(StartDate, EndDate, Moniker, Type, Verbose=FALSE){
            
            if(length(StartDate) != length(EndDate)){ stop("Start and end dates are not of equal length") }
            if (missing(Moniker)) {
              Moniker = DefaultMoniker(StartDate)
              if (Verbose) warning("No Moniker has been specified. Defaulting to a blank Moniker.")
            }
            if (missing(Type)) {Type = character()}
            
            Period = MonthsBetween(StartDate, EndDate)
            Period = MeanMonths(Period)
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Period = Period
                     , Moniker = Moniker
                     , Type = Type)
  op
})

#' @export 
setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "missing", Period = "Period")
          , definition=function(StartDate, Period, Moniker, Type, Verbose=FALSE, NumPeriods){
            
            if (missing(Type)) {Type = character()}
            
            if(length(StartDate) == 1){
              StartDate = StartDate + Period * (0:(NumPeriods-1))
            }
            
            EndDate = StartDate + Period - days(1)
            
            if (missing(Moniker)) {
              Moniker = DefaultMoniker(StartDate)
              if (Verbose) warning("No Moniker has been specified. Defaulting to a blank Moniker.")
            }
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Period = Period
                     , Moniker = Moniker
                     , Type = Type)
            op
})

#' @export 
setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "Period", Period = "missing")
          , definition=function(StartDate, EndDate, Moniker, Type, Verbose=FALSE){
            
            # This is perverse, but necessary. R will match arguments based on position first, then
            # by name. If a user passes in two un-named arguments, the second of which is class= Period,
            # We have to match by indicating that the second argument- named EndDate in the generic
            # signature- is a period. We must instantly swap Period and EndDate to return to normal.
            if (missing(Moniker)) {
              Moniker = DefaultMoniker(StartDate)
              if (Verbose) warning("No Moniker has been specified. Defaulting to a blank Moniker.")
            }
            if (missing(Type)) {Type = character()}
            op = OriginPeriod(StartDate, Period=EndDate, Moniker=Moniker, Type=Type)
})

#' @export 
setMethod("OriginPeriod", signature=c(StartDate = "integer", EndDate = "missing", Period = "missing")
          , definition=function(StartDate, Moniker, Type, StartDay, StartMonth, Verbose=FALSE){
            
            if(missing(StartMonth)) {StartMonth = 1}
            if(missing(StartDay)) {StartDay = 1}
            
            StartDate = ymd(paste(StartDate, StartMonth, StartDay, sep="/"))
            StartDate = as.Date(StartDate)
            Period = DefaultPeriod()
            EndDate = StartDate + Period - days(1)
            
            if (missing(Moniker)) {
              Moniker = DefaultMoniker(StartDate)
              if (Verbose) warning("No Moniker has been specified. Defaulting to a blank Moniker.")
            }
            if (missing(Type)) {Type = character()}
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Period = Period
                     , Moniker = Moniker
                     , Type = Type)
            op
})

#**********************************************************************************************
# 3. Properties ====
#' @export 
is.OriginPeriod = function(x){
  is(x, "OriginPeriod")
}

#' @export 
setMethod("show", signature(object="OriginPeriod"), definition=function(object){
  cat("OriginPeriod object \n")
  if (length(object@Type) == 0) {
    cat("Type:\tNo type specified\n")
  } else {
    cat("Type:\t", object@Type, "\n")  
  }
  cat("Period:\t", as.character(object@Period), "\n")
  cat("First date:\t", as.character(min(object@StartDate)), "\n")
  cat("Last date:\t", as.character(max(object@EndDate)), "\n")
  cat("Length:\t", length(object), "\n")
})

#' @export
setMethod("length", signature=c(x="OriginPeriod"), definition=function(x){
  length(x@StartDate)
})

#**********************************************************************************************
# 4. Accessors ====
#' @export 
setMethod("[", signature(x="OriginPeriod"), definition=function(x, i){
  op = OriginPeriod(x@StartDate[i], x@EndDate[i], Moniker=x@Moniker[i], Type=x@Type)
})

#' @export 
setMethod("[", signature(x="OriginPeriod", i="character"), definition=function(x, i){
  i = match(i, x@Moniker)
  op = OriginPeriod(x@StartDate[i], x@EndDate[i], Moniker=x@Moniker[i], Type=x@Type)
})

#' @export
setMethod("[[", signature(x = "OriginPeriod"), definition=function(x, i, j, ..., exact = TRUE){
  op = OriginPeriod(x@StartDate[i], x@EndDate[i], Moniker=x@Moniker[i], Type=x@Type)
})

#' @export
setMethod("[<-", signature(x = "OriginPeriod", value = "OriginPeriod"), definition=function(x, i, j, ..., value) {
  if (x@Period != value@Period) {
    msg = "Period length is not equal to object being assigned."
    msg = paste(msg, x@Period, "vs.", value@Period)
    stop(msg)
  }
  x@StartDate[i] = value@StartDate
  x@EndDate[i] = value@EndDate
  x@Moniker[i] = as.character(value@Moniker)
  x
})

#' @export
setMethod("[[<-", signature(x = "OriginPeriod", value = "OriginPeriod"), definition=function(x, i, j, ..., value) {
  if (x@Period != value@Period) {
    msg = "Period length is not equal to object being assigned."
    msg = paste(msg, x@Period, "vs.", value@Period)
    stop(msg)
  }
  x@StartDate[i] = value@StartDate
  x@EndDate[i] = value@EndDate
  x@Moniker[i] = value@Moniker
  x
})

#' @export
setMethod("$", signature(x = "OriginPeriod"), function(x, name) {
  slot(x, name)
})

#' @export
setMethod("$<-", signature(x = "OriginPeriod"), function(x, name, value) {
  slot(x, name) <- value
  if (!validObject(x)){
    stop("Assignment of this property would create an invalid object. Property not assigned.")
  }
  x
})

#**********************************************************************************************
# 5. Comparison ====
#' @export
setMethod("==", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  sameLength = length(e1) == length(e2)
  sameStart = e1@StartDate[1] == e2@StartDate[1]
  samePeriod = e1@Period == e2@Period
  
  if (length(e1@Type) == 0 & length(e2@Type) == 0){
    sameType = TRUE
  } else if (length(e1@Type) == 0 | length(e2@Type) == 0) {
    sameType = FALSE
  } else {
    sameType = e1@Type == e2@Type  
  }

  same = sameLength & sameStart & samePeriod & sameType
  same
})

#' @export
setMethod("!=", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  !(e1 == e2)
})

#' @export
setMethod(">", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  warning("> is not defined for an object of class OriginPeriod")
})

#' @export
setMethod(">=", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  warning(">= is not defined for an object of class OriginPeriod")
})

#' @export
setMethod("<", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  warning("< is not defined for an object of class OriginPeriod")
})

#' @export
setMethod("<=", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  warning("<= is not defined for an object of class OriginPeriod")
})

#**********************************************************************************************
# 6. Conversion ====
#' @export
setMethod("as.data.frame", signature("OriginPeriod"), function(x, ...){
  type = ifelse(length(x@Type)==0, "No type specified", x@Type)
  y = data.frame(StartDate = x@StartDate
                 , EndDate = x@EndDate
                 , Moniker = x@Moniker
                 , Type = rep(type, length(x))
                 , Period = rep(x@Period, length(x)))
  y
})

#**********************************************************************************************
# 7. Concatenate ====

#' @export 
rbind.OriginPeriod = function(..., deparse.level=1){
  elements = list(...)
  blnOPs = sapply(elements, is.OriginPeriod)
  elements = elements[blnOPs]
  
  startDates = lapply(elements, slot, "StartDate")
  startDates = do.call(c, startDates)
  
  monikers = c(unlist(lapply(elements, slot, "Moniker")))
  
  # Period and Type ought to be the same
  Period = elements[[1]]@Period
  type = elements[[1]]@Type
  
  x = OriginPeriod(StartDate = startDates, Period=Period, Moniker=monikers, Type=type)
  x
}

#' @export 
setMethod("c", signature(x="OriginPeriod"), function(x, ...){
  elements = list(...)
  blnOPs = sapply(elements, is.OriginPeriod)
  elements = elements[blnOPs]
  
  startDates = lapply(elements, slot, "StartDate")
  startDates = do.call("c", startDates)
  startDates = c(x@StartDate, startDates)
  monikers = c(unlist(lapply(elements, slot, "Moniker")))
  monikers = c(x@Moniker, monikers)
  
  # Period and Type ought to be the same
  Period = x@Period
  type = x@Type
  
  x = OriginPeriod(StartDate = startDates, Period=Period, Moniker=monikers, Type=type)
  x
})

#' @export
setMethod("Grow", signature=c(object="OriginPeriod", Length="numeric"), definition=function(object, Length){
  startDates = max(object@StartDate)  + object@Period * (1:Length)
  moniker = paste("New moniker", (1:Length))
  op = OriginPeriod(StartDate = startDates, Type=object@Type, Period=object@Period, Moniker=moniker)
  op = c(object, op)
  op
})
#**********************************************************************************************
# 8. Persistence ====

#' @export
#' 
setMethod("write.excel", signature=c(object = "OriginPeriod", file="character", overwrite="logical")
          , definition=function(object, file, overwrite=FALSE, sheetName){

  if (file.exists(file) & !overwrite){
    stop("Excel file already exists. Either enter a new filename or set the overwrite parameter to TRUE.")
  }
  
  if (missing(sheetName)) sheetName = "OriginPeriod"
  wbk = loadWorkbook(file, create=TRUE)
  createSheet(wbk, name=sheetName)
  
  headers = data.frame(Col1 = c(object@Type, "Period")
                       , Col2 = c("Start", "Date")
                       , Col3 = c("End", "Date"))
  
  writeWorksheet(wbk, headers, sheetName, header=FALSE)
  
  df = as.data.frame(object)
  df = df[, c("Moniker", "StartDate", "EndDate")]
  
  writeWorksheet(wbk, df, sheetName, startRow = 3, header=FALSE)
  
  saveWorkbook(wbk)
  
})

# setMethod("read", )
# How does this work exactly?
