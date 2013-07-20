SpliceDFs = function(aDF, anotherDF, colnames)
{
  if (length(colnames) == 1){
    x = as.data.frame(anotherDF[, colnames])
    names(x) = colnames
  } else {
    x = anotherDF[, colnames]
  }
  df = cbind(aDF,  x)
  df
}

GetValidDate = function(ValIn)
{
  
  if (!is.POSIXt(ValIn)){
    attemptConvert = ymd(ValIn)
    if (sum(is.Date(attemptConvert)) ==0 ){
      attemptConvert = ydm(ValIn)
      if (sum(is.Date(attemptConvert)) ==0 ){
        attemptConvert = mdy(ValIn)
        if (sum(is.Date(attemptConvert)) ==0 ){
          attemptConvert = myd(ValIn)
          if (sum(is.Date(attemptConvert)) ==0 ){
            attemptConvert = dmy(ValIn)
            if (sum(is.Date(attemptConvert)) ==0 ){
              attemptConvert = dym(ValIn)
              if (sum(is.Date(attemptConvert)) ==0 ){
                warning ("Unable to produce a valid date.")
              }
            }
          }
        }
      }
    } 
  } else{
    attemptConvert = ValIn
  }
  
  return (attemptConvert)
}

ColumnExists = function(df, ColumnName)
{  
  ColumnExists = sum(colnames(df) %in% ColumnName)
  
  return (ColumnExists != 0)
}

RenameColumn = function(df, OldName, NewName)
{
  if (ColumnExists (df, OldName)){
    whichCol = colnames(df) == OldName
    colnames(df)[whichCol] = NewName  
  } else
  {
    warning(paste0("The column name you specified ", OldName, " does not exist. No changes have been made."))
  }
  
  return(df)
}