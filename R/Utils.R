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
  whichCol = colnames(df) == OldName
  colnames(df)[whichCol] = NewName
  
  return(df)
}