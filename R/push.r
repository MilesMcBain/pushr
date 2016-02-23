.pushglobalenv <- new.env(parent=emptyenv())

push <- function(fromDF, toDF){
  if(ncol(toDF) != ncol(fromDF)){
    stop("data frame dimensions do not match, canot push()")
  }

  #TODO check push can occur safely

  to_name <- deparse(substitute(toDF))
  from_name <- deparse(substitute(fromDF))
  incomming_length <- nrow(fromDF)

  if(!exists(to_name, envir = .pushglobalenv )){
    assign(x = to_name, envir = .pushglobalenv, value = 0)
  }
  #figure out offset to apply to assignment
  offset <- get(to_name, envir = .pushglobalenv)
  eval(
    expr = parse( text = paste0(to_name, "[(1+", offset, "):", offset+incomming_length, ",] <-",from_name) ),
    envir = parent.frame()
    )
  #advance offset
  assign(to_name, envir = .pushglobalenv, value = offset+incomming_length)
}


pushtest <- function(df){
  eval(
    expr = parse(text = paste0(deparse(substitute(df)),"[1,] <- c(1.0, 1.0, 1.0)")),
    envir = parent.frame()
  )
}

#Test Code
a <- trees
b <- data.frame(list(v1 = 1.0,v2 = 1, v3 = 1.0))
push(b,a)
a


