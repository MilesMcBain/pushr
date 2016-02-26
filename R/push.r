.pushglobalenv <- new.env(parent=emptyenv())

push <- function( fromDF, toDF, toEnv = .GlobalEnv){
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

  #execute push in calling environment

  eval(
    expr = parse( text = paste0("toEnv$",to_name, "[(1+", offset, "):", offset+incomming_length, ",] <- fromDF") ),
    envir = environment()
  )
  #advance offset
  assign(to_name, envir = .pushglobalenv, value = offset+incomming_length)
}

push_initialise <- function(toDF){
 assign(x=deparse(substitute(toDF)), value = 0, envir = .pushglobalenv)
}

trim_empty <- function(trimDF){

  trim_name <- deparse(substitute(trimDF))

  if(!exists(x = trim_name, envir = .pushglobalenv)){
    stop(paste0("Found no record of data pushed to ", trim_name, ", cannot trim." ))
  }
  offset <- get(trim_name, envir = .pushglobalenv)
  if(offset == 0){
    return(trimDF[0:0,])
  } else {
    return(trimDF[1:offset,])
  }

}

