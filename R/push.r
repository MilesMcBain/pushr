.pushglobalenv <- new.env(parent=emptyenv())

push <- function( fromDT, toDT, toEnv = .GlobalEnv){
  if(ncol(toDT) != ncol(fromDT)){
    stop("data frame dimensions do not match, canot push()")
  }

  #TODO check push can occur safely

  to_name <- deparse(substitute(toDT))
  from_name <- deparse(substitute(fromDT))
  incomming_length <- nrow(fromDT)

  if(!exists(to_name, envir = .pushglobalenv )){
    assign(x = to_name, envir = .pushglobalenv, value = 0)
  }
  #figure out offset to apply to assignment
  offset <- get(to_name, envir = .pushglobalenv)

  #execute push in calling environment

  eval(
    expr = parse( text = paste0("data.table::set(toEnv$",to_name,
                                ",i=(1+", offset, "):", offset+incomming_length,
                                ",j=names(toEnv$",to_name,
                                "),value=fromDT)")
                  ),
    envir = environment()
  )
  #advance offset
  assign(to_name, envir = .pushglobalenv, value = offset+incomming_length)
}

push_initialise <- function(toDT){
  if(!data.table::is.data.table(toDT)) stop("Data needs to be a data.table try data.table() to convert.")
  assign(x=deparse(substitute(toDT)), value = 0, envir = .pushglobalenv)


}

trim_empty <- function(trimDT){

  trim_name <- deparse(substitute(trimDT))

  if(!exists(x = trim_name, envir = .pushglobalenv)){
    stop(paste0("Found no record of data pushed to ", trim_name, ", cannot trim." ))
  }
  offset <- get(trim_name, envir = .pushglobalenv)
  if(offset == 0){
    return(trimDT[0:0,])
  } else {
    return(trimDT[1:offset,])
  }

}

