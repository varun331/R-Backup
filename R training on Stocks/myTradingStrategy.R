myWrapper <- function(n) {
  eW <- eWrapper(NULL)  # use basic template
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_,2),nc=2),0),
                                            .Dimnames=list(NULL,c("LastSize","Last")))),n))
  
  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) 
  {
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- msg[2] #as.numeric(msg[2])
    data <- eW$get.Data("data") #[[1]]  # list position of symbol (by id == msg[2])
    attr(data[[id]],"index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    if(tickType == .twsTickType$LAST) {
      data[[id]][nr.data,2] <- msg[4]
    }
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  eW$tickSize  <- function(curMsg, msg, timestamp, file, ...) 
  { 
    data <- eW$get.Data("data")
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- as.numeric(msg[2])
    attr(data[[id]],"index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    if(tickType == .twsTickType$LAST_SIZE) {
      data[[id]][nr.data,1] <- msg[4]
    } 
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  return(eW)
}

myCALLBACK <- function (twsCon, eWrapper, timestamp, file, playback = 1, ...)
{
  if (missing(eWrapper))
    eWrapper <- eWrapper()
  names(eWrapper$.Data$data) <- eWrapper$.Data$symbols
  con <- twsCon[[1]]
  if (inherits(twsCon, "twsPlayback")) {
    sys.time <- NULL
    while (TRUE) {
      if (!is.null(timestamp)) {
        last.time <- sys.time
        sys.time <- as.POSIXct(strptime(paste(readBin(con,
                                                      character(), 2), collapse = " "), timestamp))
        if (!is.null(last.time)) {
          Sys.sleep((sys.time - last.time) * playback)
        }
        curMsg <- .Internal(readBin(con, "character",
                                    1L, NA_integer_, TRUE, FALSE))
        if (length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, format(sys.time,
                                                 timestamp), file, ...)
      }
      else {
        curMsg <- readBin(con, character(), 1)
        if (length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, timestamp,
                   file, ...)
        if (curMsg == .twsIncomingMSG$REAL_TIME_BARS)
          Sys.sleep(5 * playback)
      }
    }
  }
  else {
    while (TRUE) {
      socketSelect(list(con), FALSE, NULL)
      curMsg <- .Internal(readBin(con, "character", 1L,
                                  NA_integer_, TRUE, FALSE))
      if (!is.null(timestamp)) {
        processMsg(curMsg, con, eWrapper, format(Sys.time(),
                                                 timestamp), file, ...)
      }
      else {
        processMsg(curMsg, con, eWrapper, timestamp,
                   file, ...)
      }
      if (!any(sapply(eWrapper$.Data$data, is.na)))
        return(do.call(rbind, lapply(eWrapper$.Data$data,
                                     as.data.frame)))
    }
  }
}

runMyTradingStrategy <- function()
{
  stk <- twsSTK("MSFT")
  targetPrice <- 41.05
  while(TRUE) {
    lastPrice <- reqMktData(tws, stk, eventWrapper=myWrapper(1),CALLBACK=myCALLBACK)$Last
    if(lastPrice > targetPrice) {
      # BUY
      print(paste("Price: ", lastPrice, ", **BUYING 10**"))
      id <- reqIds(tws)
      IBrokers:::.placeOrder(tws, stk, twsOrder(1053, "BUY", "10", "MKT"))
    }
    # Sell at 10% trailing stop
    else if (lastPrice < 41.05*.9) {
      print(paste("Price: ", lastPrice, ", **SELLING 10**"))
      id <- reqIds(tws)
      IBrokers:::.placeOrder(tws, stk, twsOrder(1054, "SELL", "10", "MKT"))
    }
    else {
      print(paste("Price: ", lastPrice))
    }
  }
}
