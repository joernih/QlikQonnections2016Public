#' @import ggplot2
#' @export
plotenkeltavtestimering <- function(tidseriedata,aaa,mmm,startp,sluttp,sluttpro){

  este <- enkeltavtestimeringb(tidseriedata,aaa,mmm,startp,sluttp,sluttpro)

  estenkavtmodell <- cbind(este,mmm)

  colnames(estenkavtmodell) <- c("customer","year","month","wagelevel (pension)","wageforcasting","wageforcastingvariance","modelchoice")

  estenkavtmodtable <- data.frame(estenkavtmodell) %>% left_join(premtable,by=c("customer"="avtale"))

  dfggplot2 <- estenkavtmodtable

  startper <- paste(dfggplot2$year[1],"/",
                    dfggplot2$month[1],"/1",sep='')

  endper <- paste(dfggplot2$year[dim(dfggplot2)[1]],"/",
                  dfggplot2$month[dim(dfggplot2)[1]],"/1",sep='')

  dfggplot2per <- dfggplot2 %>%
    dplyr::mutate(periode=seq(as.Date(startper),
                              as.Date(endper), "month")) %>%
    dplyr::mutate(highlevel=ifelse(year>2015,wageforcasting*(1+0.10),NA)) %>%
    dplyr::mutate(lowlevel=ifelse(year>2015,wageforcasting*(1-0.10),NA))


  custplot <- ggplot(dfggplot2per,aes(y=wagelevel..pension.,x=periode)) +
    geom_line(colour = "red") +
    geom_line(colour = "red") +
    ylab("Billions (Norwegian kroner)") +
    xlab("Year") +
    geom_line(aes(y=wageforcasting)) +
    geom_line(aes(y=highlevel)) +
    geom_line(aes(y=lowlevel)) +
    theme_bw()

  return(custplot)

}


#' @export
custtabpayhtml <- function(tidseriedata,aaa,mmm,startp,sluttp,sluttpro){
  #ttt <- head(iris)
  load("data/premtable.rda")

  este <- enkeltavtestimeringb(tidseriedata,aaa,mmm,startp,sluttp,sluttpro)

  estenkavtmodell <- cbind(este,mmm)

  colnames(estenkavtmodell) <- c("customer","year","month","wagelevel (pension)","wageforcasting","wageforcastingvariance","modelchoice")

  estenkavtmodtable <- data.frame(estenkavtmodell) %>% left_join(premtable,by=c("customer"="avtale"))

  prrate <- c(t(estenkavtmodtable[dim(estenkavtmodtable)[1],8]))
  forwagecust <-  c(t(estenkavtmodtable[dim(estenkavtmodtable)[1],5]))
  cust <- estenkavtmodtable$customer[1]


  amount <- c(round(forwagecust),prrate,
                 rep(round(prrate*forwagecust/6),each=6),
                 round(prrate*forwagecust))

  tableonecustomer <- data.frame(Name=c('Forecast (wage)',
                                           'Premium Rate',
                                           paste('Payment',seq(1,6)),
                                           'Total'),
                                    Amount=amount) %>%
  dplyr::mutate(Amount=formattable::accounting(Amount,digits=2))


  htmlcustompay <- htmlTable::htmlTable(tableonecustomer,
                                           header=c(paste("Customer-group:",cust),"Amount"),
                                           align=c("l","c"),
                                           rnames=FALSE,
                                           total=TRUE)

  #ttt <- paste(R2HTML::HTML(htmlcustompay,file="C:\\Users\Helge\Google Drive\Dropbox\Jobb\SPK"))

  return(htmltools::HTML(htmlcustompay))

}


#' @import ggplot2
#' @export
plotenkeltavtestimering2 <- function(este,mmm){


  estenkavtmodell <- cbind(este,mmm)

  colnames(estenkavtmodell) <- c("customer","year","month","wagelevel (pension)","wageforcasting","wageforcastingvariance","modelchoice")

  estenkavtmodtable <- data.frame(estenkavtmodell) %>% left_join(premtable,by=c("customer"="avtale"))

  dfggplot2 <- estenkavtmodtable

  startper <- paste(dfggplot2$year[1],"/",
                    dfggplot2$month[1],"/1",sep='')

  endper <- paste(dfggplot2$year[dim(dfggplot2)[1]],"/",
                  dfggplot2$month[dim(dfggplot2)[1]],"/1",sep='')

  dfggplot2per <- dfggplot2 %>%
    dplyr::mutate(periode=seq(as.Date(startper),
                              as.Date(endper), "month")) %>%
    dplyr::mutate(highlevel=ifelse(year>2015,wageforcasting*(1+0.10),NA)) %>%
    dplyr::mutate(lowlevel=ifelse(year>2015,wageforcasting*(1-0.10),NA))


  custplot <- ggplot(dfggplot2per,aes(y=wagelevel..pension.,x=periode)) +
    geom_line(colour = "red") +
    geom_line(colour = "red") +
    ylab("Billions (Norwegian kroner)") +
    xlab("Year") +
    geom_line(aes(y=wageforcasting)) +
    geom_line(aes(y=highlevel)) +
    geom_line(aes(y=lowlevel)) +
    theme_bw()

  return(custplot)

}


#' @export
custtabpayhtml2 <- function(este,mmm){
  #ttt <- head(iris)
  load("data/premtable.rda")

  estenkavtmodell <- cbind(este,mmm)

  colnames(estenkavtmodell) <- c("customer","year","month","wagelevel (pension)","wageforcasting","wageforcastingvariance","modelchoice")

  estenkavtmodtable <- data.frame(estenkavtmodell) %>% left_join(premtable,by=c("customer"="avtale"))

  prrate <- c(t(estenkavtmodtable[dim(estenkavtmodtable)[1],8]))
  forwagecust <-  c(t(estenkavtmodtable[dim(estenkavtmodtable)[1],5]))
  cust <- estenkavtmodtable$customer[1]


  amount <- c(round(forwagecust),prrate,
              rep(round(prrate*forwagecust/6),each=6),
              round(prrate*forwagecust))

  tableonecustomer <- data.frame(Name=c('Forecast (wage)',
                                        'Premium Rate',
                                        paste('Payment',seq(1,6)),
                                        'Total'),
                                 Amount=amount) %>%
    dplyr::mutate(Amount=formattable::accounting(Amount,digits=2))


  htmlcustompay <- htmlTable::htmlTable(tableonecustomer,
                                        header=c(paste("Customer-group:",cust),"Amount"),
                                        align=c("l","c"),
                                        rnames=FALSE,
                                        total=TRUE)


  return(htmltools::HTML(htmlcustompay))

}
