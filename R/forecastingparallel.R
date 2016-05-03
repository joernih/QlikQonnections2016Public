#' @import dplyr
#' @import MARSS
#' @import sqldf
#' @export
prognoseestimering1 <- function(startp,sluttp,sluttpro,tidseriedata,modelltype,variabler,avtaler,telavt)
{
  tidseriedata <- as.data.frame(tidseriedata)
  enkavtale <- avtaler[telavt]
  #timeseriespenswage

  ## For enkeltavtale, beregner prognose basert p ulike modellvarianter
  enkeltavtallemod <- NULL
  for (telmod in modelltype){
    # telmod <- 1
    enkmodell <- modelltype[telmod]
    aaa <- enkavtale
    mmm <- enkmodell

    enkeltavtmod <- enkeltavtestimering(tidseriedata,aaa,mmm,startp,sluttp,sluttpro)
    enkeltavtallemod <- cbind(enkeltavtallemod,cbind(enkeltavtmod,telmod))

  }

  return(enkeltavtallemod)
}

#' @export
prognoseestimering <- function(startp,sluttp,sluttpro,tidseriedata,modelltype,variabler){
  tidseriedata <- as.data.frame(tidseriedata)
  avtaler <- unique(tidseriedata$avtalenr)
  alleavtaler <- NULL

  cl<-makeCluster(length(avtaler), type="FORK", outfile="")
  registerDoParallel(cl)

  alleavtaler <- foreach(telavt=avtaler, .combine = rbind) %dopar% {
    a <- prognoseestimering1(startp,sluttp,sluttpro,tidseriedata,modelltype,variabler,avtaler,telavt)
  }
  stopCluster(cl)
  return(alleavtaler)
}

#' @export
enkeltavtestimering <- function(tidseriedata,aaa,mmm,startp,sluttp,sluttpro){
  ### mmm <- 3
  # MARSS takler ikke tidsserier med for mange siffre. Vi må derfor foreta en normalisering
  normal <- 1000000
  modellmat <- modellmatriser()
  print(paste("Beregner prognose for avtale:",aaa," modelvalg:",mmm,"\n"))
  tsformat <- tidseriedata %>% dplyr::filter(avtalenr==aaa) %>% arrange(observasjonsdato) %>%
    dplyr::select(pensjonslonn) %>% ts(start=startp,end=sluttp,freq=12)/normal
  TT <- length(ts(data=NA,start=startp,end=sluttp,frequency=12))
  TTsteg <- length(ts(data=NA,start=startp,end=sluttpro,frequency=12))-TT

  # Transformerer datasettet for innlesning til MARSS
  tidseriepl <- t(tsformat)

  # Spesifikasjon av modell p? matriseform
  #modelc <- modellmat[[4]]
  modelc <- modellmat[[mmm]]
  modeldim <- dim(modelc$Z)
  n <- modeldim[1]; m <- modeldim[2]

  # Estimering av modell
  MARSSout <- MARSS(tidseriepl,model=modelc[1:8], method="kem")
  MARSSoutcoef <- coef(MARSSout,type="matrix")
  MARSSoutkfss <- MARSSkfss(MARSSout)

  # PROGNOSER
  TTestB <- MARSSoutcoef$B
  TTestQ <- MARSSoutcoef$Q
  TTestu <- MARSSoutcoef$U
  TTestZ <- MARSSoutcoef$Z
  TTestR <- MARSSoutcoef$R
  TTesta <- MARSSoutcoef$A

  # Prognose innenfor dataperioden
     ## Forventet verdi
  forci.mean <- vector()
  eta <- MARSSoutkfss$xtt1

  for(t in 1:TT){
       forci.mean[t] <- TTestZ %*% eta[,t]
  }

  ## Varians
  forci.var <- vector()
  ome <- MARSSoutkfss$Vtt1
  for (t in 1:TT){
       forci.var[t] <- TTestZ%*%ome[t]%*%t(TTestZ) + TTestR
  }

  # Utenfor dataperioden'
  forco.eta <- array(NA,c(m,1,TTsteg))
  forco.phi <- array(NA,c(m,m,TTsteg))
  forco.zeta <- vector()
  forco.psi <- vector()
  TTxtt <- MARSSoutkfss$xtt[,TT]
  TTVtt <- MARSSoutkfss$Vtt[,,TT]

  for (i in 1:TTsteg){
      if (i == 1){
        # Bygget paa realiserte observasjoner
        forco.eta[,,i] = TTestB %*% TTxtt + TTestu
        forco.phi[,,i] = TTestB %*% TTVtt %*% t(TTestB)  + TTestQ
        forco.zeta[i] = TTestZ %*% forco.eta[,,i]
        forco.psi[i] = TTestZ %*% forco.phi[,,i] %*% t(TTestZ) + TTestR
      }
      else {
        k = i -1
        # Bygget paa prediksjoner k-trinn framomver
        forco.eta[,,i] = TTestB %*% forco.eta[,,k] + TTestu
        forco.phi[,,i] = TTestB %*% forco.phi[,,k] %*% t(TTestB) + TTestQ
        forco.zeta[i] = TTestZ %*% forco.eta[,,i]
        forco.psi[i] =  TTestZ %*% forco.phi[,,i] %*% t(TTestZ) + TTestR
      }
  }

  # Matrise for prognoseberegning
    enkprognosen <- cbind(aaa,rep(seq(startp[1],sluttpro[1]),each=12),rep(seq(1,12),times=(TT+TTsteg)/12),
                          tsformat[1:(TT+TTsteg)],
                          rbind(t(rbind(forci.mean, forci.var)),
                                t(rbind(forco.zeta, forco.psi))))

    enkprognos <- cbind(enkprognosen[,c(1,2,3)],enkprognosen[,c(4,5,6)]*normal)

    return(enkprognos)
}
