#' @export
pxpvcncadjFun <-
  function(#*#
    nn='pxsocncd'
    ) {#*#
    # - [ ]  : cumulative adjustment
    getgd(nn)
    x1 <- cumsum(pxsocncd)
    x2 <-
      sweep(x1,
            STAT = as.numeric(coredata(x1[nrow(x1), ])),
            FUN = '-',
            MAR = 2)
    pxpvcncadjd <<-
      setkeyv(setnames(
        melt(data.table(coredata(x2))[, date := index(x2)], id = 'date'),
        old = c('value', 'variable'),
        new = c('adjustby', 'rcode')
      ), c('rcode', 'date'))
    putt(pxpvcncadjd)
}#*#

#' @export
pxpvpvidFun <-
  function(#*#
    nn=c('pxpvcncadjd','pxlrprppd')
    ) {#*#
    # - [ ] rcode id price-paid lastsale pv dfinal
    getgd(nn)
    ilast <- pxlrprppd[, .(lastsale = max(deed_date)), id]
    pru <-
      setkey(setkey(pxlrprppd,rcode)[pxpvcncadjd[,.(unique(rcode))]][ilast, .(price_paid, lastsale, id, rcode), on = c(id = 'id', deed_date =
                                                                                                                         'lastsale')][rcode != 'drop'], rcode, lastsale)
    stopifnot(all(pru[, unique(rcode)] %in% pxpvcncadjd[, unique(rcode)])) #this enforced on prev. line
    x4 <-
      setkey(unfactordt(copy(pxpvcncadjd))[, .(date = as.character(date), rcode =
                                                 rcode, adjustby)], rcode, date)

    pxpvpvidd <<-
      unique(setkey(x4[pru, roll = T, rollends = T][, pv := price_paid * exp(-adjustby)][, .(rcode,
                                                                                             id,
                                                                                             price_paid,
                                                                                             lastsale = date,
                                                                                             pv,
                                                                                             dfinal = pxpvcncadjd[, max(date)])]
                    , id)
      )

    putt(pxpvpvidd)
  }#*#

#' @export
pxpvpvrcFun <-
  function(#*#
    nn='pxpvpvidd'
    ) {#*#
    # - [ ] rcode dfinal PV
    getgd(nn)
    pxpvpvrcd <<- setkey(pxpvpvidd[, .(PV = sum(pv)), 'rcode,dfinal'], rcode)
    putt(pxpvpvrcd)
  }#*#

#' @export
pxpvhvrcFun <-
  function(#*#
    nn=c('pxpvcncadjd','pxpvpvrcd')
    ) {#*#
    # - [ ] rcode date HV
    getgd(nn)
    x1 <- pxpvpvrcd[pxpvcncadjd, on = c(rcode = 'rcode')][, HV := PV * exp(adjustby)]
    pxpvhvrcd <<- x1[, .(rcode, date, HV)]
    putt(pxpvhvrcd)
  }#*#

#' @export
pxpvppm1Fun <-
  function(#*#
    nn=c('pxpvpvidd','pxjoev2d','pxjoj1d',pxlrch1d='pxlrch1d')
    ,#*#
    pcl=grepstring(regpcode(c('WC','EC','W1','SW1','SW3','SW7')))
    ,#*#
    minppm2=8e3
    ) {#*#
    # - [ ] join epc and lr with pv by id
    getgd(nn)
    x1 <- pxpvpvidd[,.(rcode=max(rcode),price_paid=max(price_paid),lastsale=max(lastsale),pv=max(pv),dfinal=max(dfinal)),id] #13m unique
    x2 <- pxjoev2d[,.(brn,median,gro)] #7.3m unique brn
    x3 <- pxjoj1d[!is.na(brn),.(brn,a0,rc12,id)] #pxjoj1d[,!is.na(brn)] #7.45m
    x4 <- x1[x2[x3,on=c(brn='brn')],on=c(id='id')]
    stopifnot(all(x4[,id]%in%pxlrch1d[,id])) #this ignores the fact that only repeats now feature in many tables
    x5 <- pxlrch1d[x4,on=c(id='id')][!is.na(median)] #not sure how na median gets here, but .024 are
    x6 <- x5[!(grepl(pcl,id)&(pv/median)<minppm2&estate=='L')]
    pxpvppm1d <<- x6
    putt(pxpvppm1d)
  }#*#

#' @export
pxpvppm2Fun <-
  function(#*#
    nn=c('pxpvppm1d')
    ) {#*#
    # - [ ] rcode  cat catvalue variable value : tibble of sub-aggregated N and D of ratio
    getgd(nn)
    x1 <- c('type','estate','newbuild')
    x2 <- lapply(as.list(x1),c,'rcode')
    x3 <- lapply(x2,ppm2a,ppm1d=pxpvppm1d)
    for(i in seq_along(x3)) {
      x3[[i]][,cat:=x1[i]]
      setnames(x3[[i]],old=x1[i],new='catvalue')
      x3[[i]] <- melt(x3[[i]][,n:=as.double(n)],id.var=c('rcode','cat','catvalue'))
    }
    pxpvppm2d <<- rbindlist(x3)
    putt(pxpvppm2d)
  }#*#

#' @export
pxpvppmxFun <-
  function(#*#
    nn=c('pxpvppm1d','pxzozoned')
    ) {#*#
    # - [ ] rcode ppm2 rank col :
    getgd(nn)
    x1 <- pxpvppm1d[!is.na(pv*median),.(price=sum(pv),m2=sum(median,na.rm=T)),rcode][,.(ppm2=price/m2),rcode][,rank:=(rank(ppm2)-1)/(.N-1)]
    miss <- as.list(setdiff(pxzozoned[,unique(rc)],x1[,rcode]))
    for(i in seq_along(miss)) {
      area <- substr(miss[[i]],1,3)
      near <- x1[grepl(area,rcode)]
      miss[[i]] <- data.table(rcode=miss,ppm2=near[,mean(ppm2)],rank=near[,mean(rank)+min(diff(rank))/2])
    }
    pxpvppmxd <<- setkey(unique(rbind(x1,rbindlist(miss))[nchar(rcode)<=9][,rcode:=unlist(rcode)]),rank)[,col:=gg_colour_hue(.N*1.1)[1:.N]] #this line hacked 2019-10 eg nchar(rcode) and unique
    putt(pxpvppmxd)
}#*#

#' @export
pxpvppm2tFun <-
  function(#*#
    nn=c('pxpvppmxd','pxpvcncadjd')
    ) {#*#
    # - [ ]  rcode ppm2 rank col date adjustby ppm2t : timeseries ppm2
    getgd(nn)
    rcx <- intersect(unfactordt(pxpvcncadjd)[,sort(unique(rcode))],pxpvppmxd[,sort(unique(rcode))])
    pxpvppm2td <<- pxpvppmxd[pxpvcncadjd,on=c(rcode='rcode')][,ppm2t:=exp(adjustby)*ppm2]
    putt(pxpvppm2td)
  }#*#

#' @export
pxpvrcuniFun <-
  function(#*#
    nn=c('pxoscpd','pxpvppm1d')
    ) {#*#
  # - [ ] rc12,EE,NN,EENN,rcunique : degenerate codes mapping to unique
  getgd(nn)
  x1 <- setkey(pxoscpd,rc)
  x2 <- pxpvppm1d[,.(lastsale=max(lastsale)),rc12]
  x3 <- x1[x2][,EENN:=paste0(EE,'|',NN)]
  x4 <- setkey(x3,EENN,lastsale)[x3[,max(lastsale),EENN],mult='last']
  x5 <- x4[x3[,.(EENN,rc)],on=c(EENN='EENN')][!is.na(EE),.(rcunique=rc,rc12=i.rc,EENN,EE,NN)]
  pxpvrcunid <<- x5
  putt(pxpvrcunid)
}#*#
pxpvrcunichk <- #*#
  function(#*#
  ) {#*#
  # - [ ] <comment>
  getgd(paste0((gsub('chk','',match.call()[[1]])),'d'))#*#
  getgd('pxoscpd')#*#
  res <- nrow(pxpvrcunid[pxpvrcunique==rc12,])==nrow(unique(pxpvrcunid[,.(EE,NN)]))#*#
  res & all(c('rcunique','rc12','EENN','EE','NN')%in%names(pxpvrcunid))#*#
}#*#
