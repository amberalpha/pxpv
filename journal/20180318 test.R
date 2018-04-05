require(aappd)
aatopselect('aappd')
vin <- 57
ver <- 58
setv(app='ppd',ver=ver)
source('./R/pxos.R')
source('../pkgx/R/splitstep.R')
#parsFun(v=ver,root='../pkgx')

stepcode <- 'pv'

setv(ver=ver)
setreturn(F)

#external dependencies
gett(pcrcd)

#depFun(pre=paste0('px',stepcode),file=paste0('R/px',stepcode,'.R'),verb=F)
getgd('depd')

jomneFun(depd[pre==paste0('px',stepcode)])
