Init.function<-function(dir=data.path)
{
  SMS.control<<-read.FLSMS.control(dir=dir,file='SMS.dat')
  sp.names<<-SMS.control@species.names
  nsp<<-SMS.control@no.species
  for (ii in (1:nsp)) if (SMS.control@species.info[ii,'predator']!=2) {first.VPA<<-ii; break;} #first VPA  species number
  for (ii in (1:nsp)) if (SMS.control@species.info[ii,'predator']==0) {npr<<-ii-1; break;} # number of predators

  fa<<-SMS.control@first.age
  sp.other.names<<-c('Other',sp.names)
}

change_stock<-function(stock=my.stock.dir) {
  my.stock.dir<<-stock
  SMSdir(stock=my.stock.dir)
  setwd(data.path)
  Init.function()
}