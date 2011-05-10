setGeneric("ggplot", function(data, ...)
	standardGeneric("ggplot"))

setMethod("ggplot", signature("FLQuants"), function(data, ...){
  dat<-as.data.frame(data)
  dat$cohort<-dat$year-dat$age
  ggplot(dat,...)})

setMethod("ggplot", signature("FLQuant"), function(data, ...){
  dat       <-as.data.frame(data)
  dat$cohort<-dat$year-dat$age
  ggplot(dat,...)})

setMethod("ggplot", signature(data="FLComp"), function(data,...){
  dat<-as.data.frame(data)
  dat$cohort<-dat$year-dat$age
  ggplot(dat,...)})

setMethod("ggplot", signature(data="FLCohort"), function(data,...){
  dat<-as.data.frame(data)
  dat$cohort<-dat$year-dat$age
  ggplot(dat,...)})

