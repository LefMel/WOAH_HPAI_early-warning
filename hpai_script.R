# Data Cases
library(EVI)
library(googledrive)
library(XML)
library(rlist)
#1gdWDhELYMU0lmCQz3po3Ydb7Eb1gF7j_NCs_xluCXQI
# 1QkNtgEm2otoFGNFJ82gUH7TVfZwj8Ji-DJ_98VzayQc

drive_deauth()
temp <- tempfile(tmpdir = getwd(), fileext = ".zip")
dl <- drive_download(
  as_id("1QkNtgEm2otoFGNFJ82gUH7TVfZwj8Ji-DJ_98VzayQc"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())

link = paste("file:///",out[1], sep="")

# Works 
tables <- readHTMLTable(link)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
new_data = data.frame(tables[1])
new_data = new_data[-1,-1]
headers = c("Date",	"Global", "Africa",
            "America", "Asia_Pacific", "Europe")
colnames(new_data) = headers




#Global - EVI
load("EVI_Global_old")
EVI_Global_old = EVI_Global
EVI_Global = deviant_update(new_cases=as.numeric(new_data$Global[which(new_data$Global!="")][6707:7210]), origin = "2004-12-31", EVI_input = EVI_Global_old, r_a = 30, method = "EVI", past=90, lag_max = 15)
save(EVI_Global, file="EVI_Global")
#Global - cEVI
load("cEVI_Global_old")
cEVI_Global_old = cEVI_Global
cEVI_Global = deviant_update(new_cases=as.numeric(new_data$Global[which(new_data$Global!="")][6707:7210]), origin = "2004-12-31", EVI_input = cEVI_Global_old, r_a = 30, method = "cEVI",  past=90, lag_max = 15)
save(cEVI_Global, file="cEVI_Global")

#America - EVI
load("EVI_America_old")
EVI_America_old = EVI_America
EVI_America = deviant_update(new_cases=as.numeric(new_data$America[which(new_data$America!="")][6707:7210]), origin = "2004-12-31", EVI_input = EVI_America_old, r_a = 30, method = "EVI", past=90, lag_max = 15)
save(EVI_America, file="EVI_America")
#America - cEVI
load("cEVI_America_old")
cEVI_America_old = cEVI_America
cEVI_America = deviant_update(new_cases=as.numeric(new_data$America[which(new_data$America!="")][6707:7210]), origin = "2004-12-31", EVI_input = cEVI_America_old, r_a = 30, method = "cEVI",  past=90, lag_max = 15)
save(cEVI_America, file="cEVI_America")

#Africa - EVI
load("EVI_Africa_old")
EVI_Africa_old = EVI_Africa
EVI_Africa = deviant_update(new_cases=as.numeric(new_data$Africa[which(new_data$Africa!="")][6707:7210]), origin = "2004-12-31", EVI_input = EVI_Africa_old, r_a = 30, method = "EVI", past=90, lag_max = 15)
save(EVI_Africa, file="EVI_Africa")
#Africa - cEVI
load("cEVI_Africa_old")
cEVI_Africa_old = cEVI_Africa
cEVI_Africa = deviant_update(new_cases=as.numeric(new_data$Africa[which(new_data$Africa!="")][6707:7210]), origin = "2004-12-31", EVI_input = cEVI_Africa_old, r_a = 30, method = "cEVI",  past=90, lag_max = 15)
save(cEVI_Africa, file="cEVI_Africa")

#Europe - EVI
load("EVI_Europe_old")
EVI_Europe_old = EVI_Europe
EVI_Europe = deviant_update(new_cases=as.numeric(new_data$Europe[which(new_data$Europe!="")][6707:7210]), origin = "2004-12-31", EVI_input = EVI_Europe_old, r_a = 30, method = "EVI", past=90, lag_max = 15)
save(EVI_Europe, file="EVI_Europe")
#Europe - cEVI
load("cEVI_Europe_old")
cEVI_Europe_old = cEVI_Europe
cEVI_Europe = deviant_update(new_cases=as.numeric(new_data$Europe[which(new_data$Europe!="")][6707:7210]), origin = "2004-12-31", EVI_input = cEVI_Europe_old, r_a = 30, method = "cEVI",  past=90, lag_max = 15)
save(cEVI_Europe, file="cEVI_Europe")

#Asia_Pacific - EVI
load("EVI_Asia_Pacific_old")
EVI_Asia_Pacific_old = EVI_Asia_Pacific
EVI_Asia_Pacific = deviant_update(new_cases=as.numeric(new_data$Asia_Pacific[which(new_data$Asia_Pacific!="")][6707:7210]), origin = "2004-12-31", EVI_input = EVI_Asia_Pacific_old, r_a = 30, method = "EVI", past=90, lag_max = 15)
save(EVI_Asia_Pacific, file="EVI_Asia_Pacific")
#Asia_Pacific - cEVI
load("cEVI_Asia_Pacific_old")
cEVI_Asia_Pacific_old = cEVI_Asia_Pacific
cEVI_Asia_Pacific = deviant_update(new_cases=as.numeric(new_data$Asia_Pacific[which(new_data$Asia_Pacific!="")][6707:7210]), origin = "2004-12-31", EVI_input = cEVI_Asia_Pacific_old, r_a = 30, method = "cEVI",  past=90, lag_max = 15)
save(cEVI_Asia_Pacific, file="cEVI_Asia_Pacific")

load("EVI_Global")
load("cEVI_Global")
load("EVI_America")
load("cEVI_America")
load("EVI_Africa")
load("cEVI_Africa")
load("EVI_Europe")
load("cEVI_Europe")
load("EVI_Asia_Pacific")
load("cEVI_Asia_Pacific")

figure_global = evirlap(Index1=EVI_Global, Index2=cEVI_Global, region="Global", ln=F)
png(filename = "figure_global.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(figure_global)
dev.off()

figure_africa = evirlap(Index1=EVI_Africa, Index2=cEVI_Africa, region="Africa", ln=F)
png(filename = "figure_africa.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(figure_africa)
dev.off()

figure_america = evirlap(Index1=EVI_America, Index2=cEVI_America, region="America", ln=F)
png(filename = "figure_america.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(figure_america)
dev.off()


figure_asia_pacific = evirlap(Index1=EVI_Asia_Pacific, Index2=cEVI_Asia_Pacific, region="Asia-Pacific", ln=F)
png(filename = "figure_asia_pacific.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(figure_asia_pacific)
dev.off()

figure_europe = evirlap(Index1=EVI_Europe, Index2=cEVI_Europe, region="Europe", ln=F)
png(filename = "figure_europe.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(figure_europe)
dev.off()

# Detect
alert_Europe <- detect_final_alerts(evi_df = EVI_Global, cevi_df = cEVI_Global)

# Sensitivity Analysis


deviant <- function (new_cases, cum = FALSE, r_a = 7, r = 0.2, lag_max = 30, 
                     past = 364/2, method = "EVI", origin="1970-01-01") 
{
  if (method == "EVI") {
    start_time = Sys.time()
    start_cases = 14
    lag_1 = 7
    c_1 = 0.01
    w_s = 7
    if (cum == TRUE) 
      new_cases = c(new_cases[1], diff(new_cases))
    cases = mova(new_cases, r_a)
    roll = rollsd(cases[1:start_cases], lag_1)
    ev = evi(roll)
    ind = indic(evi = ev, cut = c_1, cases = cases[1:start_cases], 
                method = "EVI")
    status = status(cases[1:start_cases], r)
    ppv = rep(NA, length(cases))
    npv = rep(NA, length(cases))
    lag_all = rep(NA, start_cases)
    c_all = rep(NA, start_cases)
    se_all = rep(NA, start_cases)
    sp_all = rep(NA, start_cases)
    lag_all[1:start_cases] = lag_1
    c_all[1:start_cases] = c_1
    for (i in (start_cases + 1):length(cases)) {
      case_t = cases[max(1, (i - past)):i]
      lag_s = seq(lag_1, min(lag_max, (length(case_t) - 
                                         1)), 1)
      c_s = seq(0.01, 0.5, 0.01)
      all_lag = NA
      all_cut = NA
      all_se = NA
      all_sp = NA
      for (j in lag_s) {
        roll_t = rollsd(case_t, j)
        ev_t = evi(roll_t)
        for (l in c_s) {
          evicut_t = evifcut(evi = ev_t, cases = case_t, 
                             cut = l, r = r, method = "EVI")
          new_j = j
          new_l = l
          new_se = evicut_t$sens
          new_sp = evicut_t$spec
          all_lag[[length(all_lag) + 1]] <- new_j
          all_cut[[length(all_cut) + 1]] <- new_l
          all_se[[length(all_se) + 1]] <- new_se
          all_sp[[length(all_sp) + 1]] <- new_sp
        }
      }
      sesp = as.data.frame(cbind(all_lag, all_cut, all_se, 
                                 all_sp))
      index = which.max(sesp$all_se + sesp$all_sp - 1)
      print(i)
      print(sesp[index, ])
      lag_n = sesp$all_lag[index]
      c_n = sesp$all_cut[index]
      roll_n = rollsd(case_t, lag_n)
      ev_n = evi(roll_n)
      ind_n = indic(evi = ev_n, cut = c_n, cases = case_t, 
                    method = "EVI")
      evicut_n = evifcut(evi = ev_n, cases = case_t, cut = c_n, 
                         r = r, method = "EVI")
      roll = c(roll, roll_n[length(ind_n)])
      ev = c(ev, ev_n[length(ind_n)])
      ind = c(ind, ind_n[length(ind_n)])
      lag_all = c(lag_all, lag_n)
      c_all = c(c_all, c_n)
      se_all = c(se_all, all_se[index])
      sp_all = c(sp_all, all_sp[index])
      ppv[i] = evicut_n$prev * all_se[index]/(evicut_n$prev * 
                                                all_se[index] + (1 - evicut_n$prev) * (1 - all_sp[index]))
      npv[i] = (1 - evicut_n$prev) * all_sp[index]/((1 - 
                                                       evicut_n$prev) * all_sp[index] + evicut_n$prev * 
                                                      (1 - all_se[index]))
    }
    Days=as.Date(1:length(cases),  origin="1970-01-01") 
    EVI = ev
    Cases = cases
    Index = ind
  }
  if (method == "cEVI") {
    start_time = Sys.time()
    start_cases = 18
    lag_1 = 3
    c_1 = 0.001
    w_s = 7
    if (cum == TRUE) {
      new_cases = c(new_cases[1], diff(new_cases))
    }
    cases = mova(new_cases, r_a)
    cev = cEVI_fun(cases = cases[1:(start_cases)], lag_n = lag_1, 
                   c_n = c_1)
    ind = indic(cevi = cev, cases = cases[1:start_cases], 
                method = "cEVI")
    status = status(cases[1:start_cases], r)
    ppv = rep(NA, length(cases))
    npv = rep(NA, length(cases))
    lag_all = rep(NA, start_cases)
    c_all = rep(NA, start_cases)
    se_all = rep(NA, start_cases)
    sp_all = rep(NA, start_cases)
    lag_all[1:start_cases] = lag_1
    c_all[1:start_cases] = c_1
    for (i in (start_cases + 1):length(cases)) {
      i_n <- max(1, (i - past)):i
      case_t = cases[i_n]
      lag_s = seq(lag_1, min(lag_max, (length(i_n) - length(i_n)/2 - 
                                         4)), 2)
      c_s = seq(0.001, 0.5, 0.06)
      all_lag = NA
      all_cut = NA
      all_se = NA
      all_sp = NA
      for (l in c_s) {
        for (j in lag_s) {
          cevi <- rep(NA, length(case_t))
          for (k in (j + 1):(length(case_t) - (j + 1))) {
            enu = mean(case_t[(k + 2):(k + j + 1)] - 
                         case_t[(k):(k - (j - 1))], na.rm = T)
            den1 = sd(case_t[(k):(k - (j - 1))])^2/(length(case_t[(k):(k - 
                                                                         (j - 1))]))
            den2 = sd(case_t[(k + 2):(k + j + 1)])^2/(length(case_t[(k + 
                                                                       2):(k + j + 1)]))
            teststat = enu/sqrt(den1 + den2)
            Nn = length((k + 1):(k + j))
            cevi[k + j + 1] <- as.numeric((1 - stats::pt(q = teststat, 
                                                         df = Nn)) <= l)
          }
          evicut_t <- evifcut(cevi = cevi, cases = case_t, 
                              r = r, method = "cEVI")
          all_lag[[length(all_lag) + 1]] <- j
          all_cut[[length(all_cut) + 1]] <- l
          all_se[[length(all_se) + 1]] <- evicut_t[[1]]
          all_sp[[length(all_sp) + 1]] <- evicut_t[[2]]
        }
      }
      sesp = as.data.frame(cbind(all_lag, all_cut, all_se, 
                                 all_sp))
      index = which.max(sesp$all_se + sesp$all_sp - 1)
      print(i)
      print(sesp[index, ])
      lag_n = sesp$all_lag[index]
      c_n = sesp$all_cut[index]
      cevi_n = cEVI_fun(cases = case_t, lag_n = lag_n, 
                        c_n = c_n)
      ind_n = indic(cevi = cevi_n, cases = case_t, method = "cEVI")
      evicut_n = evifcut(cevi = cevi, cases = case_t, r = r, 
                         method = "cEVI")
      cev = c(cev, cevi_n[length(ind_n)])
      ind = c(ind, ind_n[length(ind_n)])
      lag_all = c(lag_all, lag_n)
      c_all = c(c_all, c_n)
      se_all = c(se_all, all_se[index])
      sp_all = c(sp_all, all_sp[index])
      ppv[i] = evicut_n$prev * all_se[index]/(evicut_n$prev * 
                                                all_se[index] + (1 - evicut_n$prev) * (1 - all_sp[index]))
      npv[i] = (1 - evicut_n$prev) * all_sp[index]/((1 - 
                                                       evicut_n$prev) * all_sp[index] + evicut_n$prev * 
                                                      (1 - all_se[index]))
    }
    
    Days=as.Date(1:length(cases),  origin="1970-01-01") 
    EVI = cev
    Cases = cases
    Index = ind
  }
  EVI_out = as.data.frame(cbind(Days, EVI, Cases, Index, ppv, 
                                npv, lag_all, c_all, se_all, sp_all, new_cases))
  EVI_output = EVI_out
  EVI_output <<- (EVI_output)
  end_time = Sys.time()
  time_elapsed = end_time - start_time
  print(time_elapsed)
  return(EVI_output)
}


deviant_update=function(all_cases=NA, new_cases=NA,
                        EVI_input, cum = FALSE, r_a=7,
                        r=0.2, lag_max=30, method="EVI", origin="1970-01-01", past=364/2){
  #source("mova.r")
  #source("medvol.r")
  #source("evi.r")
  #source("evifcut.r")
  #source("indic.r")
  #source("status.r")
  #source("rollsd.r")
  
  if(method=="EVI"){
    start_cases=14
    lag_1=7
    c_1=0.01
    w_s =7
  }else if (method=="cEVI"){
    start_cases=18
    lag_1=3
    c_1=0.001
    w_s=7
  }
  
  
  if (cum == TRUE) new_cases = c(new_cases[1], diff(new_cases))
  
  if ((!is.na(sum(all_cases)) & !is.na(sum(new_cases))) | (is.na(sum(all_cases)) & is.na(sum(new_cases))))
    stop("Please provide either new cases or all cases (old+new) as input")
  
  if (!is.na(sum(all_cases))) {
    cases = mova(c(all_cases), r_a)
  }
  if (!is.na(sum(new_cases))) {
    cases = mova(c(EVI_input$new_cases, new_cases), r_a)
  }
  
  roll=rollsd(cases[1:start_cases],lag_1)
  ev=evi(roll)
  
  if(method=="EVI"){
    ind=indic(evi = ev, cut = c_1, cases = cases[1:start_cases], method = method)
  }else if (method=="cEVI"){
    cevi=cEVI_fun(cases = cases[1:(start_cases)],lag_n = lag_1, c_n = c_1)
    ind=indic(cevi=cevi, cases=cases[1:start_cases], method="cEVI")
  }
  
  status=status(cases[1:start_cases],r)
  ppv=rep(NA, length(cases))
  npv=rep(NA, length(cases))
  lag_all=rep(NA, start_cases)
  c_all=rep(NA, start_cases)
  se_all=rep(NA, start_cases)
  sp_all=rep(NA, start_cases)
  lag_all[1:start_cases]=lag_1
  c_all[1:start_cases]=c_1
  diff= length(cases)-(nrow(EVI_input) +1)
  
  for (i in (nrow(EVI_input)+1): length(cases)){
    
    case_t = cases[max(1, (i - past)):i]
    #case_t=cases[max(1,(i-33)):i]
    #lag_s=7
    lag_s=seq(lag_1,min(lag_max,(length(case_t)-1)), 1)
    #lag_s=seq(lag_1,min(length(case_t),50), 1)
    c_s=seq(0.01,0.5, 0.01)
    #all_j=NA
    
    all_lag=NA
    all_cut=NA
    all_se=NA
    all_sp=NA
    
    if(method=="EVI"){
      
      for (j in lag_s){
        roll_t=rollsd(case_t,j)
        ev_t=evi(roll_t)
        for (l in c_s){
          evicut_t=evifcut(evi = ev_t, cases = case_t, cut = l, r = r,method = "EVI")
          new_j=j
          new_l=l
          new_se=evicut_t$sens
          new_sp=evicut_t$spec
          all_lag[[length(all_lag) + 1]] <- new_j
          all_cut[[length(all_cut) + 1]] <- new_l
          all_se[[length(all_se) + 1]] <- new_se
          all_sp[[length(all_sp) + 1]] <- new_sp
        }
      }
      
    }
    
    if(method=="cEVI"){
      i_n <- max(1, (i - past)):i
      case_t=cases[i_n]
      lag_s = seq(lag_1, min(lag_max, (length(i_n) - length(i_n)/2 - 
                                         4)), 2)
      c_s=seq(0.001,0.5, 0.06)
      all_lag<-all_cut<-all_se<-all_sp<-NA
      
      
      for (l in c_s) {
        for (j in lag_s) {
          # roll_t <- rollsd(case_t,j)
          #  ev_t <- evi(roll_t)
          cevi <- rep(NA, length(case_t))
          for(k in (j+1):(length(case_t)-(j+1))){
            enu=mean(case_t[(k+2):(k+j+1)]-case_t[(k):(k-(j-1))],na.rm = T)
            den1=sd(case_t[(k):(k-(j-1))])^2/(length(case_t[(k):(k-(j-1))]))
            den2=sd(case_t[(k+2):(k+j+1)])^2/(length(case_t[(k+2):(k+j+1)]))
            teststat=enu/sqrt(den1+den2)
            Nn=length((k+1):(k+j))
            cevi[k+j+1]<-as.numeric((1-pt(q = teststat,df = Nn))<=l)
          }
          evicut_t <- evifcut(cevi=cevi,cases = case_t, r = r,method = "cEVI")
          all_lag[[length(all_lag) + 1]] <- j
          all_cut[[length(all_cut) + 1]] <- l
          all_se[[length(all_se) + 1]] <- evicut_t[[1]]
          all_sp[[length(all_sp) + 1]] <- evicut_t[[2]]
        }
      }
    }
    
    
    sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))
    
    
    #Select the row with the right window and cut
    index=which.max(sesp$all_se+sesp$all_sp-1)
    
    #index=sesp[which(sesp$all_sp>0.80),]
    #index=which.max(index$all_se)
    #index=which(sesp$all_se==1 & sesp$all_sp>=0.95),1)
    #if (i>40)
    #   {index1=sesp[which(sesp$all_sp>0.95),]
    #  index=which.max(index1$all_se)
    #   }
    #else
    #{index=which.max(sesp$all_se+sesp$all_sp-1)}
    
    
    #index=which(sesp$se>=0.6 & sesp$sp>0.9)
    print(i)
    print(sesp[index,])
    
    
    
    #estimate the parameters for the last observed case
    lag_n=sesp$all_lag[index]
    c_n=sesp$all_cut[index]
    
    if (method=="EVI"){
      roll_n=rollsd(case_t,lag_n)
      ev_n=evi(roll_n)
      ind_n=indic(evi = ev_n,cut = c_n, cases = case_t, method=method)
      evicut_n=evifcut(evi = ev_n, cases = case_t, cut = c_n, r = r, method=method)
      roll = c(roll, roll_n[length(ind_n)])
    }else if (method=="cEVI"){
      ev_n=cEVI_fun(cases = case_t, lag_n = lag_n, c_n = c_n) #
      ind_n=indic(cevi = ev_n,cut = c_n , cases = case_t, method=method) #
      evicut_n=evifcut(cevi = ev_n, cases = case_t, r = r, method=method) #
    }
    
    ev = c(ev, ev_n[length(ind_n)])
    ind = c(ind, ind_n[length(ind_n)])
    lag_all = c(lag_all, lag_n)
    c_all = c(c_all, c_n)
    
    se_all=c(se_all,all_se[index])
    sp_all=c(sp_all,all_sp[index])
    
    ppv[i]=evicut_n$prev*all_se[index]/
      (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))
    
    npv[i]=(1-evicut_n$prev)*all_sp[index]/
      ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
    
    
    
  }
  
  Days=as.Date((length(cases)-diff):length(cases),  origin=origin) 
  EVI=ev[((length(ev)-diff):length(ev))]
  Cases=cases[((length(cases)-diff):length(cases))]
  Index=ind[((length(ind)-diff):length(ind))]
  ppv=ppv[((length(ppv)-diff):length(ppv))]
  npv=npv[((length(npv)-diff):length(npv))]
  lag_all=lag_all[((length(lag_all)-diff):length(lag_all))]
  c_all=c_all[((length(c_all)-diff):length(c_all))]
  se_all=se_all[((length(se_all)-diff):length(se_all))]
  sp_all=sp_all[((length(sp_all)-diff):length(sp_all))]
  
  
  EVI_out_add=as.data.frame(cbind(Days, EVI, Cases, Index, ppv, npv,
                                  lag_all, c_all, se_all, sp_all, new_cases))
  
  EVI_input = rbind(EVI_input, EVI_out_add)
  EVI_output <- (EVI_input)
  
  return(EVI_output)
  
}

evirlap <- function(Index1,Index2, ln=T, type="p",size.index=1,
                    Index1.lab="EVI",Index2.lab="cEVI",Index3.lab="EVI-",Index.country=NULL, origin="1970-01-01", region = NULL) {
  
  Index1$Days = as.Date(Index1$Days, origin="1970-01-01")
  Index1$Index=Index1$Index
  Index2$Days = as.Date(Index2$Days, origin="1970-01-01")
  Index2$Index=Index2$Index*2
  Index=Index1
  Index$Index=Index$Index+Index2$Index
  if(length(table(Index$Index))<3)
    Index$Index[1:3]<-1:3
  Index$cases_1=Index$Cases*Index$Index
  Index$cases_1[Index$cases_1 == 0] <- NA
  Index$cases_0=Index$Cases*(1-Index$Index)
  Index$cases_0[Index$cases_0 == 0] <- NA
  
  Index$npv=Index$npv*(1-Index$Index)
  Index$npv[Index$npv == 0] <- NA
  Index$ppv=Index$ppv*Index$Index
  Index$ppv[Index$ppv == 0] <- NA
  Index$variable<-"x"
  Index$Index[is.na(Index$Index)]<-0
  Index$Index<-factor(Index$Index,labels = c("No warning",paste(Index1.lab,"alone"),paste(Index2.lab,"alone"), Index3.lab))
  Index$Outbreaks<-Index$Cases
  if (ln==F) {
    sp3<-ggplot(Index, aes_string(x="Days",group="variable"))+
      list(
        geom_point(aes_string(y=("Outbreaks"), color="Index"), size=size.index),
        #scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        scale_colour_grey(start = 1,end = 0),
        scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        labs(title = paste0("HPAI- (c)EVI - ",region=region), y = "Outbreaks", x="Days"),
        #          labs(title = paste0("Graph combining outputs ",Index1.lab,", ", Index2.lab," and ", Index3.lab," - ",Index.country), y = "Outbreaks", x="Days"),
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.key.height = unit(0, 'cm')),
        if (type=="l")  geom_path(aes_string(y="Outbreaks",colour="Index"),size=size.index)
      )
  }
  
  if (ln==T) {
    sp3<-ggplot(Index, aes_string(x="Days",group="variable"))+
      list(
        geom_point(aes_string(y="log(Outbreaks)", color="Index"), size=size.index),
        #scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        scale_colour_grey(start = 1,end = 0),
        scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        labs(title = paste0("HPAI- (c)EVI - ",region=region), y = "Outbreaks", x="Days"),
        #          labs(title = paste0("Graph combining outputs ",Index1.lab,", ", Index2.lab," and ", Index3.lab," - ",Index.country), y = "log(Outbreaks)", x="Days"),
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.key.height = unit(0, 'cm'))
      )
    if (type=="l"){ geom_path(aes_string(y="log(Outbreaks)",colour="Index"), size=size.index) }
  }
  print(sp3)
  
}



detect_final_alerts <- function(evi_df, cevi_df, ppv_threshold = 0.5, consecutive_days = 3) 
  {
  library(dplyr)
  library(zoo)
  
  # Ensure date formatting
  evi_df$Days <- as.Date(evi_df$Days)
  cevi_df$Days <- as.Date(cevi_df$Days)
  
  # Merge EVI and cEVI data
  combined_df <- evi_df %>%
    select(Days, Cases, EVI_Index = Index, EVI_ppv = ppv, EVI_npv = npv) %>%
    left_join(
      cevi_df %>% select(Days, cEVI_Index = Index, cEVI_ppv = ppv, cEVI_npv=npv),
      by = "Days"
    ) %>%
    mutate(
      #combined_signal = EVI_Index + 2 * cEVI_Index,  # 1 = EVI only, 2 = cEVI only, 3 = both
      ppv = case_when(
        EVI_Index == 1 & cEVI_Index == 1 ~ pmin(EVI_ppv, cEVI_ppv, na.rm = TRUE),
        EVI_Index == 1 & (cEVI_Index == 0 | is.na(cEVI_Index)) ~ EVI_ppv,
        (EVI_Index == 0 | is.na(EVI_Index)) & cEVI_Index == 1 ~ cEVI_ppv,
        EVI_Index == 0 & cEVI_Index == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      npv = case_when(
        EVI_Index == 1 & cEVI_Index == 1 ~ 0,
        EVI_Index == 1 & (cEVI_Index == 0 | is.na(cEVI_Index)) ~ EVI_npv,
        (EVI_Index == 0 | is.na(EVI_Index)) & cEVI_Index == 1 ~ cEVI_npv,
        EVI_Index == 0 & cEVI_Index == 0 ~ pmin(EVI_npv, cEVI_npv, na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      valid_signal = ifelse((EVI_Index == 1 | cEVI_Index == 1) & !is.na(ppv) & ppv >= ppv_threshold, 1, 0)
      
    ) %>% 
  
  # Rolling window check
    mutate(
      signal_count = ifelse(
        row_number() >= consecutive_days,
        rollapply(valid_signal, consecutive_days, sum, fill = NA, align = "right"),
        NA
      ),
      mean_ppv_window = ifelse(
        row_number() >= consecutive_days,
        rollapply(ppv, consecutive_days, mean, fill = NA, align = "right", na.rm = TRUE),
        NA
      ),
      final_alert = ifelse(signal_count >= consecutive_days & mean_ppv_window >= ppv_threshold, 1, 0)
    )
  
  return(combined_df)
}

evirlap_alert <- function(Index, ln = TRUE, type = "p", size.index = 1,
                    Index1.lab = "EVI", Index2.lab = "cEVI", Index3.lab = "EVI-",
                    Index.country = NULL, origin = "1970-01-01", region = NULL) {
  
  Index$Days <- as.Date(Index$Days)
  
  # Set signal label (0 = no warning, 1 = EVI, 2 = cEVI, 3 = both)
  Index$Index <- as.numeric(Index$EVI_Index) + 2 * as.numeric(Index$cEVI_Index)
  
  # Prevent factor issue when not all levels are present
  if (length(table(Index$Index)) < 3) Index$Index[1:3] <- 1:3
  
  Index$cases_1 <- Index$Cases * Index$Index
  Index$cases_1[Index$cases_1 == 0] <- NA
  Index$cases_0 <- Index$Cases * (1 - Index$Index)
  Index$cases_0[Index$cases_0 == 0] <- NA
  
  Index$npv <- Index$npv * (1 - Index$Index)
  Index$npv[Index$npv == 0] <- NA
  Index$ppv <- Index$ppv * Index$Index
  Index$ppv[Index$ppv == 0] <- NA
  
  Index$variable <- "x"
  Index$Index[is.na(Index$Index)] <- 0
  Index$Index <- factor(Index$Index,
                        labels = c("No warning",
                                   paste(Index1.lab, "alone"),
                                   paste(Index2.lab, "alone"),
                                   Index3.lab))
  
  Index$Outbreaks <- Index$Cases
  
  # Basic ggplot object
  if (ln == FALSE) {
    sp3 <- ggplot(Index, aes(x = Days, group = variable)) +
      geom_point(aes(y = Outbreaks, color = Index), size = size.index) +
      scale_color_manual(values = c("grey69", "yellow3", "orange3", "red4")) +
      labs(title = paste0("HPAI- (c)EVI - ", region),
           y = "Outbreaks", x = "Days") +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.key.height = unit(0, 'cm'))
  }
  
  if (ln == TRUE) {
    sp3 <- ggplot(Index, aes(x = Days, group = variable)) +
      geom_point(aes(y = log(Outbreaks), color = Index), size = size.index) +
      scale_color_manual(values = c("grey69", "yellow3", "orange3", "red4")) +
      labs(title = paste0("HPAI- (c)EVI - ", region),
           y = "log(Outbreaks)", x = "Days") +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.key.height = unit(0, 'cm'))
  }
  
  # ➕ Optional: Overlay Final Alerts as RED CIRCLES
  if ("final_alert" %in% names(Index)) {
    sp3 <- sp3 +
      geom_point(data = Index[Index$final_alert == 1, ],
                 aes(x = Days, y = if (ln) log(Outbreaks) else Outbreaks),
                 shape = 21, fill = "red", color = "black", size = size.index + 1.5, stroke = 0.5)
  }
  
  print(sp3)
}



alert_Global <- detect_final_alerts(evi_df = EVI_Global, cevi_df = cEVI_Global, ppv_threshold = 0.7, consecutive_days = 3)
evirlap_alert(Index=alert_Global, region = "Global", ln = FALSE)

alert_Europe <- detect_final_alerts(evi_df = EVI_Europe, cevi_df = cEVI_Europe, ppv_threshold = 0.2, consecutive_days = 6)
alert_Europe_fig = evirlap_alert(Index=alert_Europe, region = "Europe", ln = FALSE)
png(filename = "alert_Europe_fig.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(alert_Europe_fig)
dev.off()


alert_Africa <- detect_final_alerts(evi_df = EVI_Africa, cevi_df = cEVI_Africa)
evirlap_alert(Index=alert_Africa, region = "Africa", ln = FALSE)



# Results specific dates, locations
# 2005 - 2006 |  Africa, Asia_Pacific, Europe, Global
global_2005 = evirlap(Index1=EVI_Global[which(new_data$Date=="7/1/2005"):which(new_data$Date=="7/1/2006"),], Index2=cEVI_Global[which(new_data$Date=="7/1/2005"):which(new_data$Date=="7/1/2006"),], region="Global", ln=F)
africa_2005 = evirlap(Index1=EVI_Africa[which(new_data$Date=="7/1/2005"):which(new_data$Date=="7/1/2006"),], Index2=cEVI_Africa[which(new_data$Date=="7/1/2005"):which(new_data$Date=="7/1/2006"),], region="Africa", ln=F)
asia_pacific_2005 = evirlap(Index1=EVI_Asia_Pacific[which(new_data$Date=="7/1/2005"):which(new_data$Date=="7/1/2006"),], Index2=cEVI_Asia_Pacific[which(new_data$Date=="7/1/2005"):which(new_data$Date=="7/1/2006"),], region="Asia_Pacific", ln=F)
europe_2005 = evirlap(Index1=EVI_Europe[which(new_data$Date=="7/1/2005"):which(new_data$Date=="7/1/2006"),], Index2=cEVI_Europe[which(new_data$Date=="7/1/2005"):which(new_data$Date=="7/1/2006"),], region="Europe", ln=F)

png(filename = "global_2005.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(global_2005)
dev.off()

png(filename = "africa_2005.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(africa_2005)
dev.off()

png(filename = "asia_pacific_2005.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(asia_pacific_2005)
dev.off()

png(filename = "europe_2005.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(europe_2005)
dev.off()

# 2014 - 2015 | America, Global
global_2014 = evirlap(Index1=EVI_Global[which(new_data$Date=="7/1/2014"):which(new_data$Date=="7/1/2015"),], Index2=cEVI_Global[which(new_data$Date=="7/1/2014"):which(new_data$Date=="7/1/2015"),], region="Global", ln=F)
america_2014 = evirlap(Index1=EVI_America[which(new_data$Date=="7/1/2014"):which(new_data$Date=="7/1/2015"),], Index2=cEVI_America[which(new_data$Date=="7/1/2014"):which(new_data$Date=="7/1/2015"),], region="America", ln=F)

# 2022 | Global, Europe
global_2022 = evirlap(Index1=EVI_Global[which(new_data$Date=="1/1/2021"):which(new_data$Date=="1/1/2023"),], Index2=cEVI_Global[which(new_data$Date=="1/1/2021"):which(new_data$Date=="1/1/2023"),], region="Global", ln=F)
europe_2022 = evirlap(Index1=EVI_Europe[which(new_data$Date=="8/1/2021"):which(new_data$Date=="1/1/2023"),], Index2=cEVI_Europe[which(new_data$Date=="8/1/2021"):which(new_data$Date=="1/1/2023"),], region="Europe", ln=F)


# 2024 | Global, Europe
global_2024 = evirlap(Index1=EVI_Global[which(new_data$Date=="1/1/2023"):which(new_data$Date=="9/27/2024"),], Index2=cEVI_Global[which(new_data$Date=="1/1/2023"):which(new_data$Date=="9/27/2024"),], region="Global", ln=F)
europe_2022 = evirlap(Index1=EVI_Europe[which(new_data$Date=="8/1/2021"):which(new_data$Date=="1/1/2023"),], Index2=cEVI_Europe[which(new_data$Date=="8/1/2021"):which(new_data$Date=="1/1/2023"),], region="Europe", ln=F)
