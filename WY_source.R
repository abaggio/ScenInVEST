##============================================
## ScenInVEST - STEP10 GAMM for Metamodel
##       R Script
##
## last modification: 31/05/16
## status: unfinished
##============================================
args <- commandArgs(TRUE)


tryCatch({
  
  #---------------------------------------------
  # setting the parameters 
  #---------------------------------------------
  Projpath = args[1]
  ###Projpath = "C:/WorkSpace/ScenInvest_run/QuickTest/"
  #Projpath = "C:/WorkSpace/ScenInvest_run/ScenInvest_Scot_250915_230516_070916/Inputs/"
  
  TempProjPath = args[2]
  ###TempProjPath = "C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/"
  #TempProjPath = TempBTWmodulesProjPath
  #TempProjPath = "C:/WorkSpace/InVESTtemp/"
  #TempProjPath = "//ABFN01/Ale_Proj/Marie/ScenInvest_SharedFolder/aa_Sept2016/ScenInvest_Scot_080916/TEMP_BTWmodules/"
  
  path_in = args[3]
  ###path_in = "C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/"
  #path_in = TempProjPath
  #path_in = "C:/WorkSpace/ScenInvest_run/ScenInvest_Scot_250915_230516_070916/TEMP_BTWmodules/"
  
mv = "C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules//MergedWatersheds_wsid_W.dbf"
  #mw = AllWatersheds_wsIDW_dbf
  #AllWatersheds_wsIDW_dbf  = "C:/WorkSpace/ScenInvest_run/ScenInvest_Scot_250915_230516_070916/TEMP_BTWmodules/MergedWatersheds_wsid_W.dbf"
  
  list_a = args[5] 
list_a = "C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s00.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s01.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s02.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s03.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s04.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s05.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s06.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s07.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s08.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s09.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s10.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s11.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s12.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s13.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/TabArea_s14.dbf"
  #lista_scen = ListTabAreaIDdbf4R
  #list_a = "C:/WorkSpace/ScenInvest_run/ScenInvest_Scot_250915_230516_070916/TEMP_BTWmodules/TabArea_s00.dbf" 
  
  #list_b = args[6]
  ###list_b = "C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s00.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s01.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s02.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s03.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s04.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s05.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s06.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s07.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s08.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s09.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s10.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s11.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s12.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s13.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_outputs_s14.dbf"
  #lista_wout = ListwresultsIDdbf4R
  #list_b = "C:/WorkSpace/ScenInvest_run/ScenInvest_Scot_250915_230516_070916/TEMP_BTWmodules/w_outputs_s00.dbf"
  
  list_c = args[6] 
  list_c = "C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s00.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s01.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s02.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s03.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s04.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s05.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s06.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s07.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s08.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s09.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s10.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s11.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s12.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s13.dbf, C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/w_results_s14.dbf"
  lista_wres = ListwresultsIDdbf4R
  list_c = "C:/WorkSpace/ScenInvest_run/ScenInvest_Scot_250915_230516_070916/TEMP_BTWmodules/w_results_s00.dbf"
  
  gam_wy = args[7]
  gam_wy = "C:/WorkSpace/ScenInvest_run/QuickTest/TEMP_BTWmodules/GAM_wy.RData"
  
  
  logerrorfilename = args[8]
  ###logerrorfilename = "C:/WorkSpace/ScenInvest_run/QuickTest/log_err_CallGAM_wyield.txt"
  ###logerrorfilename = "C:/WorkSpace/ScenInvest_run/QuickTest/log_err_CallGAM_nutrient.txt"
  #logerrorfilename = Errorfilename
  #logerrorfilename = "C:/WorkSpace/ScenInvest_run/ScenInvest_Scot_250915_230516_070916/Inputs/"
  
  Print arguments
  sink("C:/WorkSpace/ScenInvest_run/QuickTest/argts.txt")
  print(args[1])
  print(args[2])
  print(args[3])
  print(args[4])
  print(args[5])
  #print(args[6])
  print(args[7])
  print(args[8])
  print(args[9])
  sink()
  
  # Load libraries
  require(foreign)
  require(pscl)
  require(mgcv)

  Rprof ( tf <- "log.log",  memory.profiling = TRUE )
  
  path_out = path_in
  a = unlist(strsplit(list_a, ","))
  aa = basename(a)
  #aa=list.files(path_in, pattern="^\\TabArea_s", full.names = FALSE, ignore.case = TRUE)
  lista_scen = as.list(aa)[grep(".dbf", a, fixed=T)]
  #b = unlist(strsplit(list_b, ","))
  #bb = basename(b)
  #b=list.files(path_in, pattern="^\\w_out", full.names = FALSE, ignore.case = TRUE)
  #lista_wout = as.list(bb)[grep(".dbf", b, fixed=T)]
  c = unlist(strsplit(list_c, ","))
  cc = basename(c)
  #c=list.files(path_in, pattern="^\\w_res", full.names = FALSE, ignore.case = TRUE)
  lista_wres = as.list(cc)[grep(".dbf", c, fixed=T)]
  dd = basename(mv)
  #d=list.files(path_in, pattern="^\\Mer", full.names = FALSE, ignore.case = TRUE)
  mw =read.dbf(paste(path_in, dd, sep=""), as.is=F)
  tab = c("WS_ID", "VALUE_1","VALUE_2","VALUE_3","VALUE_4","VALUE_5","VALUE_6","VALUE_7","VALUE_8","VALUE_9","VALUE_10","VALUE_11","VALUE_12","VALUE_13","VALUE_14","VALUE_15","VALUE_16","VALUE_17","VALUE_18","VALUE_19","VALUE_20","VALUE_21","VALUE_22","VALUE_23")

    
  for(i in 1:length(lista_scen)){
  scen = lista_scen[i]
  scen2 = sub("\\.[[:alnum:]]+$","",basename(as.character(scen)))
  pro = read.dbf(paste(path_in, scen2,".dbf", sep=""), as.is=F)
  missing = setdiff(tab, names(pro))
  pro[missing]= 0
  pro = pro[tab]
  #scen_wo = lista_wout[i]
  #scen2wo = sub("\\.[[:alnum:]]+$","",basename(as.character(scen_wo)))
  #pro_wo = read.dbf(paste(path_in, scen2wo,".dbf", sep=""), as.is=F)
  #sub_wo = subset(pro_wo[,c(1,6)])
  #mer = merge(sub_wo, pro, by.x="ws_id", by.y="WS_ID", all.x=T, all.y=T)
  scen_wr = lista_wres[i]
  scen2wr = sub("\\.[[:alnum:]]+$","",basename(as.character(scen_wr)))
  pro_wr = read.dbf(paste(path_in, scen2wr,".dbf", sep=""), as.is=F)
  sub_wr = subset(pro_wr[,c(1,2,3,7)])
  #all = merge(sub_wr, mer, by.x="ws_id", by.y="ws_id", all.x=T, all.y=T)
  all = merge(sub_wr, pro, by.x="ws_id", by.y="WS_ID", all.x=T, all.y=T)
  
  all_w = merge(all, mw, by.x="ws_id", by.y="ws_id", all.x=T, all.y=T)
  all_w$W = ifelse(all_w$W =='A', 1, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='B', 2, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='H1', 3, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='H2', 4, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='H3', 5, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='H4', 6, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='M', 7, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='S1', 8, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='S2', 9, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='S3', 10, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='T1', 11, as.character(all_w$W))
  all_w$W = ifelse(all_w$W =='T2', 12, as.character(all_w$W))
  all_w$W = as.factor(all_w$W)
  nam = paste("df.tab", i, sep="")
  assign(nam, all_w)
  }
  
  Rprof ( NULL ) ; print ( summaryRprof ( tf )  )
  
  #df.all = rbind(df.tab1, df.tab2)
  tab.ls=ls(pattern="df.tab", all.names=T)
  list.t=lapply(tab.ls, get)
  df.all=do.call(rbind, list.t)
  
  #df.all = rbind(df.tab1, df.tab2, df.tab3, df.tab4, df.tab5, df.tab6, df.tab7, df.tab8, df.tab9, df.tab10, df.tab11, df.tab12, df.tab13, df.tab14)
  cov=c(names(df.all)[grep("VALUE_",names(df.all))])
  c=23
  cov2=c("VALUE_1","VALUE_2","VALUE_4","VALUE_7","VALUE_8","VALUE_9","VALUE_10","VALUE_11","VALUE_12","VALUE_13","VALUE_14","VALUE_16")
  c2=12
  ##### Formula to produce the GAM model
  #Originally the smooth function was used for all the variables in the gam model
  #We visually (plot(gam_300)) veriefied that all the variable are characterized by a linear function
  #we decided to fit a linear lm model without any smooth terms to save computational time and because the R-squared is still very high (0.94) compared with the gam one (0.96) 
  sm=paste("bs=",'"ts"',sep="")
  #term1 = paste(names(df.all)[grep("n_exp",names(df.all))], " ~ ", sep="")
  #term2 = paste("s(",names(df.all)[grep("wyie",names(df.all))],")", sep="")
  term3a = paste("s(",names(df.all)[grep("prec",names(df.all))],",",names(df.all)[names(df.all) %in% cov[1:c]], ",",sm,")", collapse="+",sep="")
  term3b = paste("s(jitter(",names(df.all)[names(df.all) %in% cov2[1:c2]],")", ",",sm,")", collapse="+",sep="")
  #term4 = paste("as.factor(",names(df.all)[grep("W",names(df.all))],")", sep="")
  #fmla=as.formula(paste(term1, term2, "+", term3,"+",term4, sep=""))
  #####
  ##### Formula to produce the lm model
  term1 = paste(names(df.all)[grep("wyie",names(df.all))], " ~ ", sep="")
  term2 = paste("(",names(df.all)[grep("prec",names(df.all))] ,")", sep="")
  term3 = paste("(",names(df.all)[grep("PET",names(df.all))] ,")", sep="")
  term4 = paste("(",names(df.all)[names(df.all) %in% cov[1:c]], ")", collapse="+",sep="")
  term5 = paste("as.factor(",names(df.all)[grep("W",names(df.all))],")", sep="")
  #fmla=as.formula(paste(term1, term2, "+", term3,"+",term4,"+",term5, sep=""))
  #fmla2=as.formula(paste(term1, term4,"+",term5, sep=""))
  #fmla3=as.formula(paste(term1, term2, "+", term3,"+",term5, sep=""))
  #fmla4=as.formula(paste(term1, term2, "+", term3,"+",term5, sep=""))
  #fmla5=as.formula(paste(term1, term3a, "+", term5, sep=""))
  fmla7=as.formula(paste(term1, term3b, "+", term5, sep=""))

  
  #gamm_wy = gamm(fmla, data=df.all, family=gaussian, random=list(W=~1))
  #gamm_wy2 = gamm(fmla2, data=df.all, family=gaussian, random=list(W=~1))
  #gam_wy = gam(fmla, data=df.all, family=gaussian)
  #gamm_wy3 = gamm(fmla3, data=df.all, family=gaussian, random=list(W=~1))
  #gamm_wy5 = gamm(fmla5, data=df.all, family=gaussian, random=list(W=~1))
  #gamm_wy6 = gam(fmla5, data=df.all, family=gaussian)
  #gam_wy6 = gam(fmla6, data=df.all, family=gaussian)
  gam_wy7 = gam(fmla7, data=df.all, family=gaussian)
  
  
  
  #save(gam_300,file=paste(path_out,"GAM_nut.RData",sep=""))  
  save(gam_wy7,file=gam_wy)  
  ###save(gammero_nut,file=paste(path_out,"GAMM_nut.RData",sep=""))
  ####save(gammero_nut, file= paste(gamm_nut ,?.RData?,sep=??)) ## ERROR, unexpected input in
  ###save(gammero_nut, file= gamm_nut) # working with full path & .../Outputs/GAM_nut.RData


  
}, error = function(err) { 
  ###sink(paste(path_out,"log_err_",gsub(":","_",format(Sys.time(), "%X")),".txt",sep=""))
  sink(logerrorfilename)
  print(err)
  sink()
})
 