PIFI_3point_INS1=function(data){
data$I.G0=data$INS_A/data$GLU_A
data$I.G30=data$INS_B/data$GLU_B
data$I.G120=data$INS_D/data$GLU_D

data$top_INS=NA
data$top.time_INS=NA
for (i in c(1:nrow(data))){
  data$top_INS[i]=max(data$INS_A[i],data$INS_B[i],data$INS_D[i])
  data$top.time_INS[i]=which.max(c(data$INS_A[i],data$INS_B[i],data$INS_D[i]))
}
data=mutate(data,top.time_INS=case_when(
  top.time_INS=="1"~0,
  top.time_INS=="2"~30,
  top.time_INS=="3"~120))
data$slope_INS=(data$top_INS/data$INS_A)/data$top.time_INS

data$top_INS_G=NA
for (i in 1:nrow(data)){
  if(data$top.time_INS[i]==0){
    data$top_INS_G[i]=data$top_INS[i]/data$GLU_A[i]}
  if(data$top.time_INS[i]==30){
    data$top_INS_G[i]=data$top_INS[i]/data$GLU_B[i]}
  if(data$top.time_INS[i]==120){
    data$top_INS_G[i]=data$top_INS[i]/data$GLU_D[i]}
}

data$INS0_G0=49.581407-0.17694468* data$Height+2.3510618e-05*pmax(data$Height-153.5,0)^3-3.7074437e-05*pmax(data$Height-161,0)^3+1.3563818e-05*pmax(data$Height-174,0)^3+0.20585482*data$Sex-0.1964067*data$Age

data$INSpeak_G=158.36058-0.25161952* data$Height-0.0011258718*pmax(data$Height-153.5,0)^3+0.0017754132*pmax(data$Height-161,0)^3-0.0006495414*pmax(data$Height-174,0)^3+3.9419737*data$Sex-1.4274988*data$Age

data$`PIF-Si`=data$I.G0/data$INS0_G0*100
data$`PIF-Li`=data$top_INS_G/data$INSpeak_G*100
data$`PIF-Ai`=data$slope_INS/0.1981352*100

data=select(data,-I.G0,-I.G30,-I.G120,
            -top_INS,-top.time_INS,-slope_INS,-top_INS_G,
            -INS0_G0,-INSpeak_G)
}
