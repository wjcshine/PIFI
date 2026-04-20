PIFI_3point_INS2=function(data){
data$I.G0=data$INS_A/data$GLU_A
data$I.G60=data$INS_C/data$GLU_C
data$I.G120=data$INS_D/data$GLU_D

data$top_INS=NA
data$top.time_INS=NA
for (i in c(1:nrow(data))){
  data$top_INS[i]=max(data$INS_A[i],data$INS_C[i],data$INS_D[i])
  data$top.time_INS[i]=which.max(c(data$INS_A[i],data$INS_C[i],data$INS_D[i]))
}
data=mutate(data,top.time_INS=case_when(
  top.time_INS=="1"~0,
  top.time_INS=="2"~60,
  top.time_INS=="3"~120))
data$slope_INS=(data$top_INS/data$INS_A)/data$top.time_INS

data$top_INS_G=NA
for (i in 1:nrow(data)){
  if(data$top.time_INS[i]==0){
    data$top_INS_G[i]=data$top_INS[i]/data$GLU_A[i]}

  if(data$top.time_INS[i]==60){
    data$top_INS_G[i]=data$top_INS[i]/data$GLU_C[i]}
  if(data$top.time_INS[i]==120){
    data$top_INS_G[i]=data$top_INS[i]/data$GLU_D[i]}

}

data$INS0_G0=49.581407-0.17694468*data$Height+2.3510618*10^(-05)*pmax(data$Height-153.5,0)^3-3.7074437*10^(-05)*pmax(data$Height-161,0)^3+1.3563818*10^(-05)*pmax(data$Height-174,0)^3+0.20585482*data$Sex-0.1964067*data$Age

data$INSpeak_G=561.96318-2.8392477* data$Height+0.017126055*pmax(data$Height-151.85,0)^3-0.046224198*pmax(data$Height-158.5,0)^3+0.033719371*pmax(data$Height-164.55,0)^3-0.0046212272*pmax(data$Height-178,0)^3+5.7112967*data$Sex-1.4778073*data$Age

data$`PIF-Si`=data$I.G0/data$INS0_G0*100
data$`PIF-Li`=data$top_INS_G/data$INSpeak_G*100
data$`PIF-Ai`=data$slope_INS/0.08745131*100

data=select(data,-I.G0,-I.G60,-I.G120,
            -top_INS,-top.time_INS,-slope_INS,-top_INS_G,
            -INS0_G0,-INSpeak_G)
}
