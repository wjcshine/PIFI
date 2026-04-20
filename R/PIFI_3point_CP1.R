PIFI_3point_CP1=function(data){
  data$C.G0=data$CP_A/data$GLU_A
  data$C.G30=data$CP_B/data$GLU_B
  data$C.G120=data$CP_D/data$GLU_D

  data$top_CP=NA
  data$top.time_CP=NA
  for (i in c(1:nrow(data))){
    data$top_CP[i]=max(data$CP_A[i],data$CP_B[i],data$CP_D[i])
    data$top.time_CP[i]=which.max(c(data$CP_A[i],data$CP_B[i],data$CP_D[i]))
  }
  data=mutate(data,top.time_CP=case_when(
    top.time_CP=="1"~0,
    top.time_CP=="2"~30,
    top.time_CP=="4"~120))
  data$slope_CP=(data$top_CP/data$CP_A)/data$top.time_CP

  data$top_CP_G=NA
  for (i in 1:nrow(data)){
    if(data$top.time_CP[i]==0){
      data$top_CP_G[i]=data$top_CP[i]/data$GLU_A[i]}
    if(data$top.time_CP[i]==30){
      data$top_CP_G[i]=data$top_CP[i]/data$GLU_B[i]}
    if(data$top.time_CP[i]==120){
      data$top_CP_G[i]=data$top_CP[i]/data$GLU_D[i]}
  }


  data$C_P0_G0=132.55716+0.10226667*data$Height-0.0025872043*pmax(data$Height-153.5,0)^3+0.0040798222*pmax(data$Height-161,0)^3-0.0014926179*pmax(data$Height-174,0)^3+4.8396695*data$Sex-0.88537434*data$Age

  data$C_Ppeak_G=1931.2743-9.7348166* data$Height+0.06626597*pmax(data$Height-151.85,0)^3-0.17813335*pmax(data$Height-158.5,0)^3+0.12942343*pmax(data$Height-164.55,0)^3-0.017556055*pmax(data$Height-178,0)^3+42.844334*data$Sex-2.6736386*data$Age


  data$`PIF-Sc`=data$C.G0/data$C_P0_G0*100
  data$`PIF-Lc`=data$top_CP_G/data$C_Ppeak_G*100
  data$`PIF-Ac`=data$slope_CP/0.05704698*100

  data=select(data,-C.G0,-C.G30,-C.G120,
              -top_CP,-top.time_CP,-slope_CP,
              -top_CP_G,-C_P0_G0,-C_Ppeak_G)
}
