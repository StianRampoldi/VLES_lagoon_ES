#
#
#-----VLES-----
#this code is licensed under the: Academic Free License version 3.0.
#authors for the attribution are: Daniele Brigolin, Stian Rampoldi, & Silvia Rova
#source: https://github.com/StianRampoldi/VLES_lagoon_ES
#
#

#----- importing -----
require (deSolve)
install.packages("here")
require(here)
library(here)


##----- INPUT DEFINITION -----
##directory 
#directory<- "set your folder path"
#setwd(directory)

##----- INITIAL CONDITIONS -----
#Loading reference values  from external file
references<-read.table(file = here("data", "references.csv"),row.names = 1,sep = ";",dec=",")
HER0=references['HER0',]        #surface of tangibale cultural heritage                                   [km2]
SG_old=references['SG_old',]    #extimated historic value of seagrasses                                   [km2]
SM_old=references['SM_old',]    #extimated historic value of saltmarshes                                  [km2]
SM_geom=references['SM_geom',]  #shape factor, percentage of CR (creeks/ghebi) in SM (salt-marshes/barene)[adimensional]
CD_0=references['CD_0',]        #hypotetical target number of students in environmental education         [individuals/year]
TR_0=references['TR_0',]        #hypotetical target number of voga veneta rowers                          [individuals]

#Loading initial conditions from external file
intial_conditions<-read.table(file = here("data", "initialConditions.csv"),row.names = 1,sep = ";",dec=",")

#morphology
#HER0=intial_conditions['HER0',]  #M-HER surface of tangibale cultural heritage [km2]
#HER is modelled as a static value hence dHERdt=0[km2/y] and HER(t)==HER0 for every t
NC0=intial_conditions['NC0',]     #M-NC surface navigable canals                [km2]

#habitats
BD0=intial_conditions['BD0',]     #H-BD  surface of benthonic diathoms          [km2]
SM0=intial_conditions['SM0',]     #H-SM surface of salt marshes                 [km2]
SG0=intial_conditions['SG0',]     #H-SG surface of seagrasses                   [km2]
BB0=intial_conditions['BB0',]     #H-BB surface of bare bottom/macro algae      [km2]
F0=BB0/2                          #[km2]

#fauna
TA0=intial_conditions['TA0',]     #F-TA density biomass ruditapes               [ton FW/km2]
BI0=intial_conditions['BI0',]     #F-BI density biomass birds                   [ton FW/km2]
MU0=intial_conditions['MU0',]     #F-MU density biomass Mugilidi                [ton FW/km2]
SB0=intial_conditions['SB0',]     #F-SA Density biomass Sparus aurata           [ton FW/km2]
DE0=intial_conditions['DE0',]     #F-DE Density biomass demersals               [ton FW/km2]
LA0=intial_conditions['LA0',]     #F-LA Density biomass labrax                  [ton FW/km2]

##----- FORCINGS ----
#loading forcing conditions from external files
forcing_conditions<-read.table(file = here("data", "forcings.csv"),row.names = 1,sep = ";",dec=".")
timeseries<-read.table(file = here("data", "timeseries.csv"),header=TRUE,row.names = 1,sep = ";",dec=",")

#Climate Forcing
#[°C] increase in water temperature on an yearly basis: kTrcp85=rcp 8.5 
kTrcp=forcing_conditions['kTrcp85',] 
#parameter of non linear response of MOSE openings addrcp =rcp 8.5
re_MO=forcing_conditions['re_MO_85_110',] #addese 110 or 130 (activation threshold for MOSE 110 or 130)

#time steps are years with initial year: ti=1980
ti=1
#time steps are years with final year: tf=2080
tf=101

## initialize arrays
times <- seq(ti,tf, by=1)
delta_water_temperature <- seq(ti,tf, by=1)
delta_sealev <- seq(ti,tf, by=1)

#WTR: before 2020 the scenarios are equal
delta_water_temperature[ti]<-0.02 
for (i in ti+1:40){
  delta_water_temperature[i]=delta_water_temperature[i-1]+0.02
}
#WTR: after 2020 we split the scenarios
for (i in 40+1:tf){
  delta_water_temperature[i]=delta_water_temperature[i-1]+kTrcp
}

#RSLR:rcp85= rcp 8.5
for (i in ti:tf){
  delta_sealev[i]=timeseries[i,'rcp85']
}

#create functions for water temperature increase and relative sea level rise
waterT<-approxfun(delta_water_temperature, rule = 2)  #[°C]
RSLR<-approxfun(delta_sealev, rule = 2)               #[m]

#plots for a check
#plot these functions
#plot(times,RSLR(times))
#abline(v=20, col="red")
#plot(times,waterT(times))
#abline(v=20, col="red")

#HUMAN FORCING
#stocks: residents lagoon, residents gronda, university students, tourists Venice
delta_residents_venice <- seq(ti,tf, by=1)
delta_residents_gronda <- seq(ti,tf, by=1)
delta_uni_students<- seq(ti,tf, by=1) 
delta_tourists_venice<- seq(ti,tf, by=1) 
SG_MGMT<- seq(ti,tf, by=1) 

#initial values of these actors
delta_uni_students[ti]=forcing_conditions['US0',] 
delta_tourists_venice[ti]=forcing_conditions['TV0',]

#ISbau=business as usual social scenario
delta_residents_venice[ti]=timeseries[ti,'ISbau']
delta_residents_gronda[ti]=timeseries[ti,'GR']
SG_MGMT[ti]=timeseries[ti,'GS_SG']

#1-the first 20 years count as spin-up period for the model (1980-2000)
#islands demographic scenarios: ISbau=business as usual social scenario
for (i in ti+1:20){
  delta_residents_venice[i]=timeseries[i,'ISbau']
  delta_residents_gronda[i]=timeseries[i,'GR']
  delta_uni_students[i]=delta_uni_students[i-1]
  delta_tourists_venice[i]=delta_tourists_venice[i-1]
  SG_MGMT[i]=timeseries[i,'GS_SG']
}

#2-then the values assume historical values for 24 years (2000-2024)
#3-and then they become exploratory projections (2024-2080)
for (i in 21:61){delta_tourists_venice[i]=delta_tourists_venice[i-1]*1.0368}
for (i in 62:tf){delta_tourists_venice[i]=delta_tourists_venice[i-1]*1.01}

for (i in 21:tf){
  delta_residents_venice[i]=timeseries[i,'ISbau']
  delta_residents_gronda[i]=timeseries[i,'GR']
  delta_uni_students[i]=delta_uni_students[i-1]+250
  SG_MGMT[i]=timeseries[i,'GS_SG']
}
#create the functions of the actors
RVt=approxfun(delta_residents_venice, rule = 2)
RGt=approxfun(delta_residents_gronda, rule = 2)
USt=approxfun(delta_uni_students, rule = 2)
TVt=approxfun(delta_tourists_venice, rule = 2)
GS_SGt=approxfun(SG_MGMT, rule = 2)

## PARAMETERS ----
#Loading parameters from external file
parameters<-read.table(file = here("data", "parameters.csv"),row.names = 1,sep = ";",dec=".")

#check if all loaded variables are numeric
all(sapply(parameters, is.numeric))
#all(sapply(intial_conditions, is.numeric))
#all(sapply(forcing_conditions, is.numeric))

param<-c(
  #parameters defining morphological changes
  kER=parameters['kER',],     #erosion of vegetable habitats for erosion surf in a year [1/y]
  kSA=parameters['kSA',],     #loss of navigable canals surface for sediments in a year [1/y]
  
  #erosion parameters
  d_TA=parameters['d_TA',],   #[km2/ton]
  d_Nav=parameters['d_Nav',], #[ind-1]
  d_Tou=parameters['d_Tou',], #[ind-1]
  
  #HABITAT parameters
  #BD Bentonic Diathomes (habitat surface [km2])
  ni_BD=parameters['ni_BD',],   #expansion rate of BD                     [1/year]
  kH=parameters['kH',],         #factor of habitat growth relation to temperature       [1/C'] 
  
  #SM  salt marshes/Barene
  ni_SM=parameters['ni_SM',],   #[1/year]
  kR=parameters['kR',],         #effect of rslr on SM [adimensional]
  #SG seagrasses/praterie intertidali
  ni_SG=parameters['ni_SG',],   #expansion rate of SG [1/y]
  
  #Habitat restoration and morphological interventions rate per year
  GS_CAN=parameters['GS_CAN',], #[1/year]
  GS_SM=parameters['GS_SM',],   #[1/year]
  GS_BD=parameters['GS_BD',],   #[1/year]
  
  #percentage of habitat available for restoration per year
  resSG=parameters['resSG',],   #[1/year]
  resBD=parameters['resBD',],   #[1/year]
  resSM=parameters['resSM',],   #[1/year]
  
  ##FAUNA parameters
  #general
  kF=parameters['kF',],             #modulation factor of fauna to temperature  [1/C'] 
  beta_LCM=parameters['beta_LCM',], #BetaLCM capacity response to LCM           [adimensional]
  part_F=parameters['part_F',],     #partition of fished species to FA and FS   [adimensional]
  
  #Ruditapes Philippinarum/vongola filippina/clam TA
  rTA=parameters['rTA',], #net growth coefficient for clams               [1/y] ecopath=30.59
  kTA=parameters['kTA',], #carrying capacity of clam:  ton of FW in area: [ton/km2]
  HTA=parameters['HTA',], #clam collection coefficient                    [ton/(ind*y*km^2)]
  
  #Birds/Uccelli BI
  rBI=parameters['rBI',], #net growth coefficient for birds               [1/y]
  kBI=parameters['kBI',], #carrying capacity of birds                      [ton/km2]
  HBI=parameters['HBI',], #birds collection coefficient                   [ton/(ind*y*km^2)]
  
  #Mullets/Mugilidi MU
  rMU=parameters['rMU',], #growth rate for Mullets          [1/y] ecopath: rmu=0.97 MU=8.26
  kMU=parameters['kMU',], #carrying capacity of mullets      [ton/km2] 
  HMU=parameters['HMU',], #mullet collection coefficient    [ton/(ind*y*km^2)]
  
  #Seabream/Orata/Sparus aurata SB
  rSB=parameters['rSB',], #growth rate for Seabream         [1/y] ecopath: SB=0.49 
  kSB=parameters['kSB',], #carrying capacity of Seabream    [ton/km2]
  HSB=parameters['HSB',], #Seabream collection coefficient  [ton/(ind*y*km^2)]
  
  #Demersals/Demersali DE
  rDE=parameters['rDE',], #growth rate for Demersals        [1/y] ecopath: DE=0.75
  kDE=parameters['kDE',], #carrying capacity of Demersals   [ton/km2]
  HDE=parameters['HDE',], #Demersals collection coefficient [ton/(ind*y*km^2)] 
  
  #Sea Bass/Dicentrarchus labrax/Branzino LA
  rLA=parameters['rLA',], #growth rate for LA               [1/y] ecopath: LA=0.53
  kLA=parameters['kLA',], #carrying capacity of LA          [ton/km2]
  HLA=parameters['HLA',], #LA collection coefficient        [ton/(ind*y*km^2)]
  
  ##ECOSYSTEM SERVICES parameters
 
  #percentage of demografic forcing stocks being active as actors stocks for the ES
  perSF_V=parameters['perSF_V',],   #sport fishers among the lagoon residents per year
  perSF_G=parameters['perSF_G',],   #sport fishers among the mainland residents per year
  perAF_V=parameters['perAF_V',],   #artisanal fishers among the lagoon residents per year
  perAF_G=parameters['perAF_G',],   #artisanal fishers among the mainland residents per year
  perTF=parameters['perTF',],       #tapes fishers among all residents
  perNAV_V=parameters['perNAV_V',], #boaters among the lagoon residents per year
  perNAV_G=parameters['perNAV_G',], #boaters among the mainland residents per year
  perNAV_S=parameters['perNAV_S',], #boaters among higher education students per year
  perROW_V=parameters['perROW_V',], #rowers among the lagoon residents per year
  perROW_G=parameters['perROW_G',], #rowers among the mainland residents per year
  perROW_S=parameters['perROW_S',], #rowers among higher education students per year
  perEDU_R=parameters['perEDU_R',], #environmental ed students among all residents
  perEDU_S=parameters['perEDU_S',], #environmental ed students among higher ed. students
  perHUNT=parameters['perHUNT',],   #hunters among all residents
  
  #regulating ES
  #erosion prevention
  kEP2=parameters['kEP2',], #kEP2 control of erosion_prevention2 per area of habitat  [adimensional]
  
  #water purification N=nitrogen
  #rate of removal of N in the habitats [ton/(y*km2)]
  kNh=parameters['kNh',],   #[ton/(y*km2)] high efficency
  kNm=parameters['kNm',],   #[ton/(y*km2)] medium efficency
  kNl=parameters['kNl',],   #[ton/(y*km2)] low efficency
  
  #sequestration C=carbon
  kCSG=parameters['kCSG',],           #coefficient of removal of C in SG [ton/(km2*y)]
  kCSM=parameters['kCSM',],           #coefficient of removal of C in SM [ton/(km2*y)]
  
  #cultural ES
  wSMCD=parameters['wSMCD',],         #weight SG percentage on cultural attractiveness [adimensional] 
  wSGCD=parameters['wSGCD',],         #weight of cultural user of SM percentage        [adimensional] 
  
  #excursion rates of the actors in the activities per year
  exA_EDU=parameters['exA_EDU',],     #[1/year] in education
  exA_NAV=parameters['exA_NAV',],     #[1/year] in navigation
  exA_TOU=parameters['exA_TOU',],     #[1/year] in tourism
  exA_ROW=parameters['exA_ROW',],     #[1/year] in rowing
  
  #weight of attractiveness for:
  wHER=parameters['wHER_TuNaCDTR',],  #[adimensional]
  wNAT=parameters['wNAT_TuNaCDTR',],  #[adimensional]
  wACC=parameters['acc_TuNaCDTR',],   #[adimensional]
  wSMTR=parameters['wSMTR',],         #[adimensional]
  wSGTR=parameters['wSGTR',]          #[adimensional] 
)

## INITIALIZATION ----
state <- list() #out   <- list()

#initialize the state-variables vector, based on initial conditions uploaded from the file
state <- c (BB=BB0,NC=NC0,BD=BD0,
            SM=SM0,SG=SG0,BI=BI0,TA=TA0,MU=MU0,SB=SB0,DE=DE0,LA=LA0)
##MODEL ----
#t free variable of time used to call the model function, this depends also on two  
#arrays: state of initial conditions, and parameters of the functions
#rates computes the values of the derivatives in the ODE system at time t

rates <- function(t,state, param) {
  with(as.list(c(state, param)), { 
    
    #BB (Bare Bottom/Macro algae)             [km2]
    #SI (Shallow Intertidal)                  [km2]
    #DS (Deep Subtidal)                       [km2]
    #NC (Navigable Canals)                    [km2]
    #NNC (Non-navigable Canals)               [km2]
    #SG (Seagrasses/Praterie di fanerogame)   [km2]
    #SM (Salt-marshes/Barene)                 [km2]
    #CR (Creeks/ghebi)                        [km2]
    #BD (Benthic Diatoms)                     [km2]
    
    #total area of the lagoon is 553, islands and cost occupy 91, canals 53, the rest is 409
    #spatial constraints
    CR=SM*SM_geom     #Creeks/ghebi         [km2]
    SI=BB+SG+BD+SM    #SI is the sum        [km2] 
    DS=409-SI         #Deep Subtidal        [km2] SI+DS=409
    NNC=53-NC         #Non navigable canals [km2] NNC+NC=53
    
    #f_space=km2/km2 [adimensional]
    #if BB is decreasing, f_space decreases and goes towards 0
    #if BB is increasing, f_space increases and goes towards 1
    #larger BB the larger f_space, F0=BB0/2
    f_space=BB/(F0+BB)   #[adimensional]
    
    #MOSE governance level - MOSE was first activated in 2020 with a yearly baseline of 100hours
    if(t<40){GS_MO=0}
    else{GS_MO=100+(re_MO*(1+RSLR(t))^3 )}  #hours of activeness of MOSE per year       [hours]
    f_MO=(8765.76-GS_MO)/8765.76            #percentage of hours without MOSE in a year [adimensional]
    
    #define internal stocks of human actors used in ESs
    #non economic provisioning
    FSt=(RVt(t)*perSF_V+RGt(t)*perSF_G)*((SB+LA+DE)/(kSB/30+kLA/30+kDE/30+SB+LA+DE))
    HUNTt=(RVt(t)+RGt(t))*perHUNT*(BI/kBI)
    
    #economic provisioning
    FAt=(perAF_V*RVt(t)+perAF_G*RGt(t))*((MU+DE)/(kMU/30+kDE/30+MU+DE))
    FTt=(RVt(t)+RGt(t))*perTF*(TA/kTA)
    
    #cultural services
    BOATt=(RVt(t)*perNAV_V+RGt(t)*perNAV_G+ USt(t)*perNAV_S)+21000
    ROWt= (RVt(t)*perROW_V+RGt(t)*perROW_G+USt(t)*perROW_S)
    EDUt= ((RVt(t)+RGt(t))*perEDU_R + USt(t)*perEDU_S)
    
    ##ECOSYSTEM SERVICES initial
    #between 0 and 1, goes to 1 if TA goes to +inf. goes to 0 if TA goes to 0
    f_GS_TA=TA/((kTA/2)+TA)  #[adimensional] 
    
    #Clam_harvesting (2.96 in 2016)  [ton/(km2*year)]
    Clam_harvesting=HTA*FTt*f_GS_TA
    
    #f_MO is 1 if MOSE is not active, closer to 0 the more it opens
    Lifecycle_mainteinance=((SG+CR)/409)*f_MO
    
    #Water_purification= maximum/potential percentage of nitrogen removed in per unit of area
    #Water_purification [adimensional]
    #it is a percentage 12%=0.12
    #with fixed proportions of efficency for three area groups low=l, medium=m, high=h
    Water_purification=(kNh*SG+kNh*SM+kNm*BB+kNm*BD+kNl*DS)/409
    
    #Climate_regulation is tons of CO2 captured
    Climate_regulation=SM*kCSM+SG*kCSG
    
    Cognitive_development=exA_EDU*EDUt*(
      wNAT*(1+((wSMCD*(SM-SM0)+
                  wSGCD*(SG-SG0))/(wSMCD*SM0+wSGCD*SG0)))+
        wHER*(1+(HER0-HER0)/HER0)+
        wACC*(1+(NC-NC0)/NC0))
    
    Tourism=exA_TOU*TVt(t)*(
      wNAT*(1+(SM-SM0)/SM0)+
      wHER*(1+(HER0-HER0)/HER0)+
      wACC*(1+(NC-NC0)/NC0))
    
    Navigation=exA_NAV*BOATt*(
      wNAT*(1+(SM-SM0)/SM0)+
      wHER*(1+(HER0-HER0)/HER0)+
      wACC*(1+(NC-NC0)/NC0))
    
    Traditions=exA_ROW*ROWt*(
      wNAT*(1+((wSMTR*(SM-SM0)+wSGTR*(SG-SG0))/(wSMTR*SM0+wSGTR*SG0)))+
      wHER*(1+(HER0-HER0)/HER0)+
      wACC*(1+(NC-NC0)/NC0))
    
    #area acting as wind fetch interruption to erosion
    #is a sub-area of BB protected by the areas of SM, more SM more protection
    #(SM/SI)<1 hence EP1<BB
    Erosion_prevention1=BB*(SM/SI)   #[km2]
    
    #area of bio stabilization
    Erosion_prevention2=(SG+BD)*kEP2 #bio-stabilized surface: less erodible  [km2]
    
    #sensitizing function, mediates habitat restoration actions
    if( ( 2 + (Cognitive_development-CD_0)/CD_0 + (Traditions-TR_0)/TR_0 ) > 0 ){
      #sensibility [coefficient]
      f_sens=(Cognitive_development+Traditions)/((CD_0+TR_0)+(Cognitive_development+Traditions)) 
    } else {
      f_sens=0}
    
    ### MORPHOLOGICAL AND HABITAT DYNAMICS ###

    #non linear response to water temperature
    fiDT=exp(-kH*waterT(t))  #[adimensional]
    #response to sea level rise
    fRLSR=exp(-kR*RSLR(t))   #[adimensional]
    
    #siltation of navigable canals
    siltation=(kSA*NC+NC*(Clam_harvesting*d_TA+Tourism*d_Tou+Navigation*d_Nav))*(NC/(NC+NNC)) #[km2/year]
    #excavation of non-navigable canals
    excavation=(NNC*GS_CAN)*(NNC/(NC+NNC))+NC0*0.1*f_sens*(NNC/(NC+NNC)) #[km2/year]
    
    #shift in the area of navigable canals
    dNCdt=-siltation+excavation #[km2/year]
    
    ##Bare Bottom/Macro Algae [km2]
    fromBBtoDS=f_space*(BB-Erosion_prevention1)*(kER+(Clam_harvesting*d_TA+Tourism*d_Tou+Navigation*d_Nav)) #[km2/year]
    fromBBtoBD=((BB-fromBBtoDS)/(F0+BB))*((ni_BD*fiDT*GS_BD)*BD+BD0*resBD*f_sens)          #[km2/year]
    fromBBtoSG=((BB-fromBBtoDS-fromBBtoBD)/(F0+BB))*((ni_SG*fiDT*GS_SGt(t))*SG+SG0*resSG*f_sens)      #[km2/year]
    fromBBtoSM=((BB-fromBBtoDS-fromBBtoBD-fromBBtoSG)/(BB0+BB))*((ni_SM*fRLSR*f_MO*GS_SM)*SM+SM0*resSM*f_sens)   #[km2/year]
    
    #Benthic Diatoms [km2]
    fromBDtoDS=(BD/(BD+SG))*(BD+SG-Erosion_prevention2)*(kER)                  #[km2/year]
    fromBDtoBB=(Clam_harvesting*d_TA)*(BD-fromBDtoDS)                          #[km2/year]
    fromBDtoSG=(ni_SG*fiDT*GS_SGt(t))*(BD-fromBDtoBB-fromBDtoDS)               #[km2/year]
    fromBDtoSM=(ni_SM*fRLSR*f_MO*GS_SM)*(BD-fromBDtoBB-fromBDtoSG-fromBDtoDS)  #[km2/year]
    
    #Salt-Marshes [km2]
    fromSMtoBB=(Tourism*d_Tou+Navigation*d_Nav)*SM #[km2/year]
    
    #sea-grasses [km2]
    fromSGtoBB=(Clam_harvesting*d_TA)*SG #this is erodedSG by services, goes all in BB [km2/year]
    fromSGtoSM=(ni_SM*fRLSR*f_MO*GS_SM)*(SG-fromSGtoBB)   #this is colonized by SG, goes to SG [km2/year]
    fromSGtoDS=((SG-fromSGtoBB-fromSGtoSM) /(BD+SG))*(BD+SG-Erosion_prevention2)*(kER) #[km2/year]
    
    #loss equation of morphology [km2/year]
    lossBB=fromBBtoSM + fromBBtoSG + fromBBtoBD + fromBBtoDS #[km2/year]
    lossBD=fromBDtoSG + fromBDtoSM + fromBDtoBB + fromBDtoDS #[km2/year]
    lossSG=fromSGtoBB + fromSGtoSM + fromSGtoDS              #[km2/year]
    lossSM=fromSMtoBB                                        #[km2/year]
    
    #growth equations of morphology [km2/year]
    growthBB=fromSMtoBB + fromSGtoBB + fromBDtoBB #[km2/year]
    growthBD=fromBBtoBD                           #[km2/year]
    growthSG=fromBBtoSG + fromBDtoSG              #[km2/year]
    growthSM=fromBBtoSM + fromBDtoSM + fromSGtoSM #[km2/year]
    
    #differential gain-loss equations of morphology [km2/year]
    dSMdt=growthSM-lossSM   #[km2/year]
    dBDdt=growthBD-lossBD   #[km2/year]
    dSGdt=growthSG-lossSG   #[km2/year]
    dBBdt=growthBB-lossBB   #[km2/year]
    
    ### FAUNA dynamics ###
    
    #temperature response - is unique for all fauna groups
    fkDT=exp(-kF*waterT(t) ) #[adimensional]
    
    #link with fisheries governance (artisanal and sport targeting the same compartments)
    #DE is targeted by both groups
    f_GS_DEa=(DE*(1-part_F))/((((kDE/2)*(1-part_F)))+DE*(1-part_F)) #[adimensional]
    f_GS_DEs=(DE*part_F)/((((kDE)*part_F))+(DE*part_F))             #[adimensional]
    #MU is targeted by artisanal f.
    f_GS_MUa=MU/(kMU/2 + MU)                  #[adimensional]
    #SB, LA are targeted by sport f.
    f_GS_SBs=SB/(kSB + SB)                    #[adimensional]
    f_GS_LAs=LA/(kLA + LA)                    #[adimensional]
    
    #Ruditapes philippinarum (TA - clam - vongola)
    f_GS_TA=TA/((kTA/2)+TA)                   #[adimensional]
    #data TA=11.17 rta=2.74 
    growthTA=rTA*fkDT*TA*(1-(TA/kTA))*f_MO    #[adimensional]
    harvTA=HTA*FTt*f_GS_TA                    #[adimensional]
    dTAdt=growthTA-harvTA                     #[ton/(y*km2)]
    
    #Birds - (BI)
    f_GS_BI=BI/(kBI+BI) #f_GS_BI=(ton/km2)/(ton/km2+ton/km2)=[adimensional]
    growthBI=rBI*fkDT*BI*(1-(BI/kBI))  #growthBI [ton/(y*km2)]
    huntBI=HBI*HUNTt*f_GS_BI #capture by human actors [ton/(y*km2)]
    inputBI=0.02*BI0 #input from outside of BI [ton/(y*km2)]
    
    dBIdt=growthBI+inputBI-huntBI   #[ton/(y*km2)]
    
    #Fish Fauna
    #LifeCycleMaintainance reference value
    LCMr=(SG_old+SM_old*SM_geom)/409
    
    #Mugilidae (MU)
    growthMU=beta_LCM*(Lifecycle_mainteinance/LCMr)*fkDT*rMU*MU*(1-(MU/kMU))
    catch_FAMU=HMU*FAt*f_GS_MUa          #[1/(ind*y)]*(ton/km2)*ind*adimensional = [ton/(y*km2)]
    catch_FSMU=0                         #[1/(ind*y)]*(ton/km2)*ind*adimensional = [ton/(y*km2)]
    inputMU=(0.02*MU0)*(Lifecycle_mainteinance/LCMr)*f_MO  #input from the sea of MU [ton/(y*km2)]
    
    dMUdt=growthMU+inputMU-catch_FAMU-catch_FSMU  #ton/(km2*year)
    
    #Sparus aurata (SB)
    growthSB=beta_LCM*(Lifecycle_mainteinance/LCMr)*fkDT*rSB*SB*(1-(SB/kSB)) #[ton/(y*km2)]
    catch_FASB=0                             #[ton/(y*km2)]
    catch_FSSB=HSB*FSt*f_GS_SBs              #[ton/(y*km2)]
    inputSB=(0.02*SB0)*(Lifecycle_mainteinance/LCMr)*f_MO  #input from the sea of SB [ton/(y*km2)]
    
    dSBdt=growthSB+inputSB-catch_FASB-catch_FSSB  #[ton/(y*km2)]
    
    #Demersal fish (DE)
    growthDE=beta_LCM*(Lifecycle_mainteinance/LCMr)*fkDT*rDE*DE*(1-(DE/kDE)) #[ton/(y*km2)]
    catch_FADE=HDE*FAt*f_GS_DEa      #[ton/(y*km2)]
    catch_FSDE=(HDE/10)*FSt*f_GS_DEs #[ton/(y*km2)]
    inputDE=(0.02*DE0)*(Lifecycle_mainteinance/LCMr)*f_MO  #input from the outside of DE [ton/(y*km2)]
    
    dDEdt=growthDE+inputDE-catch_FADE-catch_FSDE   #[ton/(y*km2)]
    
    #Dicentrarchus labrax (LA)
    growthLA=beta_LCM*(Lifecycle_mainteinance/LCMr)*fkDT*rLA*LA*(1-(LA/kLA)) #[ton/(y*km2)]
    catch_FALA=0 #[ton/(y*km2)]
    catch_FSLA=HLA*FSt*f_GS_LAs #[ton/(y*km2)]
    inputLA=(0.02*LA0)*(Lifecycle_mainteinance/LCMr)*f_MO  #input from the sea of LA [ton/(y*km2)]
    
    dLAdt=growthLA+inputLA-catch_FALA-catch_FSLA   #[ton/(y*km2)]
    
    ##ECOSYSTEM SERVICES -----
    
    #provisioning ES
    Artisanal_fishing=catch_FAMU+catch_FASB+catch_FADE+catch_FALA     #[ton/(y*km2)]
    Recreational_fishing=catch_FSMU+catch_FSSB+catch_FSDE+catch_FSLA  #[ton/(y*km2)]
    Hunting=huntBI                                                    #[ton/(y*km2)]
    Clam_harvesting=harvTA                                            #[ton/(y*km2)]
    
    #regulating ES
    Lifecycle_mainteinance=((SG+CR)/409)*f_MO                    #[ton/km2*y]
    Water_purification=(kNh*SG+kNh*SM+kNm*BB+kNm*BD+kNl*DS)/409  #[ton/y]
    Climate_regulation=SM*kCSM+SG*kCSG                           #[ton/y]
    
    #cultural ES
    #environmental education
    Cognitive_development=exA_EDU*EDUt*(
      wNAT*(1+((wSMCD*(SM-SM0)+
                  wSGCD*(SG-SG0))/(wSMCD*SM0+wSGCD*SG0)))+
        wHER*(1+(HER0-HER0)/HER0)+
        wACC*(1+(NC-NC0)/NC0))
    #tourism in the lagoon
    Tourism=(TVt(t)*exA_TOU)*((
      wNAT*(1+(SM-SM0)/SM0))+
        wHER*(1+(HER0-HER0)/HER0)+
        wACC*(1+(NC-NC0)/NC0))
    #leisure nagivation
    Navigation==exA_NAV*BOATt*((
      wNAT*(1+(SM-SM0)/SM0))+
        wHER*(1+(HER0-HER0)/HER0)+
        wACC*(1+(NC-NC0)/NC0))
    #traditional rowing voga
    Traditions=exA_ROW*ROWt*(
      wNAT*(1+((wSMTR*(SM-SM0)+wSGTR*(SG-SG0))/(wSMTR*SM0+wSGTR*SG0)))+
        wHER*(1+(HER0-HER0)/HER0)+
        wACC*(1+(NC-NC0)/NC0))
    
    #erosion [km2/year]
    #erosion prevention ES is splitted in two parts
    #1-a part of BB can be protected from wind by salt marshes interrupting the fetch
    Erosion_prevention1=BB*(SM/SI)
    #2-a part of SG and BD is biostabilized and protected from waves 
    Erosion_prevention2=(SG+BD)*kEP2
    
    list( c(dBBdt,dNCdt,dBDdt,dSMdt,dSGdt,dBIdt,dTAdt,dMUdt,dSBdt,dDEdt,dLAdt),
          Artisanal_fishing=Artisanal_fishing,
          Recreational_fishing=Recreational_fishing,
          Hunting=Hunting,
          Clam_harvesting=Clam_harvesting,
          Lifecycle_mainteinance=Lifecycle_mainteinance,
          Water_purification=Water_purification,
          Climate_regulation=Climate_regulation,
          Cognitive_development=Cognitive_development,
          Navigation=Navigation,
          Tourism=Tourism,
          Traditions=Traditions,
          Erosion_prevention1=Erosion_prevention1,
          Erosion_prevention2=Erosion_prevention2,
          SI=SI,
          DS=DS,
          NC=NC,
          NNC=NNC,
          CR=CR,
          BB=BB,
          RV=RVt(t),
          RG=RGt(t),
          TV=TVt(t),
          US=USt(t),
          FSt=FSt,
          HUNTt=HUNTt,
          FAt=FAt,
          FTt=FTt,
          BOATt=BOATt,
          ROWt=ROWt,
          EDUt=EDUt,
          delta_temp=waterT(t),
          delta_sea_lev=RSLR(t),
          f_sens=f_sens,
          f_space=f_space,
          fRLSR=fRLSR,
          fkDT=fkDT,
          fiDT=fiDT,
          f_MO=f_MO,
          f_GS_TA=f_GS_TA,
          f_GS_BI=f_GS_BI,
          f_GS_DEs=f_GS_DEs,
          f_GS_DEa=f_GS_DEa,
          f_GS_MUa=f_GS_MUa,
          f_GS_SBs=f_GS_SBs,
          f_GS_LAs=f_GS_LAs,
          excavation=excavation,
          growthBD=growthBD,
          lossBD=lossBD,
          growthSG=growthSG,
          lossSG=lossSG,
          growthSM=growthSM,
          lossSM=lossSM,
          growthMU=growthMU,
          growthSB=growthSB,
          growthDE=growthDE,
          growthLA=growthLA,
          growthTA=growthTA,
          growthBI=growthBI
    )
  })
}

#----- SOLUTIONS -----
#solution is saved as a data-frame the output of "ode" function from
#deSolve (the ode solver used by the model)
#it solves the "rates" function, with initial state "y",
#at the discrete time steps "times", with parameters "param"
#ode(y, times, func, parms, method )
out <- as.data.frame(ode(y=state, times=times, func=rates, parms=param))

#print the result on xlsx file
require(writexl)
write_xlsx(out,path = here("", "VLESout.xlsx"))


#remove comments with ctrl+shift+C to run the plots
# #
# #OUTPUT PLOTTING
# #print outputs as time functions to visualize results
# #
# #State variables
# #Morphology
# #Shallow intertidal
# plot(out$time+1980,out$SI,ylim=c(0,max(out$SI)),lwd = 2,xlab = "time", ylab = "SI - Shallow Intertidal",type="l")
# points(x = c(23+1980),
#        y = c(291),
#        pch = 16,
#        col = "#f29900")
# abline(v=20+1980, col="purple",lwd = 2)
# abline(v=40+1980, col="green",lwd = 2)
# grid(nx = NULL, ny = NULL,
#      lty = 2,      # Grid line type
#      col = "gray", # Grid line color
#      lwd = 2)      # Grid line width
# #Deep Subtidal
# plot(out$time,out$DS,ylim=c(0,max(out$DS)),xlab = "time", ylab = "DS - Deep Subtidal",type="l")
# plot(out$time,out$SM,ylim=c(0,max(out$SM)),xlab = "time", ylab = "SM - Salt Marshes",type="l")
# points(x = c(26,33),
#        y = c(46.54, 45.35),
#        pch = 16,
#        col = "#f29900")
# abline(v=20, col="red")
# #Sea grasses
# plot(out$time,out$SG,ylim=c(0,120),xlab = "time", ylab = "SG - Sea Grasses")
# points(x = c(24,30,37),
#        y = c(36.87, 38.06,67.94),
#        pch = 16,
#        col = "#f29900")
# abline(v=20, col="red")
# #Benthic Diatomes
# plot(out$time,out$BD,ylim=c(0,max(out$BD)),xlab = "time", ylab = "BD - Benthic Diatoms")
# points(x = c(27),
#        y = c(100),
#        pch = 16,
#        col = "#f29900")
# abline(v=20, col="red")
# #bare bottom
# plot(out$time,out$BB,ylim=c(0,max(out$BB)),xlab = "time", ylab = "BB - Bare Bottom")
# abline(v=20, col="red")
# plot(out$time,out$NC,ylim=c(0,max(out$NC)),xlab = "time", ylab = "NC - Navigable Canals")
# points(x = c(27),
#        y = c(49),
#        pch = 16,
#        col = "#f29900")
# abline(v=20, col="red")
# #Fauna
# #BI Birds
# plot(out$time,out$BI,ylim=c(0,max(out$BI)),xlab = "time", ylab = "BI")
# points(x = c(28),
#        y = c(0.331),
#        pch = 16,
#        col = "#00f04c")
# abline(v=20, col="red")
# #TA clam
# plot(out$time,out$TA,ylim=c(0,30),xlab = "time", ylab = "TA")
# points(x = c(28),
#        y = c(11.17),
#        pch = 16,
#        col = "#00f04c")
# abline(v=20, col="red")
# #DE demersal
# plot(out$time,out$DE,ylim=c(0,max(out$DE)))
# points(x = c(28),
#        y = c(0.50),
#        pch = 16,
#        col = "#00f04c")
# abline(v=20, col="red")
# #SB seabream
# plot(out$time,out$SB,ylim=c(0,max(out$SB)))
# points(x = c(28),
#        y = c(0.45),
#        pch = 16,
#        col = "#00f04c")
# abline(v=20, col="red")
# #MU mullets
# plot(out$time,out$MU,ylim=c(0,max(out$MU)))
# points(x = c(28),
#        y = c(8.82),
#        pch = 16,
#        col = "#00f04c")
# abline(v=20, col="red")
# #LA seabass
# plot(out$time,out$LA,ylim=c(0,max(out$LA)))
# points(x = c(28),
#        y = c(0.69),
#        pch = 16,
#        col = "#00f04c")
# abline(v=20, col="red")
# 
# #Actors
# #bird hunters
# plot(out$time,out$HUNTt,ylim=c(0,max(out$HUNTt)))
# points(x = c(37),
#        y = c(1320),
#        pch = 16,
#        col = "#ff146a")
# abline(v=20, col="red")
# #recreational/sport fishers
# plot(out$time,out$FSt,ylim=c(0,4500))
# points(x = c(26,37),
#        y = c(4000,2500),
#        pch = 16,
#        col = "#ff146a")
# abline(v=20, col="red")
# #artisanal fishers
# plot(out$time,out$FAt,ylim=c(0,1800))
# points(x = c(19,26,34),
#        y = c(1740,1354,865),
#        pch = 16,
#        col = "#ff146a")
# abline(v=20, col="red")
# #clam fishers
# plot(out$time,out$FTt,ylim=c(0,max(out$FTt)))
# points(x = c(26,37),
#        y = c(1041,487),
#        pch = 16,
#        col = "#ff146a")
# abline(v=20, col="red")
# #boaters
# plot(out$time,out$BOATt,ylim=c(0,30000))
# abline(v=20, col="red")
# points(x = c(37),
#        y = c(24700),
#        pch = 16,
#        col = "#ff146a")
# abline(v=20, col="red")
# #traditional rowers
# plot(out$time,out$ROWt,ylim=c(0,max(out$ROWt)))
# points(x = c(37),
#        y = c(3034),
#        pch = 16,
#        col = "#ff146a")
# abline(v=20, col="red")
# #college students
# plot(out$time,out$EDUt,ylim=c(0,max(out$EDUt)))
# points(x = c(37),
#        y = c(107514),
#        pch = 16,
#        col = "#ff146a")
# abline(v=20, col="red")
# 
# #ES - ecosystem services of the Venice Lagoon
# #Provisioning ES
# plot(out$time,out$Hunting,ylim=c(0,max(out$Hunting)))
# abline(v=20, col="red")
# #clam harvesting
# plot(out$time,out$Clam_harvesting,ylim=c(0,max(out$Clam_harvesting)))
# points(x = c(37),
#        y = c(5),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# #artisanal fishing
# plot(out$time,out$Artisanal_fishing,ylim=c(0,1.6))
# points(x = c(42),
#        y = c(1.471),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# #recreational fishing
# plot(out$time,out$Recreational_fishing,ylim=c(0,0.16))
# points(x = c(26,42), #source:300ton/year in 2006
#        y = c(0.7317, 0.1334),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# 
# #Cultural ES
# #Tourism
# plot(out$time,out$Tourism,ylim=c(0,max(out$Tourism)))
# points(x = c(39),
#        y = c(2400000),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# #Traditional Rowing
# plot(out$time,out$Traditions,ylim=c(0,max(out$Traditions)))
# points(x = c(39),
#        y = c(215000),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# #Recreational Navigation
# plot(out$time,out$Navigation,ylim=c(0,max(out$Navigation)))
# points(x = c(39),
#        y = c(372763),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# #Environmental Education
# plot(out$time,out$Cognitive_development,ylim=c(0,max(out$Cognitive_development)))
# points(x = c(39),
#        y = c(13955),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# 
# #####Regulating ES
# #Lifecycle Mainteinance
# plot(out$time,out$Lifecycle_mainteinance,ylim=c(0,0.2))
# points(x = c(28),
#        y = c(0.18),
#        pch = 16,
#        col = "#00f04c")
# abline(v=20, col="red")
# #water purification
# plot(out$time,out$Water_purification,ylim=c(0,max(out$Water_purification)))
# points(x = c(34),
#        y = c(0.12),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# #carbon sequestration
# plot(out$time,out$Climate_regulation,ylim=c(0,max(out$Climate_regulation)))
# points(x = c(39),
#        y = c(74800),
#        pch = 16,
#        col = "blue")
# abline(v=20, col="red")
# #Erosion Prevention
# plot(out$time,out$Erosion_prevention1,ylim=c(0,max(out$Erosion_prevention1)))
# abline(v=20, col="red")
# plot(out$time,out$Erosion_prevention2,ylim=c(0,max(out$Erosion_prevention2)))
# abline(v=20, col="red")
