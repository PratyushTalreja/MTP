start <- Sys.time();
TypeLidf <- 2;
# if 2-parameters LIDF: TypeLidf=1
if (TypeLidf==1)
    LIDFa	<-	-0.35;
    LIDFb	<-	-0.15;
# if ellipsoidal LIDF: TypeLidf=2
if (TypeLidf==2)
    # 	LIDFa	= average leaf angle (degrees) 0 = planophile	/	90 = erectophile
    # 	LIDFb = 0
    LIDFa	<-	c(9, 21, 33, 45, 57, 69, 81);
    LIDFb	<-	0;
##############################
#LEAF CHEM & STR PROPERTIES	##
##############################
Cab		<-	c(5, 17, 29, 41, 52, 64, 76);		# chlorophyll content (µg.cm-2) 
Car		<-	8;		# carotenoid content (µg.cm-2)
Cbrown	<- 0.0;	# brown pigment content (arbitrary units)
Cw		<-	0.01;	# EWT (cm)
Cm		<-	0.009;	# LMA (g.cm-2)
N		<-	c(1.1, 1.3, 1.5, 1.8, 2.0, 2.2, 2.4);	# structure coefficient
#variable arrays as per Durbha et al.(2007)

##################################
#	Soil Reflectance Properties	##
##################################
# rsoil1 = dry soil
# rsoil2 = wet soil
Rsoil1 <- data[,10];
Rsoil2 <- data[,11];
psoil	<-	0;		# soil factor (psoil=0: wet soil / psoil=1: dry soil)
rsoil0 <- psoil * Rsoil1 + (1 - psoil) * Rsoil2;

##################################
##	4SAIL canopy structure parm	##
##################################
	LAI		<-	c(0.4, 1.4, 2.5, 3.5, 4.6, 5.6, 6.7);     	# leaf area index (m^2/m^2)
	hspot	<-	c(0.06, 0.21, 0.36, 0.51, 0.65, 0.80, 0.95);       # hot spot
	tts		<-	45;		# solar zenith angle (°)
	tto		<-	10.;		# observer zenith angle (°)
	psi		<-	0.;         # azimuth (°)
##############################
#	direct / diffuse light	##
##############################
# the direct and diffuse light are taken into account as proposed by:
# Francois et al. (2002) Conversion of 400-1100 nm vegetation albedo 
# measurements into total shortwave broadband albedo using a canopy 
# radiative transfer model, Agronomie
# Es = direct
# Ed = diffuse
Es <- data[,8];Ed=data[,9];
rd <- pi/180;
skyl	<-	0.847- 1.61*sin((90-tts)*rd)+ 1.04*sin((90-tts)*rd)*sin((90-tts)*rd); # # diffuse radiation
PARdiro	<-	(1-skyl)*Es;
PARdifo	<-	(skyl)*Ed;
flag <- 1;#counter initialization
pratyush_2 <- matrix(0, 15625, 2106);
    for (i in 1:length(LAI))
    {
        for (j in 1:length(Cab))
        {
            for (k in 1:length(LIDFa))
            {
                for (l in 1:length(hspot))
                {
                    for (m in 1:length(N))
                    {
                      #prosailout <- PROSAIL(N[m],Cab[j],Car,Cbrown,Cw,Cm,LIDFa[k],LIDFb,TypeLidf,LAI[i],hspot[l],tts,tto,psi,rsoil0);
                      prosailout <- PROSAIL(N[m],Cab[j],Car,Cbrown,Cw,Cm,psoil,LAI[i],TypeLidf,LIDFa[k],LIDFb,hspot[l],tts,tto,psi,NULL, NULL)
                      pratyush_2[flag,] <- c(LAI[i], Cab[j], LIDFa[k], hspot[l], N[m], t(prosailout));
                      flag = flag+1 #counter increment
                    }
                }
            }
        }
    }
end <- Sys.time() - start;
print(end)