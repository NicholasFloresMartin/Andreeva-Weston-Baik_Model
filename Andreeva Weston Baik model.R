
rm(list = ls())

###############
## Read data ##
###############

B6_1750 <- read.csv("No_V_BD_Freq6.csv")
VB6_1750 <- read.csv("Vibr_BD_Freq6.csv")
B6_4000 <- read.csv("No_V_BD_Freq6.csv")
VB6_4000 <- read.csv("Vibr_BD_Freq6.csv")
B10_1000 <- read.csv("No_V_BD_Freq10.csv")
VB10_1000 <- read.csv("Vibr_BD_Freq10.csv")

B6 <- read.csv("No_V_BD_Freq6.csv")


################
## Functions  ##
################

# Note, the given equations for calculating the damping "Q factors" are for calculating 1/Q. These have been written as
# Qrad, Qth, Qvis for simplicity, thus necessitating them being written as 1/Qrad, 1/Qth, 1/Qvis is the extinction coeff.
# function.

Wres <- function(Bsize){2*pi*((6000000/(Bsize)))}
#Angular frequency of resonance for a bubble of n(R)dR - 6 is taken from the approximation of f(kHz)*Diam(mm)=6 

Qrad <- function(Wres, R0) {((Wres * R0)/C)}
#Acoustic radiation damping - taken from Ainslie and Leighton (2011), eq 37

Qth <- function(Wres, R0) {(3*(y-1))/(R0/(sqrt(Dp/(2*Wres))))}
#Acoustic thermal damping - taken from Ainslie and Leighton (2011), eq 38

Qvis <- function(Eta, R0, Wres, p) {(2*Eta*W)/(R0^2*p*Wres)}
#Acoustic viscous damping - taken from Ainslie and Leighton (2011), eq 39 - modified from Andreeva's (1964) to include Eq.(2d) from Baik (2013)

deltaWeston <- function(Qrad,Qth,Qvis,Wres) {((Qrad)*(Wres/W)) + ((Qth)*(Wres/W)^(5/2)) + ((Qvis)*(Wres/W)^2)}
#Dimensionless damping coefficient

#delta <- function(Qrad,Qth,Qvis) {Qrad+Qth+Qvis}
#Not in use

Scattering <- function(R0,Wres,deltaWeston) {(4*pi*R0^2)/((Wres^2/W^2 - 1)^2 + deltaWeston^2)}
#Scattering coefficient

Extinction <- function(Scat,Qrad,Qth,Qvis,Wres) {Scat*(1+((1/Qrad)/(1/Qth))*(Wres/W)^(7/2) + ((1/Qrad)/(1/Qvis))*(Wres/W)^3)}
#Extinction coefficient   


###############
## Constants ##
###############

W <- 2*pi*1000
#Angular frequency of driving acoustic field (change to either: 1000 Hz, 1750 Hz, or 4000 Hz)

C <- 1498
#Speed of sound in water (1498 m/s)

Kgas <- 0.0262
#Thermal conductivity of air (at 200000 Pa or 2 bar, and 18 celcius, in W/mK)  

pgas <- 2.39
#Equilibrium gas (air) density (at 200000 Pa or 2 bar, and 18 celcius, in kg/m^3)

Cp <- 1004.23
#Specific heat capacity of gas (air) at constant pressure (at 200000 Pa or 2 bar, and 18 celcius, at 18 celcius, in J/kg Celcius)

y <- 1.4
#Specific heat ratio of gas (air)

Dp <- Kgas/pgas*Cp
#Gas diffusivity

p <- 997
#Density of Water (in kg/m^3)

Eta <- 0.00105
#Shear viscosity of Water at 18 celcius (in Pa s)



###################
## Run Functions ##
###################

## For each set-up ##

## Testing... ##

B6$Wres <- mapply(Wres, Bsize=B6$Max)
B6$Qrad <- mapply(Qrad, Wres = B6$Wres, R0 = B6$Max/2000000) 
B6$Qvis <- mapply(Qvis, Eta, p, Wres = B6$Wres, R0 = B6$Max/20000) 
B6$Qth <- mapply(Qth, Wres = B6$Wres, R0 = B6$Max/2000)
B6$deltaWeston <- mapply(deltaWeston, Qrad = B6$Qrad, Qth = B6$Qth, Qvis = B6$Qvis, Wres = B6$Wres)
B6$Scat <- mapply(Scattering, R0 = B6$Max/2, Wres = B6$Wres, deltaWeston = B6$deltaWeston) 
B6$Extinct <- mapply(Extinction, Scat = B6$Scat, Qrad = B6$Qrad, Qth = B6$Qth,
                          Qvis = B6$Qvis, Wres = B6$Wres)
B6$Absorption <- B6$Extinct - B6$Scat


## B1750 ##

B6_1750$Wres <- mapply(Wres, Bsize=B6_1750$Max)
B6_1750$Qrad <- mapply(Qrad, Wres = B6_1750$Wres, R0 = B6_1750$Max/2000000) 
B6_1750$Qvis <- mapply(Qvis, Eta, p, Wres = B6_1750$Wres, R0 = B6_1750$Max/20000) 
B6_1750$Qth <- mapply(Qth, Wres = B6_1750$Wres, R0 = B6_1750$Max/2000)
B6_1750$DW <- mapply(deltaWeston, Qrad = B6_1750$Qrad, Qth = B6_1750$Qth, Qvis = B6_1750$Qvis, Wres = B6_1750$Wres)
B6_1750$Scat <- mapply(Scattering, R0 = B6_1750$Max/2, Wres = B6_1750$Wres, deltaWeston = B6_1750$DW) 
B6_1750$Extinct <- mapply(Extinction, Scat = B6_1750$Scat, Qrad = B6_1750$Qrad, Qth = B6_1750$Qth,
                          Qvis = B6_1750$Qvis, Wres = B6_1750$Wres)
B6_1750$Absorption <- B6_1750$Extinct - B6_1750$Scat


## VB1750 ##

VB6_1750$Wres <- mapply(Wres, Bsize=VB6_1750$Max)
VB6_1750$Qrad <- mapply(Qrad, Wres = VB6_1750$Wres, R0 = VB6_1750$Max/2000000) 
VB6_1750$Qvis <- mapply(Qvis, Eta, p, Wres = VB6_1750$Wres, R0 = VB6_1750$Max/20000) 
VB6_1750$Qth <- mapply(Qth, Wres = VB6_1750$Wres, R0 = VB6_1750$Max/2000)
VB6_1750$DW <- mapply(deltaWeston, Qrad = VB6_1750$Qrad, Qth = VB6_1750$Qth, Qvis = VB6_1750$Qvis, Wres = VB6_1750$Wres)
VB6_1750$Scat <- mapply(Scattering, R0 = VB6_1750$Max/2, Wres = VB6_1750$Wres, deltaWeston = VB6_1750$DW) 
VB6_1750$Extinct <- mapply(Extinction, Scat = VB6_1750$Scat, Qrad = VB6_1750$Qrad, Qth = VB6_1750$Qth,
                           Qvis = VB6_1750$Qvis, Wres = VB6_1750$Wres)
VB6_1750$Absorption <- VB6_1750$Extinct - VB6_1750$Scat


## B4000 ##

B6_4000$Wres <- mapply(Wres, Bsize=B6_4000$Max)
B6_4000$Qrad <- mapply(Qrad, Wres = B6_4000$Wres, R0 = B6_4000$Max/2000000) 
B6_4000$Qvis <- mapply(Qvis, Eta, p, Wres = B6_4000$Wres, R0 = B6_4000$Max/20000) 
B6_4000$Qth <- mapply(Qth, Wres = B6_4000$Wres, R0 = B6_4000$Max/2000)
B6_4000$DW <- mapply(deltaWeston, Qrad = B6_4000$Qrad, Qth = B6_4000$Qth, Qvis = B6_4000$Qvis, Wres = B6_4000$Wres)
B6_4000$Scat <- mapply(Scattering, R0 = B6_4000$Max/2, Wres = B6_4000$Wres, deltaWeston = B6_4000$DW) 
B6_4000$Extinct <- mapply(Extinction, Scat = B6_4000$Scat, Qrad = B6_4000$Qrad, Qth = B6_4000$Qth,
                          Qvis = B6_4000$Qvis, Wres = B6_4000$Wres)
B6_4000$Absorption <- B6_4000$Extinct - B6_4000$Scat


## VB4000 ##

VB6_4000$Wres <- mapply(Wres, Bsize= VB6_4000$Max)
VB6_4000$Qrad <- mapply(Qrad, Wres = VB6_4000$Wres, R0 = VB6_4000$Max/2000000) 
VB6_4000$Qvis <- mapply(Qvis, Eta, p, Wres = VB6_4000$Wres, R0 = VB6_4000$Max/20000) 
VB6_4000$Qth <- mapply(Qth, Wres = VB6_4000$Wres, R0 = VB6_4000$Max/2000)
VB6_4000$DW <- mapply(deltaWeston, Qrad = VB6_4000$Qrad, Qth = VB6_4000$Qth, Qvis = VB6_4000$Qvis, Wres = VB6_4000$Wres)
VB6_4000$Scat <- mapply(Scattering, R0 = VB6_4000$Max/2, Wres = VB6_4000$Wres, deltaWeston = VB6_4000$DW) 
VB6_4000$Extinct <- mapply(Extinction, Scat = VB6_4000$Scat, Qrad = VB6_4000$Qrad, Qth = VB6_4000$Qth,
                          Qvis = VB6_4000$Qvis, Wres = VB6_4000$Wres)
VB6_4000$Absorption <- VB6_4000$Extinct - VB6_4000$Scat


## B1000 ##

B10_1000$Wres <- mapply(Wres, Bsize=B10_1000$Max)
B10_1000$Qrad <- mapply(Qrad, Wres = B10_1000$Wres, R0 = B10_1000$Max/2000000) 
B10_1000$Qvis <- mapply(Qvis, Eta, p, Wres = B10_1000$Wres, R0 = B10_1000$Max/20000) 
B10_1000$Qth <- mapply(Qth, Wres = B10_1000$Wres, R0 = B10_1000$Max/2000)
B10_1000$DW <- mapply(deltaWeston, Qrad = B10_1000$Qrad, Qth = B10_1000$Qth, Qvis = B10_1000$Qvis, Wres = B10_1000$Wres)
B10_1000$Scat <- mapply(Scattering, R0 = B10_1000$Max/2, Wres = B10_1000$Wres, deltaWeston = B10_1000$DW) 
B10_1000$Extinct <- mapply(Extinction, Scat = B10_1000$Scat, Qrad = B10_1000$Qrad, Qth = B10_1000$Qth,
                          Qvis = B10_1000$Qvis, Wres = B10_1000$Wres)
B10_1000$Absorption <- B10_1000$Extinct - B10_1000$Scat


## VB1000 ##

VB10_1000$Wres <- mapply(Wres, Bsize= VB10_1000$Max)
VB10_1000$Qrad <- mapply(Qrad, Wres = VB10_1000$Wres, R0 = VB10_1000$Max/2000000) 
VB10_1000$Qvis <- mapply(Qvis, Eta, p, Wres = VB10_1000$Wres, R0 = VB10_1000$Max/20000) 
VB10_1000$Qth <- mapply(Qth, Wres = VB10_1000$Wres, R0 = VB10_1000$Max/2000)
VB10_1000$DW <- mapply(deltaWeston, Qrad = VB10_1000$Qrad, Qth = VB10_1000$Qth, Qvis = VB10_1000$Qvis, Wres = VB10_1000$Wres)
VB10_1000$Scat <- mapply(Scattering, R0 = VB10_1000$Max/2, Wres = VB10_1000$Wres, deltaWeston = VB10_1000$DW) 
VB10_1000$Extinct <- mapply(Extinction, Scat = VB10_1000$Scat, Qrad = VB10_1000$Qrad, Qth = VB10_1000$Qth,
                           Qvis = VB10_1000$Qvis, Wres = VB10_1000$Wres)
VB10_1000$Absorption <- VB10_1000$Extinct - VB10_1000$Scat




############
## n(R)dR ##
############

B6$nExtinct <- B6$Extinct * B6$Freq

B6_1750$nExtinct <- B6_1750$Extinct * B6_1750$Freq
VB6_1750$nExtinct <- VB6_1750$Extinct * VB6_1750$Freq
B6_4000$nExtinct <- B6_4000$Extinct * B6_4000$Freq
VB6_4000$nExtinct <- VB6_4000$Extinct * VB6_4000$Freq
B10_1000$nExtinct <- B10_1000$Extinct * B10_1000$Freq
VB10_1000$nExtinct <- VB10_1000$Extinct * VB10_1000$Freq


###############
## Plotting  ##
###############

library(kelvin)
library(kitagawa)

B6$Diamm <- B6$Max/1000

## Plot Dimensionless Coefficients##

png(file = "Dimensionless Coefficients Beta.png",width = 100, height = 100, res=300, units = "mm")

plot(log10(B6$Max), B6$deltaWeston, type = "l", xaxt="na", col = "grey1", lty = 1, ylim = c(-0.01, 0.5), cex.axis=0.8, xlab = "", ylab = "")
logticks(ax=1, cex.axis=0.8)
lines(log10(B6$Max), B6$Qth, type = "l", col = "grey20", lty = 2)
lines(log10(B6$Max), B6$Qvis, type = "l", col = "grey40", lty = 3)
lines(log10(B6$Max), B6$Qrad, type = "l", col = "grey60", lty = 4)
mtext("Log Bubble Diam. (µm)", side=1, line=2, cex=0.8)
mtext("Dimensionless damping const.", side=2, line=2, cex=0.8)
legend("topleft", legend=c("Delta Weston", "1/Qth", "??vis", "1/Qrad"), lty=c(1, 2, 3, 4), col=c("grey1", "grey20", "grey40", "grey60"), cex=0.6)

dev.off()

png(file = "Dimensionless Coefficients Qvis.png",width = 100, height = 100, res=300, units = "mm")

plot(log10(B6$Max), B6$deltaWeston, type = "l", xaxt="na", col = "grey1", lty = 1, ylim = c(-0.01, 0.5), cex.axis=0.8, xlab = "", ylab = "")
logticks(ax=1, cex.axis=0.8)
lines(log10(B6$Max), B6$Qth, type = "l", col = "grey20", lty = 2)
lines(log10(B6$Max), B6$Qvis, type = "l", col = "grey40", lty = 3)
lines(log10(B6$Max), B6$Qrad, type = "l", col = "grey60", lty = 4)
mtext("Log Bubble Diam. (µm)", side=1, line=2, cex=0.8)
mtext("Dimensionless damping const.", side=2, line=2, cex=0.8)
legend("topleft", legend=c("Delta Weston", "1/Qth", "1/Qvis", "1/Qrad"), lty=c(1, 2, 3, 4), col=c("grey1", "grey20", "grey40", "grey60"), cex=0.6)

dev.off()



png(file = "Delta Weston.png",width = 100, height = 100, res=300, units = "mm")
plot(log10(B6$Max), log10(B6$deltaWeston), type = "l", xaxt="na", yaxt="na", col = "grey1", lty = 1, cex.axis=0.8, xlab = "", ylab = "")
logticks(ax=1, cex.axis=0.8)
logticks(ax=2, cex.axis=0.8)
mtext("Log Bubble Diam. (µm)", side=1, line=2, cex=0.8)
mtext("Dimensionless damping const.", side=2, line=2, cex=0.8)
dev.off()



## Test Plot for Extinction cross section##


png(file = "Extinction.png",width = 125, height = 125, res=300, units = "mm")

plot(log10(B6$Max/1000), log10(B6$Extinct/1000000), type = "l", col = "grey1", xaxt="na", yaxt="na", lty = 1, xlab = "", ylab = "")
logticks(ax=1, cex.axis=0.8)
logticks(ax=2, cex.axis=0.8)
lines(log10(B6$Max/1000), log10(B6$Scat/1000000), type = "l", col = "grey30", lty = 2)
lines(log10(B6$Max/1000), log10(B6$Absorption/1000000), type = "l", col = "grey40", lty = 3)
mtext("Bubble Diameter (mm)", side=1, line=2, cex=0.9)
mtext(expression(paste(plain ("Extinct. Cross Section ("), plain("mm") ^ plain("2"), plain(")"))), side=2, line=2, cex=0.9)
legend("bottomright", legend=c("Extinction", "Scattering", "Absorption"), lty=c(1, 2, 3), col=c("grey1", "grey20", "grey40"), cex=0.8)

dev.off()


## Test Plot for Extinction cross section n(R)dR ##

png(file = "Extinction n(R)dR.png",width = 125, height = 125, res=300, units = "mm")
plot(B6$Max, B6$nExtinct/1000000000000, type = "l", col = "grey1", lty = 1, xlim = c(10, 6000), xlab = "", ylab = "", cex.axis=0.8)
mtext("Bubble Diameter (µm)", side=1, line=2, cex=0.9)
mtext(expression(paste(plain ("Extinction Cross Section ("), plain("m") ^ plain("2"), plain(") per unit vol."))), side=2, line=2, cex=0.9)
dev.off()




###########################
## Plotting all the data ##
###########################


## Plotting Theoretical extinction ##

png(file = "Theoretical Extinction.png",width = 225, height = 100, res=300, units = "mm")
par(mfrow=c(1,3), mgp=c(2,1,0), mar=c(1,1,2,0), oma=c(3,3,3,3))

plot(log10(B10_1000$Max/2000), log10(B10_1000$Extinct/1000000), type = "l", col = "grey1", xaxt="na", main="1000 Hz - 10L/min", yaxt="na", lty = 1, xlab = "", ylab = "")
logticks(ax=1, cex.axis=1.1)
logticks(ax=2, cex.axis=1.1)
lines(log10(B10_1000$Max/1000), log10(B10_1000$Scat/1000000), type = "l", col = "grey30", lty = 2)
lines(log10(B10_1000$Max/1000), log10(B10_1000$Absorption/1000000), type = "l", col = "grey40", lty = 3)
mtext("Bubble Diameter (mm)", side=1, line=2, cex=0.9)
mtext(expression(paste(plain ("Extinct. Cross Section ("), plain("mm") ^ plain("2"), plain(") bubble"))), side=2, line=2, cex=0.9)

plot(log10(B6_1750$Max/2000), log10(B6_1750$Extinct/1000000), type = "l", col = "grey1", xaxt="na", main="1750 Hz - 6L/min", yaxt="na", lty = 1, xlab = "", ylab = "")
logticks(ax=1, cex.axis=1.1)
lines(log10(B6_1750$Max/1000), log10(B6_1750$Scat/1000000), type = "l", col = "grey30", lty = 2)
lines(log10(B6_1750$Max/1000), log10(B6_1750$Absorption/1000000), type = "l", col = "grey40", lty = 3)
legend("topleft", legend=c("Extinction", "Scattering", "Absorption"), lty=c(1, 2, 3), col=c("grey1", "grey20", "grey40"), cex=1.0)
mtext("Bubble Diameter (mm)", side=1, line=2, cex=0.9)

plot(log10(B6_4000$Max/2000), log10(B6_4000$Extinct/1000000), type = "l", col = "grey1", xaxt="na", yaxt="na", main="4000 Hz - 6L/min", lty = 1, xlab = "", ylab = "")
logticks(ax=1, cex.axis=1.1)
lines(log10(B6_4000$Max/1000), log10(B6_4000$Scat/1000000), type = "l", col = "grey30", lty = 2)
lines(log10(B6_4000$Max/1000), log10(B6_4000$Absorption/1000000), type = "l", col = "grey40", lty = 3)
mtext("Bubble Diameter (mm)", side=1, line=2, cex=0.9)
dev.off()




png(file = "Extinction n(R)dR b.png",width = 175, height = 125, res=300, units = "mm")
par(mfrow=c(2,2), mgp=c(2,1,0), mar=c(1,2,1,1), oma=c(3,3,3,3))
plot(B10_1000$Max/2, B10_1000$nExtinct/1000000000000, type = "l", col = "grey1", cex.main=1.0, cex.axis=1, lty = 1, xlab = "", ylab = "")
mtext(expression(paste(plain ("\n Extinct. Cross Section ("), plain("m") ^ plain("2"), plain(") per unit vol."))), side=2, line=3, cex=0.9)
mtext("1000 Hz - 10 L/min" , side=2, line=2, cex=1)
mtext("Bubble radius (µm)", side=1, line=2, cex=0.9)

plot(VB10_1000$Max/2, VB10_1000$nExtinct/1000000000000, type = "l", col = "grey1", yaxt="na", cex.main=1.0, cex.axis=1, lty = 1, xlab = "", ylab = "")
axis(side = 4, cex.axis=1)
mtext("Bubble radius (µm)", side=1, line=2, cex=0.9)
dev.off()


png(file = "Extinction n(R)dR a.png",width = 175, height = 125, res=300, units = "mm")
par(mfrow=c(2,2), mgp=c(2,1,0), mar=c(1,2,1,1), oma=c(3,3,3,3))

plot(B6_1750$Max/2, B6_1750$nExtinct/1000000000000, type = "l", col = "grey1", main="Standard injection", xaxt="na", lty = 1, xlim=c(0,4000), cex.axis=1, xlab = "", ylab = "")
mtext(expression(paste(plain ("1750 Hz - 6 L/min" ))), side=2, line=2, cex=1)

plot(VB6_1750$Max/2, VB6_1750$nExtinct/1000000000000, type = "l", col = "grey1", main="Vibrated injection",  yaxt="na", xaxt="na", xlim=c(0,4000), lty = 1, xlab = "", ylab = "")
axis(side = 4, cex.axis=1)

plot(B6_4000$Max/2, B6_4000$nExtinct/1000000000000, type = "l", col = "grey1", lty = 1, xlab = "", ylab = "", cex.axis=1, xlim=c(0,4000))
mtext("Bubble radius (µm)", side=1, line=2, cex=1)
mtext(expression(paste(plain ("Extinct. Cross Section ("), plain("m") ^ plain("2"), plain(") per unit vol."))), side=2, line=3, cex=1)
mtext(expression(paste(plain ("4000 Hz - 6 L/min"))), side=2, line=2, cex=1)

plot(VB6_4000$Max/2, VB6_4000$nExtinct/1000000000000, type = "l", col = "grey1",  yaxt="na", lty = 1, xlab = "", ylab = "", cex.axis=1, xlim=c(0,4000))
axis(side = 4, cex.axis=1)
mtext("Bubble radius (µm)", side=1, line=2, cex=1)
dev.off()



#########################
## 90% of Area Covered ##
#########################

write.csv(B10_1000,'B10_1000.csv', row.names = TRUE)
write.csv(VB10_1000,'VB10_1000.csv', row.names = TRUE)
write.csv(B6_1750,'B6_1750.csv', row.names = TRUE)
write.csv(VB6_1750,'VB6_1750.csv', row.names = TRUE)
write.csv(B6_4000,'B6_4000.csv', row.names = TRUE)
write.csv(VB6_4000,'VB6_4000.csv', row.names = TRUE)
