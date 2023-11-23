## Inicialización de parámetros



rop<-2000                              # densidad de partículas [kg/m3]
Dparticulas<-seq(1,30,1)              # diametro particulas [micrones]
Dp<-Dparticulas*0.000001              # diámetro de particlas [m]
flow<-seq(16.7,100.2,15)                # Caudal entrada [l/min]
Q<-flow*0.0000167                    # caudal entrada [m3/s]
Dp;Q

#length(flow)


## 1) Definimos familia a utilizar
## Familias: stair, swift, echeverri, SRI, GK, SCC, URG

familia<-"GK"

# Definir diámetro del cuerpo Dc y Familia

Dc<-0.035    # Diametro del cuerpo en [m]


resultado<-modelo_de_ciclon(Dc,familia)

data.frame(parametro=resultado$parametro, valor=resultado$valor*100)

a<-resultado[1,2]
b<-resultado[2,2]
S<-resultado[3,2]
Ds<-resultado[4,2]
h<-resultado[5,2]
z<-resultado[6,2]
H<-resultado[7,2]
B<-resultado[8,2]
N<-(1/a)*(h+z/2)
Vi<-Q/(a*b)



## 2) Factor de Configuración  

# 2.1) Longitud natural del ciclón (L)
L<- 2.9*Ds*(Dc**2/(a*b))**(1/3)
ifelse(L<H-S,print("Long natural correcta"), print("long natural incorrecta"))

# 2.2) Volumen evaluado sobre la salida del ciclón (Vsc)
Vsc<-(pi/4)*(S-a/2)*(Dc**2-Ds**2)

# 2.3) Volumen del ciclón evaluado sobre la long natural (Vr)
Kl<-Dc-(Dc-B)*((S+L-h)/z)
Vr<-(pi/4)*Dc**2*(h-S)+(pi/12)*Dc**2*(L+S-h)*(1+(Kl/Dc)+(Kl/Dc)**2)-(pi/4)*Ds**2*L

# 2.4) Factor de dimensionalidad de proporciones volumétricas (Kc)
Kc<-(Vsc+Vr/2)/(Dc**3)


# 2.5) Factor de Configuración (G)
G<- 8*Kc/((a/Dc*b/Dc)**2)
print(paste("Factor de Configuración:",G))


# 3) Tiempo de Relajación
Ti<-(rop*Dp**2)/(18*0.0000174)


# 4) Exponenete de vórtice
n<- 1-(1-0.67*Dc**0.14)*(273/283)**0.3
print(paste("Exponente de Vórtice:",n))

# 5) Eficiencia Fraccional
Eficiencia<-function(caudal, diametro){
  Ti<-(rop*diametro**2)/(18*0.0000174)
  1-exp(-2*(G*Ti*caudal*(n+1)/Dc**3)**(0.5/(n+1)))}

#Eficiencia(caudal=Q[2], diametro=Dp)

dflist <- lapply(Q, Eficiencia, Dp)
res<-data.frame(Dp,sapply(dflist,c))
colnames(res)<-c("Dp",flow)
res[]

# D50 en función del caudal
D50<-(9*0.0000174*B/(2*pi*N*Vi*(rop-1.2)))**(1/2)
D50*1000000


# 6) Velocidad de Saltación
# 6.1) Velocidad equivalente (W)
W<-(4*9.81*0.0000174*(rop-1)/(3*1**2))**(1/3)

# 6.2 ) Vel de saltación (Vs)
Vs<-4.913*W*(b/Dc)**0.4*Dc**0.067*Vi**(2/3)/(1-b/Dc)**(1/3)
print(paste("Velocidad de saltación[m/s]:",Vs))
ifelse(Vi>1.35*Vs, print("Vel de entrada muy alta"), print("Vel de entrada normal"))

# 7) Estimación de la caída de presión (Eq de Shepherd y Lapple)

# 6.1) N de Cabezas de velocidad a la entrada

Nh<-16*(a*b)/(Ds**2)    ## K=16

## 6.2) Caída de presión [Pa]

DeltaP<-0.5*1*Vi**2*Nh
print(paste("Caída de presión [Pa]:",DeltaP))
curvaPC<-data.frame(caudal=c(0,68,82,91,122,130,133), deltaP=c(118,98.1,78.5,58.9,39.2,20,0))
plot(flow, DeltaP, type = "b", ylab = "Caída de Presión [Pa]", xlab = "caudal [l/min]", xlim = c(0,140), ylim=c(0,120))
lines(curvaPC, col=2, lwd=4)


# RESUMEN
# Crear el gráfico inicial con la primera línea
plot(Dp*1000000,res[,2]*100, type = "l", xlab="Diámetro de Partícula [micrones]", ylab="Eficiencia [%]")

# Agregar las líneas adicionales en el mismo gráfico
for (i in 3:ncol(res)) {
  lines(res[i] * 100, lty = i, col = i)
}
legend(20,80, c(flow), lty=c(1,3:ncol(res)), col=c(1,3:ncol(res)), title = "Caudal [l/min]")

plot(Q/0.000016667, D50*1000000, xlab="Caudal [l/min]", ylab = "D50 [micrones]", type="b")

## Tiempo estimado de recolección de material
# 1) TSP: MP sedimentable total (rangos 50-100 micrones)
# suponemos una concentración media de 100 microgramos/m3 de TSP y eficiencia 100%
C<-100
MasaTSP<-Q*C*86400/100000 # masa diaria recolectada en gramos
print(paste("Potencial masa de TSP a recuperar[g/día]:",MasaTSP))
print(paste("Potencial masa de TSP en 30 días [g]:",MasaTSP*30))

# 2) MP10: valores de referencia para Tandil tomados de Sosa et al 2017
C_10<-25
Ti_10<-(rop*0.00001**2)/(18*0.0000174)
E_10<-1-exp(-2*(G*Ti_10*Q*(n+1)/Dc**3)**(0.5/(n+1)))
print(paste("Eficiencia de colección MP10:",E_10))
Masa_10<-Q*C_10*86400/100000
print(paste("Potencial masa de MP10 a recuperar[g/día]:",Masa_10))
print(paste("Potencial masa de MP10 en 30 días [g]:",Masa_10*30))

# 3) MP2.5: valores de referencia para Tandil tomados de Sosa et al 2017
C_25<-10
Ti_25<-(rop*0.0000025**2)/(18*0.0000174)
E_25<-1-exp(-2*(G*Ti_25*Q*(n+1)/Dc**3)**(0.5/(n+1)))
print(paste("Eficiencia de colección MP2.5:",E_25))
Masa_25<-Q*C_25*86400/100000
print(paste("Potencial masa de MP2.5 a recuperar[g/día]:",Masa_25))
print(paste("Potencial masa de MP2.5 en 30 días [g]:",Masa_25*30))