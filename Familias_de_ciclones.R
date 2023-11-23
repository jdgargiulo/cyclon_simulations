## Parametrizaciones de Familias de ciclones

parametro<-c("a", "b", "S", "Ds", "h", "z", "H",   "B")
stair<-    c( 0.5,  0.2,  0.5,   0.5,  1.5, 2.5, 4 ,  0.375)
swift<-    c( 0.44, 0.21, 0.5,   0.4,  1.4, 2.5, 3.9, 0.4)
echeverri<-c( 0.5,  0.2,  0.625, 0.5,  1.5, 2.5, 4,   0.375)
SRI<-      c( 0.25,0.125, 0.35, 0.27, 0.45,1.13,1.58,0.24)
GK<-       c(0.25,0.125,0.23,0.23,0.4,0.9,1.31,0.2)  
SCC<-      c(0.3,0.15, 0.35,0.27,0.43,1.13,1.58,0.25)
URG<-      c( 0.5,  0.25, 0.75, 0.5,   2,   2,   4, 0.5)


Familia<-data.frame(parametro, stair, swift, echeverri, SRI,GK, SCC, URG)
Familia

modelo_de_ciclon <- function(Dc, familia) {
  if (familia == "stair") {
    parametros <- Dc * Familia$stair
  } else if (familia == "swift") {
    parametros <- Dc * Familia$swift
  } else if (familia == "echeverri") {
    parametros <- Dc * Familia$echeverri
  } else if (familia == "SRI") {
    parametros <- Dc * Familia$SRI
  } else if (familia == "GK") {
    parametros <- Dc * Familia$GK
  } else if (familia == "SCC") {
    parametros <- Dc * Familia$SCC
  } else if (familia == "URG") {
    parametros <- Dc * Familia$URG
  } else {
    stop("Unknown familia")
  }
  result<-data.frame(parametro=Familia$parametro,valor=parametros)
  return(result)
}





