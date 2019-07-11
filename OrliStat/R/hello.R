#' Intervalo de Confianza
#'
#' @param sigma Sigma de la poblacion
#' @param n muestra
#' @param media promedio de la muestra
#' @export IC
IC <- function(sigma,n,media) {
  z1=1.645 #Valor z para un 90% de confianza
  z2=1.96 #Valor z para un 95% de confianza
  z3=2.575 #Valor z para un 99% de confianza

  LIC1=media-(z1*(sigma/sqrt(n))) #Limite inferior 90%
  LSC1=media+(z1*(sigma/sqrt(n))) #Limite superior 90%

  LIC2=media-(z2*(sigma/sqrt(n))) #Limite inferior 95%
  LSC2=media+(z2*(sigma/sqrt(n))) #Limite superior 95%

  LIC3=media-(z3*(sigma/sqrt(n))) #Limite inferior 99%
  LSC3=media+(z3*(sigma/sqrt(n))) #Limite superior 99%

  cat("90%: IC=",LIC1,",",LSC1,"\n")
  cat("95%: IC=",LIC2,",",LSC2,"\n")
  cat("99%: IC=",LIC3,",",LSC3,"\n")
}


#' Calculo tamanno de muestra
#'
#' @param p proporcion
#' @param d error
#' @param N Poblacion
#' @export muestra
#'

muestra <- function(p,d,N) {
  z <- 1.96
  q <- 1-p

  n=((z*sqrt(p*q))/d)^2 #Tama?o de la muestra
  n= round(n) #Se redondea el tamano de la muestra

  cat("Con un 95% de confianza el tamano de la muestra es",n,"\n")

  if(!is.na(N)) {
    n2=n/(1+(n/N))
    n2= round(n2)
    cat("Con correccion por finitud es",n2,"\n")
  }
}
