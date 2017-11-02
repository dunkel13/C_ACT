# Primer programa: llenar la tabla de rentistas (mujeres) con base a los valores de q_x
#======================================================================================

# Aqui se leen los datos. Para que el programa funcione, cambiar la ubicacion de los datos!!! 
#rentistas_m_2005_ <- read_excel("~/Septimo  Semestre/CÃƒÂ¡lculo Actuarial/rentistas_m_(2005).xlsx")
#View(rentistas_m_2005_)
rentistas_m_2005_ <- read_excel("~/R/FMwd/datos.xlsx", 
                                sheet = "rentistas.m.2005")
View(rentistas_m_2005_)


# Se establece el valor l_0=100000. 
x=100000
rentistas_m_2005_[1,3]=x

# Se calcula por medio de un comando "for" los valores d_x y l_x para la tabla rentistas (mujeres).
for(i in 1:111){
  rentistas_m_2005_[i,4]=rentistas_m_2005_[i,2]*rentistas_m_2005_[i,3];
  for(l in 2:111)
    rentistas_m_2005_[l,3]=rentistas_m_2005_[l-1,3]-rentistas_m_2005_[l-1,4]
}

# Se calcula por medio de un comando "for" los valores p_x para la tabla rentistas (mujeres).
for(k in 1:111){
  rentistas_m_2005_[k,5]=1-rentistas_m_2005_[k,2]
}

# Se calcula por medio de un comando "for" los valores esperados para la tabla rentistas (mujeres).
for(j in 1:111){
  rentistas_m_2005_[j,6]=(sum(rentistas_m_2005_[(j+1):111,3])/rentistas_m_2005_[j,3])+0.5
}
# Aqui se puede ver la tabla rentistas (mujeres) completa.
colnames(rentistas_m_2005_)<- c("x","qx","dx","lx","px","e0_x")
View(rentistas_m_2005_)

# =========================================================================================
# Segundo programa: "predecir" los q_15, q_16, ... , q_100  para 2017, 2018, ... , 2050 
# para la tabla de rentistas (mujeres) con base en los valores de q_x del 1985 y q_x 
# del 2007. 
# =========================================================================================

# Aqui se leen los datos. Para que el programa funcione, cambiar la ubicacion de los datos!!! 
q_1985_ <- read_excel("~/Universidad Nacional/7mo semestre/Calculo actuarial/Tablas de Mortalidad Aseg y Rentistas (Nueva)-b (1).xls",sheet = "Hoja2")
View(q_1985_)
q_2007_ <- read_excel("~/Universidad Nacional/7mo semestre/Calculo actuarial/Tablas de Mortalidad Aseg y Rentistas (Nueva)-b (1).xls",sheet = "Hoja1")
View(q_2007_)

# Se crean los vectores y matrices en donde se almacenaran los valores de las pendientes,
# los interceptos y los q_estimados para q_15, q_16, ... ,q_100.
pendiente<-matrix(NA,nrow=1, ncol=85)
intercepto<-matrix(NA,nrow=1, ncol=85)
qest<- matrix(NA,nrow = 34, ncol = 86, byrow = TRUE)
row.names(qest) = c("2017","2018","2019","2020","2021","2022", "2023", "2024","2025","2026","2027", "2028","2029", "2030","2031","2032","2033","2034","2035","2036","2037","2038","2039","2040","2041","2042","2043","2044","2045","2046","2047","2048","2049","2050")
colnames(qest) = c(15:100)

# Se crean funciones que calculan la pendiente y el intercepto para estimar los valores
# de q_15, q_16, ... ,q_100 para 2017, 2018, ... ,2050.

# Funcion que calcula la pendiente. 
pen<- function(x)
{
  m<-(q_2007_[x,2]-q_1985_[x,2])/(2007-1985) 
  print(m)
}  

# Este "for" guarda los valores de la funcion "pen(x)" y los almacena en la matriz
# "pendiente".
for(i in 16:100)
{
  pendiente[i-15] <- pen(i) 
}
pendiente<-as.numeric(pendiente)

# Funcion que calcula el intercepto. 
interc<- function(x)
{
  b<-q_1985_[x,2]-(1985*pendiente[x-15]) 
  print(b)
}

# Este "for" guarda los valores de la funcion "interc(x)" y los almacena en la matriz
# "intercepto".
for(i in 16:100)
{
  intercepto[i-15] <- interc(i) 
}
intercepto<-as.numeric(intercepto)

# Aqui se crea un vector que contiene los valores de 2017, 2018, ... ,2050.
t<-c(2017:2050)

# Funcion que guarda los valores estimados de q_15, q_16, ... ,q_100 para un aÃ±o
# respectivo. "x" toma los valores del aÃ±o y "y" toma el valor de la fila que se quiere
# "llenar".
rec<-function(x,y)
{
  for(j in 1:86) {
    qest[y,j]<-(x*pendiente[j]) + intercepto[j]

  }
  print(qest[y,])   
}

# Con este "for" se guardan los valores de q_15, q_16, ... ,q_100 en la matriz "qest". 
for(i in 1:34)
{
  qest[i,]<-rec(2016+i,i) 
}   

# Aqui se puede ver que hay valores para los cuales el q-estimado es menor a cero, esto no 
# puede suceder porque q es una probabilidad. Esto ocurre debido a que la estimacion 
# realizada se hizo por medio de una recta con pendiente negativa y en algun valor de t 
# (tiempo) q tomara el valor de cero y de ahi en adelante el valor de q sera negativo.
View(qest)

# Para "corregir" el problema mencionado anteriormente, se tomara el ultimo valor en el q 
# fue mayor que cero y se supondra que a partir de ahi q quedara constante. Con este "for" se
# toman los valores de "qest" menores a 0 y se transforman al ultimo valor
# que haya sido mayor a 0.
for(i in 1:86)
{
  for(j in 1:34)
  {
  if (qest[j,i]<0){
    qest[j,i]<- qest[j-1,i]
  } else {
    qest[j,i]<-qest[j,i]
  }
  }
} 

View(qest)

# Se crea una matriz que contenga los interceptos y las pendientes calculada por medio de
# la ecuacion ln(qest) = beta_0 + beta_1*t, con qest siendo el valor estimado de q y t el aÃ±o.
regre<-matrix(NA,nrow = 86, ncol = 2)
row.names(regre)<-c(15:100)
colnames(regre)<-c("Intercepto", "pendiente")

# Se calculan las ecuaciones que la forma ln(qest) = beta_0 + beta_1*t, con qest siendo el 
# valor estimado de q y t el aÃ±o.
reg<-function(x){
  reg1<-lm(log(qest[,x-14]) ~ t)
  reg1<- reg1$coefficients 
  print(reg1)
}

# Se "llena" la matriz "regre" con los datos calculados con la funcion "reg".
for(k in 15:100){
  regre[k-14,]<-reg(k)
}


# Aqui se grafican los valores de q_15, q_16, ... ,q_100 para los aÃ±os 2017, 2018, ... ,2050. 
plot(t,qest[,2], main = "q con el paso del tiempo ", xlab = "aÃ±os", ylab= " q",axes = TRUE ,col="white")
for(i in 15:100){
  par(new=TRUE)
  plot(-(exp(regre[i-14,1])*exp(regre[i-14,2]*t)), qest[,i-14], type = "l",axes = FALSE, main =" ", xlab = " ", ylab= " " , col = "blue")
}
