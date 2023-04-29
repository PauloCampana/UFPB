mode=function(x){3*median(x)-2*mean(x)}

attach(airline.safety)

disperção=function(x){
  disperção1=c(mean(x),median(x),mode(x),var(x),sd(x))
  descrição1=c("Média","Mediana","Moda","Variância","Desvio Padrão")
  return(data.frame(Descrição=descrição1,Disperção=disperção1))}

disperção(incidents_00_14)

clamp=function(x){round(((max(x)-min(x))/sqrt(length(x))),2)}
classes=function(x){
        ifelse(x<=clamp(x),1,
        ifelse(x<=2*clamp(x),2,
        ifelse(x<=3*clamp(x),3,
        ifelse(x<=4*clamp(x),4,
        ifelse(x<=5*clamp(x),5,
        ifelse(x<=6*clamp(x),6,
        ifelse(x<=7*clamp(x),7,
        ifelse(x<=8*clamp(x),8,
        ifelse(x<=9*clamp(x),9,)))))))))}

table(classes(incidents_00_14))
barplot(classes(incidents_00_14))

classes1=function(x){
  ret<-1
  while(x>ret*clamp(x)){
    ret=ret+1
  }
}

table(classes(incidents_00_14))
table(classes1(incidents_00_14))
