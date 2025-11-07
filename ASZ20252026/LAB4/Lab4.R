#### Analiza szeregow czasowych (M.S.) - laboratorium nr 4 (22 X 2025)
####  Tematy: Transformacje danych. 
####  Addytywny i multiplikatywny model klasycznej dekompozycji + analiza skladnika losowego
#### Dane zrodlowe - opis i linki:   plik ASC-lab4.txt


# Wczytanie potrzebnych pakietow
library(tseries)
library(forecast)

### 1. Wczytanie danych w formacie CSV i wizualizacja


dane <- read.csv("nazwa_pliku",header=TRUE,dec=".")


head(dane)
colnames(dane) <- c("Data","kolumna")

Xt <- dane$kolumna

# konwersja do obiektu typu ts()
Xts <- ts(Xt,start=c(yy,mm),frequency=ile) 

plot(Xts,type="l",lwd=2,col="blue",xlab="Time",ylab="X(t)",main="Dane empiryczne: szereg czasowy X(t) jako obiekt ts()",las=1)
grid()

Acf(...)

## tutaj cd. pracy....


### 2. Model klasycznej dekompozycji - funkcja decompose()

# dekompozycja
decompadd <- decompose(Xts,type="typ")

mt_add <- as.ts(decompadd$co-tutaj)     # trend ^m(t)

st_add <- as.ts(decompadd$co-tutaj2)  # skladnik sezonowy ^s(t)

et_add <- as.ts(decompadd$co-tutaj3)    # reszty ^e(t)

# Wizualizacja wyodrebnionych komponent
par(mfrow = c(2, 2))
plot(Xts,col="blue",xlab="t",main="Dane empiryczne ",lwd=2,las=1)
grid()
plot(mt_add,col="red",xlab="t",main="Oszacowany trend {m(t)}",lwd=2,las=1)
grid()
plot(st_add,col="maroon",xlab="t",main="Roczna komponenta sezonowa {s(t)}",lwd=2,las=1)
abline(h=0,col="gray")
plot(et_add,col="goldenrod4",xlab="t",main="Zaszumienie losowe {e(t)}",lwd=2,las=1)
abline(h=0,col="gray")


# analiza reszt {e(t)}

par(mfrow=c(1,1))
eps_t <- as.numeric(na.remove(jakie-dane))

Acf(cos-tutaj1,lag.max=20,col="brown",lwd=2,main="ACF dla reszt z dekompozycji addytywnej",las=1)


qqnorm(cos-tutaj1,pch=19,xlab="kwantyle gaussowskie",ylab="kwantyle probkowe",las=1)
qqline(cos-tutaj1,col="red")
grid()

Box.test(cos-tutaj1,lag=5,type="Ljung-Box")

shapiro.test(cos-tutaj1)

##############