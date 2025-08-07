
# zad 1
xyz <- rnorm(10)
print(xyz)

# zad 2

Litera r (jak random) rozpoczyna nazwę funkcji - generatora liczb losowych. Taka funkcja generuje próbę prostą o liczebności n (pierwszy argument funkcji) z określonego rozkładu.
Litera p (jak probability) rozpoczyna nazwę funkcji wyznaczającej wartości dystrybuanty danego rozkładu w punktach określonych przez wektor x (pierwszy argument tych funkcji).
Litera d (jak density) rozpoczyna nazwę funkcji wyznaczającej wartości gęstości (dla rozkładów ciągłych) lub prawdopodobieństwa (dla rozkładów dyskretnych) danego rozkładu w punktach określonych przez wektor x (pierwszy argument tych funkcji).
Litera q (jak quantile) rozpoczyna nazwę funkcji wyznaczającej wartości kwantyli danego rozkładu w punktach q (pierwszy argument tych funkcji).


dnorm(xyz, mean=0, sd=1, log = FALSE)
pnorm(xyz, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
qnorm(xyz, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
rnorm(xyz, mean=0, sd=1)

# zad 3

