"""
This script generates a model for an autoregressive time series, computes a moving sum, 
and then calculates the difference of the moving sum. 
The end objective is to compare test data with a given dataset in order to ultimately estimate parameters for the given dataset and given p and q
"""

#Generate autoregressive time series
N=1000
x=numeric(N)
e=rnorm(N)
x[1]=e[1]
for (i in 2:N) {x[i]=0.5*x[i-1]+e[i]}
plot(x,type="l")

y=numeric(N)
#Compute moving sum
for (j in 1:N) {y[j]=sum(x[1:j])}

#Calculate difference of the moving sum
z=numeric(N-1)
for (k in 1:(N-1)) {z[k]=y[k+1]-y[k]}