for (i in 7:1){
  
  Fi[i] = uniroot(step2fun, N = N[i+1], C = C[i], interval = c(-1,2))$root
  N[i] = N[i+1]*exp(M+Fi[i])
  
}
