interpolator<-function(X, Y, x, i)
  Y[i] + (x - X[i])/(X[i+1] - X[i])*(Y[i+1] - Y[i]);

AgeProportion<-function(age, percentile, mx, mn = 0){
  
  pmn = 0; pmx = percentile[length(percentile)]; i = 1;
  if(mn > 0)
  {
    for (i in 1:(length(age)-1))
    {
      if(age[i+1] > mn) {
        pmn = interpolator(age, percentile, mn, i);
        break;
      }
    }
  }
  
  for (j in i:(length(age)-1))
  {
    if(age[j+1]>mx) {
      pmx = interpolator(age, percentile, mx, j);
      break;
    }
  }
  pmx - pmn;
}
