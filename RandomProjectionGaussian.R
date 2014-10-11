RandomProjectionGaussian<-function(k,n)
{
 
  #creates a gaussian sampled random matrix of dimension k*n
  ##this can be multiplied with the data matrix to reduce the dimensionality
  A=matrix(0,k,n)
  for(i in 1:k)
  {
    A[i,]=rnorm(n,0,1)
  }
  return(A);
}
