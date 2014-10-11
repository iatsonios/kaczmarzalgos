KaczmarzClassic<-function(A,b,no_iter)
{
  m=dim(A)[1]
  n=dim(A)[2]
  #initialization step
  x=matrix(0,1,n)
  for(i in 1:no_iter)
    
  {
    picked_row=i%%m+1

    print(picked_row)
    
    x=x+((b[picked_row]-A[picked_row,]%*%t(x))/sum(A[picked_row,]^2))*A[picked_row,]
    #print(x)
    #error[i]=sum((x-t(x_sol))^2)
    
  }
  
  return(x)
}
