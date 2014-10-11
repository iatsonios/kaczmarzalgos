KaczmarzRandomized<-function(A,b,no_iter)
{
  m=dim(A)[1]
  n=dim(A)[2]
  #initialization step
  x=matrix(0,1,n)
  
  
  #compute norm per row
  norm_per_row=matrix(0,1,n)
  for(i in 1:m)
  {
    norm_per_row[i]=sum(A[i,]^2)
  }
  
  prob_distrib=norm_per_row/sum(norm_per_row)
  for(i in 1:no_iter)
    
  {
    r=runif(1)
    picked_row=min(which(cumsum(prob_distrib)>r))
    
    print(picked_row)
    
    x=x+((b[picked_row]-A[picked_row,]%*%t(x))/sum(A[picked_row,]^2))*A[picked_row,]
    #print(x)
    #error[i]=sum((x-t(x_sol))^2)
    
  }
  
  return(x)
}
