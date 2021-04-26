/*
Analysis of the incidence to evaluate Rt
data scale days
example: SP city
data: starting with day 1 and moving on 
 */
#include<math.h>
#include<time.h>
#include<stdio.h>
#include<stdlib.h>

#define NP 232 // total number of data (cases of covid) in the archieve
#define NPI (NP) // total number of data (isolation index) in the archieve 
#define t1 (1./3.) // t1^{-1} exposed period 
#define t2 (1./6.4) // t^2{-1} infected period
#define mua (1./(75.*365.)) // mua^{-1} mortality  
#define ismedio 0.2839531 // social isolation index measured on february 
#define NPN (NP+100) // increase size if we need to interpolate missing data
#define N   12252021 // population size (IBGE SITE)

int main()
{
  int i,j,k,l1,l2,cl,n,ki,nmax;
  double xux,XB[NPN],XBI[NPI],YDC[NPN],YDI[NPN],YI[NPI],YC[NPN],YT[NPN],YR[NPN];
  double p1, p2,g,norm,s1,s2,aux,Rt,max[100][2],min[100][2];
  double h[NPN],l[NPN],c[NPN],z[NPN],mu[NPN],b[NPN],d[NPN],alpha[NPN],Y[NPN],X[NPN];
  FILE *ar;

  //reading the archieve of data (number of cases)
  ar=fopen("dadosC.dat","r");
  for(i=0;i<NP;i++){
    fscanf(ar,"%lf %lf",&XB[i],&YDC[i]); //time=1,2,3,...; number of cases=10,254,...
    // printf("%d %lf %lf\n",i,XB[i],YDC[i]); //time=1,2,3,...; number of cases=10,254,...
  }
  fclose(ar);

  //spline - if there are missing data in the archieve
  X[0] = XB[0];
  Y[0] = YDC[0];
  for(i=1;;i++) {
    X[i]=X[i-1]+1;
    Y[i]=0.;
    if(fabs(X[i]-XB[NP-1])<0.8) break;
  }
  nmax=i+1;
 
  if(nmax>NP){
    for(i=0;i<NP-1;i++) h[i]=XB[i+1]-XB[i];
    for(i=1;i<NP-1;i++) alpha[i]=3./h[i]*(YDC[i+1]-YDC[i])-3./h[i-1]*(YDC[i]-YDC[i-1]);
    
    l[0]=1.;
    mu[0]=z[0]=0.;
    for(i=1;i<NP-1;i++){
      l[i]=2*(XB[i+1]-XB[i-1])-h[i-1]*mu[i-1];
      mu[i]=h[i]/l[i];
      z[i]=(alpha[i]-h[i-1]*z[i-1])/l[i];
    }
    
    l[NP-1] = 1.;
    z[NP-1] = c[NP-1] = 0.;
    
    for(j=NP-2;j>=0;j--){
      c[j]=z[j]-mu[j]*c[j+1];
      b[j]=(YDC[j+1]-YDC[j])/h[j]-h[j]*(c[j+1]+2.*c[j])/3.;
      d[j]=(c[j+1]-c[j])/(3.*h[j]);
    }
    
    j=0;
    for(i=0;i<NP-2;){
      if((X[j]<XB[i+1])&&(X[j]>=XB[i]))   
	{
	  Y[j]=YDC[i]+b[i]*(X[j]-XB[i])+c[i]*pow((X[j]-XB[i]),2.)+d[i]*pow((X[j]-XB[i]),3.);
	  if(Y[j]<0) Y[j]=0.;
	  j++;
	}
      else i++;
    }
    j=0;
    for(i=0;i<nmax;i++) {
       if(fabs(X[i]-XB[j]) < 0.8){
	Y[i]=YDC[j];
	j++;
      }
    }
    
    for(i=0;i<nmax;i++) {
      XB[i]=X[i];
      YDC[i]=Y[i];
     }
  }
    
    //7 days moving average
  for(j=0;j<nmax;j++)
    {
      YC[j]=0;
      l1=0;
      for(k=j-3;k<=j+3;k++) {
	if(k>=0&&k<nmax){
	  YC[j]+=YDC[k];
	  l1++;
	}
      }
      YC[j]/=l1;
    }
  
  //writing the data with 7 days moving average
  ar=fopen("suavisaCasos.dat","w");
  for(j=0;j<nmax;j++) fprintf(ar,"%lf %lf %lf\n",XB[j],YC[j],YDC[j]);
  fclose(ar);
  
  
  //Rt evaluation 
  s1=t1+mua;
  s2=t2+mua;
  p1=s2-s1;
  p2=s1-s2;
  norm=1./(s1*p1)+1./(s2*p2); // normalization of g(t)
  
  ar=fopen("Rt.dat","w");  
  for(i=1;i<nmax;i++){
    Rt=0.;
    for(k=1;k<=i;k++){
      g=exp(-s1*XB[k])/p1 + exp(-s2*XB[k])/p2;
      Rt=Rt+YC[i-k]*g;
    }
    Rt/=norm;
    
    //writing the Rt data 
    if(Rt>0.01){
      YR[i]=YC[i]/Rt;
      YT[i]=YR[i]*t2;
      fprintf(ar,"%lf %lf %lf\n", XB[i],YR[i],YT[i]); //time, Rt, transmission index
    }      
  }
  fclose(ar);
  
  //maximos
  printf("local minimum of incidence\n");
  k=0;
  for(i=3;i<nmax-3;i++){
    if(YC[i]>=YC[i-1])
      if(YC[i]>=YC[i-2])
	if(YC[i]>=YC[i-3])
	  if(YC[i]>YC[i+1])
	    if(YC[i]>YC[i+2])
	      if(YC[i]>YC[i+3]){
		max[k][0]=XB[i];
		max[k][1]=YC[i]/N*100000;
		printf("day=%.0lf value=%.3lf\n",max[k][0],max[k][1]);
		k++;
	      }
  }
  
  printf("\n local maximum of incidence \n");
  //minimos
  k=0;
  for(i=3;i<nmax-3;i++){
    if(YC[i]<=YC[i-1])
      if(YC[i]<=YC[i-2])
	if(YC[i]<=YC[i-3])
	  if(YC[i]<YC[i+1])
	    if(YC[i]<YC[i+2])
	      if(YC[i]<YC[i+3]){
		min[k][0]=XB[i];
		min[k][1]=YC[i]/N*100000;
		printf("day=%.0lf value=%.3lf\n",min[k][0],min[k][1]);
		k++;
	      }
  }
    
  //reading the archieve of data (isolation index)
  ar=fopen("dadosI.dat","r");
  for(i=0;i<NPI;i++){
    fscanf(ar,"%lf %lf",&XBI[i],&YDI[i]); //time=1,2,...; social isolation index
    YDI[i]=YDI[i]-ismedio; //minus the isolation measured on february
  }
  fclose(ar);

  //writing the data with 7 days moving average
  for(j=0;j<NPI;j++)
    {
      YI[j]=0;
      l1=0;
      for(k=j-3;k<=j+3;k++) {
	if(k>=0&&k<NP){
	  YI[j]+=YDI[k];
	  l1++;
	}
      }
      YI[j]/=l1;
    }

  //writing the data with 7 days moving average
  ar=fopen("suavisaIsolamento.dat","w");
  for(j=0;j<NPI;j++) {
    fprintf(ar,"%lf %lf %lf\n",XBI[j],YI[j],YDI[j]); 
  }
  fclose(ar);

  //final archieve without lag
  ar=fopen("print.dat","w");
  for(i=1;i<nmax;i++) {
    YC[i]=YC[i]/N*100000.;
    for(j=0;j<NPI;j++){
      if(fabs(XB[i]-XBI[j])<0.1){
	fprintf(ar,"%lf %lf %lf %lf %lf\n",XB[i],YC[i],YR[i],YI[i],YT[i]);//time, cases, Rt, isolation index,tranmission index
      }
    }
  }
  fclose(ar);
  //final archieve with lag
  ar=fopen("printLag.dat","w");
  for(i=1;i<nmax-7;i++) {
    for(j=0;j<NPI;j++){
      if(fabs(XB[i]-XBI[j])<0.1){
	fprintf(ar,"%lf %lf %lf %lf %lf\n",XB[i+7],YC[i+7],YR[i+7],YI[i],YT[i+7]);//time, cases, Rt, isolation index,tranmission index
      }
    }
  }
  fclose(ar);
}

