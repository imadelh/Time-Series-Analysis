
% calcul de la fonction d'autocorrélation partielle empirique de x
% jusqu'à lag, soit lag+1 valeurs (formulation récursive des équations de Yule Walker,
% algorithme de Durbin Levinson) à partir de la fonction de covariance covx  
%  les indices de phie et v sont conformes, ceux de ve et ge sont augmentés de 1

function pacfe=cpar(covx,lag)
n=(length(covx)+1)/2;

re1=covx(n+1)/covx(n);
phie(1,1)=re1;ve(1)=covx(n)*(1-re1^2);
for m=2:lag
   s=covx(m+n);
   for j=1:m-1
      s=s-phie(m-1,j)*covx(m-j+n);
   end
   phie(m,m)=1/ve(m-1)*s;
   for k=1:m-1
      phie(m,k)=phie(m-1,k)-phie(m,m)*phie(m-1,m-k);
   end
   ve(m)=ve(m-1)*(1-phie(m,m)^2);
end
pacfe(1)=1;
for m=1:lag
   pacfe(m+1)=phie(m,m);
end





            
