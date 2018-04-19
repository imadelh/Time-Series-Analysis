%%
% Generate a random variable in {1,1}
clear all; close all;
tic
n=1000;
randn('seed',20)

% Generate Z
z=sign(randn(1,n));
figure; plot(z); grid; title('la séquence z = + -1')
figure; histfit(z); grid; title('histogramme de z = + -1')
figure; qqplot(z); grid; title('qqplot de z = + -1')


%% Bruit gaussien

zb=(randn(1,n));
figure; plot(zb); grid; title('la séquence z')
figure; histfit(zb); grid; title('histogramme de z')
figure; qqplot(zb); grid; title('qqplot de z')



%% Case AR(1)
% AR est stationnaire, non gaussien - gaussian if the noise is

% AR(1)
% cas de l'AR(1)
phi=0.75;
% See Marginale La

for i=2:n
    x(i)=phi*x(i-1)+z(i);
end

figure; plot(x); grid; title('la série x = AR(1) normalisée')
figure; histfit(x); grid; title('histogramme de x')
figure; qqplot(x); grid; title('qqplot de x')

acf = autocorr(x)
phi_hat = acf(2)
%X is not gaussian

%%
%ARIMA (n'est pas stationaire)
%  
% On realise un echantillon de X(10000)
% On regarde la distribution de X(n) (n->infini) (n fixé très grand)
% X_n n'est pas stationnaire, on va generer N echantillon de X_n

clear all;
n = 10000 ; % l'instant de la simulation
N = 1000; % taille de l'echantillon de X_n


X = zeros(1,N) ;
for i=1:N
    % Loi normal
    %Z1 = randn(1,n);
    Z = sign(randn(1,n));
    
    % Loi de cauchy
    %Z2 = randn(1,n);
    %C = Z1./Z2;
    
    X(i) =sum(Z); % X(i)  echantillon de X_n
    %X(i) =sum(C); %  cauchy
end
plot(X);
qqplot(X);
% On trouve une ligne droite > La distribution de X_n suit une loi normale

%% Hist de Cauchy

histfit(C);


%% ARFIMA
d=0.4;

omega=0:2*pi/n:2*pi-2*pi/n;
fomega=(1-exp(-1i*omega)).^-d;
fomega(1)=0;
%fomega(1)=0.5*(fomega(2)+fomega(n));
%fomega(1)=1000;

clear x
z = randn(1,n);
x=real(ifft(fomega.*fft(z)));
figure; plot(x); grid; title('la série x = ARFIMA normalisée')
figure; histfit(x); grid; title('histogramme de x')
figure; qqplot(x); grid; title('qqplot de x')

