%%

clear all
close all

display('testbispectre')
graine=floor(sum(100*clock));
rand('seed',graine)

%% Analyse bispectre
%Parameters


pn=14;
pm=6;
pp=pn-pm;
n=2^pn; % taille de l'�chantillon
m=2^pm; % nombre de parties
p=2^pp; % taille de chaque partie
phi=0.7; % param�tre de l'AR1
nT=100; % monte carlo pour �tablir la statistique
v=0.8; % param�tre de la distribution asym�trique
delta=(1-2*v)/sqrt(v*(1-v)); % coefficient d'asym�trie pour cette loi-l�

lambda=2*m^2/n*delta^2; % pour le coeff de d�centralisation � la fin

X=[];
for iT=1:nT % T pour test
    if iT==1
        display(strcat('cas asym�trique ''deux diracs'' avec v =',num2str(v))); display(strcat('valeur de delta, coefficient d''asym�trie =',num2str(delta))); display(strcat('''graine'' du g�n�rateur al�atoire =',num2str(graine)))
    end
    z=rand(1,n)<v;
    % ---
    %z=z-mean(z); 
    z = 0.2*z - 0.8*(1-z); % convient aussi
    %z = rand(1,n)
    % ---
    % la suite est autoris�e : quand X est une matrice, l'utilisation d'un
    % seul indice se fait par 'colonnes successives'
    X(1)=1/sqrt(1-phi^2)*z(1);
    for i=2:n
        X(i)=phi*X(i-1)+z(i);
    end
    
    % Spectrale estimation
    
    X=reshape(X,p,m);
    [GXcl,q]=bispectre1(X);

    
    if iT==1
        figure; plot(m/sqrt(n)*sqrt(2*pi)*abs(GXcl)); grid; title('module du bispectre normalis� pour une r�alisation')
    end
    u=4*pi*m^2/n*abs(GXcl).^2; % les u_i sont chi2(2,lambda)
   
    T(iT)=sum(u);
    V(iT)=var(u);
end

tg=(T-2*q)/sqrt(4*q); % test de 'gaussiannit�'
R=V./(4*(T/q-1)); % test de 'lin�arit�'
deltac=sqrt( n/(2*m^2) * (T/q-2) ); % coefficient d'asym�trie estim�

figure; plot(1:nT,tg,'b-',1:nT,2*ones(1,nT),'r--',1:nT,-2*ones(1,nT),'r--'); grid; title('test gaussien')
figure; plot(1:nT,R,'b-',1:nT,2*ones(1,nT),'r--'); grid; title('rapport var empirique sur var attendue')
figure; plot(1:nT,deltac,'b-',1:nT,abs(delta)*ones(1,nT),'r--'); grid; title('valeur absolue de delta estim�e en bleu, r�elle en rouge')
