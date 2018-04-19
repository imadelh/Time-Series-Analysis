
% bispectre
% fonction qui calcule le bispectre lisse par frame averaging

function [GXcl,q]=bispectre1(X)
% X est un tableau à p lignes et m colonnes
% p=2^pp est la longueur d'un sous-segment
% m=2^pm est le nombre de sous-segments
% n=2^pn est la longueur totale de x, n=mp, pn=pm+pp
% pm < pn/2, de telle sorte que m < sqrt(n)
% suite à quelques difficultés (cf lisez-moi), ce script a été
% modifié le 4 déc 16

% extraction de p et de m
p=size(X,1);
m=size(X,2);

% ---
%X=X-mean(mean(X));
% ---

% calcul de la transformée de Fourier
fftX=fftshift(fft(X)); % table des fft (une fft par colonne)

% après shift, on a les relations suivantes entre indices i
% et fréquences (normalisées) f :
% i(f)=p(f+1/2)+1
% f(i)=-1/2+(i-1)/p
% rappel : dans Matlab, fft fait le même calcul que celui qui est défini
% dans le cours nlts, à savoir sans 'normalisation'

% calcul de l'estimée de la densité spectrale
perX=1/p*abs(fftX).^2; % périodogramme par colonne
PXc=1/2/pi*mean(perX,2); % estimateur de la dsp (as omega function)

% ---
%fftX=fftshift(fft(X-ones(length(X),1)*mean(X)));
% ---

% calcul du numérateur (= estimée du bispectre (bidimensionnel))
% dans le domaine princal des bi-pulsations
% 0<k1<k2 et k1+k2 < p/2
% pour avoir les indices du spectre on résout à chaque fois -1/2 + x/p = frequ voulue
% et on ajoute 1 à x car Matlab commence l'indexation a 1.

q = (p^2)/16 - p/2 +1; %nombre de points dans le domaine principal
%q = (p/4)*(p/4-1)/2; %moitie du domaine

TN=zeros(q,m); % tableau (3D) pour le numérateur
compte=1;
for i2=(p/2+2):(3*p/4) % soit f2 de 1/p à 1/4-1/p
    f2 = (i2-p/2-1)/p;
    for i1=(p/2+1+p*f2+1):(p-1-p*f2+1) % soit f1 de f2+1/p à 1/2-1/p-f2
    %for i1=(p/2+1+p*f2+1):(3*p/4+1) % de f2+1/p à 1/4 (demi domaine)
        I=3*p/2+3-i1-i2; % soit f(I)=-f1-f2
        TN(compte,:)=fftX(i1,:).*fftX(i2,:).*fftX(I,:);
        compte=1+compte;
    end
end
BXc=1/p*mean(TN,2)/(2*pi)^2; % p (et non n comme dans le cours), car m est contenu dans la moyenne
% BXc ext complexe

% calcul du dénominateur (au même format bidimensionnel que le numérateur)
TD=zeros(q,1); % tableau (2D) pour le dénominateur
compte=1;
for i2=(p/2+2):(3*p/4)
    f2 = (i2-p/2-1)/p;
    for i1=(p/2+1+p*f2+1):(p-1-p*f2+1)
    %for i1=(p/2+1+p*f2+1):(3*p/4) % de f2+1/p à 1/4-1/p (demi domaine)
        I=3*p/2+3-i1-i2;
        TD(compte)=sqrt(PXc(i1)*PXc(i2)*PXc(I));
        compte=1+compte;
    end
end
%[compte-1,q]
GXcl=BXc./TD;
% GXc est complexe