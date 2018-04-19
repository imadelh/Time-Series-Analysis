
% bispectre
% fonction qui calcule le bispectre lisse par frame averaging

function [GXcl,q]=bispectre1(X)
% X est un tableau � p lignes et m colonnes
% p=2^pp est la longueur d'un sous-segment
% m=2^pm est le nombre de sous-segments
% n=2^pn est la longueur totale de x, n=mp, pn=pm+pp
% pm < pn/2, de telle sorte que m < sqrt(n)
% suite � quelques difficult�s (cf lisez-moi), ce script a �t�
% modifi� le 4 d�c 16

% extraction de p et de m
p=size(X,1);
m=size(X,2);

% ---
%X=X-mean(mean(X));
% ---

% calcul de la transform�e de Fourier
fftX=fftshift(fft(X)); % table des fft (une fft par colonne)

% apr�s shift, on a les relations suivantes entre indices i
% et fr�quences (normalis�es) f :
% i(f)=p(f+1/2)+1
% f(i)=-1/2+(i-1)/p
% rappel : dans Matlab, fft fait le m�me calcul que celui qui est d�fini
% dans le cours nlts, � savoir sans 'normalisation'

% calcul de l'estim�e de la densit� spectrale
perX=1/p*abs(fftX).^2; % p�riodogramme par colonne
PXc=1/2/pi*mean(perX,2); % estimateur de la dsp (as omega function)

% ---
%fftX=fftshift(fft(X-ones(length(X),1)*mean(X)));
% ---

% calcul du num�rateur (= estim�e du bispectre (bidimensionnel))
% dans le domaine princal des bi-pulsations
% 0<k1<k2 et k1+k2 < p/2
% pour avoir les indices du spectre on r�sout � chaque fois -1/2 + x/p = frequ voulue
% et on ajoute 1 � x car Matlab commence l'indexation a 1.

q = (p^2)/16 - p/2 +1; %nombre de points dans le domaine principal
%q = (p/4)*(p/4-1)/2; %moitie du domaine

TN=zeros(q,m); % tableau (3D) pour le num�rateur
compte=1;
for i2=(p/2+2):(3*p/4) % soit f2 de 1/p � 1/4-1/p
    f2 = (i2-p/2-1)/p;
    for i1=(p/2+1+p*f2+1):(p-1-p*f2+1) % soit f1 de f2+1/p � 1/2-1/p-f2
    %for i1=(p/2+1+p*f2+1):(3*p/4+1) % de f2+1/p � 1/4 (demi domaine)
        I=3*p/2+3-i1-i2; % soit f(I)=-f1-f2
        TN(compte,:)=fftX(i1,:).*fftX(i2,:).*fftX(I,:);
        compte=1+compte;
    end
end
BXc=1/p*mean(TN,2)/(2*pi)^2; % p (et non n comme dans le cours), car m est contenu dans la moyenne
% BXc ext complexe

% calcul du d�nominateur (au m�me format bidimensionnel que le num�rateur)
TD=zeros(q,1); % tableau (2D) pour le d�nominateur
compte=1;
for i2=(p/2+2):(3*p/4)
    f2 = (i2-p/2-1)/p;
    for i1=(p/2+1+p*f2+1):(p-1-p*f2+1)
    %for i1=(p/2+1+p*f2+1):(3*p/4) % de f2+1/p � 1/4-1/p (demi domaine)
        I=3*p/2+3-i1-i2;
        TD(compte)=sqrt(PXc(i1)*PXc(i2)*PXc(I));
        compte=1+compte;
    end
end
%[compte-1,q]
GXcl=BXc./TD;
% GXc est complexe