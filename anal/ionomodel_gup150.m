function [altitude,ne,te,ti,coll,cO,cM2,cH]=ionomodel(heights,modinfo)
global ionomodel_control
if modinfo
 fprintf('** This ionomodel uses the guisdap-1.50 model**\n')
end
heights=[min(heights)-1:max(1,mean(diff(sort(heights)))):max(heights)+1]';
if ionomodel_control==2; % constant SNR
 ne=1e11*(heights/100).^2;
else
 Elowerwidth=15; Flowerwidth=75; Fupperwidth=125; Fmaxheight=300;
 if ionomodel_control==1; % Strong ionosphere 
  Eupperwidth=50;  Emaxheight=105; Emaxdensity=6e11; Fmaxdensity=1e12;
 elseif ionomodel_control==-1; % Weak ionosphere
  Eupperwidth=60;  Emaxheight=115; Emaxdensity=5e10; Fmaxdensity=2e11;
 else; % Standard ionosphere
  Eupperwidth=60;  Emaxheight=115; Emaxdensity=2e11; Fmaxdensity=5e11;
 end
 ne=zeros(size(heights));
 ind=find(heights<=Emaxheight);
 ne(ind)=ne(ind)+Emaxdensity*exp(-((heights(ind)-Emaxheight)/Elowerwidth).^2);
 ind=find(heights>Emaxheight);
 ne(ind)=ne(ind)+Emaxdensity*exp(-((heights(ind)-Emaxheight)/Eupperwidth).^2);
 ind=find(heights<=Fmaxheight);
 ne(ind)=ne(ind)+Fmaxdensity*exp(-((heights(ind)-Fmaxheight)/Flowerwidth).^2);
 ind=find(heights>Fmaxheight);
 ne(ind)=ne(ind)+Fmaxdensity*exp(-((heights(ind)-Fmaxheight)/Fupperwidth).^1.3);
end
ExosphericTemp=1000; CenterHeight=140; ScaleLength=30;
ti=ExosphericTemp*(1+(atan((heights-CenterHeight)/ScaleLength))/(pi/2))/2;
MaxRatio=1.6; CenterHeight=140; ScaleLength=30;
ratio=0.5+(MaxRatio-0.5)*(1+(atan((heights-CenterHeight)/ScaleLength))/(pi/2))/2;
te=ti.*ratio;
xi=[149 150 250 251];
yi=[0.0 0.0 1.0 1.0];
cO=inter3(heights',xi,yi)';
cM2=1-cO;
cH=zeros(size(cO));
coll=max(3578*(exp(-(heights-100)/5.8)),10);
altitude=heights;
