% guispert.m: special experiment specific hacks
% GUISDAP v1.70   01-11-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if name_site=='T' | name_site=='L'
 lpg_bcs(1)='x';
 if name_site=='T'
  npp=426; npr=5808; nslic=9; ngat=363;
 else
  npp=505; npr=7072; nslic=10; ngat=442;
 end
 nsig=(npp+npr)*nslic;
 if a_control(4)==1 & exist('N_averaged')
   d_var1=d_var1+d_data.*d_data/N_averaged;
   d_var2=d_var2+d_data.*conj(d_data)/N_averaged;
   d_var1=[sum(reshape(d_var1(1:npp*nslic),npp,nslic),2);sum(reshape(d_var1(npp*nslic+(1:npr*nslic)),npr,nslic),2);d_var1((nsig+1):end)];
   d_var2=[sum(reshape(d_var2(1:npp*nslic),npp,nslic),2);sum(reshape(d_var2(npp*nslic+(1:npr*nslic)),npr,nslic),2);d_var2((nsig+1):end)];
 end
 d_data=[sum(reshape(d_data(1:npp*nslic),npp,nslic),2);sum(reshape(d_data(npp*nslic+(1:npr*nslic)),npr,nslic),2);d_data((nsig+1):end)];
 if a_control(4)==1
   if exist('N_averaged')
     nsig=npp+npr; N_averaged=N_averaged*nslic;
     d_var1=[d_var1(1:nsig)-d_data(1:nsig).*d_data(1:nsig)/N_averaged;d_var1((nsig+1):end)];
     d_var2=[d_var2(1:nsig)-d_data(1:nsig).*conj(d_data(1:nsig))/N_averaged;d_var2((nsig+1):end)];
   else
     d_var1=[sum(reshape(d_var1(1:npp*nslic),npp,nslic),2);sum(reshape(d_var1(npp*nslic+(1:npr*nslic)),npr,nslic),2);d_var1((nsig+1):end)];
     d_var2=[sum(reshape(d_var2(1:npp*nslic),npp,nslic),2);sum(reshape(d_var2(npp*nslic+(1:npr*nslic)),npr,nslic),2);d_var2((nsig+1):end)];
   end
   d_var1(npp+(1:ngat))=4*6/16*abs(d_var1(npp+ngat+(1:ngat))+j*d_var2(npp+ngat+(1:ngat)));
   d_var2(npp+(1:ngat))=d_var1(npp+(1:ngat));
 end
 d_data(npp+(1:ngat))=zerolagfix(d_data(npp+(1:ngat)),d_data(1:npp),128);
 clear npp npr nslic ngat nsig
else %for arc!!
  d_data(94+(1:31))=zerolagfix(d_data(94+(1:31)),d_data(1:94),128);
  if a_control(4)==1
    d_var1(94+(1:31))=4*6/16*abs(d_var1(94+31+(1:31))+j*d_var2(94+31+(1:31)));
    d_var2(94+(1:31))=d_var1(94+(1:31));
  end
  ch_height=292.9;
end
