% envcalc.m: Script to calculate vc_env and vc_p variables from td_variables
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
%
%
% See also: impulse_resp Barker
%
Nvc=length(vc_ch);
maxlen=600/p_dtau;
vc_env=zeros(maxlen,Nvc);
vc_envo=zeros(1,Nvc);
vc_p=zeros(maxlen,Nvc);
lenmax=zeros(1,2); % order vc_env, vc_p 

impuls_resp; % create impulse responses for receiver channels

fprintf('Virtual channel: ')
for vc=find(vc_ch>0)
  fprintf(' %.0f',vc)
  envel=0;
  ind=find(td_ch==vc_ch(vc) & abs(td_am)==1 ...
           & td_t1>=vc_t1(vc) & td_t2<=vc_t2(vc) );
  start_env=floor(min(td_t1(ind)))-2;
  for i=ind
    clear enve
    alku=td_t1(i);
    loppu=td_t2(i);
    alku_c=ceil(alku);
    loppu_f=floor(loppu);
    enve(alku_c-1-start_env,1)=alku_c-alku;
    if loppu_f>alku_c
      enve((alku_c:(loppu_f-1)) -start_env,1)=ones(loppu_f-alku_c,1);end
    enve(loppu_f-start_env,1)=loppu-loppu_f;
    ii=(alku_c-1:loppu_f)-start_env;
    maxi=ii(length(ii));
    if length(envel)<maxi, envel(maxi,1)=0;end
    envel(ii)=envel(ii)+td_am(i)*enve(ii);
  end 
  len=length(envel);
  if envel(len)~=0; envel(len+1)=0; len=len+1; end
  vc_env(1:len,vc)=envel;
  vc_envo(vc)=start_env;
  lenmax(1)=max(lenmax(1),len);
%  plot((0:len-1)*p_dtau,envel)  ,keyboard

  ind=1:max(find(ch_p(:,vc_ch(vc))~=0));
  impresp=ch_p(ind,vc_ch(vc));
  if vc_mf(vc)>0,
    B=flipud(Barker(vc_mf(vc)));
    adcint=vc_adcint(vc); temp=[];
    len=length(impresp);  
    for i=1:length(B);
      ind=(i-1)*adcint+1:(i-1)*adcint+len;
      temp(max(ind),1)=0;
      temp(ind)=temp(ind)+B(i)*impresp;
    end 
    impresp=temp;
  end
  len=length(impresp);
  if impresp(len)~=0; impresp(len+1)=0; len=len+1; end
  vc_p(1:len,vc)=impresp;
  lenmax(2)=max(lenmax(2),len);
%  plot((0:len-1)*p_dtau,impresp),keyboard
end

vc_env(lenmax(1)+1:maxlen,:)=[];
vc_p(lenmax(2)+1:maxlen,:)=[];

plot((0:lenmax(2)-1)*p_dtau,vc_p),title('Filter impulse responses')

clear Nvc maxlen vc ind start_env i alku loppu alku_c loppu_f env ii
clear len impresp adcint temp enve envel lenmax maxi vc
