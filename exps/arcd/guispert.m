% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if name_site=='V' | name_site=='T'
 npp=330; npr1=420; npr2=34596; nslic=1;  %pg_bcs(5)='x';
 d_date=datenum(d_time(1,:));
 if strcmp(name_ant(1:3),'vhf')
  % Oct 2003
  if d_date>=datenum(2003,10,28,16,18,0) & d_date<=datenum(2003,10,28,16,40,0)
   ch_el=70;
  elseif d_date>=datenum(2003,10,28,20,11,0) & d_date<=datenum(2003,10,28,22,0,0)
   ch_el=90;
  elseif d_date>=datenum(2003,10,30,4,5,0) & d_date<=datenum(2003,10,30,17,21,0)
   ch_el=60;
  end
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
 end
end
ngat=npp-63;

if length(d_data)==358500   % arc_dlayer_ht
 cal_samp=45;
 nslic=10;

% Calibration and background, not time sliced 1:2*cal_samp
 data0=d_data(1:cal_samp*2);
 pos1=cal_samp*2;
 samples=npp;
 npul=128;

%Undecoded power profile from channel 1 10 slices
 data1=reshape(d_data(pos1+1:pos1+samples*nslic),samples,nslic);
 pos2=pos1+samples*nslic;

% Decoded signal 127 lags 10 slices
 data2=reshape(d_data(pos2+1:pos2+ngat*npul*nslic),ngat*npul,nslic);
 pos3=pos2+ngat*npul*nslic;

%Decoded signal, 3 short E layer lags 10 slices
 data3=reshape(d_data(pos3+1:pos3+ngat*4*nslic),ngat*4,nslic);
 pos4=pos3+ngat*4*nslic;

%Decoded signal, coherently integrated
 data4=d_data(pos4+1:end);

%Put everything together again
 d_data=[data0;sum(data1,2);sum(data2,2);sum(data3,2);data4];

end 

nsig=npr1+(1:ngat);
d_data(nsig)=remove_stripe(d_data(nsig),d_data(90+(1:npp)),128);
if a_control(4)==1
 d_var1(nsig)=d_var1(nsig)/(1+1/32/63);
 d_var2(nsig)=d_var2(nsig)/(1+1/32/63);
end
nsig=npr2+(1:ngat);
d_data(nsig)=remove_stripe(d_data(nsig),d_data(90+(1:npp)),128);
if a_control(4)==1
 d_var1(nsig)=d_var1(nsig)/(1+1/8/15);
 d_var2(nsig)=d_var2(nsig)/(1+1/8/15);
end
clear npp npr ngat nsig
