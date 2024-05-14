% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if expver==0
 if ch_el(1)==0
  fprintf(' Antenna elevation is zero...  Changing to the correct value\n')
  ch_el = [30.0*ones(size(ch_el))];
 end
 if exist('a_code')
  phased=1-(a_code(1)-1)/2;
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el,phased*12,10^4.31/2);
  global vhf_tx
  ch_Pt=vhf_tx(phased+1);
 end
else
 ch_Pt=ch_Pt(1);
 if ch_el(2)==0, ch_el(2)=ch_el(1); end
 if length(a_code)==1
  if d_date<datenum(2001,03,18) | ( d_date>datenum(2001,09,16) & d_date<datenum(2002,10,04) ) 
   polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
   hv=max(roots(sum(polhv)));
   ch_Pt=polyval(polhv(a_code,:),hv)*1000;
  end
  if d_date<datenum(2000,12,31) & d_date>datenum(2000,11,23)
   %channels are reversed in early data
   [ch_el ch_az ch_gain]=vhf_elaz(ch_el(3-a_code),12*(2-a_code),10^4.31/2); 
  else
   [ch_el ch_az ch_gain]=vhf_elaz(ch_el(a_code),12*(a_code-1),10^4.31/2);
  end
  if a_code==2         
   if d_date>=datenum(2001,09,17,11,0,0) & d_date<=datenum(2001,09,17,12,0,0)
   % 17-20 September 2001 - tau1v (really tau1a)
   % during this period the uhf and vhf were run together in a pseudo-cp2 mode
   % uhf was pointed az=90 el=75 i.e. east
   % vhf beam 1 (west panel, cp4 boresight beam) was pointed az=360 (boresight) el 90 (vertical)
   % vhf beam 2 (east panel, cp4 west beam) was pointed az=360 (boresight) el 75 (north)
    [ch_el ch_az ch_gain]=vhf_elaz(75,0,10^4.31/2);
   end
   ch_Pt=polyval(polhv(2,:),hv)*1000;
  elseif a_code==1
   if d_date>=datenum(2001,09,17,11,0,0) & d_date<=datenum(2001,09,17,12,0,0)
   % 17-20 September 2001 - tau1v (really tau1a)
   % during this period the uhf and vhf were run together in a pseudo-cp2 mode
   % uhf was pointed az=90 el=75 i.e. east
   % vhf beam 1 (west panel, cp4 boresight beam) was pointed az=360 (boresight) el 90 (vertical)
   % vhf beam 2 (east panel, cp4 west beam) was pointed az=360 (boresight) el 75 (north)
    [ch_el ch_az ch_gain]=vhf_elaz(90,0,10^4.31/2);
   end
  else
   error('No such analysis_code')
  end
 else
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el,0,10^4.31/2);
 end
end
