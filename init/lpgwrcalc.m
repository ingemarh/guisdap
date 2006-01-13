% lpgwrcalc: Range ambiguity function for all signal lag profile groups
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% lpgwrcalc.m
% 
% calculates the range ambiguity functions for all signal lag profile groups
% and also the range to the first gate and the width of the ambiguity functions
%
% Variables produced:
% lpg_h: Range to the center of range ambiguity function of the first gate
% lpg_w: Width of the range ambiguity function (twice the second moment)
% lpg_wr: The range ambiguity function 
%
% See also: init_GUP wrlpg
fprintf('\n Calculating the range ambiguity functions for signal lag profile groups:\n\n')
lpg_wr=zeros(p_R0,length(lpg_ra));
maxr=0;
for i=find(lpg_bcs=='s' | lpg_bcs=='x')
  w=wrlpg(i); r=1:length(w); lpg_wr(r,i)=w;
  if maxr<length(w), maxr=length(w); end
  indw=find(abs(w)>0.065*max(w));   % main body of ambiguity function
  if length(indw)>0;
    pp=sum(indw.*w(indw))/sum(w(indw));
    lpg_h(i)=pp;
%   lpg_w(i)=2*sqrt(sum(w(indw).*(indw-pp).^2)/sum(w(indw)));
    lpg_w(i)=4*sum(abs(w(indw).*(indw-pp)))/sum(abs(w(indw)));
  else
    lpg_h(i)=0; lpg_w(i)=0;
  end
  ranges=(lpg_h(i)+[0 (lpg_nt(i)-1)*lpg_dt(i)])*p_dtau;
  if lpg_nt(i)>1
    fprintf('Lpg %d: ranges %.0f-%.0f us',i,ranges)
    fprintf(' (%.1f-%.1f km) width %.0f us\n',ranges*.15,lpg_w(i)*p_dtau)
  else
    fprintf('Lpg %d: range %.0fus',i,ranges(1))
    fprintf(' (%.1 km) width %.0f us\n',ranges(1)*.15,lpg_w(i)*p_dtau)
  end
  plot(r*p_dtau,w)
  title(['range ambiguity function for lpg=' num2str(i)]), grid
  drawnow
end
lpg_wr(maxr+1:end,:)=[];
fprintf('\nRange ambiguity functions calculated\n\n')
clear i w indw pp r ranges maxr
