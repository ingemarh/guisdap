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
lpg_wr=zeros(1000,length(lpg_ra));
for i=find(lpg_bcs=='s' | lpg_bcs=='x')
  w=wrlpg(i); r=col(1:length(w));
  lpg_wr(length(w),i)=0;lpg_wr(r,i)=w;
  indw=find(abs(w)>0.065*max(w));   % main body of ambiguity function
  if length(indw)>0;
    pp=sum(indw.*w(indw))/sum(w(indw));
    lpg_h(i)=pp;
%   lpg_w(i)=2*sqrt(sum(w(indw).*(indw-pp).^2)/sum(w(indw)));
    lpg_w(i)=4*sum(abs(w(indw).*(indw-pp)))/sum(abs(w(indw)));
  else
    lpg_h(i)=0; lpg_w(i)=0;
  end
  ranges=[lpg_h(i), lpg_h(i)+(lpg_nt(i)-1)*lpg_dt(i)]*p_dtau;
  fprintf('Lpg %3.0f: ranges  %4.0f ... %4.0f us',i,ranges)
  fprintf(' (%5.1f ... %5.1f km)',ranges*.150)
  fprintf('    width %5.0f us \n',lpg_w(i)*p_dtau)
  plot(r*p_dtau,w);
  title(['range ambiguity function for lpg=' num2str(i)]);grid;%prtsc
  drawnow
end
fprintf('\n\nRange ambiguity functions calculated\n\n')
clear ind file fid i w z0 z1 k k0 dt nt kold indw left right j pp jold r ranges
