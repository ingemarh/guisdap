% DS_start: Radar initialization for the design package
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% Corrected 11Dec96 AH
function DS_start(radar,nlp,nfir)

global ch_fradar ch_gain
global p_dtau p_XMITloc p_RECloc p_calTemp p_ND
global vc_adcint vc_sampling vc_env vc_envo vc_p vc_ch
global name_site 

if nargin~=0
  radars=[...
  'EISCAT';
  'ESR   ' ];
  [M,N]=size(radars);
  Nr=min([length(radar),N]);

  for i=1:M
   if strcmp(radars(i,1:Nr),radar(1:Nr))
		    radar=radars(i,:);
		    break
		  end
  end
else
  radar='ESR';
end

p_dtau=1; p_ND=1;
vc_adcint=[]; vc_sampling=[]; vc_env=[]; vc_envo=[];vc_p=[]; vc_ch=[];

if strcmp(radar,'EISCAT')
  ch_fradar=931.5e6*ones(1,8);
  ch_gain=10^4.81*ones(1,8);

  p_XMITloc=[69.583, 19.21, .030];
  p_RECloc=p_XMITloc;
  p_calTemp=210;
  if name_site=='T' | name_site=='V' 
    p_RECloc=[69.583, 19.21, .030];
  elseif name_site=='K'
    p_RECloc=[67.863, 20.44, .412];
    p_calTemp=30;
  elseif name_site=='S' | name_site=='R'
    p_RECloc=[67.367, 26.65, .180];
    p_calTemp=30;
  end
elseif strcmp(radar,'ESR   ')
  ch_fradar=500e6;
  ch_gain=10^4.28;
  p_XMITloc=[78.15, 16.05, .450];
  p_RECloc=p_XMITloc;
  p_calTemp=210;
end


if nargin==3
  COR_init(nlp,nfir)
elseif nargin==2
  COR_init(nlp,1)
else
  COR_init
end

