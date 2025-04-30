% radar_eq.m: radar equation for monostatic and bistatic cases
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% 
% function radar_eq

  function ad_coeff=radar_eq(eff)

  global ch_fradar ch_gain ch_Pt ch_az ch_el p_XMITloc p_RECloc ch_elT
  global v_lightspeed v_electronradius v_Boltzmann p_dtau p_R0 p_N0 
  global ad_range ad_w
  global lpg_h lpg_w lpg_code lpg_bcs lp_vc vc_ch lpg_s
  global ADDR_SHIFT name_site 
  global sc_angle sc_R0 sc_R1 ch_range k_radar k_radar0
  global a_Offsetppd lpg_wr GUP_iniver

  scale=(p_dtau*1e-6*v_lightspeed/2); % Scale factor from p_dtau units to meters

  Cbeam=0.460; % This factor depends on the beam geometry
  if nargin==0, eff=0.651; end
  lambda=v_lightspeed./ch_fradar;
  ch_gain0=ch_gain(1); %Transmitter antenna gain
% antenna efficiency is the ratio of the true gain and
% the theoretical gain 4*pi*A/lambda^2

% First find the ranges and the scattering angles from the common volume
% For monostatic case quite simply 
% Check that distance from transmitter to receiver less than 100 m

if max(abs(gg2gc(p_XMITloc)-gg2gc(p_RECloc)))<.1
  sc_angle=pi;
  sc_R0=p_R0*scale;
  sc_R1=p_R0*scale;
  k_radar=k_radar0;

  % Effective beam cross section
    Aeff=Cbeam*eff*(4*pi*sc_R0^2)./ch_gain;

  % Volume for unit pulse length 
    Veff=Aeff*scale; 
  % Comment: Pulse lengths are also expressed in units of p_dtau
  %    Multiplication by scale is included here so that
  %    is is not needed in functions dirthe and power_prof
elseif GUP_iniver<1.71
  warning('GUISDAP:radar_eq',sprintf('GUP init version %g obsolete, workaround activated',GUP_iniver))
  [gg_sp,angle,ranges]=loc2gg(p_RECloc,[ch_el(1),ch_az(1),ch_range(1)],p_XMITloc);
  sc_angle=angle; sc_R0=ranges(2)*1E3; sc_R1=ranges(1)*1E3;
% Effective scattering volume in m^3 for the remotes
  P=(32*pi/0.3215)^2/3.63;
  Veff=(pi/P)^1.5*(sc_R0^2*sc_R1^2)/sqrt(sc_R0^2+sc_R1^2)/sin(sc_angle);
% fprintf(' Veff is %.4g\n',Veff)
  % Update range variables for the new geometry
  ind=find(ismember(lpg_bcs,lpg_s)); range=sc_R1/scale;
  lpg_h(ind)=range*ones(size(ind));
  lpg_w(ind)=(range/100)*ones(size(ind));
  ad=ADDR_SHIFT+lpg_addr(ind);
  ad_range(ad)=range*ones(size(ad));
  ad_w(ad)=(range/100)*ones(size(ad));
  % Update now the radar k 
  k_radar=k_radar0*sin(sc_angle/2);
else
  ch=1;
  [gg_sp,angle,ranges]=loc2gg(p_RECloc,[ch_el(ch),ch_az(ch),ch_range(ch)],p_XMITloc);
  sc_angle=angle;
  sc_R0=ranges(2)*1E3;
  sc_R1=ranges(1)*1E3;
% IH: Since we now use monostatic setups, use that Veff
%     corrected later by intersection range.
%     Note: the following assumes that we do not cut the volume in pieces
  Aeff=Cbeam*eff*(4*pi*sc_R0^2)./ch_gain;
  Veff=Aeff*scale;%*1.9853; 
  if isempty(a_Offsetppd)
    ppdoff=0;
  else
    ppdoff=a_Offsetppd/p_dtau;
  end
  if ch_fradar(1)<300e6 %VHF tristatic
   ch_gain0=10^4.31/2.; %VHF tx half antenna
  elseif ch_fradar(1)<450e6 %SYISR tristatic
   ch_gain0=10^4.6*sin(ch_elT*pi/180)^2.5;
   ppdoff=ppdoff-(sc_R0+sc_R1)/scale/2;
  end
  opening_angle=sqrt(2*pi/ch_gain(1))*(1+sin(sc_angle)^2);

  %rx beam width   opening_angle*range*sc_angle_factor/pulse_speed
  range_cover=round(opening_angle*sc_R1/sin(sc_angle)/scale);
  %rise time: intersection + squewing
  rise_time=round(opening_angle*sc_R0*(tan(sc_angle-pi/2)+tan(pi/2-sc_angle/2))/scale);
  fprintf('Intersection: [Width Rise]=[%.0f %.0f] us',[range_cover rise_time]*p_dtau)
  fprintf(' Veff=%.4g\n',Veff(1))
  a11=[1:rise_time]/rise_time;
  if range_cover>rise_time
   a1=[a11 ones(1,range_cover-rise_time-1) fliplr(a11)];
  else
   a1=[a11(1:range_cover) fliplr(a11(1:range_cover-1))];
   rise_time=range_cover-1;
  end
  % Now go from square beam to a round one
  a1=.5+.5*sin((a1-.5)*pi);
  % Update range variables for the new geometry
  range=sc_R1/scale;
  % Edges do not fill the beam, correct for range
  range_factor=(1+rise_time/range*(sin((1:rise_time)/rise_time*pi/2)-1)).^(1-cos(sc_angle));
  a1(1:rise_time)=a1(1:rise_time).*range_factor;
  a1((end-rise_time+1):end)=a1((end-rise_time+1):end)./fliplr(range_factor);
  % Update now the radar k 
  k_radar=k_radar0*sin(sc_angle/2);
  if issparse(lpg_wr), lpg_wr=full(lpg_wr); end
end

% Single electron cross section at the beam intersection for unit power
  polfac=1-0.5*sin(sc_angle)^2;
%fprintf(' Polarization factor is %.2f\n',polfac)
  P0perPt=4*pi*(v_electronradius^2)*polfac*...
    (ch_gain0./(4*pi*sc_R0^2)).*(ch_gain./(4*pi*sc_R1^2)).*(lambda.^2/(4*pi));

  p_coeff0=P0perPt.*Veff*p_N0; 
% Calculate now the factor for virtual channels
  vc=find(vc_ch>0); % These virtual channels in use
  p_coeff0=p_coeff0(vc_ch(vc))./(v_Boltzmann*Ap(vc,0)/(p_dtau*1e-6));

for sig=find(ismember(lpg_bcs,lpg_s))
  % Must find the virtual channel now
  %lp=lpg_lp(sig); vc=lp_vc(lp(1)); pcoeff=p_coeff0(vc);
  vc=lp_vc(lpg_lp(sig)); pcoeff=mean(p_coeff0(vc));
  addr=ADDR_SHIFT+lpg_addr(sig);
  if max(abs(gg2gc(p_XMITloc)-gg2gc(p_RECloc)))<.1 | GUP_iniver<1.71
% For monostatic case calculate for each point the range factor
    ad_coeff(addr)=pcoeff*(sc_R1./(scale*ad_range(addr))).^2;
  else
% For bistatic case calculate for each point the antenna beam factor
%  a=conv(a1,ones(1,round(lpg_w(sig))))/lpg_w(sig);
%  a=[0 a 0]; at=dummyrange-ppdoff+(1:length(a))-(length(a)-1)/2;
%  at=[0 at 100000/p_dtau]; a=[0 a 0];
   a=conv(lpg_wr(:,sig),a1)/sum(lpg_wr(:,sig));
   at=(1:length(a))-(length(a1)-1)/2-lpg_h(sig)-ppdoff+p_R0/p_dtau;
   b=interp1(at,a,ad_range(addr))'; c=sign(b*lpg_w(sig)-1);
   ad_range(addr)=range*c; ad_w(addr)=range_cover*c;
   ad_coeff(addr)=pcoeff*b;
   lpg_w(sig)=range_cover;
   lpg_h(sig)=range;
  end
end
