d_date=datenum(d_time(1,:));
global d_raw sc_angle
if name_site=='T'
 if d_date<datenum(2007,11,14)
  phasepush=phasecorr(d_raw,vc_penv(:,1:64),10,63,4);
 elseif d_date<datenum(2009,2,6) & d_date>datenum(2009,1,24.5)
  %LO1 error
  phasepush=phasecorr(d_raw,vc_penv(:,1:64),10,64,4,1);
 else
  phasepush=phasecorr(d_raw,vc_penv(:,1:64),10,59);
 end
elseif name_site=='K' | name_site=='S'
 phasepush=-21; %Average for 640 us pulse
elseif name_site=='L'
 phasepush=phasecorr(d_raw(1:end/2),vc_penv(:,1:64),25,64);
end
if ~isempty(phasepush), r_phasepush=phasepush; end
if ~isempty(r_phasepush)
 r_param(:,5)=r_param(:,5)-r_phasepush*v_lightspeed/ch_fradar(1)/2*sin(sc_angle/2);
end
