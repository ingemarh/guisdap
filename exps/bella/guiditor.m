global d_raw
if name_site=='T'
 phasepush=phasecorr(d_raw,vc_penv(:,1:64),15,89,3);
elseif name_site=='K' | name_site=='S'
 phasepush=-21; %Average for 640 us pulse
end
if ~isempty(phasepush), r_phasepush=phasepush; end
