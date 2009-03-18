global d_raw
if d_date>datenum(2009,1,23)
 if name_site=='L'
  phasepush=phasecorr(d_raw(1:end/2),vc_penv(3:end,:),5,57,4);
 else
  phasepush=[];
 end
 if ~isempty(phasepush), r_phasepush=phasepush; end
end
