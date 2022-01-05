global d_raw
if name_site=='P'
 phasepush=-21; %general UHF value
elseif name_site=='K' | name_site=='S'
 phasepush=-21; %Average for 640 us pulse
else
 if name_site=='V'
  ntx=[34 94];
 elseif name_site=='L'
  ntx=[79 229];
 elseif name_site=='T'
  ns=[79 229];
 end
 lo=[];
 vc1=[1,5,9];
 vc2=[2,6,7];
 for i=1:3
  vc11=vc1(i):9:36;
  vc21=vc2(i):9:36;
  for j=1:4
   ind=(i-1+3*(j-1))*ntx(1)+(1:ntx(1));
   phasepush((i-1)*4+j)=phasecorr(d_raw(col(ind)),vc_penv(:,vc11(j)),12,1*30,[],lo);
   ind=ntx(1)*12+(i-1+3*(j-1))*ntx(2)+(1:ntx(2));
   phasepush((i-1)*4+j+12)=phasecorr(d_raw(col(ind)),vc_penv(:,vc21(j)),12,30,[],lo);
  end
 end
 phasepush
 phasepush=mean(phasepush);
end
if ~isempty(phasepush), r_phasepush=phasepush; end
