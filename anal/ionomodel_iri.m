function [altitude,ne,te,ti,coll,cO,cM2,cH]=ionomodel(heights,modinfo)
global d_time p_XMITloc
if isempty(d_time)
 dtime=clock;
else
 dtime=d_time(1,:);
end
if isempty(p_XMITloc)
 loc=[69.6 19.2];
else
 loc=p_XMITloc(1:2);
end
if modinfo
 iripath=getenv('IRIPATH');
 [i1,i2,i3,i4]=textread(fullfile(iripath,'ig_rz.dat'),'%d,%d,%d,%d',1,'headerlines',2);
 i5=datenum(dtime);
 if i5>datenum(i4,i3,31) | i5<datenum(i2,i1,1)
  error('Date is outside iri model time range, please update the iri model')
 end
 if exist('iri')==3
  [d1,d]=fileparts(iripath);
 else
  d='IRI2012';
  if ~exist(d), d='IRI'; end
 end
 fprintf('** The model uses the %s model at the tx position (%.1f %.1f)**\n',d,loc)
end
[tsec,year]=tosecs(dtime);
hh=[min(heights)-1 max(heights)+1.1 1]; if hh(2)-hh(1)>100, hh(3)=0; end
m_iri=iri([1 4 3 6 8 9 10 12],[tsec year],loc,hh);
altitude=m_iri(:,8);
tn=msis(altitude*1e3,[tsec/86400 rem(tsec,86400)],loc);
d=find(isnan(m_iri(:,2)) & altitude<150);
if ~isempty(d)
 m_iri(d,2:3)=tn(d,9)*ones(1,2);
end
d=find(isnan(m_iri(:,2)));
if ~isempty(d)
 d1=find(isfinite(m_iri(:,2)));
 m_iri(d,2:3)=exp(inter3(altitude(d),altitude(d1),log(m_iri(d1,2))))'*ones(1,2);
end
ne=m_iri(:,1); net=(isnan(ne) | ne<=0); d=find(net);
if ~isempty(d)
 d1=find(~net);
 if isempty(d1)
  ne(:)=1e7;
 elseif min(altitude(d))>max(altitude(d1)) | max(altitude(d))<min(altitude(d1))
  ne(d)=1e7;
 else
  ne(d)=exp(inter3(altitude(d),altitude(d1),log(ne(d1))))';
 end
end
ti=m_iri(:,3);
te=m_iri(:,2);
cM2=(m_iri(:,5)+m_iri(:,6)+m_iri(:,7))/100;
cM2(find(altitude>400))=0;
cM2(find(isnan(cM2)))=1;
cH=m_iri(:,4)/100;
d=find(isnan(cH));
d1=find(isfinite(cH));
if isempty(d1)
 cH(:)=0;
else 
 cH(d)=inter3(altitude(d),[0;altitude(d1);4000],[0;cH(d1);1])';
end
cO=1-(cM2+cH);
coll=1.63e-16*tn(:,2)+2.99e-16*tn(:,3)+4.28e-16*tn(:,4); %only molaculars
coll(find(coll<1))=1;
