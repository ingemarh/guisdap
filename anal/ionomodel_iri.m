function [altitude,ne,te,ti,coll,cO,cM2,cH]=ionomodel(heights)
global ionomodel_first
if isempty(ionomodel_first)
 fprintf('\n** The model uses the IRI-2001 model for this position**\n')
 ionomodel_first=1;
end
global d_time p_XMITloc
[tsec,year]=tosecs(d_time(1,:));
hh=[min(heights)-1 max(heights)+1 1]; if hh(2)-hh(1)>100, hh(3)=0; end
m_iri=iri([1 4 3 6 8 9 10 12],[tsec year],p_XMITloc(1:2),hh);
altitude=m_iri(:,8);
tn=msis(altitude*1e3,[tsec/86400 rem(tsec,86400)],p_XMITloc(1:2));
d=find(isnan(m_iri(:,2)) & altitude<150);
if ~isempty(d)
 m_iri(d,2:3)=tn(d,9)*ones(1,2);
end
d=find(isnan(m_iri(:,2)));
if ~isempty(d)
 d1=find(isfinite(m_iri(:,2)));
 m_iri(d,2:3)=exp(inter3(altitude(d),altitude(d1),log(m_iri(d1,2))))'*ones(1,2);
end
ne=m_iri(:,1);
d=find(isnan(ne));
if ~isempty(d)
 d1=find(isfinite(m_iri(:,1)));
 if isempty(d1)
  ne(:)=1e7;
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
cH(d)=inter3(altitude(d),[0;altitude(d1);4000],[0;cH(d1);1])';
cO=1-(cM2+cH);
coll=1.63e-16*tn(:,2)+2.99e-16*tn(:,3)+4.28e-16*tn(:,4); %only molaculars
coll(find(coll<1))=1;
