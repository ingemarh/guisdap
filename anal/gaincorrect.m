function gaincorrect(glp,grps,p)
% glp: The lagprofile groups containing the gain measurements
% grps: The groups of lagprofilegroups handles by the same gain parameters: start,stop,tdiff
global d_data d_var1 d_var2 ADDR_SHIFT lpg_ra lpg_nt lpg_ri lpg_dt lpg_h lpg_lag
gx=[];
if nargin<3, p=0; end

for i=glp
  add=lpg_ra(i)+ADDR_SHIFT+(0:lpg_nt(i)-1)*lpg_ri(i);
  gx=[gx;gainfit(d_data(add),sqrt(real(d_var1(add)+d_var2(add))),lpg_dt(i),p)];
end
gx=mean(gx(:,1:2),1);
for i=1:size(grps,1)
  j=grps(i,1):grps(i,2);
  for j=grps(i,1):grps(i,2)
    t=[lpg_h(j)-grps(i,3)+lpg_lag(j)/2+(0:lpg_nt(j)-1)*lpg_dt(j)];
    [err,g]=gaincurve([gx 0],t');
    add=lpg_ra(j)+1+(0:lpg_nt(j)-1)*lpg_ri(i);
    d_data(add)=d_data(add)./g;
  end
end

function x=gainfit(data,sd,dt,p)
% Gain curve for receiver recovery: g=a*exp(-t/b)+c
ld=length(data);
sd=median(sd);
t=dt*(0:ld-1)'; g=median(data);
x0=[0 1000 g]; x=[tan(x0(1)) log(x0(2:3))];
opts=optimset(optimset('fminsearch'),'maxfun',1e4,'maxiter',1e4,'display','off');
d=find(abs(data-g)<3*sd);
lld=0;
while lld~=length(d)
 lld=length(d);
 x=fminsearch('gaincurve',x,opts,t(d),data(d));
 [err,g]=gaincurve(x,t,data);
 d=find(abs(data-g)<3*sd);
end
if p
 figure(p),plot(t,data,t,g),drawnow
end
