function guisdaplogo(mmm,linew)
global GUP_ver
if nargin<2, linew=20; end
if nargin<1, mmm=[]; end
if ~isempty(mmm)
 fill(1.5-.1*[-1 -1 1 1],[0 1 1 0],[.5 .5 .5]), hold on
 set(gca,'defaulttextunit','data')
 m=text(1.5,.5,sprintf('GUISDAP %g',GUP_ver));
 set(m,'horiz','center','fontunits','norm','fontsize',.1,'fontweight','bold','edgecolor',[1 0 0],'backgroundcolor',[1 1 0],'linewidth',linew/5)
end
tl=.5+[0 .5;2 .5;1 .5+sqrt(3)];
f=fill(tl(:,1),tl(:,2),'y');
hold on
p=plot([tl(:,1);tl(1:2,1)],[tl(:,2);tl(1:2,2)],'r');
set(p,'linewidth',linew)
f=-100:4:100;
df0=rand(1,2)-.5; dfd=rand(1)-.5; dsp=rand(1)-.5; df=(rand(size(f))-.5)*.1;
%df0=zeros(1,2); dfd=0; dsp=0; df=0;
f0=20+5*df0; fd=20+5*dfd;
sp=(.6+.4*dsp)*(exp(-((f-f0(1))/fd).^2)+exp(-((f+f0(2))/fd).^2))/2+.5*df;
sp=[-.2 sp -.2];
f=[f(1) f f(end)];
fill(f/200+1.5,sp+1.4,'k')
axis square
set(gca,'xlim',[0 3],'ylim',[0 3],'visible','off')
hold off
