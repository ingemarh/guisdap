function guisdaplogo(name,linew)
global GUP_ver local
if nargin<2, linew=20; end
if nargin<1, name=[]; end
bcol=[1 1 0]; %bcol=[1 1 1];

tlx=.5+[1 2 1 0 1]; tly=1+[0 0 sqrt(3) 0 0];

f=-100:4:100;
df0=rand(1,2)-.5; dfd=rand(1)-.5; dsp=rand(1); df=rand(size(f))-.5;
f0=20+5*df0; fd=20+5*dfd;
sp=(exp(-((f-f0(1))/fd).^2)+exp(-((f+f0(2))/fd).^2))*(2*dsp+1);
sp=.2*[-1 sp+.1*(sp+1).*df -1];
f=[f(1) f f(end)];

if ~isempty(name)
 p=fill(1.5-.1*[-1 -1 1 1],[0 1 1 0],[.5 .5 .5],tlx,tly,bcol,f/190+1.5,sp+1.4,'k');
 set(gca,'defaulttextunit','data')
 m=text(1.5,.5,sprintf('GUISDAP %s',strtok(GUP_ver,'-')));
 set(m,'horiz','center','fontunits','norm','fontsize',.1,'fontweight','bold','edgecolor',[1 0 0],'backgroundcolor',bcol,'linewidth',linew/4)
else
 p=fill(tlx,tly,bcol,f/190+1.5,sp+1.4,'k');
end
set(p(end-1),'edgecolor',[1 0 0],'linewidth',linew)

axis square
set(gca,'xlim',[0 3],'ylim',[0 3],'visible','off')
