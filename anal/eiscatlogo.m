function eiscatlogo(linewidth)
% AT

[x,y]=arc(0,0,10,0,360);stroke(x,y,linewidth);
axis square
hold on
[x,y]=arc(0,0,7.3,180,0);stroke(x,y,linewidth);
[x,y]=arc(0,0,7.5,234.3,305.7);stroke(x,y,linewidth);
[x,y]=arc(0,-14,9,44.81,135.5);stroke(x,y,linewidth);
[x,y]=arc(0,-11,6,20,160);[x,y]=scale(x,y,0.8,1);stroke(x,y,linewidth);
[x,y]=arc(0,-11,6,12.5,167.5);[x,y]=scale(x,y,0.4,1);stroke(x,y,linewidth);
[x,y]=arcto(7.37,0,1.3,0,3.8,1,0.2);stroke(x,y,linewidth);
[x,y]=arcto(3.8,1,5.4,1.75,0,1.75,0.2);stroke(x,y,linewidth);
[x,y]=arcto(0,1.75,-5.4,1.75,-3.8,1,0.2);stroke(x,y,linewidth);
[x,y]=arcto(-3.8,1,-1.3,0,-7.3,0,0.2);stroke(x,y,linewidth);
[x,y]=lineto(-7.3,0,-7.37,0);stroke(x,y,linewidth);
[x,y]=lineto(0,1.75,0,7.3);stroke(x,y,linewidth);
[x,y]=lineto(1.15,1.75,0.62,7.25);stroke(x,y,linewidth);
[x,y]=lineto(2.3,1.75,1.2,7.18);stroke(x,y,linewidth);
[x,y]=lineto(3.45,1.75,1.8,7.05);stroke(x,y,linewidth);
[x,y]=lineto(4.59,1.68,2.35,6.91);stroke(x,y,linewidth);
[x,y]=lineto(7.32,0,2.89,6.7);stroke(x,y,linewidth);
[x,y]=lineto(-1.15,1.75,-0.62,7.25);stroke(x,y,linewidth);
[x,y]=lineto(-2.3,1.75,-1.2,7.18);stroke(x,y,linewidth);
[x,y]=lineto(-3.45,1.75,-1.8,7.05);stroke(x,y,linewidth);
[x,y]=lineto(-4.66,1.68,-2.35,6.91);stroke(x,y,linewidth);
[x,y]=lineto(-7.32,0,-2.89,6.7);stroke(x,y,linewidth);
linewidth=0.3527785*linewidth;
[x,y]=lineto(0,1.73,5,-8);stroke(x,y,linewidth);
[x,y]=lineto(0,1.73,-5,-8);stroke(x,y,linewidth);
[x,y]=lineto(0,1.73,0,-9.5);stroke(x,y,linewidth);
[x,y]=lineto(2,1.73,0,-9.3);stroke(x,y,linewidth);
[x,y]=lineto(0,-9.3,-2,1.73);stroke(x,y,linewidth);
[x,y]=lineto(-6,0,0,-6.8);stroke(x,y,linewidth);
[x,y]=lineto(6,0,0,-6.8);stroke(x,y,linewidth);
[x,y]=arc(-5,-8,0.2,0,360);strokefill(x,y,linewidth);
[x,y]=arc(0,-9.3,0.2,0,360);strokefill(x,y,linewidth);
[x,y]=arc(5,-8,0.2,0,360);strokefill(x,y,linewidth);
[x,y]=arc(0,-6.8,0.2,0,360);strokefill(x,y,linewidth);

outsidecircletext('EISCAT SCIENTIFIC ASSOCIATION',14,90,8.65);

axis off
hold off

function [X,Y]=arc(x,y,r,start,stop)
p=linspace(start,stop,100)*pi/180;
X=x+r*cos(p);
Y=y+r*sin(p);
return

function [X,Y]=arcto(x0,y0,x1,y1,x2,y2,r)
% Inte färdig än
A=sqrt((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0));
B=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
C=sqrt((x2-x0)*(x2-x0)+(y2-y0)*(y2-y0));
phi=acos((A*A+B*B-C*C)/(2*A*B))/2;
l1=r/tan(phi);
xa=x1+l1/A*(x0-x1);ya=y1+l1/A*(y0-y1);
xb=x1+l1/B*(x2-x1);yb=y1+l1/B*(y2-y1);
l2=r/sin(phi);
xm=(xa+xb)/2;ym=(ya+yb)/2;rm=sqrt((xm-x1)*(xm-x1)+(ym-y1)*(ym-y1));
xc=x1+l2/rm*(xm-x1);yc=y1+l2/rm*(ym-y1);
start=atan2(ya-yc,xa-xc)*180/pi;
stop=atan2(yb-yc,xb-xc)*180/pi;
crossproduct=cross([x1-x0,y1-y0,0],[x2-x1,y2-y1,0]);
if crossproduct(3)<0 && start<stop
    start=start+360;
elseif crossproduct(3)>0 && start>stop
    start=start-360;
end
[xx,yy]=arc(xc,yc,r,start,stop);
X=[x0,xa,xx,xb,x2];
Y=[y0,ya,yy,yb,y2];
return

function [X,Y]=lineto(x1,y1,x2,y2)
X=[x1,x2];
Y=[y1,y2];
return

function [X,Y]=scale(x,y,dx,dy)
X=x*dx;
Y=y*dy;
return

function h=stroke(x,y,linewidth)
h=plot(x,y,'k','linewidth',linewidth);
return

function h=strokefill(x,y,linewidth)
h=fill(x,y,'k','linewidth',linewidth);
return

function h=outsidecircletext(thetext,fontsize,ang,r)
un=get(gca,'units');
set(gca,'units','points')
tmp=get(gca,'position');
scalefactor=min(tmp(3:4));
fsize=scalefactor*fontsize/150;
set(gca,'units',un)

for j=1:length(thetext)
    h=text(r*cos(ang*pi/180),r*sin(ang*pi/180),thetext(j));
    set(h,'fontsize',fsize,'fontunits','normalized')
    temp=get(h,'extent');w(j)=temp(3);
    delete(h)
end
ww=(w(1:end-1)+w(2:end))/2;
phi=ang*pi/180+(sum(ww)/2-[0,cumsum(ww)])/r;
for j=1:length(thetext)
    h=text(r*cos(phi(j)),r*sin(phi(j)),thetext(j));
    set(h,'fontsize',fsize,'fontweight','bold','fontunits','normalized')
    set(h,'horizontalalignment','center')
    set(h,'verticalalignment','middle')
    set(h,'rotation',phi(j)*180/pi-90)
end
return
