function eiscatlogo(ax,linewidth)
% AT guisdaplogo([ax,]linewidth)
if nargin<2
    linewidth=ax; ax=gca;
end

if date<datetime([2025 01 01])
    [x,y]=arc(0,0,10,0,360);stroke(ax,x,y,linewidth);
    axis(ax,'square')
    hold(ax,'on')
    [x,y]=arc(0,0,7.3,180,0);stroke(ax,x,y,linewidth);
    [x,y]=arc(0,0,7.5,234.3,305.7);stroke(ax,x,y,linewidth);
    [x,y]=arc(0,-14,9,44.81,135.5);stroke(ax,x,y,linewidth);
    [x,y]=arc(0,-11,6,20,160);[x,y]=scale(x,y,0.8,1);stroke(ax,x,y,linewidth);
    [x,y]=arc(0,-11,6,12.5,167.5);[x,y]=scale(x,y,0.4,1);stroke(ax,x,y,linewidth);
    [x,y]=arcto(7.37,0,1.3,0,3.8,1,0.2);stroke(ax,x,y,linewidth);
    [x,y]=arcto(3.8,1,5.4,1.75,0,1.75,0.2);stroke(ax,x,y,linewidth);
    [x,y]=arcto(0,1.75,-5.4,1.75,-3.8,1,0.2);stroke(ax,x,y,linewidth);
    [x,y]=arcto(-3.8,1,-1.3,0,-7.3,0,0.2);stroke(ax,x,y,linewidth);
    [x,y]=lineto(-7.3,0,-7.37,0);stroke(ax,x,y,linewidth);
    [x,y]=lineto(0,1.75,0,7.3);stroke(ax,x,y,linewidth);
    [x,y]=lineto(1.15,1.75,0.62,7.25);stroke(ax,x,y,linewidth);
    [x,y]=lineto(2.3,1.75,1.2,7.18);stroke(ax,x,y,linewidth);
    [x,y]=lineto(3.45,1.75,1.8,7.05);stroke(ax,x,y,linewidth);
    [x,y]=lineto(4.59,1.68,2.35,6.91);stroke(ax,x,y,linewidth);
    [x,y]=lineto(7.32,0,2.89,6.7);stroke(ax,x,y,linewidth);
    [x,y]=lineto(-1.15,1.75,-0.62,7.25);stroke(ax,x,y,linewidth);
    [x,y]=lineto(-2.3,1.75,-1.2,7.18);stroke(ax,x,y,linewidth);
    [x,y]=lineto(-3.45,1.75,-1.8,7.05);stroke(ax,x,y,linewidth);
    [x,y]=lineto(-4.66,1.68,-2.35,6.91);stroke(ax,x,y,linewidth);
    [x,y]=lineto(-7.32,0,-2.89,6.7);stroke(ax,x,y,linewidth);
    linewidth=0.3527785*linewidth;
    [x,y]=lineto(0,1.73,5,-8);stroke(ax,x,y,linewidth);
    [x,y]=lineto(0,1.73,-5,-8);stroke(ax,x,y,linewidth);
    [x,y]=lineto(0,1.73,0,-9.5);stroke(ax,x,y,linewidth);
    [x,y]=lineto(2,1.73,0,-9.3);stroke(ax,x,y,linewidth);
    [x,y]=lineto(0,-9.3,-2,1.73);stroke(ax,x,y,linewidth);
    [x,y]=lineto(-6,0,0,-6.8);stroke(ax,x,y,linewidth);
    [x,y]=lineto(6,0,0,-6.8);stroke(ax,x,y,linewidth);
    [x,y]=arc(-5,-8,0.2,0,360);strokefill(ax,x,y,linewidth);
    [x,y]=arc(0,-9.3,0.2,0,360);strokefill(ax,x,y,linewidth);
    [x,y]=arc(5,-8,0.2,0,360);strokefill(ax,x,y,linewidth);
    [x,y]=arc(0,-6.8,0.2,0,360);strokefill(ax,x,y,linewidth);

    outsidecircletext(ax,'EISCAT SCIENTIFIC ASSOCIATION',14,90,8.65);

    axis(ax,'off')
    hold(ax,'off')
else
    cla(ax)
    hold(ax,'on')
    warning('off','MATLAB:polyshape:repairedBySimplify')
    d="M95.75,191.51C42.95,191.51,0,148.55,0,95.75S42.95,0,95.75,0s95.75,42.95,95.75,95.75-42.95,95.75-95.75,95.75ZM95.75,6.84C46.73,6.84,6.84,46.73,6.84,95.75s39.88,88.91,88.91,88.91,88.91-39.88,88.91-88.91S144.78,6.84,95.75,6.84Z";
    h=Path(ax,d);
    d="M95.75,17.84c-42.96,0-77.91,34.95-77.91,77.91s34.95,77.91,77.91,77.91,77.91-34.95,77.91-77.91S138.71,17.84,95.75,17.84ZM171.95,95.75c0,16.91-5.55,32.55-14.91,45.21-6.08-4.86-12.8-9.03-20.05-12.34l-11.95-18.32,22.74-21.18h23.86c.19,2.19.31,4.4.31,6.64ZM95.75,146.12c-1.08,0-2.15-.05-3.22-.08l-3.67-14.86,4.31,4.01c-.06.23-.11.47-.11.72,0,1.48,1.2,2.69,2.69,2.69s2.69-1.2,2.69-2.69c0-.25-.05-.49-.11-.72l4.33-4.03-3.68,14.87c-1.07.04-2.15.08-3.22.08ZM98.55,147.76l-2.8,11.31-2.79-11.31c.93.03,1.86.07,2.79.07s1.87-.05,2.8-.07ZM55.7,129.99c7.37-3.32,15.27-5.77,23.56-7.17-8.08,3.1-15.51,8.43-21.97,15.48-1.82-.78-3.61-1.63-5.38-2.52l3.79-5.79ZM50.38,134.99c-.75-.39-1.51-.77-2.25-1.18,1.5-.84,3.04-1.63,4.59-2.4l-2.34,3.58ZM73.25,67.92h21l-15.25,23.31-5.76-23.31ZM74.53,80.27s-.09-.05-.13-.07c-.09-.05-.18-.1-.28-.15-.26-.13-.52-.26-.83-.35l-22.77-6.74c-.81-.24-1.45-.83-1.76-1.61-.15-.39-.21-.8-.18-1.2.03-.41.16-.8.37-1.17l.03-.05c.4-.63,1.12-.98,1.84-1.01h20.66l3.05,12.35ZM112.56,91.14l-15.13-23.22h20.88l-5.74,23.22ZM120.06,67.92h20.64c.75,0,1.46.41,1.85,1.07.43.73.5,1.59.19,2.38-.31.79-.95,1.37-1.76,1.61l-22.77,6.73c-.27.08-.5.18-.73.29-.16.08-.31.16-.46.25l3.05-12.33ZM113.84,93.1l1.58-6.39s0,0,0,0c.17.31.38.6.62.86.02.03.05.06.07.09.1.1.2.2.31.3.08.07.17.14.26.21.1.07.19.15.29.21.2.13.4.24.63.33.07.03.15.05.22.08.19.07.39.13.6.18.08.02.16.04.25.05.29.05.58.09.9.09h25.7l-21.17,19.73-10.26-15.74ZM134.14,127.39c-6.59-2.76-13.57-4.85-20.85-6.14l10.48-9.77,10.37,15.9ZM113.27,95.37l9.55,14.65-11.68,10.89c-1.34-.21-2.69-.39-4.05-.54l6.18-25ZM108.51,123.36l-1.88,1.75c-.19-.19-.38-.37-.58-.55l.44-1.77c.68.17,1.35.36,2.02.56ZM104.61,123.27c-.54-.44-1.08-.86-1.62-1.22.62.1,1.23.22,1.84.35l-.21.87ZM104.2,124.92l-.79,3.2-6.06,5.65c-.45-.33-1-.54-1.6-.54s-1.15.2-1.6.54l-6.04-5.63-.8-3.23c2.68-2.26,5.51-3.48,8.43-3.48s5.77,1.22,8.45,3.49ZM86.91,123.26l-.21-.86c.6-.13,1.21-.25,1.82-.35-.54.35-1.08.78-1.61,1.21ZM85.47,124.55c-.2.19-.4.37-.59.56l-1.88-1.75c.68-.2,1.36-.39,2.04-.57l.43,1.76ZM80.36,120.91l-11.64-10.84,9.56-14.61,6.15,24.91c-1.37.15-2.72.34-4.07.54ZM77.72,93.19l-10.27,15.7-21.22-19.77h25.7c.19,0,.36-.03.53-.05.08,0,.17-.01.25-.02.18-.03.36-.07.53-.11.08-.02.15-.04.22-.07.59-.19,1.09-.47,1.52-.83,0,0,0,0,.01-.01.46-.39.83-.85,1.1-1.35l1.61,6.52ZM67.78,111.53l10.44,9.72c-7.26,1.29-14.22,3.36-20.8,6.11l10.36-15.84ZM57.71,140.35c5.07,2.09,10.33,3.75,15.71,4.98-2.26,6.22-4.18,13.4-5.7,21.26-7.31-2.9-14.07-6.89-20.07-11.79,2.96-5.34,6.32-10.21,10.06-14.44ZM58.95,139c6.51-6.99,14.03-12.17,22.18-15.03l2.59,2.41c-3.6,4.13-6.88,10.01-9.69,17.32-5.16-1.16-10.21-2.73-15.08-4.71ZM84.8,127.39l1.77,1.65,4.18,16.94c-5.14-.26-10.24-.92-15.22-1.96,2.69-7.06,5.83-12.74,9.27-16.64ZM104.95,129.02l1.75-1.63c3.44,3.9,6.57,9.57,9.27,16.64-4.99,1.04-10.07,1.7-15.22,1.96l4.2-16.96ZM107.78,126.38l2.59-2.41c8.15,2.86,15.67,8.04,22.18,15.03-4.88,1.97-9.92,3.55-15.08,4.71-2.8-7.31-6.09-13.19-9.69-17.32ZM133.79,140.35c3.74,4.23,7.1,9.1,10.06,14.44-6,4.9-12.76,8.89-20.07,11.79-1.52-7.86-3.44-15.04-5.7-21.26,5.38-1.22,10.64-2.88,15.71-4.98ZM112.25,122.83c8.3,1.4,16.22,3.85,23.61,7.18l3.76,5.77c-1.78.89-3.57,1.75-5.4,2.53-6.45-7.05-13.89-12.38-21.97-15.48ZM138.82,131.43c1.54.76,3.06,1.54,4.55,2.38-.73.41-1.49.78-2.23,1.17l-2.32-3.55ZM171.27,85.76l-44.78-59.71c23.83,10.55,41.26,32.96,44.78,59.71ZM123.38,24.76l46.98,62.64h-50.79c-1.97,0-2.88-1.42-3.05-2.64s.29-2.84,2.18-3.4l22.77-6.73c1.32-.39,2.36-1.35,2.87-2.63.5-1.28.39-2.69-.31-3.88l-26.94-45.49c2.14.62,4.24,1.34,6.29,2.14ZM114.69,21.94l26.23,44.29c-.07,0-.14-.03-.21-.03-.01,0-.02,0-.03,0h-20.68l-13.42-45.87c2.76.39,5.47.92,8.12,1.6ZM104.71,20.1l13.49,46.11h-21.51l-.17-46.63c2.77.03,5.5.21,8.19.53ZM95.84,68.63l16.16,24.78-6.62,26.77c-3.16-.3-6.37-.47-9.62-.47s-6.44.17-9.6.47l-6.59-26.68,16.27-24.87ZM94.81,19.58l.17,46.63h-21.68l13.49-46.11c2.64-.31,5.31-.49,8.02-.52ZM84.94,20.34l-13.42,45.87h-20.68s-.02,0-.03,0c-.07,0-.14.02-.22.03l26.23-44.29c2.65-.68,5.37-1.21,8.12-1.6ZM74.42,22.62l-26.91,45.44s-.02.03-.03.04c-.35.6-.55,1.25-.61,1.91-.05.66.04,1.34.3,1.98.5,1.28,1.55,2.24,2.87,2.63l22.77,6.73c1.65.49,2.21,1.79,2.21,2.92,0,.16-.01.32-.03.47-.18,1.22-1.08,2.64-3.05,2.64H21.16l46.97-62.62c2.06-.8,4.15-1.52,6.29-2.14ZM65.01,26.06l-44.78,59.71c3.52-26.75,20.95-49.16,44.78-59.71ZM19.86,89.11h23.86l22.78,21.23-11.95,18.26c-7.26,3.32-14,7.49-20.09,12.36-9.36-12.66-14.91-28.3-14.91-45.21,0-2.24.12-4.45.31-6.64ZM35.51,142.33c3.43-2.74,7.07-5.27,10.9-7.53,1,.57,2.02,1.09,3.03,1.63l-3.03,4.63c-.22-.06-.44-.1-.67-.1-1.48,0-2.69,1.2-2.69,2.69s1.2,2.69,2.69,2.69,2.69-1.2,2.69-2.69c0-.63-.22-1.2-.58-1.65l3.11-4.75c1.69.85,3.4,1.66,5.13,2.41-3.61,4.14-6.87,8.87-9.75,14.02-3.98-3.4-7.61-7.21-10.82-11.35ZM69.26,167.19c1.51-8.01,3.42-15.28,5.68-21.54,5.32,1.12,10.75,1.81,16.24,2.06l3.13,12.66c-.75.48-1.25,1.31-1.25,2.26,0,1.49,1.2,2.69,2.69,2.69s2.69-1.2,2.69-2.69c0-.95-.5-1.79-1.25-2.26l3.13-12.66c5.49-.25,10.92-.93,16.23-2.06,2.26,6.25,4.17,13.53,5.68,21.54-8.26,3.07-17.18,4.76-26.49,4.76s-18.24-1.69-26.49-4.76ZM145.17,153.68c-2.89-5.16-6.14-9.88-9.75-14.02,1.74-.75,3.45-1.56,5.14-2.42l3.1,4.76c-.36.46-.58,1.03-.58,1.66,0,1.48,1.2,2.69,2.69,2.69s2.69-1.2,2.69-2.69-1.2-2.69-2.69-2.69c-.23,0-.45.04-.67.09l-3.02-4.63c1.01-.53,2.03-1.05,3.02-1.62,3.83,2.26,7.47,4.78,10.9,7.53-3.21,4.15-6.84,7.95-10.82,11.35Z";
    h=Path(ax,d);
    warning('on','MATLAB:polyshape:repairedBySimplify')
    axis(ax,'off')
    hold(ax,'off')
    axis(ax,'square')
    axis(ax,[0,192,0,192])
    set(ax,'ydir','reverse')
end

function [X,Y]=arc(x,y,r,start,stop)
p=linspace(start,stop,100)*pi/180;
X=x+r*cos(p);
Y=y+r*sin(p);
return

function [X,Y]=arcto(x0,y0,x1,y1,x2,y2,r)
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

function h=stroke(ax,x,y,linewidth)
h=plot(ax,x,y,'k','linewidth',linewidth);
return

function h=strokefill(ax,x,y,linewidth)
h=fill(ax,x,y,'k','linewidth',linewidth);
return

function h=outsidecircletext(ax,thetext,fontsize,ang,r)
un=get(ax,'units');
set(ax,'units','points')
tmp=get(ax,'position');
scalefactor=min(tmp(3:4));
fsize=scalefactor*fontsize/150;
set(ax,'units',un)

for j=1:length(thetext)
    h=text(ax,r*cos(ang*pi/180),r*sin(ang*pi/180),thetext(j));
    set(h,'fontsize',fsize,'fontunits','normalized')
    temp=get(h,'extent');w(j)=temp(3);
    delete(h)
end
ww=(w(1:end-1)+w(2:end))/2;
phi=ang*pi/180+(sum(ww)/2-[0,cumsum(ww)])/r;
for j=1:length(thetext)
    h=text(ax,r*cos(phi(j)),r*sin(phi(j)),thetext(j));
    set(h,'fontsize',fsize,'fontweight','bold','fontunits','normalized')
    set(h,'horizontalalignment','center')
    set(h,'verticalalignment','middle')
    set(h,'rotation',phi(j)*180/pi-90)
end
return

%%%%%%%

function h=Path(ax,d)
resolution=10;
d=Split(d);
counter=1;
X=[];Y=[];
while counter<=length(d)
    switch d(counter)
        case "M"
            x=str2num(d(counter+1));
            y=str2num(d(counter+2));
            X=[X,x];
            Y=[Y,y];
            x0=x;
            y0=y;
            cpx=x;
            cpy=y;
            x1=[];
            y1=[];
            x2=[];
            y2=[];
            counter=counter+3;
        case "c"
            drawing=true;
            t=linspace(0,1,resolution);
            while drawing
                x1=str2num(d(counter+1))+cpx;
                y1=str2num(d(counter+2))+cpy;
                x2=str2num(d(counter+3))+cpx;
                y2=str2num(d(counter+4))+cpy;
                x=str2num(d(counter+5))+cpx;
                y=str2num(d(counter+6))+cpy;
                X=[X,(1-t).^3*cpx+3*(1-t).^2.*t*x1+3*(1-t).*t.^2*x2+t.^3*x];
                Y=[Y,(1-t).^3*cpy+3*(1-t).^2.*t*y1+3*(1-t).*t.^2*y2+t.^3*y];
                cpx=x;
                cpy=y;
                if any(regexp(d(counter+7),'[a-zA-Z]'))
                    drawing=false;
                end
                counter=counter+6;
            end
            counter=counter+1;
        case "C"
            drawing=true;
            t=linspace(0,1,resolution);
            while drawing
                x1=str2num(d(counter+1));
                y1=str2num(d(counter+2));
                x2=str2num(d(counter+3));
                y2=str2num(d(counter+4));
                x=str2num(d(counter+5));
                y=str2num(d(counter+6));
                X=[X,(1-t).^3*cpx+3*(1-t).^2.*t*x1+3*(1-t).*t.^2*x2+t.^3*x];
                Y=[Y,(1-t).^3*cpy+3*(1-t).^2.*t*y1+3*(1-t).*t.^2*y2+t.^3*y];
                cpx=x;
                cpy=y;
                if any(regexp(d(counter+7),'[a-zA-Z]'))
                    drawing=false;
                end
                counter=counter+6;
            end
            counter=counter+1;
        case "s"
            drawing=true;
            t=linspace(0,1,resolution);
            while drawing
                if isempty(x2)
                    x1=cpx;
                    y1=cpy;
                else
                    x1=2*cpx-x2;
                    y1=2*cpy-y2;
                end
                x2=str2num(d(counter+1))+cpx;
                y2=str2num(d(counter+2))+cpy;
                x=str2num(d(counter+3))+cpx;
                y=str2num(d(counter+4))+cpy;
                X=[X,(1-t).^3*cpx+3*(1-t).^2.*t*x1+3*(1-t).*t.^2*x2+t.^3*x];
                Y=[Y,(1-t).^3*cpy+3*(1-t).^2.*t*y1+3*(1-t).*t.^2*y2+t.^3*y];
                cpx=x;
                cpy=y;
                if any(regexp(d(counter+5),'[a-zA-Z]'))
                    drawing=false;
                end
                counter=counter+4;
            end
            counter=counter+1;
        case "S"
            drawing=true;
            t=linspace(0,1,resolution);
            while drawing
                if isempty(x2)
                    x1=cpx;
                    y1=cpy;
                else
                    x1=2*cpx-x2;
                    y1=2*cpy-y2;
                end
                x2=str2num(d(counter+1));
                y2=str2num(d(counter+2));
                x=str2num(d(counter+3));
                y=str2num(d(counter+4));
                X=[X,(1-t).^3*cpx+3*(1-t).^2.*t*(x1)+3*(1-t).*t.^2*(x2)+t.^3*(x)];
                Y=[Y,(1-t).^3*cpy+3*(1-t).^2.*t*(y1)+3*(1-t).*t.^2*(y2)+t.^3*(y)];
                cpx=x;
                cpy=y;
                if any(regexp(d(counter+5),'[a-zA-Z]'))
                    drawing=false;
                end
                counter=counter+4;
            end
            counter=counter+1;
        case "Z"
            X=[X,x0,NaN];
            Y=[Y,y0,NaN];
            cpx=x0;
            cpy=y0;
            counter=counter+1;
        case "l"
            drawing=true;
            while drawing
                x=str2num(d(counter+1))+cpx;
                y=str2num(d(counter+2))+cpy;
                X=[X,x];
                Y=[Y,y];
                cpx=x;
                cpy=y;
                if any(regexp(d(counter+3),'[a-zA-Z]'))
                    drawing=false;
                end
                counter=counter+2;
            end
            counter=counter+1;
        case "h"
            x=str2num(d(counter+1))+cpx;
            X=[X,x];
            Y=[Y,cpy];
            cpx=x;
            counter=counter+2;
        case "H"
            x=str2num(d(counter+1));
            X=[X,x];
            Y=[Y,cpy];
            cpx=x;
            counter=counter+2;
        otherwise
            disp(append('"',d(counter),'" not yet implemented'))
            break
    end
end
pol=polyshape(X,Y);
h=plot(ax,pol,'linestyle','none','facecolor','black','facealpha',1);
return

function d=Split(D)
len=strlength(D);
counter=1;
commands=regexp(D,'[a-zA-Z]');
for j=1:length(commands)
    d(counter)=extract(D,commands(j));
    counter=counter+1;
    if commands(j)==len
        break
    end
    s=extractBetween(D,commands(j)+1,commands(j+1)-1);
    if strlength(s)<1
        continue
    end
    d(counter)=extract(s,1);
    decim=false;
    if strcmp(d(counter),".")
        decim=true;
    end
    for k=2:strlength(s)
        switch extract(s,k)
            case "."
                if decim
                    counter=counter+1;
                    d(counter)=".";
                else
                    d(counter)=append(d(counter),".");
                    decim=true;
                end
            case "-"
                counter=counter+1;
                d(counter)="-";
                decim=false;
            case ","
                counter=counter+1;
                d(counter)="";
                decim=false;
            otherwise
                d(counter)=append(d(counter),extract(s,k));
        end
    end
    counter=counter+1;
end
return