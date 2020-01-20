function mydatetick(ax,xlim,first)
global local
persistent xticks
set(ax,'XLim',xlim)
tickform='HH:MM'; td=diff(xlim);
if td>7, tickform='dd/mm';
elseif td<.003, tickform='HH:MM:SS'; end
if first
 if td>7
  set(ax,'xlim',[ceil(xlim(1)) floor(xlim(2))])
 elseif td>.003
  set(ax,'xlim',[ceil(xlim(1)*1440) floor(xlim(2)*1440)]/1440)
 end 
 fs=get(ax,'fontsize'); set(ax,'fontsize',fs/1.0) % To encourage more ticks
 datetick(ax,'x',tickform,'keeplimits')
 xticks=get(ax,'xtick'); lt=sum((xlim(1)<xticks) & (xticks<xlim(2)));
 if ~local.x && (isempty(xticks) || lt==0 || (lt<4 && td>.003))
   freduce=12.429; %Matlab bug...
 elseif lt<4
   freduce=2;
 else
   freduce=0;
 end
 if freduce
  set(ax,'fontsize',fs/freduce)
  datetick(ax,'x',tickform,'keeplimits')
  xticks=get(ax,'xtick');
 end
 set(ax,'fontsize',fs,'xlim',xlim)
else
 set(ax,'xtick',xticks)
 datetick(ax,'x',tickform,'keeplimits','keepticks')
end
fs=get(ax,'fontsize');
try
 if strcmp(ax.XAxis.MinorTick,'on')
  ax.XAxis.MinorTickValuesMode='auto'; drawnow
  mt=ax.XAxis.MinorTickValues; dmt=round(1./median(diff(mt)));
  switch dmt
   case 5
    mt=(floor(xlim(1)*6):ceil(xlim(2)*6))/6;
   case 20
    mt=(floor(xlim(1)*24):ceil(xlim(2)*24))/24;
   case 40
    mt=(floor(xlim(1)*48):ceil(xlim(2)*48))/48;
   case 120
    mt=(floor(xlim(1)*144):ceil(xlim(2)*144))/144;
   case 240
    mt=(floor(xlim(1)*288):ceil(xlim(2)*288))/288;
   case 480
    mt=(floor(xlim(1)*288):ceil(xlim(2)*288))/288;
   case 7200
    mt=(floor(xlim(1)*14400):ceil(xlim(2)*14400))/14400;
  end
  [c,ia,ib]=intersect(mt,xticks); mt(ia)=[];
  ax.XAxis.MinorTickValues=mt;
 end
end
set(ax,'fontsize',fs)
