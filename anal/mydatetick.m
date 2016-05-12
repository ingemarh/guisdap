function mydatetick(xlim,first)
global local
persistent xticks
set(gca,'XLim',xlim)
tickform='HH:MM'; td=diff(xlim);
if td>7, tickform='dd/mm';
elseif td<.003, tickform='HH:MM:SS'; end
if first
 if td>7
  set(gca,'xlim',[ceil(xlim(1)) floor(xlim(2))])
 elseif td>.003
  set(gca,'xlim',[ceil(xlim(1)*1440) floor(xlim(2)*1440)]/1440)
 end 
 fs=get(gca,'fontsize'); set(gca,'fontsize',fs/1.4) % To encourage more ticks
 datetick(gca,'x',tickform,'keeplimits')
 xticks=get(gca,'xtick'); lt=sum((xlim(1)<xticks) && (xticks<xlim(2)));
 if ~local.x && (isempty(xticks) || lt==0 || (lt<4 && td>.003))
   freduce=12.429; %Matlab bug...
 elseif lt<4
   freduce=2;
 else
   freduce=0;
 end
 if freduce
  set(gca,'fontsize',fs/freduce)
  datetick(gca,'x',tickform,'keeplimits')
  xticks=get(gca,'xtick');
 end
 set(gca,'fontsize',fs,'xlim',xlim)
end
set(gca,'xtick',xticks)
datetick(gca,'x',tickform,'keeplimits','keepticks')
