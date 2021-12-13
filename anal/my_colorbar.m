function handle=my_colorbar(label,lg,h)

if nargin<2 || isempty(lg), lg='linear'; end
if nargin<3, h=gca; end

% Determine color limits by context.  If any axes child is an image
% use scale based on size of colormap, otherwise use current CAXIS.

ch=get(h,'children');
hasimage=0; t=[];
for i=1:length(ch)
  if strcmp(get(ch(i),'type'),'image')
    hasimage=1;
    t=get(ch(i),'UserData'); % Info stored by imshow or imagesc
  elseif strcmp(get(ch(i),'Type'),'surface'), % Texturemapped surf?
    if strcmp(get(ch(i),'FaceColor'),'texturemap')
      hasimage=2;
      t=get(ch(i),'UserData'); % Info stored by imshow or imagesc
    end
  end
end
if hasimage
  if isempty(t), t=[0.5 size(colormap(h),1)+0.5]; end
else
  t=caxis(h);
end

f=get(h,'parent');

% Search for existing colorbar
pos=get(h,'Position');
ch=findobj(f,'type','axes'); ax=[];
for i=1:length(ch)
  d=get(ch(i),'userdata');
  if length(d)==length(pos) & d==pos
    ax=ch(i); break
  end
end

if strcmp(get(f,'NextPlot'),'replace'),
  set(f,'NextPlot','add')
end

if isempty(ax)
  rect=[pos(1)+1.02*pos(3) pos(2) .035*pos(3) pos(4)];

  % Create axes for stripe
  ax=axes(f,'Position', rect);
%else
%  axes(ax)
end

% Create color stripe
n=size(colormap(f),1);
if strcmp(lg,'log')
  tt=((0:n)-.5)'*diff(t)/(n-1)+t(1);
  surface(ax,[0 1],10.^tt,[tt tt])
  set(ax,'CLim',t,'ylim',10.^t,'yscale','log','layer','top')
else
  image(ax,[0 1],t,[1:n]')
% set(ax,'TickDir','in')
  set(ax,'ylim',t,'Ydir','normal')
% Justify color axis
% set(ax,'yticklabel',strjust(get(ax,'yticklabel')))
end

set(ax,'userdata',pos,'YaxisLocation','right','xtick',[])
fs=get(ax,'fontsize');
yt=get(ax,'ytick');
ylabel(ax,label,'Rotation',-90,'VerticalAlignment','baseline','fontsize',fs)
set(ax,'fontsize',fs,'ytick',yt)
set(h,'Nextplot','Replace')

if nargout>0, handle=ax; end
