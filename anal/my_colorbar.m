function handle=my_colorbar(lg)

if nargin<1 | isempty(lg), lg='linear'; end
ax=[];

% Determine color limits by context.  If any axes child is an image
% use scale based on size of colormap, otherwise use current CAXIS.

ch=get(gca,'children');
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
  if isempty(t), t=[0.5 size(colormap,1)+0.5]; end
else
  t=caxis;
end

h=gca;

% Search for existing colorbar
ch=get(gcf,'children'); ax=[];
for i=1:length(ch),
  d=get(ch(i),'userdata');
  if prod(size(d))==1 & d==h
    ax=ch(i); break
  end
end

if strcmp(get(gcf,'NextPlot'),'replace'),
  set(gcf,'NextPlot','add')
end

stripe=0.03; edge=0.08; 

if isempty(ax)
  pos=get(h,'Position');
  [az,el]=view;
  if all([az,el]==[0 90]), space=0.02; else, space=.1; end
  set(h,'Position',[pos(1) pos(2) pos(3)*(1-stripe-edge-space) pos(4)])
  rect=[pos(1)+(1-stripe-edge)*pos(3) pos(2) stripe*pos(3) pos(4)];

  % Create axes for stripe
  ax=axes('Position', rect);
else
  axes(ax)
end

% Create color stripe
n=size(colormap,1);
if strcmp(lg,'log')
  tt=((0:n)-.5)'*diff(t)/(n-1)+t(1);
  surface([0 1],10.^tt,[tt tt])
  set(ax,'CLim',t,'ylim',round(10.^t),'yscale','log','layer','top')
else
  image([0 1],t,[1:n]')
% set(ax,'TickDir','in')
  set(ax,'ylim',t,'Ydir','normal')
% Justify color axis
% set(ax,'yticklabel',strjust(get(ax,'yticklabel')))
end

set(ax,'userdata',h,'YaxisLoc','right','xtick',[])
set(gcf,'CurrentAxes',h)
set(gcf,'Nextplot','Replace')

if nargout>0, handle=ax; end
