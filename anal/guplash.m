function guplash()
f=figure('NumberTitle','off','Menu','none','ToolBar','none');
p=get(f,'position'); set(f,'position',[p([1 2]) p(4)*[1 1]/2])
guisdaplogo(1)
drawnow
pause(1)
close(f)
