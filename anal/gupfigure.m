function [varargout]=gupfigure(h)
if nargin==0
 h0=figure;
elseif isempty(findobj('Figure',h))
 h0=figure(h);
else
 set(0,'CurrentFigure',h)
 h0=h;
end
if nargout>0
 varargout{1}=h0;
end
