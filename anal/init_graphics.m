% Init_graphics.m: opens a sufficient number of figure windows. 
% GUISDAP v.1.80 02-11-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% The sizes and locations are defined here.
% Parameter:
% di_figures (global): requested windows (on input) and figure handles (on output)
Positions=...
[  40, 50,550,300;  % Correlator dump
   50, 50,310,500;  % Raw electron density
  600,300,600,400;  % Fit results
  500,200,600,400]; % Results

Names=...
['Correlator dump     ';
 'Raw electron density';
 'Fit results         ';
 'Results             '];

used=findobj('type','figure');
for i=1:4
 F=findobj(used,'userdata',i);
 if di_figures(i)
  if isempty(F)
   F=1; while ~isempty(find(used==F)), F=F+1; end
   figure(F), clf
   set(F,'Position',Positions(i,:),'NumberTitle','off','Name',Names(i,:),'UserData',i);
   set(F,'DefaultAxesFontWeight','bold','DefaultTextFontWeight','bold')
   used=[used;F];
  end
  di_figures(i)=sign(di_figures(i))*F;
 elseif ~isempty(F)
  close(F)
  used=used(find(used~=F));
 end
end

clear Positions Names used F
