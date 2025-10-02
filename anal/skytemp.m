function t408=skytemp(site,t,az,el)
% function t408=tempsky(site,unixt,az,el) at 408MHz
% Input: site (3|W|D|T|V|L, arrays of time,az,el
global path_GUP
insert(py.sys.path,int32(0),fullfile(path_GUP,'pygup'))
getsky=py.importlib.import_module('add_tsky_main');
t408=single(getsky.matface(fullfile(path_GUP,'matfiles'),site,t,az,el));

% VHF_30: 150+tsky
% UHF_ip3: 85+tsky+10*((90-el)*pi/180).^3
% ESR_ip3: 66+tsky+9*((90-el)*pi/180).^3
% SAN_11: 120+tsky+75*((90-el)*pi/180).^3
