function onera_desp_lib_load(libfile,headerfile,makeproto)
%function onera_desp_lib_load(libfile,@prototypefunction);
%function onera_desp_lib_load(libfile,headerfile);
% checks for the presence of the onera_desp_lib dynamic library in memory
% if not present, attempts to load it using either a prototype function
% or a headerfile
if ~libisloaded('onera_desp_lib'),
    if nargin < 2,
        if exist('onera_desp_lib_proto.m','file'),
            headerfile = @onera_desp_lib_proto; % use prototype file
        else
            headerfile = 'onera_desp_lib'; % use headerfile
        end
    end
    if nargin < 1,
        libfile = 'onera_desp_lib';
    end
    if nargin > 2
     loadlibrary(libfile,headerfile,'alias','onera_desp_lib','mfilename','onera_desp_lib_proto');
    else
     loadlibrary(libfile,headerfile,'alias','onera_desp_lib');
    end
end
