% %Function to read the plasma parameters from hdf5 files
%clear
% %data_path = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/03dec85/ForMadrigal/EISCAT_1985-12-03_cp1f@uhf.hdf5';
% data_path = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/04dec85/ForMadrigal/EISCAT_1985-12-04_cp1f@kir.hdf5';
% % data_path = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/11dec85/ForMadrigal/EISCAT_1985-12-11_cp3c@sod.hdf5';

function [Time,par2D,par1D,rpar2D,err2D]=load_param_hdf5(hdf5file)%,fileno,do_err)

% [Time,par2D,par1D,rpar2D]=load_param_hdf5(data_path,status)
% par2D [Ran,Alt,Ne,Te,Ti,Vi,Coll,Comp,Res]
% par1D [Az,El,Pt,Tsys]
% rpar2D [Ran,Alt,Ne]

% if nargin<2, fileno=[]; end
% if nargin<3, do_err=0; end
% if isempty(fileno), ask=1; fileno=1; end

global name_ant name_expr r_RECloc
Time=[]; par2d=[]; par1D=[]; rpar2d=[]; err2d=[];

filename = hdf5file;
[~,b] = fileparts(filename);
a = strfind(b,'@');
name_ant = b(a+1:end);

inform      = h5info(filename,'/metadata');
metavar = {inform.Datasets.Name}';
if ~isempty(find(strcmp(metavar,'names')))
    namesdata = h5read(filename,'/metadata/names');
    nnexpr = find(strcmp(deblank(namesdata(1,:)),'name_expr'));
    name_expr = deblank(namesdata{2,nnexpr});
end

if ~isempty(find(strcmp(metavar,'utime_pp')))
    do_rpar = false;
else
    do_rpar = true;
end

time_id   = 1;                             % [ut]
par1d_id  = [4 5 6 10];                    % [az el Pt Tsys]
par2d_id  = [19 18 20 22 21 24 23 54 55];  % [range, alt, ne, tr, ti, vi, collf, po+, res]
rpar2d_id = [61 61 62];                    % [range alt ne]
err2d_id  = [28 30 29 32 31];              % [dne dtr dti dvi dco]
rpar2d_for_merging_id  = [61 62 63 64];    % [pprange pp(ne) pperror ppw]
Te_id  = 94;
dTe_id = 95;
recloc_id = 9;

matdata.metadata.par0d    = deblank(h5read(filename,'/metadata/par0d'));
matdata.data.par0d        = h5read(filename,'/data/par0d');
matdata.metadata.par1d    = deblank(h5read(filename,'/metadata/par1d'));
matdata.data.par1d        = h5read(filename,'/data/par1d');
matdata.metadata.utime    = deblank(h5read(filename,'/metadata/utime'));
matdata.data.utime        = h5read(filename,'/data/utime');

columns = find(str2num(char(matdata.metadata.utime(end,:)))==time_id);
Time = datenum(datetime(matdata.data.utime(:,columns),'ConvertFrom','posixtime'));
rec = length(Time(:,1));

columns = find(str2num(char(matdata.metadata.par0d(end,:)))==recloc_id);
r_RECloc = matdata.data.par0d(columns);

[ii2d,ii2d_pp] = deal(0,0);
if ~isempty(find(strcmp(metavar,'par2d')))
    ii2d = 1;
    matdata.metadata.par2d    = deblank(h5read(filename,'/metadata/par2d'));
    matdata.data.par2d        = h5read(filename,'/data/par2d');
    columns = find(str2num(char(matdata.metadata.par2d(end,:)))==20);
    ndata2d =  length(matdata.data.par2d(:,columns));
end
if ~isempty(find(strcmp(metavar,'par2d_pp'))) && do_rpar
    matdata.metadata.par2d_pp = deblank(h5read(filename,'/metadata/par2d_pp'));
    matdata.data.par2d_pp     = h5read(filename,'/data/par2d_pp');
else
    do_rpar = false;
end

nh_id = 76;
column = find(str2num(char(matdata.metadata.par1d(end,:)))==nh_id);
if isempty(column) 
    column = find(str2num(char(matdata.metadata.par0d(end,:)))==nh_id);
    nh = matdata.data.par0d(column);
    nh = nh*ones(size(Time,1),1);
else
    nh = matdata.data.par1d(:,column);
end
nhmax = max(nh);
nh_alt = nhmax;
n_tot=size(Time,1);

for ii = par1d_id
    columns = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
    if isempty(columns)
        columns = find(str2num(char(matdata.metadata.par0d(end,:)))==ii);
        if isempty(columns)
            par_tmp = NaN(rec,1);
        else
            for jj = 1:length(columns)
                par_tmp = matdata.data.par0d(columns)*ones(rec,1);
            end
            if jj>1   % Tsys 
                par_tmp = median(par_tmp')';
            end
            if ii == 6 % Pt
                par_tmp = par_tmp/10000; % 10 kW
            end
        end
    else
        for jj = 1:length(columns)
            par_tmp(:,jj) = matdata.data.par1d(:,columns(jj));
        end
        if jj>1   % Tsys 
            par_tmp = median(par_tmp')';
        end
        if ii == 6 % Pt
            par_tmp = par_tmp/10000; % 10 kW
        end
    end
    par1D = [par1D par_tmp];
    par_tmp = [];   
end   

d=find(par1D(:,2)>90); % Cast azimuth and elevation into range 0-360, 0-90 degrees
par1D(d,2)=180-par1D(d,2);
par1D(d,1)=par1D(d,1)+180; 
par1D(:,1)=mod(par1D(:,1)+360,360);

b = 0;
if ii2d == 1    
    for ii = par2d_id
        columns = find(str2num(char(matdata.metadata.par2d(end,:)))==ii);
        if isempty(columns) && ii == 18       % if alt is empty, it is in par1d
            column = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
            alt = matdata.data.par1d(:,column); 
            par_tmp = kron(alt,ones(nh_alt,1));                                          % alt = [a s d f g h...] --> alt = [a a b b c c d d e e f f g g h h...] (for nh_alt =2)
        elseif isempty(columns) && ii == 19    % if range is empty, it is in par1d
            column = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
            range = matdata.data.par1d(:,column); 
            par_tmp = kron(range,ones(nh_alt,1));                                   % range = [a s d f g h...] --> range = [a a b b c c d d e e f f g g h h...] (for nh_alt =2)
        elseif isempty(columns) && ii == 22    % if Tr is empty
            b = 1;
            columns = find(str2num(char(matdata.metadata.par2d(end,:)))==Te_id);
            if isempty(columns)
                par_tmp = NaN(ndata2d,1);
            else 
                par_tmp = matdata.data.par2d(:,columns);
            end
        elseif isempty(columns)
            par_tmp = NaN(ndata2d,1);
        else
            if ii == 55 % res
                par_tmp = matdata.data.par2d(:,columns(1));
            else
                par_tmp = matdata.data.par2d(:,columns);
            end
        end
        par2d = [par2d par_tmp];
        par_tmp = [];
    end   

    for ii = err2d_id
        columns = find(str2num(char(matdata.metadata.par2d(end,:)))==ii);
        if isempty(columns) && ii == 30
            columns = find(str2num(char(matdata.metadata.par2d(end,:)))==dTe_id);
            if isempty(columns)
                par_tmp = NaN(ndata2d,1);
            else 
                par_tmp = matdata.data.par2d(:,columns);    
            end
        elseif isempty(columns)
            par_tmp = NaN(ndata2d,1);
        else
            par_tmp = matdata.data.par2d(:,columns);
        end
        err2d = [err2d par_tmp];
        par_tmp = [];
    end   
else
    for ii = par2d_id
        columns = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
        if isempty(columns) && ii == 22    % if Tr is empty
            b = 1;
            columns = find(str2num(char(matdata.metadata.par1d(end,:)))==Te_id);
            if isempty(columns)
                par_tmp = NaN(rec,1);
            else 
                par_tmp = matdata.data.par1d(:,columns);
            end
        elseif isempty(columns)
            par_tmp = NaN(rec,1);
        else
            if ii == 55 % res
                par_tmp = matdata.data.par1d(:,columns(1));
            else
                par_tmp = matdata.data.par1d(:,columns);
            end
        end
        par2d = [par2d par_tmp];   
        par_tmp = [];
    end
    
    for ii = err2d_id
        columns = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
        if isempty(columns) && ii == 30      % if dTr is empty
            columns = find(str2num(char(matdata.metadata.par1d(end,:)))==dTe_id);
            if isempty(columns)
                par_tmp = NaN(rec,1);
            else 
                par_tmp = matdata.data.par1d(:,columns);    
            end
        elseif isempty(columns)
            par_tmp = NaN(rec,1);
        else
            par_tmp = matdata.data.par1d(:,columns);
        end
        err2d = [err2d par_tmp];
        par_tmp = [];
    end   
end

if b == 0    % (Tr -> Te and dTr --> dTe),    the code assumes that either (Tr, dTr), [b=0] or (Te,dTe) [b=1] coexists. Is this true?
    err2d(:,2) = (err2d(:,3)./par2d(:,5) + err2d(:,2)./par2d(:,4)).*(par2d(:,4).*par2d(:,5)); %dTe = (dTi/Ti+dTr/Tr)*Te
    a = find(isinf(err2d(:,2)));
    err2d(a,2) = NaN;
    par2d(:,4) = par2d(:,4).*par2d(:,5);       % Te = Tr*Ti
end

npar2d = length(par2d_id);
nerr2d = length(err2d_id);
par2D  = NaN(nh_alt,n_tot,npar2d);
err2D  = NaN(nh_alt,n_tot,nerr2d);
    
nhalt_tmp = 0;
for ii = 1:n_tot
    altrange = (nhalt_tmp+1):(nhalt_tmp+nh(ii));
    par2D(1:nh(ii),ii,:) = par2d(altrange,:); 
    err2D(1:nh(ii),ii,:) = err2d(altrange,:); 
    nhalt_tmp = nhalt_tmp + nh(ii);
end

%if ii2d_pp == 1
if do_rpar
    for ii = rpar2d_id
        columns = find(str2num(char(matdata.metadata.par2d_pp(end,:)))==ii);
        par_tmp = matdata.data.par2d_pp(:,columns);
        rpar2d = [rpar2d par_tmp];
        par_tmp = [];
    end

    for ii = rpar2d_for_merging_id
        columns = find(str2num(char(matdata.metadata.par2d_pp(end,:)))==ii);
        if ii == rpar2d_for_merging_id(1)
            if ~isempty(columns)
                pprange = matdata.data.par2d_pp(:,columns); 
            else pprange = []; 
            end
        end
        if ii == rpar2d_for_merging_id(2)
            if ~isempty(columns)
                pp = matdata.data.par2d_pp(:,columns);
            else pp = [];
            end
        end
        if ii == rpar2d_for_merging_id(3)
            if ~isempty(columns)
                pperror = matdata.data.par2d_pp(:,columns);
            else pperror = [];
            end
        end
        if ii == rpar2d_for_merging_id(4)
            if ~isempty(columns)
                ppw = matdata.data.par2d_pp(:,columns);
            else ppw = [];
            end
        end
    end
    
    npprange_id = 77;
    column = find(str2num(char(matdata.metadata.par1d(end,:)))==npprange_id);
    if isempty(column) 
        column = find(str2num(char(matdata.metadata.par0d(end,:)))==npprange_id);
        npprange = matdata.data.par0d(column);
        npprange = npprange*ones(size(Time,1),1);
    else
        npprange = matdata.data.par1d(:,column);
    end
    npprangemax = max(npprange);
    npprange_range = npprangemax;
    nrpar2d = length(rpar2d_id);
    rpar2D  = NaN(npprange_range,n_tot,nrpar2d);
    
    npprange_tmp = 0;
    for ii = 1:n_tot
        rangerange = (npprange_tmp+1):(npprange_tmp+npprange(ii));
        if ~isempty(pp), pp_tomerge = pp(rangerange); else pp_tomerge = []; end 
        if ~isempty(pperror), pperror_tomerge = pperror(rangerange); else pperror_tomerge = []; end
        if ~isempty(ppw), ppw_tomerge = ppw(rangerange); else ppw_tomerge = []; end
        if ~isempty(pprange), pprange_tomerge = pprange(rangerange); else pprange_tomerge = []; end
        [pprange_merged,pperror_merged,ppw_merged,pp_merged,pprofile_id] = pp_merge(pprange_tomerge/1000,pperror_tomerge,ppw_tomerge/1000,pp_tomerge);   % /1000: m --> km 
        npprange_merged(ii) = length(pprange_merged);
        rpar2D(1:npprange_merged(ii),ii,:) = [pprange_merged pprange_merged pp_merged];
        npprange_tmp = npprange_tmp + npprange(ii);
    end

    % Remove redundant NaNs from rpar2D (since the merged is smaller than unmerged)
    npprange_merged_max = max(npprange_merged);
    rpar2D = rpar2D(1:npprange_merged_max,:,:);

    re=6370;
    elev = par1D(:,2);
    for ii = 1:n_tot  % caluclate ppalt fromm pprange
        x = rpar2D(:,ii,1)/re;
        rpar2D(:,ii,2) = re*sqrt(1+x.*(x+2*sin(elev(ii)/57.2957795)))-re;
    end
else
    rpar2D = [];
end

Time = Time';