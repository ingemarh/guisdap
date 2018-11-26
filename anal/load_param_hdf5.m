
%clear

%data_path = '/home/rikard/matlab/AnalysedData/2017-09-09_folke_300@32ma/Hdf5_2017-09-09_folke_300@32ma.hdf5';
%data_path = '/home/rikard/matlab/AnalysedData/2008-11-24_tau1_ant@uhf/Hdf5_2008-11-24_tau1_ant@uhf.hdf5';

function [Time,par2D,par1D,rpar2D,err2D]=load_param_hdf5(data_path)%,fileno,do_err)
% Function to read the plasma parameters from hdf5 files
%
% [Time,par2D,par1D,rpar2D]=load_param_hdf5(data_path,status)
% par2D [Ran,Alt,Ne,Te,Ti,Vi,Coll,Comp,Res]
% par1D [Az,El,Pt,Tsys]
% rpar2D [Ran,Alt,Ne]

global name_ant name_expr

Time=[]; par2d=[]; par1D=[]; rpar2d=[]; err2d=[];
% if nargin<2, fileno=[]; end
% if nargin<3, do_err=0; end
% if isempty(fileno), ask=1; fileno=1; end

%filename = fullfile(data_path,'Hdf5_2017-09-09_folke_300@32ma.hdf5');
filename = data_path;

matdata.metadata.par0d    = deblank(h5read(filename,'/metadata/par0d'));
matdata.metadata.par1d    = deblank(h5read(filename,'/metadata/par1d'));
matdata.metadata.par2d    = deblank(h5read(filename,'/metadata/par2d'));
matdata.metadata.par2d_pp = deblank(h5read(filename,'/metadata/par2d_pp'));

gfddata = h5read(filename,'/metadata/gfd');
gfdfields = fieldnames(gfddata);

for ii = 1:length(gfdfields)
    if ischar(gfddata.(char(gfdfields(ii)))')
        matdata.metadata.gfd.(char(gfdfields(ii))) = deblank(gfddata.(char(gfdfields(ii)))');
   else
        matdata.metadata.gfd.(char(gfdfields(ii))) = gfddata.(char(gfdfields(ii)))';
    end    
end

matdata.data.par0d    = h5read(filename,'/data/par0d')';
matdata.data.par1d    = h5read(filename,'/data/par1d');
matdata.data.par2d    = h5read(filename,'/data/par2d');
matdata.data.par2d_pp = h5read(filename,'/data/par2d_pp');

name_expr = matdata.metadata.gfd.name_expr;
[~,b]=fileparts(data_path);
a = strfind(b,'@');
name_ant = b(a+1:a+3);

time_id   = 1;                             % [ut]
par1d_id  = [4 5 6 10];                    % [az el Pt Tsys]
par2d_id  = [19 18 20 22 21 24 23 54 55];  % [range, alt, ne, tr, ti, vi, co, po+, res]
rpar2d_id = [61 61 62];                    % [range alt ne]
err2d_id  = [28 30 29 32 31];              % [dne dtr dti dvi dco]

columns = find(str2num(char(matdata.metadata.par1d(end,:)))==time_id);
Time = datenum(datetime(matdata.data.par1d(:,columns),'ConvertFrom','posixtime'));
rec = length(Time(:,1));

for ii = par1d_id
    columns = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
    if isempty(columns)
        columns = find(str2num(char(matdata.metadata.par0d(end,:)))==ii);
        if isempty(columns)
            par_tmp = NaN(rec,1);
        else
            for jj = 1:length(columns)
                par_tmp(:,jj) = ones(rec,1)*matdata.data.par0d(:,columns(jj));
            end
            if jj>1  % Tsys
                par_tmp = median(par_tmp');
            end
        end
    else
        for jj = 1:length(columns)
        par_tmp(:,jj) = matdata.data.par1d(:,columns(jj));
        end
        if jj>1   % Tsys 
            par_tmp = median(par_tmp')';
        end
    end
    par1D = [par1D par_tmp];
end   

for ii = par2d_id
    columns = find(str2num(char(matdata.metadata.par2d(end,:)))==ii);
    if isempty(columns)
        columns = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
        if isempty(columns)
            par_tmp = ones(rec,1)*NaN;
        else
            if ii == 55 % res
                par_tmp = matdata.data.par1d(:,columns(1));
            else
                par_tmp = matdata.data.par1d(:,columns);
            end
        end
    else
        if ii == 55 % res
            par_tmp = matdata.data.par2d(:,columns(1));
        else
            par_tmp = matdata.data.par2d(:,columns);
        end
    end
    par2d = [par2d par_tmp];
end   

for ii = err2d_id
    columns = find(str2num(char(matdata.metadata.par2d(end,:)))==ii);
    if isempty(columns)
        columns = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
        if isempty(columns)
            par_tmp = ones(rec,1)*NaN;
        else
            par_tmp = matdata.data.par1d(:,columns);
        end
    else
        par_tmp = matdata.data.par2d(:,columns);
    end
    err2d = [err2d par_tmp];
end   

err2d(:,2) = (par2d(:,5)./err2d(:,3) + par2d(:,4)./err2d(:,2)).*(par2d(:,4).*par2d(:,5)); %dTe = (dTi/Ti+dTr/Tr)*Te
a = find(isinf(err2d(:,2)));
err2d(a,2) = NaN;

par2d(:,4) = par2d(:,4).*par2d(:,5);       % Te = Tr*Ti
par2d(:,6) = -par2d(:,6);                  % pos = upflow

for ii = rpar2d_id
    columns = find(str2num(char(matdata.metadata.par2d_pp(end,:)))==ii);
    par_tmp = matdata.data.par2d_pp(:,columns);
    rpar2d = [rpar2d par_tmp];
end

% re=6370;
% x = rpar2d(:,1)/re;          % pprange/Re
% elev = par1D(:,2);
% rpar2d(:,2) = re*sqrt(1+x.*(x+2*sin(r_el/57.2957795)))-re;

nh_id = 70;
column = find(str2num(char(matdata.metadata.par1d(end,:)))==nh_id);
if isempty(column) 
    column = find(str2num(char(matdata.metadata.par0d(end,:)))==nh_id);
    nh = matdata.data.par0d(:,column);
    nhmax = nh;
else
    nh = matdata.data.par1d(:,column);
    nhmax = max(nh);
end

npprange_id = 71;
column = find(str2num(char(matdata.metadata.par1d(end,:)))==npprange_id);
if isempty(column) 
    column = find(str2num(char(matdata.metadata.par0d(end,:)))==npprange_id);
    npprange = matdata.data.par0d(:,column);
    npprangemax = npprange;
else
    npprange = matdata.data.par1d(:,column);
    npprangemax = max(npprange);
end

nh_alt = nhmax;
npprange_alt = npprangemax;
n_tot=size(Time,1);
npar2d=length(par2d_id);
nerr2d=length(err2d_id);
nrpar2d=length(rpar2d_id);

%create 3D-matrices
par2D  = NaN(nh_alt,n_tot,npar2d);
err2D  = NaN(nh_alt,n_tot,nerr2d);
rpar2D = NaN(npprange_alt,n_tot,nrpar2d);

nhalt_tmp = 0;
for ii = 1:n_tot
    if length(nh)==1
        altrange = (nh*(ii-1)+1):(nh*ii);
        par2D(:,ii,:) = par2d(altrange,:); 
        err2D(:,ii,:) = err2d(altrange,:);
    else
        altrange = (nhalt_tmp+1):(nhalt_tmp+nh(ii));
        par2D(1:nh(ii),ii,:) = par2d(altrange,:); 
        err2D(1:nh(ii),ii,:) = err2d(altrange,:); 
        nhalt_tmp = nhalt_tmp + nh(ii);
    end
end

npprangealt_tmp = 0;
for ii = 1:n_tot
    if length(npprange)==1
        altrange = (npprange*(ii-1)+1):(npprange*ii);
        rpar2D(:,ii,:) = rpar2d(altrange,:); 
    else
        altrange = (npprangealt_tmp+1):(npprangealt_tmp+npprange(ii));
        rpar2D(1:npprange(ii),ii,:) = rpar2d(altrange,:); 
        npprangealt_tmp = npprangealt_tmp + npprange(ii);
    end
end

re=6370;
elev = par1D(:,2);
for ii = 1:n_tot  % caluclate ppalt fromm pprange
   x = rpar2D(:,ii,1)/re;
   rpar2D(:,ii,2) = re*sqrt(1+x.*(x+2*sin(elev(ii)/57.2957795)))-re;
end

Time = Time';

d=find(par1D(:,2)>90); % Cast azimuth and elevation into range 0-360, 0-90 degrees
par1D(d,2)=180-par1D(d,2);
par1D(d,1)=par1D(d,1)+180; 
par1D(:,1)=mod(par1D(:,1)+360,360);

 end
