

function TheSuperMasterFcn(dirpath, datapath, showfigs)

global newhdf5file

% Show figures?
% No:  showfigs = []
% Yes: showfigs = 1
if nargin<3
    showfigs = []; 
end
if nargin<2
    error('Experiment and output path needed.')
end
if ~exist(datapath)
    mkdir(datapath);
end

% ugly hack: start_GUP clears all variables, so store and re-read!!
save(fullfile(tempdir,'my_input.mat'),'dirpath','datapath','showfigs');
start_GUP;
load(fullfile(tempdir,'my_input.mat'));
delete(fullfile(tempdir,'my_input.mat'));

image_filelist = [dir(fullfile(dirpath,'*.png'));dir(fullfile(dirpath,'*.gif'));dir(fullfile(dirpath,'*.jpg'));dir(fullfile(dirpath,'*.jpeg'));dir(fullfile(dirpath,'*.eps.gz'));;dir(fullfile(dirpath,'*.eps'))];
if isempty(image_filelist)
    b = 0; 
else
    b = 1;
end

logs_file = [];
logs_filename = [];
targz_filecheck = dir(fullfile(dirpath,'*tar.gz'));
targz_file = [];
if ~isempty(targz_filecheck)
    n = 1; m = 1;
    for ii = 1:length(targz_filecheck)
        targz_filename = targz_filecheck(ii).name;
        q = strfind(targz_filename,'logs.tar.gz');
        if isempty(q)
            targz_file{n} = fullfile(dirpath,targz_filename);
            n = n+1;
        else
            logs_file{m} = fullfile(dirpath,targz_filename);
            [~,name,ext] = fileparts(logs_file{m}); 
            logs_filename{m} = [name ext];
            m = m+1;
        end
    end
end

hdf5check = dir(fullfile(dirpath,'overview','*hdf5')); 
hdf5_file = [];
hdf5letter_file = [];
if ~isempty(hdf5check)
    n = 1; m = 1;
    for ii = 1:length(hdf5check)
        hdf5_filename = hdf5check(ii).name;
        if ~isletter(hdf5_filename(1))
            hdf5_file{n} = fullfile(dirpath,'overview',hdf5_filename);          % old hdf5-file starting with a number
            n = n+1;
        else
            hdf5letter_file{m} = fullfile(dirpath,'overview',hdf5_filename);    % old hdf5-file starting with a letter
            m = m+1;
        end
    end
end

display(targz_file')
% display(hdf5_file')
% display(hdf5letter_file')
% display(logs_file')

if isempty(targz_file) && isempty(hdf5_file) && isempty(hdf5letter_file)
    error('No "old" hdf5-file nor an tar.gz-file with mat-files exist.')
elseif isempty(targz_file) && isempty(hdf5_file)
    hdf5_file = hdf5letter_file;
end

pulses = {'arc','beata','bella','cp','CP','folke','hilde','ipy','manda','steffe','taro','tau','tyco'};
ants   = {'32m','42m','uhf','vhf','esa','esr','kir','sod','tro','lyr'};

figure_check = zeros(length(image_filelist),1);

for ii = 1:length(targz_file)
        
    untarpath = fullfile(tempdir, 'UntaredContent');
    if exist(untarpath)
        rmdir(untarpath,'s')
    end
    mkdir(untarpath);
        
    untar(targz_file{ii},untarpath)
    untar_filelist = dir(untarpath);
    
    while length(untar_filelist(3:end)) == 1 && ~contains(untar_filelist(3).name,'.mat')
        untarfolder = untar_filelist(3).name;
        untarpath = fullfile(untarpath,untarfolder);
        untar_filelist = dir(untarpath);
    end
        
    storepath = mat2hdf5(untarpath,datapath);    
    folders = regexp(storepath,filesep,'split');
    storefolder = char(folders(end));
    display(storepath)
    
    %%% copying figures to the new data folders
    if length(targz_file)==1
        for jj = 1:length(image_filelist)
            figurefile = fullfile(dirpath,image_filelist(jj).name);
            copyfile(figurefile,storepath)
        end
        figure_check(:) = 1;
    else
        bb = strfind(storefolder,'_');
        cc = strfind(storefolder,'@');
        pulse = storefolder(bb(2)+1:bb(3)-1);
        intper = storefolder(bb(3)+1:cc-1);
        ant = storefolder(cc+1:end);
        
        for jj = 1:length(image_filelist)
            figurefile = fullfile(dirpath,image_filelist(jj).name);
            [~,figname,ext] = fileparts(figurefile);
            if strcmp(ext,'.gz')
                [~,figname,~] = fileparts(figname);
            end
            
            fig_pulse  = '';
            fig_intper = '';
            fig_ant    = '';
            
            dd_  = strfind(figname,'_');
            ddat = strfind(figname,'@');
            dd = sort([dd_ ddat]);
            
            fig_intper_set = 0;
            for oo = 1:length(dd)
                if oo == length(dd)
                    figpart = figname(dd(oo)+1:end);
                else
                    figpart = figname(dd(oo)+1:dd(oo+1)-1);
                end
                
                if ~isempty(str2num(figpart)) && isempty(fig_intper)
                    fig_intper = figpart;
                    fig_intper_set = 1;
                end
                for pp = 1:length(pulses)
                    if contains(figpart,pulses(pp))
                        fig_pulse = figpart;
                    end
                end
                for qq = 1:length(ants)
                    if contains(figpart,ants(qq))
                        fig_ant = figpart;
                    end
                end
            end
            
            if fig_intper_set == 0 && ~isempty(intper)
                fig_intper = intper;
            end
            
            if contains(fig_ant,ant) && (str2num(fig_intper)==str2num(intper)) && contains(fig_pulse,pulse)
                copyfile(figurefile,storepath)
                figure_check(jj) = figure_check(jj)+1;
            elseif contains(figname,'plasmaline') && strcmp(intper,fig_intper) && contains(fig_pulse,pulse) 
                copyfile(figurefile,storepath)
                figure_check(jj) = figure_check(jj)+1;
            elseif contains(figname,'scan') && contains(fig_ant,ant) && contains(fig_pulse,pulse) 
                copyfile(figurefile,storepath) 
                figure_check(jj) = figure_check(jj)+1;
            end
        end
    end
        
        
    notesfile = fullfile(dirpath,'notes.txt');
    if exist(notesfile)
        copyfile(notesfile,storepath)
    end
    
    gupfilecheck =  dir(fullfile(untarpath,'.gup'));        
    if isempty(gupfilecheck) && ~isempty(hdf5_file)
        mat2hdf5_OldMeta(hdf5_file{1})  % OBS!! takes the first hdf5-file in list, takes the experiment data and put them in the hdf5-file just generated by mat2hdf5_instexpr_master.m
    end
        
    if b == 0
        vizu('new',untarpath,'HQ')
        vizu('save','24hrplt')
        newimages_list = [dir(fullfile(untarpath,'*.png'));dir(fullfile(untarpath,'*.eps'))];
        for jj = 1:length(newimages_list)
            newimage = fullfile(untarpath,newimages_list(jj).name);
            [~,~,ext] = fileparts(newimage);
            if strcmp(ext,'.eps')
                gzip(newimage,untarpath)
                newimage = [newimage '.gz'];
            end
            copyfile(newimage,storepath)
        end
    end
    copyfile(targz_file{ii},storepath)            
end

% Check if a figure was not copied and saved at all or copied and saved more than once
z1 = find(figure_check==0);
z2 = find(figure_check>1);
for z3 = 1:length(z1)
    display(['Warning: ' image_filelist(z1(z3)).name ' was not copied to a new data folder'])
end
for z3 = 1:length(z2)
    display(['Warning: ' image_filelist(z2(z3)).name ' was copied to more than one new data folder'])
end

for ii = 1:length(hdf5_file)
    
    OldHdf5File = hdf5_file{ii};
    storepath = EISCAThdf5_FromOldExpr(OldHdf5File,datapath);
    
    notesfile = fullfile(dirpath,'notes.txt');
    if exist(notesfile)
        copyfile(notesfile,storepath)
    end
    
    [~,filename] = fileparts(OldHdf5File);
    logsfilename = [filename(1:8) '-logs.tar.gz'];
    c = find(strcmp(logsfilename,logs_filename)==1);
    if c
        copyfile(logs_file{c},storepath)
    end
    
    
    if b == 0
        vizu('new',newhdf5file,'HQ')       % newhdf5file is generated in EISCAThdf5_fromOldExpr.m
        vizu('save','24hrplt')
        newimages_list = [dir(fullfile(datapath,'*.png'));dir(fullfile(datapath,'*.eps'))];
        for jj = 1:length(newimages_list)
            newimage = fullfile(datapath,newimages_list(jj).name);
            [~,~,ext] = fileparts(newimage);
            if strcmp(ext,'.eps')
                gzip(newimage,datapath)
                newimage = [newimage '.gz'];
            end
            copyfile(newimage,storepath)
        end
    end
    copyfile(OldHdf5File,storepath);
end  

if ~isempty(showfigs)
    for ii = 1:length(image_filelist)
        cmd = ['display ' fullfile(dirpath,image_filelist(ii).name) ' &'];
        system(cmd);
    end
end

if exist(fullfile(tempdir, 'UntaredContent'))
    rmdir(fullfile(tempdir, 'UntaredContent'),'s')
end
