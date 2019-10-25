

function EISCAT_hdf5(dirpath, datapath, showfigs)

%global newhdf5file

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

image_filelist = [dir(fullfile(dirpath,'*.png'));dir(fullfile(dirpath,'*.gif'));dir(fullfile(dirpath,'*.jpg'));dir(fullfile(dirpath,'*.jpeg'));dir(fullfile(dirpath,'*ps.gz'));;dir(fullfile(dirpath,'*ps'))];
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

hdf5oldfiles     = dir(fullfile(dirpath,'overview','*hdf5')); 
hdf5_allfiles  = [];
hdf5_files     = [];
hdf5ncar_files = [];
hdf5rest_files = [];

if ~isempty(hdf5oldfiles)
    l = 1; n = 1; m = 1;
    for ii = 1:length(hdf5oldfiles)
        hdf5_filename = hdf5oldfiles(ii).name;
        hdf5_allfiles{ii} = fullfile(dirpath,'overview',hdf5_filename);
        if contains(hdf5_filename,'.vr') || contains(hdf5_filename,'.tr') || contains(hdf5_filename,'.kr') || contains(hdf5_filename,'.sr') 
            hdf5_files{l} = fullfile(dirpath,'overview',hdf5_filename);
            l = l+1;
        elseif contains(hdf5_filename,'NCAR')
            hdf5ncar_files{m} = fullfile(dirpath,'overview',hdf5_filename);
            m = m+1;
        else
            hdf5rest_files{n} = fullfile(dirpath,'overview',hdf5_filename);
            n = n+1;
        end
    end
end


display(targz_file')
display(hdf5_allfiles')
% display(hdf5_files')
% display(hdf5ncar_files')
% display(hdf5rest_files')
% display(logs_file')

oldhdf5_files = [];
if isempty(targz_file) && isempty(hdf5oldfiles) 
    error('No "old" hdf5-file nor an tar.gz-file with mat-files exist.')
elseif isempty(targz_file) && ~isempty(hdf5oldfiles)
    oldhdf5_files = hdf5_allfiles;                     % consider all old hdf5 files when making new EISCAT hdf5 files
elseif ~isempty(targz_file) && ~isempty(hdf5_files)
    oldhdf5_files = hdf5_files;                        % consider only the .vr, .tr, .kr, and .sr of the old hdf5 files when making new EISCAT hdf5 files
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
        
    [storepath,EISCAThdf5file] = mat2hdf5(untarpath,datapath);    
    folders = regexp(storepath,filesep,'split');
    storefolder = char(folders(end));
    display(EISCAThdf5file)
    
    %%% copying figures to the new data folders
    if length(targz_file)==1
        for jj = 1:length(image_filelist)
            figurefile = fullfile(dirpath,image_filelist(jj).name);
            copyfile(figurefile,storepath)
            [~,figname,ext] = fileparts(figurefile);
            if ~strcmp(ext,'.gz')
                store_image2Hdf5(figurefile,EISCAThdf5file)
            end
        end
        figure_check(:) = 1;
    else
        bb = strfind(storefolder,'_');
        cc = strfind(storefolder,'@');
        pulse = storefolder(bb(2)+1:bb(3)-1);
        intper = storefolder(bb(3)+1:cc-1);
        ant = storefolder(cc+1:end);
        
        for jj = 1:length(image_filelist)
            a = 0;
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
                figure_check(jj) = figure_check(jj)+1; a = 1;
            elseif contains(figname,'plasmaline') && strcmp(intper,fig_intper) && contains(fig_pulse,pulse) 
                copyfile(figurefile,storepath)
                figure_check(jj) = figure_check(jj)+1; a = 1;
            elseif contains(figname,'scan') && contains(fig_ant,ant) && contains(fig_pulse,pulse) 
                copyfile(figurefile,storepath) 
                figure_check(jj) = figure_check(jj)+1; a = 1;
            end
            if ~strcmp(ext,'.gz') && a == 1
                store_image2Hdf5(figurefile,EISCAThdf5file)
            end
        end
    end
        
        
    notesfile = fullfile(dirpath,'notes.txt');
    if exist(notesfile)
        copyfile(notesfile,storepath)
    end
    
    % Check if 
    info = h5info(EISCAThdf5file,'/metadata');
    metavar = {info.Datasets.Name}';
    hdf5fileformeta = [];
    if isempty(find(strcmp(metavar,'gfd')))
        [~,tarfilename1,~] = fileparts(targz_file{ii});
        [~,tarfilename,~]  = fileparts(tarfilename1);
        if ~isempty(hdf5ncar_files)
            ff = contains(hdf5ncar_files,tarfilename);
            gg = find(ff == 1);
            if ~isempty(gg)
                hdf5fileformeta = hdf5ncar_files{gg};
            end
        %elseif     
        end
        display(hdf5fileformeta)
        metacompl(hdf5fileformeta,EISCAThdf5file)  
    else
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

% Check if a figure was not copied and saved at all, or copied and saved more than once
z1 = find(figure_check==0);
z2 = find(figure_check>1);
for z3 = 1:length(z1)
    display(['Warning: ' image_filelist(z1(z3)).name ' was not copied to a new data folder'])
end
for z3 = 1:length(z2)
    display(['Warning: ' image_filelist(z2(z3)).name ' was copied to more than one new data folder'])
end



for ii = 1:length(oldhdf5_files)
    
    OldHdf5File = oldhdf5_files{ii};
    [storepath,EISCAThdf5file] = cedar2hdf5(OldHdf5File,datapath);
    display(EISCAThdf5file)
    
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
%        vizu('new',newhdf5file,'HQ')       % newhdf5file is generated in cedar2hdf5.m
        vizu('new',EISCAThdf5file,'HQ')       % EISCAThdf5file is generated in cedar2hdf5.m
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
