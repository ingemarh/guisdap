
function EISCAT_hdf5(dirpath, datapath, showfigs)
% EISCAT_hdf5(dirpath, datapath, showfigs)
% The master script for creating EISCAT HDF5-files from old madrigal
% formated files and handling corresponding figures and additional files
% dirpath: path to the directory with old files 
% datapath: path to the folder where the EISCAT HDF5 file and additional
% files are stored
% Show figures?
% No:  showfigs = []
% Yes: showfigs = 1 (non-empty)

global path_GUP % name_ant

if nargin<3
    showfigs = []; 
end
if nargin<2
    error('Experiment and output path needed.')
end

if ~exist('path_GUP','var') || isempty(path_GUP)
   error('Guisdap needs to be initiated. Run start_GUP.')
end

if ~exist(dirpath)
    error(['The directory ' dirpath ' does not exist. Why not choose a directory that actually does exist?'])
end

if ~exist(datapath)
    mkdir(datapath);
end

% ugly hack: start_GUP clears all variables, so store and re-read!!
% save(fullfile(tempdir,'my_input.mat'),'dirpath','datapath','showfigs');
% start_GUP;
% load(fullfile(tempdir,'my_input.mat'));
% delete(fullfile(tempdir,'my_input.mat'));

image_filelist        = [dir(fullfile(dirpath,'*.png'));dir(fullfile(dirpath,'*.gif'));dir(fullfile(dirpath,'*.jpg'));dir(fullfile(dirpath,'*.jpeg'));dir(fullfile(dirpath,'*.pdf'));dir(fullfile(dirpath,'*ps.gz'));dir(fullfile(dirpath,'*ps'))];
notesfiles = dir(fullfile(dirpath,'notes*txt'));
logfiles = dir(fullfile(dirpath,'*log')); 

logs_files = [];
logs_filename = [];
targz_filecheck = [dir(fullfile(dirpath,'*tar.gz'));dir(fullfile(dirpath,'*tar'))];
targz_files = [];

if ~isempty(targz_filecheck)
    n = 1; m = 1;
    for ii = 1:length(targz_filecheck)
        targz_filename = targz_filecheck(ii).name;
        if contains(targz_filename,'logs.tar')
            logs_files{m} = fullfile(dirpath,targz_filename);
            [~,name,ext] = fileparts(logs_files{m}); 
            logs_filename{m} = [name ext];
            m = m + 1;
        else
            targz_files{n} = fullfile(dirpath,targz_filename); 
            n = n + 1;
        end
    end
end

hdf5oldfiles     = dir(fullfile(dirpath,'overview','*hdf5')); 
hdf5_allfiles  = [];
hdf5_files     = [];
%hdf5vel_files  = [];
hdf5ncar_files = [];
hdf5rest_files = [];

if ~isempty(hdf5oldfiles)
    l = 1; n = 1; m = 1; o = 1;
    for ii = 1:length(hdf5oldfiles)
        hdf5_filename = hdf5oldfiles(ii).name;
        if contains(hdf5_filename,'.ar') || contains(hdf5_filename,'.vr') || contains(hdf5_filename,'.tr') || contains(hdf5_filename,'.kr') || contains(hdf5_filename,'.sr')
            hdf5_files{l} = fullfile(dirpath,'overview',hdf5_filename); l = l+1;
            hdf5_allfiles{o} = fullfile(dirpath,'overview',hdf5_filename); o = o + 1;
        elseif contains(hdf5_filename,'NCAR_')
            hdf5ncar_files{m} = fullfile(dirpath,'overview',hdf5_filename); m = m+1;
            hdf5_allfiles{o} = fullfile(dirpath,'overview',hdf5_filename); o = o + 1;
        else
            hdf5rest_files{n} = fullfile(dirpath,'overview',hdf5_filename); n = n+1;
            hdf5_allfiles{o} = fullfile(dirpath,'overview',hdf5_filename); o = o + 1;
        end
    end
end

oldhdf5_files = [];
if isempty(targz_files) && isempty(hdf5oldfiles) 
    keyboard
    disp('No "old" hdf5-files nor any tar.gz-files with mat-files exist.')
    error('Noold" hdf5-files nor any tar.gz-files with mat-files exist.')
elseif isempty(targz_files) && ~isempty(hdf5oldfiles)
    oldhdf5_files = hdf5_allfiles;                     % consider all old hdf5 files when making new EISCAT hdf5 files
elseif ~isempty(targz_files)  && ~isempty(hdf5_files)
    oldhdf5_files = hdf5_files;                        % consider only the .ar. .vr, .tr, .kr, and .sr of the old hdf5 files when making new EISCAT hdf5 files
end

data_files = [targz_files,oldhdf5_files];
pulses = {'arc','beata','bella','cp','CP','folke','hilde','ipy','manda','steffe','taro','tau','tyco','gup0','gup3'};
ants   = {'32m','42m','uhf','vhf','esa','esr','eis','kir','sod','tro','lyr'};

antcheck = 0;
for df = 1:length(data_files)
    if contains(data_files{df},'ant')
        antcheck = 1;
    end
end

figure_check = zeros(length(image_filelist),1);
npdf = 0;

for ii = 1:length(data_files)
    disp(['Handling:' newline data_files{ii}])
    %pdf_forHDF5 = {};
    if contains(data_files{ii},'.tar')
        untarpath = fullfile(tempdir,'UntaredContent');
        if exist(untarpath)
            rmdir(untarpath,'s')
        end
        mkdir(untarpath);
        try
            untar(data_files{ii},untarpath)
        catch
            warning(['Ooops ... ' targz_file ' could not be untared, and is therefore ignored.'])
            continue
        end
        untar_filelist = dir(untarpath);
        untar_filelist = untar_filelist(~ismember({untar_filelist.name},{'.','..'}));   % ignore '.' and '..'

        %while length(untar_filelist(3:end)) == 1 && ~contains(untar_filelist(3).name,'.mat')
        while length(untar_filelist) == 1 && ~contains(untar_filelist(1).name,'.mat')
            untarfolder = untar_filelist(1).name;
            untarpath = fullfile(untarpath,untarfolder);
            untar_filelist = dir(untarpath);
            untar_filelist = untar_filelist(~ismember({untar_filelist.name},{'.','..','.gup','gfd_setup.m'}));   % ignore '.' and '..'
        end
       
        load(fullfile(untar_filelist(1).folder,untar_filelist(1).name))
        if exist('Vg')
            matvecfile = fullfile(untar_filelist(1).folder,untar_filelist(1).name);
            [storepath,EISCAThdf5file] = matvecvel2hdf5(matvecfile,datapath);
            vecvel = 1;
        else
            [storepath,EISCAThdf5file] = mat2hdf5(untarpath,datapath); 
            vecvel = 0;
        end    
    else
        if contains(data_files{ii},'.ar')
            [storepath,EISCAThdf5file] = cedarvel2hdf5(data_files{ii},datapath);
            vecvel = 1;
        else
            [storepath,EISCAThdf5file] = cedar2hdf5(data_files{ii},datapath);
            vecvel = 0;
        end
    end
    
    nfiles = length(EISCAThdf5file);

    isempt = [];
    for nf = 1:nfiles
        if isempty(EISCAThdf5file{nf})
            isempt = [isempt nf];
        end
    end
    EISCAThdf5file(isempt) = [];
    storepath(isempt) = [];
    nfiles = length(EISCAThdf5file);

    for nnn = 1:nfiles    
        folders = regexp(storepath{nnn},filesep,'split');
        storefolder = char(folders(end));
        display(EISCAThdf5file{nnn})

        %%% copying figures to the new data folders

        bb = strfind(storefolder,'_');
        cc = strfind(storefolder,'@');
        bbcc = sort([bb cc]);
        if length(bb) == 4
            pulse = storefolder(bb(2)+1:bb(4)-1);
            intper = storefolder(bb(4)+1:cc-1);
        else
            pulse = storefolder(bb(2)+1:bb(3)-1);
            intper = storefolder(bb(3)+1:cc-1);
        end
        ant = storefolder(cc+1:end);

        nfigs_expr = 0;

        for jj = 1:length(image_filelist)
            figurefile = fullfile(dirpath,image_filelist(jj).name);
            [~,figname,ext] = fileparts(figurefile);
            if strcmp(ext,'.gz')
                [~,figname,~] = fileparts(figname);
            end

            if length(data_files) > 1
                fig_pulse  = '';
                fig_intper = '';
                fig_ant    = '';

                dd_  = strfind(figname,'_');
                ddat = strfind(figname,'@');
                dd = sort([dd_ ddat]);

                fig_intper_set = 0;
                c = 0;
                for oo = 1:length(dd)

                    if oo == length(dd)
                        figpart = figname(dd(oo)+1:end);
                    elseif length(bbcc) == 5 && oo == 1
                        figpart = figname(dd(oo)+1:dd(oo+2)-1);
                        if strcmp(pulse,figpart)
                            c = 1;
                        end
                    elseif length(bbcc) == 5 && oo == 2 && c == 1
                        continue
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

                if (strcmp(fig_intper,intper) && contains(fig_ant,ant) && strcmp(fig_pulse,pulse)) && c == 1 || ...
                   (strcmp(fig_intper,intper) && contains(fig_ant,ant) && contains(fig_pulse,pulse)) || ...   
                   (isempty(fig_intper) && contains(fig_ant,ant) && contains(fig_pulse,pulse)) || ... 
                   (contains(figname,'plasmaline') && strcmp(intper,fig_intper) && contains(fig_pulse,pulse)) || ...
                   (contains(figname,'scan') && contains(fig_ant,ant) && contains(fig_pulse,pulse)) || ...
                   ((contains(figname,'BoreSight') || contains(figname,'WestBeam')) && contains(fig_pulse,pulse))
                    if ~strcmp(ext,'.gz') && ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')  
                        if strcmp(ext,'.pdf')
                            npdf = npdf + 1;
                            copyfile(figurefile,storepath{nnn})
                            [~,pdfname,ext] = fileparts(figurefile);
                            pdf_forHDF5(npdf) = {[pdfname ext]};
                        else    
                            store_image2Hdf5(figurefile,EISCAThdf5file{nnn})
                        end
                        figure_check(jj) = figure_check(jj) + 1;
                        nfigs_expr =  nfigs_expr +1;
                    else
                        if strcmp(ext,'.gz')
                            epsfile = gunzip(figurefile);
                            figurefile = epsfile{1};
                        end
                        pdffile = eps2pdf(figurefile);
                        npdf = npdf + 1;
                        copyfile(pdffile,storepath{nnn})
                        [~,pdfname,ext] = fileparts(pdffile);
                        pdf_forHDF5(npdf) = {[pdfname ext]};
                        delete(pdffile,figurefile)
                        figure_check(jj) = figure_check(jj) + 1;
                        nfigs_expr =  nfigs_expr +1;
                    end
                end
            elseif length(data_files) == 1 
                if ~strcmp(ext,'.gz') && ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')  
                    if strcmp(ext,'.pdf')
                        npdf = npdf + 1;
                        copyfile(figurefile,storepath{nnn})
                        [~,pdfname,ext] = fileparts(figurefile);
                        pdf_forHDF5(npdf) = {[pdfname ext]};
                    else
                        store_image2Hdf5(figurefile,EISCAThdf5file{nnn})
                    end
                    figure_check(jj) = figure_check(jj) + 1;
                    nfigs_expr =  nfigs_expr +1;
                else
                    if strcmp(ext,'.gz')
                        epsfile = gunzip(figurefile);
                        figurefile = epsfile{1};
                    end
                    pdffile = eps2pdf(figurefile);
                    npdf = npdf + 1;
                    copyfile(pdffile,storepath{nnn})
                    [~,pdfname,ext] = fileparts(pdffile);
                    pdf_forHDF5(npdf) = {[pdfname ext]};
                    delete(pdffile,figurefile)
                    figure_check(jj) = figure_check(jj) + 1;
                    nfigs_expr =  nfigs_expr +1;
                end
            end
        end 

        if npdf
            strds2hdf5(EISCAThdf5file{nnn},'/figures','figure_links',pdf_forHDF5')
            npdf = 0; clear pdf_forHDF5
        end

        for nn = 1:length(notesfiles)
            notesfile = fullfile(dirpath,notesfiles(nn).name);
            addNote2Hdf5(notesfile,EISCAThdf5file{nnn},nn)
        end

        for ll = 1:length(logs_filename)
            [~,data_filename,~] = fileparts(data_files{ii});
            if strcmp(data_filename(1:8),logs_filename{ll}(1:8))
                copyfile(logs_files{ll},storepath{nnn})
                strds2hdf5(EISCAThdf5file{nnn},'/metadata','logs_links',{logs_filename{ll}})
            end
        end

        for nn = 1:length(logfiles)
            logfile = fullfile(dirpath,logfiles(nn).name);
            copyfile(logfile,storepath{nnn})
        end

        % Check if metadata exist
        info = h5info(EISCAThdf5file{nnn},'/metadata/software');
        if ~isempty(info.Groups)
            metagroups = {info.Groups.Name}';
        else
            metagroups = {''}';
        end
        hdf5fileformeta = [];

        if contains(data_files{ii},'.tar') && isempty(find(strcmp(metagroups,'/metadata/software/gfd'),1))
            [~,tarfilename1,~] = fileparts(data_files{ii});
            [~,tarfilename,~]  = fileparts(tarfilename1);
            if ~isempty(hdf5ncar_files)
                gg = find(contains(hdf5ncar_files,tarfilename));
                if ~isempty(gg)
                    hdf5fileformeta = hdf5ncar_files{gg};
                end
            end
            if isempty(hdf5fileformeta) && length(hdf5rest_files) == 1
                hdf5fileformeta = hdf5rest_files{1};
            end

            if hdf5fileformeta
                metacompl(hdf5fileformeta,EISCAThdf5file{nnn})  
                disp(['gfd was missing ... Taking metadata from ' hdf5fileformeta '.'])
            elseif isempty(hdf5fileformeta) && length(hdf5rest_files) > 1
                warning(['gfd was missing ... several (>1) older HDF5-files (not NCAR) were found. The correct complementing metadata need to be manually extracted.'])
            else
                warning(['gfd was missing ... but no complementing metadata was found.'])
            end
        else
        end
        
        vizugo = [];
        if nfigs_expr == 0
            if contains(data_files{ii},'.tar.gz') && vecvel == 0
                input = untarpath;
                vizugo = 1;
            elseif vecvel == 1
                if isempty(image_filelist) %%% Make a (or two) new plot(s) from efield!
                    
                    hdf5file_info = h5info(EISCAThdf5file{nnn},'/metadata/');
                    meta_datasets = {hdf5file_info.Datasets.Name}';
                    if ~isempty(find(contains(meta_datasets,'par0d')==1))
                        metadata0d = deblank(h5read(EISCAThdf5file{nnn},'/metadata/par0d'));
                        data0d     = double(h5read(EISCAThdf5file{nnn},'/data/par0d'));     
                        nr0        = find(strcmp(metadata0d(1,:),'nrec')==1);                 end
                    if ~isempty(find(contains(meta_datasets,'par1d')==1))
                        metadata1d = deblank(h5read(EISCAThdf5file{nnn},'/metadata/par1d')); 
                        data1d     = double(h5read(EISCAThdf5file{nnn},'/data/par1d'));
                        nr1         = find(strcmp(metadata1d(1,:),'nrec')==1);                end    
                    if ~isempty(find(contains(meta_datasets,'par2d')==1))
                        metadata2d = deblank(h5read(EISCAThdf5file{nnn},'/metadata/par2d'));
                        data2d     = double(h5read(EISCAThdf5file{nnn},'/data/par2d'));       end    
                    if ~isempty(find(contains(meta_datasets,'names')==1))
                        names       = deblank(h5read(EISCAThdf5file{nnn},'/metadata/names')); end    
                    utime = h5read(EISCAThdf5file{nnn},'/data/utime');
                    
                    if exist('nr1','var') && ~isempty(nr1)
                        lrecs = data1d(:,nr1);
                    elseif exist('nr0','var') && ~isempty(nr0)
                        lrec  = data0d(nr0);
                        lrecs = lrec*ones(length(utime(:,1)),1);
                    end
                    
                    utime_tmp = [];
                    for nnr = 1:length(lrecs)
                        utime_tmp = [utime_tmp; repmat(utime(nnr,:),lrecs(nnr),1)]; 
                    end
                    utime = utime_tmp;
                    
                    v   = {'vi_east' 'vi_north' 'vi_up'};
                    dv  = {'dvi_east' 'dvi_north' 'dvi_up'};
                    var = {'vi_crossvar_12' 'vi_crossvar_23' 'vi_crossvar_13'};
                    pos = {'lat' 'lon' 'h'};
                    Vg = []; Dv = []; Var = []; Vpos = []; Vgv = [];
                    
                    for vv = 1:3
                        if (exist('nr1','var') && ~isempty(nr1)) || (exist('nr0','var') &&  ~isempty(nr0) && data0d(nr0)>1)   % data in 2d
                            aa = find(strcmp(metadata2d(1,:),v{vv})==1);
                            bb = find(strcmp(metadata2d(1,:),dv{vv})==1);
                            cc = find(strcmp(metadata2d(1,:),var{vv})==1);
                            dd = find(strcmp(metadata2d(1,:),pos{vv})==1);
                            if isempty(dd)
                                dd = find(strcmp(metadata1d(1,:),pos{vv})==1);
                                if isempty(dd)
                                    dd = find(strcmp(metadata0d(1,:),pos{vv})==1);
                                    Vpos = [Vpos data0d(dd)*ones(length(utime(:,1)),1)];
                                else
                                    Vpos  = [Vpos data1d(:,dd)]; 
                                end
                            else
                                Vpos  = [Vpos data2d(:,dd)]; 
                            end
                            Vg  = [Vg data2d(:,aa)];
                            Dv  = [Dv data2d(:,bb)];
                            Var = [Var data2d(:,cc)];
                        else                                                        % data in 1d
                            aa = find(strcmp(metadata1d(1,:),v{vv})==1);
                            bb = find(strcmp(metadata1d(1,:),dv{vv})==1);
                            cc = find(strcmp(metadata1d(1,:),var{vv})==1);
                            dd = find(strcmp(metadata1d(1,:),pos{vv})==1);
                            if isempty(dd)
                                dd = find(strcmp(metadata0d(1,:),pos{vv})==1);
                                Vpos = [Vpos data0d(dd)*ones(length(utime(:,1)),1)];
                            else
                                Vpos  = [Vpos data1d(:,dd)]; 
                            end
                            Vg  = [Vg data1d(:,aa)];
                            Dv  = [Dv data1d(:,bb)];
                            Var = [Var data1d(:,cc)];
                        end
                    end
                    
                    Vpos(:,3) = Vpos(:,3)/1e3;    % m --> km 
                    Vgv   = [Dv.^2 Var];        
                    Vdate = [timeconv(utime(:,1),'unx2mat')';timeconv(utime(:,2),'unx2mat')'];
                    ee= find(strcmp(names(1,:),'name_expr')==1);
                    ff = find(strcmp(names(1,:),'name_ant')==1);
                    name_expr = names{2,ee};
                    name_ant  = names{2,ff};
                    [~,file,~] = fileparts(EISCAThdf5file{nnn});
                    plotfilename = file(8:end);
                    vfile = [storepath{nnn} '/' plotfilename];
                    save([vfile '.mat'],'Vdate','Vpos','Vg','Vgv','name_expr','name_ant');

                    if contains(name_expr,'cp1')
                        plotfilename_orig = plotfilename;
                        f = strfind(plotfilename_orig,'_V');
                        plottype = 'tVm3';
                        altlim = [90 160 500];
                        region = ['E';'F'];
                        for pp = 1:2
                             hh = find(Vpos(:,3)>=altlim(pp) & Vpos(:,3)<altlim(pp+1));
                             if hh
                                plotfilename = [plotfilename_orig(1:f-1) '_' region(pp) plotfilename_orig(f:end)];
                                plotfile = [storepath{nnn} '/' plotfilename];
                                efield([vfile '.mat'],plottype,[altlim(pp) altlim(pp+1)])
                                print('-dpdf',[plotfile '.pdf'])
                                npdf = npdf + 1;
                                pdf_forHDF5(npdf) = {[plotfilename '.pdf']};
                                print('-dpng256',[plotfile '.png'])
                                store_image2Hdf5([plotfile '.png'],EISCAThdf5file{nnn});
                                insert_exif(gcf,plotfile,{'pdf' 'png'})
                                delete([plotfile '.png'])
                                clf
                            end
                        end
                    else
                        if contains(name_expr,'cp2')
                            plottype = 'tVm3';
                        else
                            plottype = 'pVm';
                        end
                        efield([vfile '.mat'],plottype);
                        print('-dpdf',[vfile '.pdf'])
                        npdf = npdf + 1;
                        pdf_forHDF5(npdf) = {[plotfilename '.pdf']};
                        print('-dpng256',[vfile '.png'])
                        store_image2Hdf5([vfile '.png'],EISCAThdf5file{nnn});
                        insert_exif(gcf,vfile,{'pdf' 'png'})
                        delete([vfile '.png'])
                        clf
                    end
                    if npdf
                        strds2hdf5(EISCAThdf5file{nnn},'/figures','figure_links',pdf_forHDF5')
                        npdf = 0; clear pdf_forHDF5
                    end
                    delete([vfile '.mat'])
                end
            else
                input = EISCAThdf5file{nnn};
                metadata0d = h5read(EISCAThdf5file{nnn},'/metadata/par0d');
                aa = find(strcmp(deblank(metadata0d(1,:)),'nrec'));
                if aa
                    data0d = h5read(EISCAThdf5file{nnn},'/data/par0d');
                    nrec = data0d(aa);
                    if nrec == 1 || nrec > 5
                        vizugo = 1;
                    else
                        warning(['No figures, and nrec = ' num2str(nrec) ' so no new figures generated.'])
                    end
                else
                    vizugo = 1;
                end
            end

            if vizugo
                vizu('new',input,'HQ')
                file = vizu('save','24hrplt');
                newpng = [file '.png'];
                newpdf = [file '.pdf'];
                store_image2Hdf5(newpng,EISCAThdf5file{nnn});
                copyfile(newpdf,storepath{nnn})
                [~,pdfname,ext]=fileparts(newpdf);
                strds2hdf5(EISCAThdf5file{nnn},'/figures','figure_links',{[pdfname ext]})
            end
        end  
        copyfile(data_files{ii},storepath{nnn})
    end
end

figure_check;
% Check if a figure was not copied and saved at all, or copied and saved more than once
z1 = find(figure_check==0);
z2 = find(figure_check>1);
for z3 = 1:length(z1)
    warning([image_filelist(z1(z3)).name ' was not copied or stored'])
end
for z3 = 1:length(z2)
    warning([image_filelist(z2(z3)).name ' was copied or stored more than once'])
end


if ~isempty(showfigs)
    for ii = 1:length(image_filelist)
        cmd = ['display ' fullfile(dirpath,image_filelist(ii).name) ' &'];
        system(cmd);
    end
end

if exist(fullfile(tempdir,'UntaredContent'))
    rmdir(fullfile(tempdir,'UntaredContent'),'s')
end
