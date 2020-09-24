function [ rtn ] = getAP( i )
%GETAP retrieve latest apf107.dat
  persistent JYs JMNs JDs iiaps IAPDAs IRs F107Ds F107_81s F365s ...
    IYBEG IYEND IDEND IMEND s;
  if isempty(JYs)
    %s = 'ftp://spdf.gsfc.nasa.gov/pub/models/iri/iri2012/apf107.dat';
    %fmt = '%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%.1f%.1f%.1f';
    %[f,status] = urlread(s);
    %if status == 0 % bad Internet, try local file
    if true
      global path_GUP
      s = fullfile(path_GUP,'share','iri','apf107.dat');
      f = fopen(s,'r');
      if f == -1
        status = 0;
        fprintf(1,'WARNING %s not in directory\n',s);
      else
        status = 1;
      end
      sc = textscan(f,fmt);
      fclose(f);
    else
      sc = textscan(f,fmt);
    end
    if status == 1
      JYs = sc{1};
      JMNs = sc{2};
      JDs = sc{3};
      iiaps{1} = sc{4};
      iiaps{2} = sc{5};
      iiaps{3} = sc{6};
      iiaps{4} = sc{7};
      iiaps{5} = sc{8};
      iiaps{6} = sc{9};
      iiaps{7} = sc{10};
      iiaps{8} = sc{11};
      IAPDAs = sc{12};
      IRs = sc{13};
      F107Ds = sc{14};
      F107_81s = sc{15};
      F365s = sc{16};
      numdays = length(JYs);
      if numdays ~= length(JDs) || ...
         numdays ~= length(JMNs) || ...
         numdays ~= length(IAPDAs) || ...
         numdays ~= length(F107Ds) || ...
         numdays ~= length(F107_81s) || ...
         numdays ~= length(F365s)
        fprintf(1,'WARNING %s not scanned properly\n',s);
        return;
      end
      IYBEG=1958;
      IYEND=JYs(numdays)+1900;
      if IYEND < IYBEG
        IYEND = IYEND + 100;
      end
      IMEND=JMNs(numdays);
      IDEND=JDs(numdays);
    end
  end
  switch i
    case 'JY'
      rtn = JYs;
    case 'JMN'
      rtn = JMNs;
    case 'JD'
      rtn = JDs;
    case 'iiap'
      rtn = iiaps;
    case 'IAPDA'
      rtn = IAPDAs;
    case 'IR'
      rtn = IRs;
    case 'F107D'
      rtn = F107Ds;
    case 'F107_81'
      rtn = F107_81s;
    case 'F365'
      rtn = F365s;
    case 'BEGEND'
      rtn = { IYBEG IYEND IMEND IDEND };
    otherwise
      rtn = cell.empty;
  end
end


