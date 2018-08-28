function [ f2 ] = getURSI( )
%GETURSI retrieve URSI data

  persistent f2ursi;
  if isempty(f2ursi)
    f2ursi = zeros(12,IRI2012.F2numI,IRI2012.F2numJ,2,IRI2012.float_t);
    for mn = 1:12
      FILNAM = sprintf('ursi%2d.asc', mn+10);
      IUCCIR = fopen(FILNAM,'r');
      if IUCCIR == -1
        fprintf(1,' The file %s is not in your directory.\n', FILNAM);
        continue;
      end
      sc = textscan(IUCCIR,'%f');
      sc = sc{1};
      len = length(sc);
      fclose(IUCCIR);
      kk=1;
      for K=1:2
        for J=1:IRI2012.F2numJ
          for I=1:IRI2012.F2numI
            if kk > len
              fprintf(1,' The file %s F2 scan is short (%d/%d).\n', ...
                FILNAM,len,2*IRI2012.F2numJ*IRI2012.F2numI);
              break;
            end
            f2ursi(mn,I,J,K) = sc(kk);
            kk = kk + 1;
          end
          if kk > len
            break;
          end
        end
        if kk > len
          break;
        end
      end
    end
  end
  f2 = f2ursi;
end


