function [ f2,fm3 ] = getCCIRF2( )
%GETCCIRF2 get CCIR data

  persistent f2ccir fm3ccir;
  if isempty(f2ccir)
    f2ccir = zeros(12,IRI2012.F2numI,IRI2012.F2numJ,2,IRI2012.float_t);
    fm3ccir = zeros(12,IRI2012.FM3numI,IRI2012.FM3numJ,2,IRI2012.float_t);
    for mn = 1:12
      FILNAM = sprintf('ccir%2d.asc', mn+10);
      IUCCIR = fopen(FILNAM,'r');
      if IUCCIR == -1
        fprintf(1,' The file %s is not in your directory.\n', FILNAM);
        %  icalls=  icalls+1;
        continue;
      end
      sc = textscan(IUCCIR,'%f');
      sc = sc{1};
      len = length(sc);
      fclose(IUCCIR);
      expectedLen = 2*IRI2012.F2numJ*IRI2012.F2numI;
      kk=1;
      for K=1:2
        for J=1:IRI2012.F2numJ
          for I=1:IRI2012.F2numI
            if kk > len
              fprintf(1,' The file %s F2 scan is short (%d/%d).\n', ...
                FILNAM,len,expectedLen);
              break;
            end
            f2ccir(mn,I,J,K) = sc(kk);
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
      for K=1:2
        for J=1:IRI2012.FM3numJ
          for I=1:IRI2012.FM3numI
            if kk > len
              fprintf(1,' The file %s FM3 scan is short (%d/%d).\n', ...
                FILNAM,len,expectedLen);
              break;
            end
            fm3ccir(mn,I,J,K) = sc(kk);
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
  f2 = f2ccir;
  fm3 = fm3ccir;
end
