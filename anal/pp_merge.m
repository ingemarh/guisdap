% Merge power profiles with similar resolution r_w

function [r_pp_merged,r_pperr_merged,r_ppw_merged,r_pprange_merged,r_pprofile_id] = pp_merge(r_pp,r_pperr,r_ppw,r_pprange)

% figure(1)
% subplot(3,2,1)
% plot(r_pprange,'.')
% ylabel('r\_pprange')
% title('Original profiles')
% subplot(3,2,3)
% plot(r_ppw,'.')
% ylabel('r\_ppw')


rres   = max(1,min(r_ppw)); %range resolution given by p_dtau or smallest volume
r_ww   = unique(round(r_ppw/rres));

if isempty(diff(r_ww))    % if there is one(1) 
    rres = 1;
    r_dw = 1;
else
    r_dw   = unique(diff(r_ww))+1;
end

rres_x = round(r_ppw/rres/r_dw(1));  
ranges = round(r_pprange/rres);%+1e-3*rres_x;
  
r_pp_merged      = [];
r_pperr_merged   = [];
r_ppw_merged     = [];
r_pprange_merged = [];
r_pprofile_id     = [];

n_profiles = length(unique(rres_x));                 % number of profiles after merging
merge_pprofiles = sort(unique(rres_x),'descend');    % sort from high to low 'r_ppw widths'

id = 0;

for kk = 1:n_profiles

    ii = find(rres_x==merge_pprofiles(kk));
    r_pp_tmp      = r_pp(ii);
    r_pperr_tmp   = r_pperr(ii);
    r_ppw_tmp     = r_ppw(ii);
    r_pprange_tmp = r_pprange(ii);
    ranges_tmp    = ranges(ii);
    
    
    
    a3 = [];
    
    for range=unique(ranges_tmp)'    % must be a row vector    
    
        aa = find(ranges_tmp==range);
    
        if length(aa)>1
       
        w2=1./r_pperr_tmp(aa).^2;
       
        r_pp_tmp(aa(1))      = sum(r_pp_tmp(aa).*w2)/sum(w2);
        r_pperr_tmp(aa(1))   = 1/sqrt(sum(w2));
        r_ppw_tmp(aa(1))     = sum(r_ppw_tmp(aa).*w2)/sum(w2);
        r_pprange_tmp(aa(1)) = sum(r_pprange_tmp(aa).*w2)/sum(w2);
       
        a3 = [a3 aa(2:end)'];
       
        end
    end
    
    r_pp_tmp(a3)      = [];
    r_pperr_tmp(a3)   = [];
    r_pprange_tmp(a3) = [];
    r_ppw_tmp(a3)     = []; 
    
    [r_pprange_sorted,jj] = sort(r_pprange_tmp);
    r_pp_sorted           = r_pp_tmp(jj);  
    r_pperr_sorted        = r_pperr_tmp(jj);  
    r_ppw_sorted          = r_ppw_tmp(jj);  
    
    % Also separate if there is a big jump in the range:
    
    diffrange_tmp = diff(r_pprange_sorted);
    mean_diff     = mean(diffrange_tmp);
    aa = find(abs(diffrange_tmp)>10*mean_diff);                 % find sudden big steps in the range 
    
    if isempty(aa)
        id = id + 1;
        r_pprofile_id    = [r_pprofile_id id*ones(1,length(r_pp_sorted))];
    else
        for gg = 1:(length(aa)+1)
            bb = [0 aa' length(r_pp_sorted)];
            id = id + 1;
            r_pprofile_id    = [r_pprofile_id id*ones(1,bb(gg+1)-bb(gg))];
        end
    end
   
        r_pprange_merged = [r_pprange_merged r_pprange_sorted'];
        r_pp_merged      = [r_pp_merged r_pp_sorted'];
        r_pperr_merged   = [r_pperr_merged r_pperr_sorted'];
        r_ppw_merged     = [r_ppw_merged r_ppw_sorted(jj)']; 
  
    
    
end

r_pp_merged      = r_pp_merged';
r_pperr_merged   = r_pperr_merged';
r_pprange_merged = r_pprange_merged';
r_ppw_merged     = r_ppw_merged'; 
r_pprofile_id     = r_pprofile_id'; 
 
% figure(1)
% subplot(3,2,2)
% plot(r_pprange_merged,'.b')
% title('Merged profiles')
% ylabel('merged r\_pprange')
% subplot(3,2,4)
% plot(r_ppw_merged,'.b')
% ylabel('merged r\_ppw')
% subplot(3,2,6)
% plot(r_pprofile_id,'.b')
% ylabel('power profile id')
% ylim([0 id+1])