global iHandle
fprintf('*************************************************************\n')
fprintf(' Data will be integrated by Nigel Wade''s integration front end\n')
fprintf('*************************************************************\n')
if ~exist('source')
  global source strategy
end
if isempty(source)
  global source strategy
  t1=sprintf('start %d/%d/%d %d:%d:%d;',analysis_start([3 2 1 4:6]));
  t2=sprintf('end %d/%d/%d %d:%d:%d;',analysis_end([3 2 1 4:6]));
  if a_realtime
    rr=' realtime;';
  else
    rr=[];
  end
  if strfind(data_path,'@')
    if ~isempty(recurse)
      rr=['recurse ' recurse ';' rr];
    end
    source=sprintf('gupmat {%s %s directory %s; %s}',t1,t2,data_path,rr);
  else
    source=sprintf('EISCAT dtst {%s %s directory %s; site %s; %s}',t1,t2,data_path,name_site,rr);
  end
  strategy=sprintf('filter time {%s %s} filter power {min %g kW;}',t1,t2,a_txlim/1000);
  if length(a_integr)==1
    if a_integr
      strategy=[strategy sprintf(' integrate time {period %d; skip %d}',a_integr,a_skip)];
    else
      strategy=[strategy ' integrate move {}'];
    end
  elseif isempty(strategy)
    error('No default setup for scan cycle exists yet: Define a strategy!')
  end
  clear rr t1 t2
end
[status,iHandle] = PI_init(source,strategy,'');
