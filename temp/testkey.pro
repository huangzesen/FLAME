files = file_search('/home/hara/work/analysis/vex/flux_rope/shadow/','*.sav')
nid = []
;for k1 = 0, 100 do begin
  ;print,k1
  ID = []
  frpars1 = []
  for i1 = 0, n_elements(files)-1 do begin
    restore,files[i1]
    frpars = result[where(result.bipolar.status eq 1)]
    frpars1 = [frpars1, frpars]
    undefine,seed
    rarr = long(randomu(seed, 1000, /double)*90d + 10d )
    ;print,n_elements(rarr)
    rj1 = 0
    for j1 = 0, n_elements(frpars)-1 do begin
      frpar = frpars[j1]
      timestr = time_string(frpar.mtime,format=6)
      yyyy = long(timestr.substring(0,3))
      y = yyyy mod 10
      yy = yyyy mod 100
      ;print,y
      mm = long(timestr.substring(4,5))
      m = mm mod 10
      dd = long(timestr.substring(6,7))
      d = dd mod 10
      hh = long(timestr.substring(8,9))
      h = hh mod 10
      mm1 = long(timestr.substring(10,11))
      m1 = mm1 mod 10
      ss = long(timestr.substring(12,13))
      s = ss mod 10
;      ind = rarr[j1]
      ind = long(yy*1e4 + mm*1e2 + dd + rarr[rj1]*1e6)
      
      idx = where(ID eq ind)
      while idx[0] ne -1 do begin
        rj1 += 1
        ind = long(yy*1e4 + mm*1e2 + dd + rarr[rj1]*1e6)
        idx = where(ID eq ind)
      endwhile
      
      ID = [ID, ind]
    end
  end
  nid = [nid,n_elements(uniq(id,sort(id)))]

;endfor

end
