;+
;Procedure:	flm_info_init
;
;PURPOSE:	
; This procedure initiate the infotable of flux rope, each info structure
; contains:
;   FRPAR: flux rope parameter from HARA
;   KEY: flux rope ID
; The resultant variable will be named 'INFOTABLE' and will be saved to:
;   infopath + datestr + infoname + infover +'.sav'
;
;USAGE:	
;
;INPUTS:	
;
;KEYWORDS:	
; 
; FRPARPATH, INFOPATH, INFOVER, INFONAME : return various paths and file names.
; 
; NOEXECUTION: do nothing and return
; 
;CREATED BY: 	 huangzs on Mar 25, 2019
;UPDATES:	
;
;-

pro flm_info_init, $
  frparpath = frparpath, $
  infopath = infopath, $
  infover = infover, $
  infoname = infoname, $
  noexecution = noexecution

compile_opt idl2

frparpath = '/home/hara/work/analysis/vex/flux_rope/shadow/'
infopath = '/home/huangzs/work/thesis/analysis/vexinfo/infotable/'
infover = 'v0.0'
infoname = 'vex_info_'

if keyword_set(noexecution) then return

; find the sav files
files = file_search(frparpath,'*.sav')

; create infotable
infotable = []
for i1 = 0, n_elements(files)-1 do begin
  restore, files[i1]
  frpars = result[where(result.bipolar.status eq 1)]
  
  for j1 = 0, n_elements(frpars)-1 do begin
    
  ; info structure
  key = flm_info_keygen(frpars[j1], infotable)
  info = create_struct('key', key, 'frpar', frpars[j1])
  infotable = [infotable,info]
    
  endfor
endfor

timestr = time_string(systime(/seconds))
datestr = timestr.substring(0,10)
if (file_test(infopath+datestr, /directory) ne 1) then file_mkdir, infopath+datestr
save, infotable, filename = infopath + datestr + infoname + infover + '.sav'

end