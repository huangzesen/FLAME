;+
;Procedure:	flm_info_restore
;
;PURPOSE:	
; This procedure restore infotable from disk.
; By default the procedure will retrieve infopath, infoname, infover from
; flm_info_init, if they were not specified by the user. 
;
;USAGE:	
; flm_info_restore, infotable, $ 
;   [infopath = infopath], [infoname = infoname], [infover = infover]
;
;INPUTS:	
; INFOTABLE: the result will be passed back through this argument.
;
;KEYWORDS:	
; 
; INFOPATH, INFONAME, INFOVER: keyword to locate the IDL sav files.
; 
; VERBOSE: show more information
;
;CREATED BY: 	 huangzs on Mar 26, 2019
;UPDATES:	
;
;-

pro flm_info_restore, infotable, $
  infopath = infopath, $
  infoname = infoname, $
  infover = infover, $
  verbose = verbose


compile_opt idl2

; restore paths and file names
flm_info_init, /noexe, infopath = infopath0, infoname = infoname0, infover = infover0
if ~keyword_set(infopath) then infopath = infopath0
if ~keyword_set(infoname) then infoname = infoname0
if ~keyword_set(infover) then infover = infover0
if keyword_set(verbose) then begin
  print, 'infopath: ', infopath
  print, 'infoname: ', infoname
  print, 'infover: ', infover
  print, 'savname: ', infoname+infover+'.sav'
endif

undefine, infotable

; search sav files
files = file_search(infopath,'*.sav')
idx = strmatch(files,infoname+infover)
if idx[0] eq -1 then begin
  dprint, 'No such sav files.'
  return
endif else if n_elements(idx) ne 1 then begin
  dprint, 'Mutiple match: '
  for i1 = 0, n_elements(idx) do $
    print, strcompress(string(idx)), files[idx[i1]]
  read,'Select: ', ind
  restore, files[ind]
endif else begin
  restore,files[idx[0]]
endelse


end