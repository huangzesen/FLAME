;+
;Procedure:	flm_info_save 
;
;PURPOSE:	
; This procedure save infotable. By default, the sav file will be saved as:
;   savname = infopath + datestr + infoname + infover + '.sav'
; Use keyword NODATESTR to discard the datestr part
; Use keyword NOEXECUTION and VERBOSE, do nothing but shows existing sav files.
; For more information, plz check the code
;
;USAGE:	
; flm_info_save, infotable, [/noexe] 
;   [infopath = infopath], [infoname = infoname], [infover = infover], 
;   [verbose = verbose], [nodatestr = nodatestr] 
;
;INPUTS:	
; INFOTABLE: information structure array to be saved
;
;KEYWORDS:	
; INFOPATH, INFONAME, INFOVER: keyword to locate the IDL sav files.
; 
; VERBOSE: show more information
; 
; NOEXECUTION: do nothing but shows the existing sav files
; 
; NODATESTR: discard date string in the save path
; 
;
;CREATED BY: 	 huangzs on Mar 26, 2019
;UPDATES:	
;
;-

pro flm_info_save, infotable, $
  infopath = infopath, $
  infoname = infoname, $
  infover = infover, $
  verbose = verbose, $
  noexecution = noexecution, $
  nodatestr = nodatestr

compile_opt idl2

; retrieve paths and file names
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

; list existing files
if keyword_set(verbose) and keyword_set(noexecution) then begin
  files = file_search(infopath, '*.sav')
  dprint, verbose = verbose, 'Existing sav files:'
  for i1 = 0, n_elements(files)-1 do begin
    print, strcompress(string(i1)), files[i1]
  endfor
endif


; check file existence and save
datestr = (time_string(systime(/seconds))).substring(0,10)
if ~keyword_set(nodatestr) then $
  savname = infopath + datestr + infoname + infover + '.sav' else $
  savname = infopath + infoname + infover + '.sav'

if ~file_test(infopath, /directory) then file_mkdir, infopath+datestr

if file_test(savname) then begin
  dprint, verbose = verbose, 'file exists:', savname, 'Overwrite?'
  flagstr = ''
  read, 'Enter YES to overwrite: ', flagstr
  if strcmp(flagstr, 'YES') then begin
    dprint, verbose = verbose, 'Writing to ...', savname
    save, infotable, filename = savname
  endif
endif else begin
  dprint, verbose = verbose, 'Writting to ...', savname
  save, infotable, filename = savname
endelse


end