;+
;Procedure:	flm_info_restore
;
;PURPOSE:	
; This procedure restore infotable from disk.
; By default the procedure will retrieve infopath, infoname, infover from
; flm_info_init, if they were not specified by the user. 
;
;USAGE:	
; flm_info_restore, infotable, [/noexe], [/csv], $ 
;   [infopath = infopath], [infoname = infoname], [infover = infover], $
;   [configcsv = configcsv]
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
; NOEXECUTION: do nothing
; 
; CSV: restore csv files
; 
; CONFIGCSV: CSV file configuration, plz check the code for detailed options
;
;CREATED BY: 	 huangzs on Mar 26, 2019
;UPDATES:	
;
;-

pro flm_info_restore, infotable, $
  infopath = infopath, $
  infoname = infoname, $
  infover = infover, $
  noexecution = noexecution, $
  verbose = verbose, $
  csv = csv, $
  configcsv = configcsv



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

if keyword_set(noexecution) then return

;---------------------Restore IDL sav files or CSV files-----------------------;

if ~keyword_set(csv) then begin
; Restore IDL sav files

  ; search sav files
  files = file_search(infopath,'*.sav')
  print, ''
  print, 'INFOPATH: ', infopath
  print, 'Available IDL sav files in infopath:'
  for i1 = 0, n_elements(files)-1 do begin
    print, strcompress(i1), files[i1]
  endfor

  ; restore sav files
  idx = strmatch(files,'*'+infoname+infover+'*')
  if total(idx) eq 0 then begin
    dprint, verbose=verbose, 'No such sav files.'
    return
  endif else if total(idx) gt 1 then begin
    print, ''
    dprint, verbose = verbose, 'INFONAME+INFOVER: ', infoname+infover
    dprint, verbose = verbose, 'Mutiple match: '
    ind = where(idx ne 0)
    for i1 = 0, n_elements(ind)-1 do $
      print, strcompress(string(ind[i1])), files[ind[i1]]
    read,'Select: ', ind1
    restore, files[ind1]
  endif else begin
    ind = where(idx ne 0)
    restore,files[ind[0]]
  endelse

endif else if keyword_set(csv) then begin
; Restore CSV files
; CSV config options
;   INFOPATH: infotable path
;   INFONAME: infotable name
;   INFOVER: infotable version
;   TAGLIST: list of tags to save to csv file

  ; CSV configuration
  if keyword_set(configcsv) then begin
    tagnames = tag_names(configcsv)
    if total(strmatch(tagnames, 'infopath', /FOLD_CASE)) then infopath = configcsv.infopath
    if total(strmatch(tagnames, 'infoname', /FOLD_CASE)) then infoname = configcsv.infoname
    if total(strmatch(tagnames, 'infover', /FOLD_CASE)) then infover = configcsv.infover
    if total(strmatch(tagnames, 'taglist', /FOLD_CASE)) then taglist = configcsv.taglist    
  endif
  csvname = infopath + infoname + infover + '.csv'

  ; read CSV file -- store as csvdata
  if file_test(csvname) eq 1 then begin
    dprint, verbose = verbose, 'Found CSV file!'
    print, csvname
    csvdata = read_csv(csvname, header = header)
  endif else begin
    dprint, verbose = verbose, 'Cannot find file!'
    print, 'INFOPATH: ', infopath
    print, 'Searching CSV in infopath...'
    ; Search CSV files in infopath
    files = file_search(infopath, '*.csv')
    if total(strmatch(files, '')) ne 0 then begin
      dprint, verbose = verbose, 'Cannot find CSV files in infopath!'
      print, 'Returning...'
      return
    endif
    ; select csv files
    for i1 = 0, n_elements(files)-1 do $
      print, strcompress(string(i1)), files[i1]
      read, 'Select: ', ind
    while (ind lt 0 or ind gt (n_elements(files)-1)) do begin
      dprint, 'Wrong Input!'
      read, 'Select: ', ind
    endwhile
    csvname = files[ind]
    csvdata = read_csv(csvname, header = header)
  endelse

  ; create infotable structure
  infotable = []
  tagnames = tag_names(csvdata)
  for i1 = 0, n_elements(csvdata.field1)-1 do begin
    info = create_struct(header[0], (csvdata.(0))[i1])
    for j1 = 1, n_elements(header)-1 do begin
      info = create_struct(info, header[j1], (csvdata.(j1))[i1])
    endfor
    infotable = [infotable, info]
  endfor


endif

;--------------------------End of Restoring------------------------------------;

end ; program end