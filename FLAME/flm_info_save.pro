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
; flm_info_save, infotable, [/noexe], [/csv], $ 
;   [infopath = infopath], [infoname = infoname], [infover = infover], $
;   [verbose = verbose], [nodatestr = nodatestr], [configcsv = configcsv]
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
; CSV: save the infotable to CSV
; 
; CONFIGCSV: CSV saving configurations, plz check the code for options
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
  mission = mission, $
  verbose = verbose, $
  noexecution = noexecution, $
  nodatestr = nodatestr, $
  csv = csv, $
  configcsv = configcsv

compile_opt idl2

; retrieve paths and file names
flm_info_init, /noexe, infopath = infopath0, infoname = infoname0, infover = infover0, mission = mission
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


;-------------------------Save to IDL sav files or CSV-------------------------;
datestr = (time_string(systime(/seconds))).substring(0,10)
if ~keyword_set(csv) then begin
; IDL sav files

  ; check file existence and save
  if ~keyword_set(nodatestr) then begin
    savname = infopath + datestr + infoname + infover + '.sav'
    if ~file_test(infopath, /directory) then file_mkdir, infopath+datestr
  endif else begin
    savname = infopath + infoname + infover + '.sav'
    if ~file_test(infopath, /directory) then file_mkdir, infopath
  endelse

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
    file_mkdir,file_dirname(savname)
    save, infotable, filename = savname
  endelse  


endif else if keyword_set(csv) then begin
; CSV files  
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

  ; check taglist and combine tag to data1
  if ~keyword_set(taglist) then taglist = ['key']
  data1 = []
  header = []
  dprint, verbose = verbose, 'Combining tags...'
  dprint, verbose = verbose, taglist
  
  ; obtain tag names of infotable
  infotags = tag_names(infotable)

  for i1 = 0, n_elements(taglist)-1 do begin
    tagname = taglist[i1]
    ind = strmatch(infotags, tagname, /FOLD_CASE)
    
    switch (total(ind)) of
      (1): begin
        data1 = [[data1], [infotable.(where(ind eq 1))]]
        header = [header, tagname]
        break
      end
      (0): begin
        dprint, verbose = verbose, 'tagname no match!', tagname
        break
      end
      else: begin 
        dprint, verbose = verbose, 'Multiple match!', infotags[where(ind eq 1)]
        print, 'Discarding...'
      end
    endswitch

  endfor
  

  ; check file existence and save
  if ~keyword_set(nodatestr) then begin
    csvname = infopath + datestr + infoname + infover + '.csv'
    if ~file_test(infopath, /directory) then file_mkdir, infopath+datestr
  endif else begin
    csvname = infopath + infoname + infover + '.csv'
    if ~file_test(infopath, /directory) then file_mkdir, infopath
  endelse

  if file_test(csvname) then begin
    dprint, verbose = verbose, 'file exists:', csvname, 'Overwrite?'
    flagstr = ''
    read, 'Enter YES to overwrite: ', flagstr
    if strcmp(flagstr, 'YES') then begin
      dprint, verbose = verbose, 'Writing to ...', csvname
      write_csv, csvname, transpose(data1), header = header
    endif
  endif else begin
    dprint, verbose = verbose, 'Writting to ...', csvname
    write_csv, csvname, transpose(data1), header = header
  endelse  
  

endif
  




end