;+
;Procedure:	flm_label_main
;
;PURPOSE:	
; This procedure act as the main label control
;
;USAGE:	
;
;INPUTS:	
;
;KEYWORDS:	
; MISSION: mission code, can be 'VEX' or 'MVN'
; AMOUNT: amount of events to label
; LNAME: label name to make label
; LDEFVAL: label default value (NULL value)
;
;CREATED BY: 	 huangzs on Apr 05, 2019
;UPDATES:	
;
;-

pro flm_label_main, $
  mission = mission, $
  amount = amount, $
  lname = lname, $
  ldefval = ldefval, $
  start1 = start1, $
  end1 = end1

compile_opt idl2

if ~keyword_set(mission) then mission = 'MVN'

; Restore files
switch (mission) of
  ('MVN'): begin
    savpath = '/home/huangzs/work/thesis/analysis/mvn/mvninfo/infotable/'
    lstackpath = '/home/huangzs/work/thesis/analysis/mvn/mvninfo/lstack/'
    lstackname = 'mvn-lstack.sav'
    if ~file_test(lstackpath) then file_mkdir, lstackpath
    break
  end
  ('VEX'): begin
    savpath = '/home/huangzs/work/thesis/analysis/vex/vexinfo/infotable/'
    lstackpath = '/home/huangzs/work/thesis/analysis/vex/vexinfo/lstack/'
    lstackname = 'vex-lstack.sav'
    if ~file_test(lstackpath) then file_mkdir, lstackpath
    break
  end
  else: begin 
    ; do nothing
    print, 'Mission not supported.'
    print, 'Mission: ', mission
    return
  end
endswitch

; Restore infotable
itfiles = file_search(savpath, '*.sav')
for i1 = 0, n_elements(itfiles)-1 do print, strcompress(string(i1)), itfiles[i1]
read, 'Select a sav file: ', ind
restore, itfiles[ind]
itfullsavname = itfiles[ind]

; Restore label stack file
lsfiles = file_search(lstackpath, '*.sav')
if keyword_set(lsfiles) ne 0 then begin
  for i1 = 0, n_elements(lsfiles)-1 do $
    print, strcompress(string(i1)), lsfiles[i1]
  read, 'Select a label stack file (Enter -1 to make a new one): ', ind

  if ind ne -1 then begin
    restore, lsfiles[ind] 
  endif else begin
    print, 'Old label stack name: ', lstackname
    print, 'Label stack path: ', lstackpath
    lstackname0 = lstackname
    read, 'New label stack name: ', lstackname
  endelse

endif   



; init stack
if ~n_elements(lstack) then lstack = obj_new('stack')

;----------------------------------------;
; Main label loop              
;----------------------------------------;
if ~keyword_set(start1) then start1 = 0
if ~keyword_set(end1) then end1 = n_elements(infotable)-1
if ~keyword_set(lname) then begin
  print, 'Existing tags:'
  (infotable[0]).gettags, tags = tags
  print,tags
  lname = ''
  read, 'Label name: ', lname
endif
if ~keyword_set(ldefval) then ldefval = -1

for i1 = start1, end1 do begin
  
  info = infotable[i1]
  
  ; Jump the finished part
  info.lgetval, lname, val0
  if val0 ne ldefval then continue
  
  info.view, figptr = figptr
  if figptr ne !NULL then print, 'Figure path: ', *figptr
  print, 'Label name: ', lname
  print, 'enter -2 to retrieve, -3 to exit, '
  read, 'Label value: ', lval
  switch (lval) of

    ;----------------------------------------------------------- 
    ; RETRIEVE LABEL STACK
    (-2): begin
      print, 'Retriving the stack...'

      ; Retrieve history and push into retstack
      retstack = obj_new('stack')
      while(~lstack.IsEmpty()) do begin
        elem = lstack.pop()
        elem.view
        retstack.push,elem
        FLAG1: print, 'Continue? ', 'Enter y to continue, n to stop'
        strflag = ''
        read, strflag
        
        if strflag.contains('y',/FOLD_CASE) then begin
          continue 
        endif else if strflag.contains('n',/FOLD_CASE) then begin
          break 
        endif else begin 
          print, 'Wrong input!'
          stop 
          goto, FLAG1
        endelse
        
      endwhile
      if lstack.IsEmpty() then print, 'Label Stack Empty!' 

      ; Relabel the retstack
      while(~retstack.IsEmpty()) do begin
        elem = retstack.pop()
        elem.view
        print, 'Label name: ', lname
        elem.lgetval, lname, lval, success = s
        if s eq 0 then stop
        print, 'Old label value: ', lval
        read, 'New value: ', newlval
        elem.lsetval, lname, newlval
        idx = elem.FindInfo(infotable, success = s)
        if s ne 1 then stop
        infotable[idx] = elem
        lstack.push, elem
      endwhile
      if retstack.IsEmpty() then print, 'Retrieve Stack Empty!'
      print, 'Finished Relabeling!'
      
      ; NOTE: No BREAK here because we still need to label the info in else
      i1 = i1-1
      break

    end

    ;-----------------------------------------------------------
    ; EXIT THE PROGRAM
    (-3): begin
      print, 'Exiting the label program...'
      i1 = n_elements(infotable)
      break
    end

    ;-----------------------------------------------------------
    ; LABEL THE INFO
    else: begin 
      info.lsetval, lname, lval, success = s
      if s eq 0 then begin
        print, 'Label name not existed.'
        print, 'Label name: ', lname
        stop
      endif else if s eq 1 then begin
        idx = info.FindInfo(infotable, success = s)
        if s ne 1 then stop
        infotable[idx] = info
      endif
      lstack.push,info
      print, 'Ind: ', i1+1, ' Out of ', n_elements(infotable)
    end
  endswitch
  
  
endfor
; End of main loop
;-------------------------------------------------------------------------------

; Save the label stack
print, 'Saving the label stack...'
print, 'Exsiting label stack files: '
for i1 = 0, n_elements(lsfiles)-1 do $
  print, strcompress(string(i1)), lsfiles[i1]

if ~file_test(lstackpath+lstackname) then begin
  save, lstack, filename = lstackpath + lstackname 
endif else begin
  print, 'File already existed!'
  print, lstackpath+lstackname
  flagstr = ''
  read, 'Overwrite? Type YES to overwrite: ', flagstr
  if flagstr eq 'YES' then begin
    save, lstack, filename = lstackpath + lstackname 
  endif else begin
    lstackname0 = lstackname
    lstackname = ''
    read, 'New label stack name: ', lstackname
    save, lstackpath+lstackname  
  endelse
endelse

; Save the infotable
print, 'Saving the Infotable...'
print, 'Existing infotbale: '
for i1 = 0, n_elements(ltfiles)-1 do $
  print, strcompress(string(i1)), ltfiles[i1]

print, 'Overwrite the old infotable?'
print, 'Old infotable: ', itfullsavname
flagstr = ''
read, 'Type YES to overwrite: ', flagstr
if flagstr eq 'YES' then $
  save, infotable, filename = itfullsavname $
else $
  stop

;------------------------------------------------------------------------------;
print, 'End of program...'
stop


end