;+
;Procedure:	flm_info_addtag
;
;PURPOSE:	
; This procedure add tag to info structure array (infotable).
; 
;USAGE:	
;
;INPUTS:	
;
;KEYWORDS:	
;
;CREATED BY: 	 huangzs on Mar 26, 2019
;UPDATES:	
;
;-

function flm_info_addtag, infotable, tagname, $
  vartype = vartype, $
  defval = defval, $
  remtag = remtag, $
  addtag = addtag


compile_opt idl2

if ~keyword_set(remtag) then addtag = 1

; default values
if ~keyword_set(vartype) then begin
  vartype = 'FLOAT'
  defval = !values.f_nan
endif else if keyword_set(vartype) then begin

  switch strupcase(vartype) of
    ('FLOAT'): begin
      if ~keyword_set(defval) then defval = !values.f_nan
      break
    end
    ('DOUBLE'): begin
      if ~keyword_set(defval) then defval = !values.f_nan
      break
    end
    ('INTEGER'): begin
      if ~keyword_set(defval) then defval = FIX(0)
      break
    end
    ('LONG'): begin
      if ~keyword_set(defval) then defval = LONG(0)
      break
    end
    ('POINTER'): begin
      if ~keyword_set(defval) then defval = PTR_NEW(0)
      break
    end
    else: begin
      dprint, vartype, ' Unsupported date type.'
      return, 0
    end
  endswitch
endif

; remove tag
if keyword_set(remtag) then begin
  tagnames = tag_names(infotable)
  ind = strmatch(tagnames, tagname, /FOLD_CASE)
  if total(ind) lt 1 then begin
    print, 'No tag name matched!'
    print, 'Returning ...'
    return, 0
  endif
  tagind = where(ind eq 0) 
endif



infos = infotable
infosnew = []
for i1 = 0, n_elements(infos)-1 do begin

  info = infos[i1]

  ; add tag or remove tag
  if keyword_set(addtag) then begin
    infonew = create_struct(info, tagname, defval)
  endif else if keyword_set(remtag) then begin
    infonew = create_struct(tagnames[tagind[0]], info.(tagind[0]))
    for j1 = 1, n_elements(tagind)-1 do begin
      infonew = create_struct(infonew, tagnames[tagind[j1]], info.(tagind[j1]))
    endfor
  endif

  infosnew = [infosnew, infonew]

endfor

return,infosnew

end