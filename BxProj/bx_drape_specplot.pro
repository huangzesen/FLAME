;+
;Procedure:	bx_drape_specplot
;
;PURPOSE:	
; make drape pattern spectrum plot.
; Please make a window before using this procedure
;
;USAGE:	
; General Usage:
; bx_drape_specplot, val, [gridn], [limits = limits], [/noexe]
; 
; 1. bx_drape_specplot, val, gridn
;   VAL and GRIDN are both 3-D matrix
; 2. bx_drape_specplot, val, [gridn] 
;   VAL is 2-D, in this case GRIDN is not necessary
; 3. bx_drape_specplot, val, gridn, limits={precision:something}
;   VAL and GRIDN are both 2-D matrix
;
;INPUTS:	
; VAL: value matrix, either 2-D or 3-D
; GRIDN: (Optional) when VAL is 3-D, degenration matrix GRIDN is needed.
;
;KEYWORDS:	
; 
; NOEXECUTION: (Optional) do nothing, used for returning default limits struct.
; 
; LIMITS: (Optional) specify the figure parameters, notably including:
;   PRECISION: figure precision in Rm, (assume grid is p Rm x p Rm). Note that
;     the procedure assumes the total cubic is 6Rm x 6Rm x 6Rm. If this option is not
;     set, the procedure will use default grid size.
;   
;
; 
;
;CREATED BY: 	 huangzs on Mar 22, 2019
;UPDATES:	
;
;-

pro bx_drape_specplot, val, gridn, $
  limits = limits, $
  noexecution=noexecution

compile_opt idl2


; keyword LIMITS
Rmars = 3389D
limits0 = $
  {isotropic: 1, $
  xtitle: 'Y!LMSO!N(R!LM!N)', ytitle: 'Z!LMSO!N(R!LM!N)', $
  title: '-B!LY!N IMF', $
  xstyle:1, ystyle:1, $
  ;xtickv: (indgen(7)-3)*rmars, ytickv: (indgen(7)-3)*rmars, $
  xticks:6, yticks:6, $
  xtickname: ['3', '2', '1', '0', '-1', '-2', '-3'], $
  ytickname: ['-3', '-2', '-1', '0', '1', '2', '3'], $
  zrange: [-1,1], ztitle: 'B!LX!N/|B|', $
  charsize: 1.5D $
}
if ~keyword_set(limits) then begin
  limits = limits0
endif else begin
  extract_tags, limits, limits0, /preserve
endelse

if keyword_set(noexecution) then return



; check N_PARAMS
; n_params == 1:
;   dimension of val should be [npix, npix]
; n_params == 2:
;   dimension of val and gridn should be [npix, npix, npix]
; others:
;   Error
switch (n_params()) of
  ; Input: val
  (1): begin
    if size(val, /n_dimensions) ne 2 then begin
      print, 'Wrong dimension!'
      print, 'Single parameter accept only 2-dimensional matrix.'
      return
    endif
    break
  end
  ; Input: val, gridn
  (2): begin
    if size(val, /n_dimensions) eq 3 then begin
      ; sum up the matrix
      val0 = val
      val = total(val0*gridn,1,/nan) / total(gridn,1,/nan)    
    endif else if size(val, /n_dimensions) eq 2 then begin
      ; do nothing, the grid is used for reference
    endif else begin
      print, 'Wrong dimension!'
      print, '2 paramters accept only 3-dimensional matrix'
      return
    endelse
    break
  end
  ; Error
  else: begin 
    dprint, /verbose, 'Too few parameters!'
    dprint, /verbose, 'Call pattern:'
    print, 'bx_drape_specplot, val, [gridn], [limits = limits]'  
  end
endswitch

loadct2, 70, /reverse, /graybkg

; Precision of the figure (resample to change the grid size)
if total(strmatch(tag_names(limits),'PRECISION',/FOLD_CASE)) ne 0 then begin
  print, 'Code under construction....2019-04-02'
  return
  if (n_params()) ne 2 or size(val, /n_dimensions) ne 2 then begin
    print, 'Wrong parameters!'
    print, 'Please provide gridn or wrong dimension of val!'
    return
  endif
  if round(6/limits.precision) gt (size(val, /dimensions))[0] then begin
    print, 'Up sampling is not supported!'
    return
  endif
  npix = (size(val, /dimensions))[0]
  halfnpix = floor(npix/2.)
  npix1 = round(6/limits.precision)
  halfnpix1 = floor(npix/2.)
  valnew = replicate(!values.f_nan, npix, npix)
  for i1 = 0, npix-1 do begin
    for j1 = 0, npix-1 do begin
      ; donothing
    endfor
  endfor
  
endif else begin
  npix = (size(val, /dimensions))[0]
  halfnpix = floor(npix/2.)
  x = (-indgen(npix) + halfnpix) * 3 * rmars / halfnpix
  y = (indgen(npix) - halfnpix) * 3 * rmars / halfnpix
endelse

;wset
specplot, x, y, val, limits=limits
t = findgen(1000)/1000 * 2 * !pi
oplot, cos(t)*rmars, sin(t)*rmars

; Restore color table
loadct2,43


end