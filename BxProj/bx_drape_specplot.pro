;+
;Procedure:	bx_drape_specplot
;
;PURPOSE:	
; make drape pattern spectrum plot.
; Please make a window before using this procedure
;
;USAGE:	
; bx_drape_specplot, val, gridn, [limits = limits], [/noexe]
;
;INPUTS:	
;
;KEYWORDS:	
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
    if size(val, /n_dimensions) ne 3 then begin
      print, 'Wrong dimension!'
      print, '2 paramters accept only 3-dimensional matrix'
      return
    endif
    ; sum up the matrix
    val0 = val
    npix = (size(val, /dimensions))[0]
    halfnpix = floor(npix/2)
    val = replicate(!values.f_nan, npix, npix)
    for i1 = 0, npix-1 do begin
      for j1 = 0, npix-1 do begin
        val[i1, j1] = $
        total(val0[0:halfnpix, i1, j1] * gridn[0:halfnpix, i1, j1], /nan) / $
        total(gridn[0:halfnpix, i1, j1])
      endfor
    endfor
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

npix = (size(val, /dimensions))[0]
halfnpix = floor(npix/2)

; plot the result
x = (-indgen(npix) + halfnpix) * 3 * rmars / halfnpix
y = (indgen(npix) - halfnpix) * 3 * rmars / halfnpix
t = findgen(1000)/1000 * 2 * !pi
;wset
specplot, x, y, val, limits=limits
oplot, cos(t)*rmars, sin(t)*rmars

; Restore color table
loadct2,43


end