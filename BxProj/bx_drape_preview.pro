;+
; This procedure is OBSOLETE!
; Please use bx_drape_specplot instead.
; 
;Procedure:	bx_drape_preview
;
;PURPOSE:	
; make drape pattern preview
;
;USAGE:	
; bx_drape_preview, opt_preview, valmyz, [valpyz], $
;   [minmaxval = minmaxval], [mse = mse], $
;   [noIMFsector = noIMFsector], $
;   [TITLE = TITLE], [XTITLE = XTITLE], [YTITLE = YTITLE], $
;   [SPECPLOT = SPECPLOT]
;
;INPUTS:	
;
;KEYWORDS:	
;
; SPECPLOT: use specplot procedure to create figure
;
;CREATED BY: 	 huangzs on Mar 22, 2019
;UPDATES:	
;
;-

pro bx_drape_preview, opt_preview, valmyz, valpyz, $
  minmaxval=minmaxval, $
  mse=mse, $
  noIMFsector=noIMFsector, $
  TITLE = TITLE, $
  XTITLE = XTITLE, $
  YTITLE = YTITLE, $
  specplot = specplot, $
  gridm = gridm, $
  gridp = gridp

compile_opt idl2

if keyword_set(mse) then begin
  if ~keyword_set(XTITLE) then XTITLE = '$Y_{MSE}/Rm$'
  if ~keyword_set(YTITLE) then YTITLE = '$Z_{MSE}/Rm$'
  if ~keyword_set(TITLE) then TITLE = 'Bx Drape Pattern Preview (MSE)'
endif else begin
  if ~keyword_set(XTITLE) then XTITLE = ' $Y_{MSO}/Rm$'
  if ~keyword_set(YTITLE) then YTITLE = '$Z_{MSO}/Rm$'
  if ~keyword_set(TITLE) then TITLE = 'Bx Drape Pattern Preview (MSO)'
endelse

npix = opt_preview.npixel
halfnpix = floor(npix/2)

loadct2, 70, /reverse, /graybkg
if ~keyword_set(minmaxval) then minmaxval = opt_preview.minmaxval


;--------------------------First Figure----------------------------------------;
if keyword_set(SPECPLOT) then begin
  valm = gridm.val
  gridmn = gridm.n
  nval = valm * gridmn
  valmyz = replicate(!values.f_nan, npix, npix)
  for i1 = 0, npix-1 do begin
    for j1 = 0, npix-1 do begin
      temparr = nval[0:halfnpix,i1,j1]
      if total(finite(temparr)) eq 0 then valmyz[i1,j1] = !values.f_nan else begin
        temparr[where(finite(temparr) eq 0)] = 0
        valmyz[i1,j1] = total(temparr)/total(gridmn[0:halfnpix,i1,j1])
      endelse
    endfor
  endfor
  wi,0
  specplot, -indgen(npix), indgen(npix), valmyz, limits={isotropic:1}
  
  valp = gridp.val
  gridpn = gridp.n
  nval = valp * gridpn
  valpyz = replicate(!values.f_nan, npix, npix)
  for i1 = 0, npix-1 do begin
    for j1 = 0, npix-1 do begin
      temparr = nval[0:halfnpix,i1,j1]
      if total(finite(temparr)) eq 0 then valpyz[i1,j1] = !values.f_nan else begin
        temparr[where(finite(temparr) eq 0)] = 0
        valpyz[i1,j1] = total(temparr)/total(gridpn[0:halfnpix,i1,j1])
      endelse
    endfor
  endfor
  wi,1
  specplot, -indgen(npix), indgen(npix), valpyz, limits={isotropic:1}
  
  
  return
endif else begin
  gm = IMAGE(valmyz, indgen(npix,start=(npix-1),increment=-1), indgen(npix), $
  RGB_TABLE=ct, AXIS_STYLE=2, MARGIN=0.1, $
  MIN_VALUE = -minmaxval, MAX_VALUE = minmaxval, $
  XTITLE=xtitle, $
  YTITLE=ytitle, $
  XTICKVALUES=ceil(findgen(7)*(npix-1)/6), $
  YTICKVALUES=ceil(findgen(7)*(npix-1)/6), $
  XTICKNAME=['3','2','1','0','-1','-2','-3'], $
  YTICKNAME=['-3','-2','-1','0','1','2','3'], $
  TITLE='$-B_Y IMF$ ' + TITLE, $
  POSITION=[0.18,0.10,0.98,0.90])

cirm = PLOT(cos(findgen(1000)/1000*2*!pi)*halfnpix/3+halfnpix, $
  sin(findgen(1000)/1000*2*!pi)*halfnpix/3+halfnpix, $
  /overplot)

cbm = COLORBAR(TARGET=gm, ORIENTATION=1, $
  POSITION=[0.10,0.05,0.15,0.9], TICKDIR=1, $
  TITLE='$B_X/B$ ')
endelse


if keyword_set(noIMFsector) then begin
  gm.title = TITLE
  return
endif

gp = IMAGE(valpyz, indgen(npix,start=(npix-1),increment=-1), indgen(npix), $
  RGB_TABLE=ct, AXIS_STYLE=2, MARGIN=0.1, $
  MIN_VALUE=-minmaxval, MAX_VALUE=minmaxval, $
  XTITLE=xtitle, $
  YTITLE=ytitle, $
  XTICKVALUES=ceil(findgen(7)*(npix-1)/6), $
  YTICKVALUES=ceil(findgen(7)*(npix-1)/6), $
  XTICKNAME=['3','2','1','0','-1','-2','-3'], $
  YTICKNAME=['-3','-2','-1','0','1','2','3'], $
  TITLE='$+B_Y IMF$ ' + TITLE, $
  POSITION=[0.18,0.10,0.98,0.90])

cirp = PLOT(cos(findgen(1000)/1000*2*!pi)*halfnpix/3+halfnpix, $
  sin(findgen(1000)/1000*2*!pi)*halfnpix/3+halfnpix, $
  /overplot)
  
cbp = COLORBAR(TARGET=gp, ORIENTATION=1, $
  POSITION=[0.10,0.05,0.15,0.9], TICKDIR=1, $
  TITLE='$B_X/B$   ')


; Restore color table
loadct2,43


end


