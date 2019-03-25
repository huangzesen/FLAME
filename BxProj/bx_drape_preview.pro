;+
;Procedure:	
;
;PURPOSE:	
;
;USAGE:	
;
;INPUTS:	
;
;KEYWORDS:	
;
;CREATED BY: 	 huangzs on Mar 22, 2019
;UPDATES:	
;
;-

pro bx_drape_preview, valmyz, valpyz, $
  minmaxval=minmaxval, $
  mse=mse, $
  noIMFsector=noIMFsector, $
  TITLE = TITLE, $
  XTITLE = XTITLE, $
  YTITLE = YTITLE

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

ct = COLORTABLE(70, /REVERSE)
if ~keyword_set(minmaxval) then minmaxval = 0.15

gm = IMAGE(valmyz, indgen(101,start=100,increment=-1), indgen(101), $
  RGB_TABLE=ct, AXIS_STYLE=2, MARGIN=0.1, $
  MIN_VALUE = -minmaxval, MAX_VALUE = minmaxval, $
  XTITLE=xtitle, $
  YTITLE=ytitle, $
  XTICKVALUES=ceil(findgen(7)*100/6), $
  YTICKVALUES=ceil(findgen(7)*100/6), $
  XTICKNAME=['3','2','1','0','-1','-2','-3'], $
  YTICKNAME=['-3','-2','-1','0','1','2','3'], $
  TITLE='$-B_Y IMF$ ' + TITLE, $
  POSITION=[0.18,0.10,0.98,0.90])

cirm = PLOT(cos(findgen(1000)/1000*2*!pi)*50./3+50, $
  sin(findgen(1000)/1000*2*!pi)*50./3+50, $
  /overplot)

cbm = COLORBAR(TARGET=gm, ORIENTATION=1, $
  POSITION=[0.10,0.05,0.15,0.9], TICKDIR=1, $
  TITLE='$B_X/B$ ')

if keyword_set(noIMFsector) then begin
  gm.title = TITLE
  return
endif

gp = IMAGE(valpyz, indgen(101,start=100,increment=-1), indgen(101), $
  RGB_TABLE=ct, AXIS_STYLE=2, MARGIN=0.1, $
  MIN_VALUE=-minmaxval, MAX_VALUE=minmaxval, $
  XTITLE=xtitle, $
  YTITLE=ytitle, $
  XTICKVALUES=ceil(findgen(7)*100/6), $
  YTICKVALUES=ceil(findgen(7)*100/6), $
  XTICKNAME=['3','2','1','0','-1','-2','-3'], $
  YTICKNAME=['-3','-2','-1','0','1','2','3'], $
  TITLE='$+B_Y IMF$ ' + TITLE, $
  POSITION=[0.18,0.10,0.98,0.90])

cirp = PLOT(cos(findgen(1000)/1000*2*!pi)*50./3+50, $
  sin(findgen(1000)/1000*2*!pi)*50./3+50, $
  /overplot)
  
cbp = COLORBAR(TARGET=gp, ORIENTATION=1, $
  POSITION=[0.10,0.05,0.15,0.9], TICKDIR=1, $
  TITLE='$B_X/B$   ')



end