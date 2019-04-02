;+
;Procedure:	flm_plot_stamp
;
;PURPOSE:	
; This procedure make a ID stamp or time stamp on the existing figure.
;
;USAGE:	
;
;INPUTS:	
;
;KEYWORDS:	
;
;CREATED BY: 	 huangzs on Apr 01, 2019
;UPDATES:	
;
;-

pro flm_plot_stamp, info, charsize = chsize, on = on, off = off
common plot_stamp_com, active

compile_opt idl2

if n_elements(active) eq 0 then active = 1
if keyword_set(on)  then begin & active = 1 & return & endif
if keyword_set(off) then begin & active = 0 & return & endif

if n_elements(chsize) eq 0 then chsize = !p.charsize*1.
if chsize le 0 then chsize = 1.5
if active then begin
  ;xp = !x.window[1] + chsize * !d.y_ch_size/!d.x_size
  ;yp = !y.window[1]
  ; 24 is the length of systime()
  xp = 1 - chsize * !d.x_ch_size/!d.x_size * 24
  yp = 0.98
  yp1 = yp - chsize * 2 * !d.y_ch_size/!d.x_size
  xyouts,xp,yp,systime(),charsize=chsize,/norm,orien=0.
  if keyword_set(info) then $
  xyouts,xp,yp1, 'Key: '+strcompress(string(info.key),/remove_all),charsize=chsize,/norm,orien=0.
endif



end