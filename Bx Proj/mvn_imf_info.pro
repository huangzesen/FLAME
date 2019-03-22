;+
;Procedure:	  mvn_imf_info
;
;PURPOSE:	
; This procedure is designed to calculate the IMF information for each orbit.
; The result is saved in a structure named 'imfinfo' for later usage.
; The structure included:
;   time:imf.x[i1],$                ; time
;   mag:B,$                         ; IMF
;   orb:orb1,$                      ; orbit number
;   msemat:[[X],[Y],[Z]],$          ; MSE coor change matrix
;   B1:B1, B2:B2,$                  ; magnetic field before/after
;   dB1:dB1, dB2:dB2,$              ; magnetic field turbulence before/after
;   delta1:delta1, delta2:delta2,$  ; abbreviation of magnetic field
;   angle:angle,$                   ; angle of IMF before/after
;   flag:0,$                        ; flag reserved for later usage
;   rottheta:rottheta               ; coordinates rotation theta
;
;USAGE:	
; mvn_imf_info
;
;INPUTS:	
; None
;
;KEYWORDS:	
;
; proxy: use proxy imf data
; 
; noexecution: do nothing (used to get the savpath and savname)
; 
; savpath, savname: return savpath and savname	
;
;CREATED BY: 	 huangzs on Mar 19, 2019
;UPDATES:	
;
;-

pro mvn_imf_info,$
  proxy=proxy,$
  savpath=savpath,$
  savname=savname,$
  noexecution=noexecution

compile_opt idl2

;------------------------------------------------------------------------------;
; load upstream IMF data and create IMF information structure	
; 
;   PROXY: add IMF proxy to imf data												
; 

imfpath = '/home/hara/work/analysis/maven/swind/'
imfname = 'mvn_mag_imf_mso_201411_201805.tplot'
proxypath = '/home/huangzs/work/fr/IMFproxy/'
savpath = '/home/huangzs/work/thesis/analysis/norm_bx_proj/'
savname = 'mk_norm_bx_proj.sav'

if keyword_set(noexecution) and file_test(savpath+savname) then return
if ~file_test(savpath,/directory) then file_mkdir,savpath

; load IMF data
tplot_restore, filenames = [imfpath+imfname]
get_data,'mvn_mag_imf_mso_avg', data = imf0
x = imf0.x
y = imf0.y
dy = imf0.dy
orb = floor(mvn_orbit_num(time = imf0.x))

; load IMF proxy data
if keyword_set(proxy) then begin
  files = file_search(proxypath+'*dayside_exc.sav')
  restore,files[0]
  proxy_dy = replicate(!values.f_nan,n_elements(mag_sheath.orb),3)
  proxy_x = mag_sheath.time
  proxy_y = mag_sheath.mag
  x = [imf0.x,proxy_x]
  y = [imf0.y,proxy_y]
  dy = [imf0.dy,proxy_dy]
  orb = [orb,mag_sheath.orb]
  idx = sort(x)
  x = x[idx]
  y = y[idx]
  dy = dy[idx]
  orb = orb[idx]
endif

imf = {imf, x:x, y:y, dy:dy, orb:orb}

; create IMF information structure
for i1 = 0, n_elements(imf.x)-2 do begin
  orb1 = imf.orb[i1]
  orb2 = imf.orb[i1+1]
  
  ; no ambient upstream condition observation, dismiss
  if (orb2 - orb1) gt 1 then begin
    continue
  endif
  
  ; Calculate MSE matrix
  msemat = replicate(0,3,3)
  B1 = imf.y[i1,*]
  B2 = imf.y[i1+1,*]
  dB1 = imf.dy[i1,*]
  dB2 = imf.dy[i1+1,*]
  delta1 = sqrt( total( (db1/b1)^2 ) )
  delta2 = sqrt( total( (db2/b2)^2 ) )
  angle = acos( total(B1*B2) / ( norm(b1,lnorm=2) * norm(b2,lnorm=2) ) )
  B = (B1+B2)/2
  normB = B/norm(B,lnorm=2)
  normE = crossp([1,0,0],normB)
  X = [1,0,0]
  Y = crossp(normE,X)
  Z = normE
  ; rottheta is the rotation theta. when used:
  ; x'=cost*x+sint*y, y'=-sint*x+cost*y
  rottheta = acos(total(Y*[0,1,0]))  

  imfinfotemp = {imfinfo,$
    time:imf.x[i1],$
    mag:B,$
    orb:orb1,$
    msemat:[[X],[Y],[Z]],$
    B1:B1, B2:B2,$
    dB1:dB1, dB2:dB2,$
    delta1:delta1, delta2:delta2,$
    angle:angle,$
    flag:0,$
    rottheta:rottheta}

  if ~keyword_set(imfinfo) then begin
    imfinfo = imfinfotemp
  endif else begin
    imfinfo = [imfinfo,imfinfotemp]
  endelse
  
endfor

save,imfinfo, filename = savpath+savname



end