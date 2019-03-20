;+
;Procedure:	  mk_norm_bx_proj
;
;PURPOSE:	
; This procedure make normalized Bx projection on MSO/MSE Y-Z plane, following
; the convention in Dibraccio et al (2018) figure 2.
; 
;
;USAGE:	
; mk_norm_bx_proj
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
;
;CREATED BY: 	 huangzs on Mar 18, 2019
;UPDATES:	
;
;-

pro mk_norm_bx_proj,$
  proxy=proxy,$
  savpath=savpath,$
  savname=savname,$
  noexecution=noexecution

compile_opt idl2

;------------------------------------------------------------------------------;
; load imf information and record the Bx ratio observation
;

savpath = '/home/huangzs/work/thesis/analysis/norm_bx_proj/'
savname = 'ratio.sav'
Rmars = 3389D
startorb = 212
endorb = 7640
; Grid for +By IMF
gridp = replicate(create_struct('n',0,'val',!values.f_nan),101,101,101)  
; Grid for -By IMF
gridm = replicate(create_struct('n',0,'val',!values.f_nan),101,101,101)  


; restore imf information
mvn_imf_info, proxy=proxy, savpath=imfinfopath, savname=imfinfoname, /noexe
restore,imfinfopath+imfinfoname

; loop from orbstart to orbend
for orb = orbstart, orbend-1 do begin

  ; find the corresponding IMF info struct
  idx = where(imfinfo.orb eq orb)
  if idx[0] eq -1 then continue
  imfinfo0 = imfinfo[idx[0]]
    
  t1 = mvn_orb_num(orbnum = orb)
  t2 = mvn_orb_num(orbnum = orb+1)
  timespan,t1,t2-t1,/sec
  mvn_spice_load,/download,/no_download
  mvn_mag_load,spice='MSO'
  maven_orbit_tplot,/loadonly
  get_mvn_eph,[t1,t2],eph,res=60,/no_download

  ; obtain the plasma and magnetic field information
  get_data,'pileup', data = pileup
  get_data,'wake',data = wake
  get_data,'mvn_B_1sec_MSO',data = magmso

  ; determine magnetotail region
  idx1 = finite(pileup.y)
  idx2 = finite(wake.y)
  idx = idx1 + idx2
  tmagtail = pileup.x[where(idx eq 1)]

  ; loop through the ephemeris data
  for i1 = 0, n_elements(eph)-1 do begin
    ind = where(abs(tmagtail-eph[i1].time) lt 6)
    if ind[0] eq -1 then continue
    time = tmagtail[ind[0]]
    x = eph[i1].x_ss
    y = eph[i1].y_ss
    z = eph[i1].z_ss
    
    ; obtain magnetic field data
    ; leave the typo here to check whether there will be bug (Mar-19)
    ind = where(abs(magmso.x-time) lt 2)
    mag = magmso.y[ind[0],*]
    magabs = norm(mag,lnorm=2)
    ratio = mag[0]/magabs

    ; Add ratio to the grid
    ; +By IMF
    indx = round(x_ss/rmars)
    indy = round(y_ss/rmars)
    indz = round(z_ss/rmars)  
    if imfinfo0.B[1] gt 0 then begin
      ; Check whether the grid is filled
      if gridp[indx,indy,indz].n eq 0 then begin
        gridp[indx,indy,indz] = ratio
        gridp[indx,indy,indz].n += 1  
      endif else begin
        gridp[indx,indy,indz] = (gridp[indx,indy,indz]*n+ratio)/(n+1)
        gridp[indx,indy,indz].n += 1
      endelse
    endif else if imfinfo0.B[1] lt 0 then begin
      ; Check whether the grid is filled
      if gridm[indx,indy,indz].n eq 0 then begin
        gridm[indx,indy,indz] = ratio
        gridm[indx,indy,indz].n += 1  
      endif else begin
        gridm[indx,indy,indz] = (gridm[indx,indy,indz]*n+ratio)/(n+1)
        gridm[indx,indy,indz].n += 1
      endelse
    endif

  end

end

save, gridp, gridm, filename = savpath+savname


end