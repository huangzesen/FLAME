;+
;Procedure:	  mk_norm_bx_proj
;
;PURPOSE:	
; This procedure make normalized Bx projection on MSO/MSE Y-Z plane, following
; the convention in Dibraccio et al (2018) figure 2.
; 
;
;USAGE:
;	
; mk_norm_bx_proj, [/mse], [/proxy]
; 
;   Calculate the result matrix and save to the default directory with default name
;   /home/huangzs/work/thesis/analysis/mk_norm_bx_proj/date/ + savname
;   
; mk_norm_bx_proj, /para, [config = config]
; 
;   Parallelized calculation, pass NCHUNK, SERINUM using a structure, otherwise
;   the program will let you enter the parameters manually.
;   
; mk_norm_bx_proj, /noexe, [/preview], [savpath = savpath], [savname = savname], $
;   [savfiles = savfiles], [savindex = savindex]
;   
;   Retrieve the sav files by using keyword NOEXECUTION, savpath, savname and savfiles 
;   can be retrieved using keyword SAVPATH, SAVNAME and SAVFILES. If you want to have a 
;   quick look of the data, you can use keyword PREVIEW to let the program generate a 
;   preview image of the result. SAVINDEX can be used to combine several sav files, 
;   otherwise the program will let you enter index manually with only one file allowed. 
;   
; examples of OPT_PREVIEW0:
;   {MODEs: 'average', MINMAXVAL: 0.8, NOIMFSECTOR: 1}
;
;INPUTS:	
; None
;
;KEYWORDS:	
;
; PROXY: use proxy imf data
; 
; MSE: convert the coordinate system to mse
; 
; NOEXECUTION: do nothing (used to get the savpath and savname)
; 
; PARALLEL: (Optional) Parallelize the program
; 
; CONFIG: (Optional) program configuration, a structure containing:
;   'nchunk' : number of chunks
;   'serinum' : serial number of this chunk
; 
; SAVPATH, SAVNAME, SAVFILES: return savpath and savname
; 
; PREVIEW: (Optional) provide a preview of the result
;
; OPT_PREVIEW0: (Optional) preview options, should be a structure containing:

;   'mode' : available options
;     'average' : Average ratio in x direction
;     'slice' : show the pattern in slice
;   please check the code for available keywords
; 
; SAVINDEX: (Optional) Use when PREVIEW is used, to pass a array of index of preview sav files
; 
; ORBSTART, ORBEND: start/end of looping orbit, default is 212/7640
; 
;
;CREATED BY: 	 huangzs on Mar 18, 2019
;UPDATES:	
;
;-

pro mk_norm_bx_proj, $
  proxy=proxy, $
  mse=mse, $
  savpath=savpath, $
  savname=savname, $
  savfiles=savfiles, $
  parallel=parallel, $
  config=config, $
  orbstart=orbstart, $
  orbend=orbend, $
  preview=preview, $
  opt_preview0=opt_preview0, $
  savindex=savindex, $
  noexecution=noexecution

compile_opt idl2

;------------------------------------------------------------------------------;
; load imf information and record the Bx ratio observation
;

if ~keyword_set(savpath) then begin
  timestr = time_string(systime(/seconds))
  datestr = timestr.substring(0,9) + '/'
  savpath0 = '/home/huangzs/work/thesis/analysis/norm_bx_proj/'
  savpath = '/home/huangzs/work/thesis/analysis/norm_bx_proj/'+datestr
endif
  
if ~keyword_set(savname) then $
  savname = 'ratio1.sav'


;-----------------------------------Preview------------------------------------;
if keyword_set(noexecution) then begin
  savfiles = file_search(savpath0,'*.sav')
  print,'Available sav files:'
  for i1 = 0, n_elements(savfiles)-1 do $
    print,strcompress(string(i1)),' ',savfiles[i1]
    
  if keyword_set(preview) then begin

    ;-----------------------PREVIEW OPTIONS-----------------------;
    ; mode:
    ;   'average' : average Bx in x direction
    ;   'slice' : slice plot
    ; pars:
    ;   'flag' : useless
    ;   'nslice' : (for mode 'slice') number of slices
    ;   'minmaxval' : min/max value for function image
    ;   'alt' : abandon data below alt
    ;   'npixel' : n pixel of the grid
    ;   'noIMFsector' : combine +By IMF and -By IMF period
    ;-------------------------------------------------------------;
    
    delvar, opt_preview
    pars0 = {pars, flag: 0, nslice: 3, alt: 1000D, npixel: 201}
    if ~keyword_set(opt_preview0) then begin
      opt_preview = {opt_preview, mode: 'average', minmaxval: 0.1, INHERITS pars0}
    endif else begin
      opt_preview = create_struct(name='opt_preview', opt_preview0, pars0)
    endelse
    

    ; combine sav files
    if ~keyword_set(savindex) then begin
      read,'Select sav file: ', ind
      if ind eq -1 then return    ; Return if ind == -1
      restore,savfiles[ind]
    endif else if keyword_set(savindex) then begin
      ; restore sav files
      for i1 = 0, n_elements(savindex)-1 do begin
        restore,savfiles[savindex[i1]]
        if i1 eq 0 then begin
          gridm0 = gridm
          gridp0 = gridp
          valm0 = gridm0.val
          valp0 = gridp0.val
          idxm0 = where(~finite(valm0))
          idxp0 = where(~finite(valp0))
          valm0[idxm0] = 0
          valp0[idxp0] = 0
          gridm0.val = valm0
          gridp0.val = valp0
        endif else begin
          valm = gridm.val
          valp = gridp.val
          idxm = where(~finite(valm))
          idxp = where(~finite(valp))
          valm[idxm] = 0
          valp[idxp] = 0
          gridm.val = valm
          gridp.val = valp
          ; weighted addition
          gridm0.val = ((gridm0.val * gridm0.n) + (gridm.val * gridm.n)) / float(gridm0.n + gridm.n)
          gridp0.val = ((gridp0.val * gridp0.n) + (gridp.val * gridp.n)) / float(gridp0.n + gridp.n)
          gridm0.n = gridm0.n + gridm.n
          gridp0.n = gridp0.n + gridp.n  
        endelse
      endfor
      gridm = gridm0
      gridp = gridp0
    endif
    
        
    ; manipulate the matrix according to opt_preview
    mode = opt_preview.mode
    npix = opt_preview.npixel        ; n pixel of the grid
    pars = opt_preview
    halfnpix = floor(npix/2)              ; half n pixel
    tagnames = tag_names(opt_preview)
    
    ; tag NOIMFSECTOR, combine gridm and gridp into grid
    if total(strmatch(tagnames, 'noIMFsector', /FOLD_CASE)) eq 1 then begin
      grid = replicate(create_struct('n',0,'val',!values.f_nan),npix,npix,npix) 
      valm = gridm.val
      valp = gridp.val
      idxm = where(~finite(valm))
      idxp = where(~finite(valp))
      valm[idxm] = 0
      valp[idxp] = 0
      grid.val = ((valm * gridm.n) + (valp * gridp.n)) / float(gridm.n + gridp.n)
      grid.n = gridm.n + gridp.n
      val = grid.val
      val[where(~finite(val))] = 0
      gridn = grid.n
    endif
    
    ; preparing the image
    valm = gridm.val
    valp = gridp.val
    gridmn = gridm.n
    gridpn = gridp.n
    idxm = where(~finite(valm))
    idxp = where(~finite(valp))
    valm[idxm] = 0
    valp[idxp] = 0
    
    ; tag ALT, discard data below alt
    Rmars = 3389D
    if total(strmatch(tagnames, 'alt', /FOLD_CASE)) eq 1 then begin
      for i1 = 0, npix-1 do for j1 = 0, npix-1 do for k1 = 0, npix-1 do $
        if sqrt( (i1-halfnpix)^2 + (j1-halfnpix)^2 + (k1-halfnpix)^2 ) lt (Rmars+pars.alt)/Rmars/3 * halfnpix then begin
          valm[i1,j1,k1] = 0
          valp[i1,j1,k1] = 0
          if total(strmatch(tagnames, 'noIMFsector', /FOLD_CASE)) eq 1 then val[i1,j1,k1] = 0
        endif
    endif
    
    
    ;-----------------------DRAW PREVIEW---------------------;
    
    ; no IMF sector
    if total(strmatch(tagnames, 'noIMFsector', /FOLD_CASE)) eq 1 then begin
      if mode eq 'average' then begin
        gridnyz = total(gridn[0:halfnpix,*,*],1)
        gridnyz[where(gridnyz eq 0)] += 1
        valyz = total(val[0:halfnpix,*,*] * gridn[0:halfnpix,*,*] ,1) / gridnyz
        print,'Max/Min of val: ', max(valyz), min(valyz)
        bx_drape_preview,opt_preview, valyz,minmaxval=pars.minmaxval, mse=mse, /noIMFsector
        return
      endif
    endif else if mode eq 'slice' then begin
      for i1 = 0, pars.nslice-1 do begin
        gridnyz = TOTAL(gridn[FLOOR(i1*halfnpix/pars.nslice):CEIL((i1+1)*halfnpix/pars.nslice),*,*],1)
        gridnyz[where(gridnyz) eq 0] += 1
        
        valyz = $
          total(valm[floor(i1*halfnpix/pars.nslice):ceil((i1+1)*halfnpix/pars.nslice)] * $
          gridn[floor(i1*halfnpix/pars.nslice):ceil((i1+1)*halfnpix/pars.nslice),*,*],1) / gridnyz
        
        bx_drape_preview, opt_preview, valyz, minmaxval=pars.minmaxval, mse=mse, /noIMFsector
        return
      endfor
    endif
    
    ; have IMF sector
    if mode eq 'average' then begin
      gridmnyz = total(gridmn[0:halfnpix,*,*],1)
      gridpnyz = total(gridpn[0:halfnpix,*,*],1)
      gridmnyz[where(gridmnyz eq 0)] += 1
      gridpnyz[where(gridpnyz eq 0)] += 1
      valmyz = total(valm[0:halfnpix,*,*] * gridmn[0:halfnpix,*,*], 1)/gridmnyz
      valpyz = total(valp[0:halfnpix,*,*] * gridpn[0:halfnpix,*,*], 1)/gridpnyz
      print,'Max/Min of -Y IMF period: ', max(valmyz), min(valmyz)
      print,'Max/Min of +Y IMF period: ', max(valpyz), min(valpyz)
      bx_drape_preview, opt_preview, valmyz, valpyz, minmaxval=pars.minmaxval, mse=mse
      return
    endif else if mode eq 'slice' then begin
      for i1 = 0, pars.nslice-1 do begin
        gridmnyz = total(gridmn[floor(i1*halfnpix/pars.nslice):ceil((i1+1)*halfnpix/pars.nslice),*,*],1)
        gridpnyz = total(gridpn[floor(i1*halfnpix/pars.nslice):ceil((i1+1)*halfnpix/pars.nslice),*,*],1)
        gridmnyz[where(gridmnyz eq 0)] += 1
        gridpnyz[where(gridpnyz eq 0)] += 1
        
        valmyz = $
          total(valm[floor(i1*halfnpix/pars.nslice):ceil((i1+1)*halfnpix/pars.nslice),*,*] * $
          gridmn[floor(i1*halfnpix/pars.nslice):ceil((i1+1)*halfnpix/pars.nslice),*,*],1) / gridmnyz
        
        valpyz = total(valp[floor(i1*halfnpix/pars.nslice):ceil((i1+1)*halfnpix/pars.nslice),*,*] * $
          gridpn[floor(i1*halfnpix/pars.nslice):ceil((i1+1)*halfnpix/pars.nslice),*,*],1) / gridpnyz
        
        bx_drape_preview, opt_preview, valmyz, valpyz, minmaxval=pars.minmaxval, mse=mse
      endfor
      return
    endif
    
    

  endif
  
  return
endif

;------------------------------end of preview----------------------------------;



;---------------------------Start of Part 2------------------------------------;
; some constants
Rmars = 3389D
npix = 201
halfnpix = floor(npix/2)
if ~keyword_set(orbstart) then orbstart0 = 212 else orbstart0 = orbstart & undefine, orbstart
if ~keyword_set(orbend) then orbend0 = 7640 else orbend0 = orbend & undefine, orbend


; PARALLELIZATION
if keyword_set(parallel) then begin  
  
  ; Setting start/end point
  if keyword_set(config) then paraconfig = config else begin
    read,'Number of chunks? ',nchunk
    read,'Serial Number of this chunk? ',serinum
    paraconfig = create_struct(name='paraconfig','nchunk',nchunk,'serinum',serinum) 
  endelse
  nchunk = paraconfig.nchunk
  serinum = paraconfig.serinum
  orbstep = (orbend0-orbstart0)/nchunk
  if serinum eq nchunk then begin
    orbstart = floor(orbstart0 + (serinum-1)*orbstep)
    orbend = ceil(orbend0)
  endif else begin
    orbstart = floor(orbstart0 + (serinum-1)*orbstep)
    orbend = ceil(orbstart0 + serinum*orbstep)
  endelse  
  
  ; Setting the path
  savpath = savpath +'para'+'/'
  
  ; Setting the savname
  if nchunk lt 100 then begin
      savname = strcompress(string(long(serinum)),/remove_all) + $
        '-npix' + strcompress(string(npix),/remove_all)
      if keyword_set(mse) then savname = savname + '-mse'
      savname = savname + '.sav'
  endif else begin
    print,'Too many chunks!'
    return
  endelse
    
endif else begin
  orbstart = orbstart0
  orbend = orbend0
endelse


; Initialize the ratio grid 
; Grid for +By IMF
gridp = replicate(create_struct('n',0,'val',!values.f_nan),npix,npix,npix)  
; Grid for -By IMF
gridm = replicate(create_struct('n',0,'val',!values.f_nan),npix,npix,npix)  


; restore imf information
mvn_imf_info, proxy=proxy, savpath=imfinfopath, savname=imfinfoname, /noexe
restore,imfinfopath+imfinfoname

;--------------------- Start of Main Loop ---------------------;
; loop from orbstart to orbend
for orb = orbstart, orbend-1 do begin

  ; find the corresponding IMF info struct
  idx = where(imfinfo.orb eq orb)
  if idx[0] eq -1 then continue
  imfinfo0 = imfinfo[idx[0]]
    
  t1 = mvn_orbit_num(orbnum = orb)
  t2 = mvn_orbit_num(orbnum = orb+1)
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

    ; convert to MSE system (if required)
    if keyword_set(mse) then begin
      yp = y*cos(imfinfo0.rottheta) + z*sin(imfinfo0.rottheta)
      zp = z*cos(imfinfo0.rottheta) - y*sin(imfinfo0.rottheta)
      y = yp
      z = zp
    endif
 
    ; Add ratio to the grid
    ; +By IMF
    indx = round(x/rmars/3.*halfnpix+halfnpix)
    indy = round(y/rmars/3.*halfnpix+halfnpix)
    indz = round(z/rmars/3.*halfnpix+halfnpix)
    if imfinfo0.mag[1] gt 0 then begin
      ; Check whether the grid is filled
      if gridp[indx,indy,indz].n eq 0 then begin
        gridp[indx,indy,indz].val = ratio
        gridp[indx,indy,indz].n += 1  
      endif else begin
        n = gridp[indx,indy,indz].n
        gridp[indx,indy,indz].val = (gridp[indx,indy,indz].val*n+ratio)/(n+1)
        gridp[indx,indy,indz].n += 1
      endelse
    endif else if imfinfo0.mag[1] lt 0 then begin
      ; Check whether the grid is filled
      if gridm[indx,indy,indz].n eq 0 then begin
        gridm[indx,indy,indz].val = ratio
        gridm[indx,indy,indz].n += 1  
      endif else begin
        n = gridm[indx,indy,indz].n
        gridm[indx,indy,indz].val = (gridm[indx,indy,indz].val*n+ratio)/(n+1)
        gridm[indx,indy,indz].n += 1
      endelse
    endif

  end

end

;----------------------------end of main loop---------------------------;


; Save the result
savpath = savpath
if ~file_test(savpath,/directory) then file_mkdir,savpath
save, gridp, gridm, filename = savpath+savname


end