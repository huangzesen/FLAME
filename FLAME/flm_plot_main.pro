;+
;Procedure:	flm_plot_main
;
;PURPOSE:	
; This procedure draw preview of a case, identified by info structure. After
;   drawing the preview, the procedure will return a pointer to the figure or a
;   NULL pointer.
;
;USAGE:	
; flm_plot_main, info, [getptr = getptr], [figpath = figpath]
;   
;INPUTS:	
; INFO: flux rope information structure
;
;OUTPUTS:
; FIGPTR: pointer to the preview figure saved on disk or !NULL.
; 
;KEYWORDS:	
; GETPTR: return the pointer to the flux rope preview figure WITHOUT drawing it.
; FIGPATH: path to the figure
;
;CREATED BY: 	 huangzs on Mar 31, 2019
;UPDATES:	
;
;-


pro flm_plot_hodogram, time, mag, dir = dir
; dir: 
;   1: Y-Z plane
;   2: X-Z plane
;   3: X-Y plane
; mag:
;   should be [n,3] array

compile_opt idl2

magtot = sqrt(total(mag^2,2))
xr = minmax(mag[*,0])
yr = minmax(mag[*,1])
zr = minmax(mag[*,2])
delta = max([xr[1]-xr[0],yr[1]-yr[0],zr[1]-zr[0]])/2 * 1.1

if delta eq 0 then return

switch (dir) of
  (1): begin
    v1 = mag[*,1]
    v2 = mag[*,2]
    xtitle = 'Int/nT'
    ytitle = 'Min/nT'
    break
  end
  (2): begin
    ; Max-Min
    v2 = mag[*,0]
    v1 = mag[*,2]
    ytitle = 'Max/nT'
    xtitle = 'Min/nT'
    title = 'Max-Min'
    break
  end
  (3): begin
    ; Max-Int
    v2 = mag[*,0]
    v1 = mag[*,1]
    ytitle = 'Max/nT'
    xtitle = 'Int/nT'
    title = 'Max-Int'
    break
  end
  else: begin 
    print, 'Wrong input!'
    return
  end
endswitch
xm = mean(minmax(v1))
ym = mean(minmax(v2))
plot, [0,0], xrange = [xm-delta,xm+delta], yrange = [ym-delta,ym+delta], $
  xtitle = xtitle, ytitle = ytitle, title = title, $
  /xstyle, /ystyle, $
  /nodata, /iso, /noerase, $
  xticks = 5, yticks = 5
oplot, v1, v2
plots, v1[0], v2[0], color = 1, psym = 4    ; magenta diamond
plots, v1[-1], v2[-1], color = 1, psym = 7  ; magenta X
end






pro flm_plot_main, info, getptr = getptr, figpath = figpath

compile_opt idl2

if ~keyword_set(figpath) then figpath = $
  '/home/huangzs/work/thesis/analysis/vex/figure/'

t1 = info.frpar.time[0]
dt = info.frpar.dt
mt = info.frpar.mtime
if dt lt 60 then trange = [mt-150, mt+150] else trange = [t1-2*dt, t1+3*dt]

; load mag
vex_mag_load, trange, result = result, /no_server


; make canvas
wi, 0, xs = 1400, ys = 1000


; MAG SUMMARY PLOT
; at the left
;!p.position = [0.05, 0.35, 0.35, 0.95]

; tplot names: vex_mag_l3_bvso_1sec vex_mag_l3_btot_1sec
;   VSO: Venus Solar Orbital, TOT: abbreviation for TOTAL
mag = result.mag
time = result.time
magtot = sqrt(total(mag^2,2))
; change to MVA coordinate
evec=[[info.frpar.evec.max],[info.frpar.evec.int],[info.frpar.evec.min]]
magmva=evec##mag
idx = where(time ge t1 and time le t1+dt)
time1 = time[idx]
mag1 = mag[idx,*]
yrange = minmax(mag1)*1.5
store_data, 'vex_mag_l3_bmva_1sec', data = {x:time, y:magmva}
options, 'vex_mag_l3_bmva_1sec', colors = [2,4,6], $
  labels = ['MAX','INT','MIN'], labflag = 1, yrange = yrange
tplot_options, 'region', [0.05, 0.30, 0.50, 0.95]

tplot, 'vex_mag_l3_bvso_1sec'
tplot, 'vex_mag_l3_btot_1sec', add = 2 
tplot, 'vex_mag_l3_bmva_1sec', add = 2

timebar, info.frpar.trange[0], linestyle = 2, color = 1 ; magenta dashed line
timebar, info.frpar.bipolar.tbound[0], linestyle = 2, color = 3 ; sky-blue dashed line
timebar, info.frpar.trange[1], linestyle = 2, color = 1 ; magenta dashed line
timebar, info.frpar.bipolar.tbound[1], linestyle = 2, color = 3 ; sky-blue dashed line


; HODOGRAM
; at lower right
!p.position = [0.05, 0.05, 0.30, 0.30]
if n_elements(time1) gt 2 then flm_plot_hodogram, time1, mag1, dir = 3

!p.position = [0.30, 0.05, 0.55, 0.30]
if n_elements(time1) gt 2 then flm_plot_hodogram, time1, mag1, dir = 2


; EPHEMERIS
; later...


; ID STAMP
!p.position = [0., 0., 0., 0.]
flm_plot_stamp, info


; MAKE PNG
timestr = time_string(info.frpar.mtime, format = 6)
year = timestr.substring(0,3)
month = timestr.substring(4,5)
figpath1 = figpath + year + '/' + month + '/'
file_mkdir, figpath1
figname = strcompress(string(info.key), /remove_all)
savname = figpath1 + figname
makepng, savname

; return the savname pointer
getptr = ptr_new(savname+'.png') 


end