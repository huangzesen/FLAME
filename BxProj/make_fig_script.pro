
figpath = '/home/huangzs/work/thesis/analysis/norm_bx_proj/figures/'

; restore files
files = file_search('/home/huangzs/work/thesis/analysis/norm_bx_proj/combine/','*.sav')
for i1 = 0, n_elements(files)-1 do print, strcompress(string(i1)), files[i1]
read, 'Select MSO: ', ind
restore,files[ind]

; get limits
wi, 0, xs=1000,ys=900
bx_drape_specplot, /noexe, limits = limits

; -By IMF MSO
limits = {xtitle: 'Y!LMSO!N(R!LM!N)', ytitle: 'Z!LMSO!N(R!LM!N)', title: '-B!LY!N IMF' }
bx_drape_specplot, gridm.val, gridm.n, limits = limits
makepng,'/home/huangzs/work/thesis/analysis/norm_bx_proj/figures/-By-IMF-MSO',/mkdir
stop

; +By IMF MSO
limits = {xtitle: 'Y!LMSO!N(R!LM!N)', ytitle: 'Z!LMSO!N(R!LM!N)', title: '+B!LY!N IMF' }
bx_drape_specplot, gridp.val, gridp.n, limits = limits
makepng,'/home/huangzs/work/thesis/analysis/norm_bx_proj/figures/+By-IMF-MSO'
stop

; restore files
files = file_search('/home/huangzs/work/thesis/analysis/norm_bx_proj/combine/','*.sav')
for i1 = 0, n_elements(files)-1 do print, strcompress(string(i1)), files[i1]
read, 'Select MSE: ', ind
restore,files[ind]

; -By IMF MSE
limits = {xtitle: 'Y!LMSE!N(R!LM!N)', ytitle: 'Z!LMSE!N(R!LM!N)', title: '-B!LY!N IMF' }
bx_drape_specplot, gridm.val, gridm.n, limits = limits
makepng,'/home/huangzs/work/thesis/analysis/norm_bx_proj/figures/-By-IMF-MSE'
stop

; +By IMF MSE
limits = {xtitle: 'Y!LMSE!N(R!LM!N)', ytitle: 'Z!LMSE!N(R!LM!N)', title: '+B!LY!N IMF' }
bx_drape_specplot, gridp.val, gridp.n, limits = limits
makepng,'/home/huangzs/work/thesis/analysis/norm_bx_proj/figures/+By-IMF-MSE'
stop

; no IMF sector MSE
limits = {xtitle: 'Y!LMSE!N(R!LM!N)', ytitle: 'Z!LMSE!N(R!LM!N)', title: 'No IMF Sector (MSE)' }
valm = gridm.val
valp = gridp.val
fm = finite(valm)
fp = finite(valp)
idxm = where((fm+fp) gt 0 and fm eq 0)
idxp = where((fm+fp) gt 0 and fp eq 0)
valm[idxm] = 0
valp[idxp] = 0
val = (valm*gridm.n+valp*gridp.n)/(gridm.n+gridp.n)
gridn = gridm.n+gridp.n
bx_drape_specplot, val, gridn, limits = limits
makepng,'/home/huangzs/work/thesis/analysis/norm_bx_proj/figures/noIMFsector'
stop


end