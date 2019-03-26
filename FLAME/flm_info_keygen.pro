;+
;Function:	flm_info_keygen()
;
;PURPOSE:	
; This procedure generate key of a flux rope candidate:
;   (2-digits-random-integer)+(yymmdd)
;
;USAGE:	
; flm_info_keygen(FRPAR, [INFOTABLE])   
;
;INPUTS:	
; 
; FRPAR: Flux rope parameter structure, must include a keyword named 'mtime'.
;   'mtime' should be a double-precision unix-style time variable.
; 
; INFOTABLE: Flux rope information structure array, must include a keyword named
;   'key'. 'key' should be a 7-digits long integer following the naming convention
;   defined in this procedure.  
;
;KEYWORDS:	
; None
;
;CREATED BY: 	 huangzs on Mar 25, 2019
;UPDATES:	
;
;-

function flm_info_keygen, frpar, infotable

compile_opt idl2

if keyword_set(infotable) then keys = infotable.key

; random array
rarr = long(randomu(seed, 1000, /double) * 90D + 10D)

; time string
timestr = time_string(frpar.mtime, format = 6)
yy = long(timestr.substring(0,3)) mod 100
mm = long(timestr.substring(4,5))
dd = long(timestr.substring(6,7))

; create key
r = 0
id = long(rarr[r]*1e6 + yy*1e4 + mm*1e2 + dd)

; check duplication
if keyword_set(keys) then begin
  ind = where(keys eq id)
  while(ind[0] ne -1) do begin
    if r gt 10 then stop
    r += 1
    id = long(rarr[r]*1e6 + yy*1e4 + mm*1e2 + dd)
    ind = where(keys eq id)
  endwhile
endif

key = id
return, key

end