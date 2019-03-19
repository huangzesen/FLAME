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
; Survey each orbit to find the projection information

end