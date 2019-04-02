flm_info_restore,infotable0

;infotable = flm_info_addtag(infotable0, 'figptr', vartype = 'pointer')

t0 = systime(/seconds)

for i1 = 0, n_elements(infotable)-1 do begin
  if infotable[i1].figptr eq !NULL then continue
  flm_plot_main, infotable[i1], getptr = ptr
  infotable[i1].figptr = ptr
  print, strcompress(string(i1)) + ' out of ' $
    + strcompress(string(n_elements(infotable))) + ' finished.'
  t1 = systime(/seconds)
  catch, error_status
  if error_status ne 0 then begin
    print, 'Error index: ', Error_status
    print, 'Error message: ', !error_state.msg
  endif
endfor

stop

flm_info_save,infotable
end