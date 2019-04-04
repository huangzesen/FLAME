flm_info_restore,infotable,mission='MVN'
infos = []
for i1 = 0, n_elements(infotable)-1 do begin
frpar = infotable[i1].frpar
key = flm_info_keygen(frpar,infos)
info = create_struct('key',key,infotable[i1])
infos = [infos,info]
end
end