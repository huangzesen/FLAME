;+
; :Description:
;     Constructor
;-
function info::Init, _EXTRA=extra
  compile_opt idl2

  if (ISA(extra)) then $
    self->info::SetProperty, _EXTRA=extra
  return, 1
end

;+
; :Description:
;     Destructor
;-
pro info::Cleanup
  compile_opt idl2

end

;+
; :Description:
;     Accessor
;-
pro info::GetProperty, key=key, mission=mission, data=data, infostr=infostr, _REF_EXTRA=extra
  compile_opt idl2

  key = self.key
  mission = self.mission
  data = self.data
  info = create_struct('key',key,'mission',mission)
  datastr = *data
  tags = tag_names(datastr)
  for i1 = 0, n_elements(tags)-1 do begin
    info = create_struct(info, tags[i1], datastr.(i1))
  endfor
end

;+
; :Description:
;     Mutator
; Keywords:
;   KEY: key of the info
;   MISSION: mission code, can be 'MVN' or 'VEX'
;   DATA: pointer to the data
;-
pro info::SetProperty, key=key, mission=mission, data=data, infostr=infostr,  _EXTRA=extra
  compile_opt idl2
  
  if n_elements(infostr) then begin
    key = infostr.key
    mission = infostr.mission
    tags = tag_names(infostr)
    extract_tags, datastr, infostr, except=['key','mission']
    data = ptr_new(datastr)
  endif
  self.key = key
  self.mission = mission
  self.data = data
end

;+
; :Description:
;			Viewer
;-
pro info::View, figptr = figptr
  compile_opt idl2

  if self.data eq !NULL then begin
    print, 'Empty data!'
    return
  endif
  data = *(self.data)

  tags = TAG_NAMES(data)
  if total(strmatch(tags, 'figptr', /FOLD_CASE)) eq 0 then begin
    print, 'data has no tag FIGPTR.'
    return
  end 
  
  figptr = data.figptr
  if figptr ne !NULL then $
    SPAWN, 'gnome-open ' + *figptr else $
    print, 'No figure saved!'
  
  return
end

;+
; :Description:
;			Add tag
; Arguments:
;   name, value
;-  
function info::AddTag, name, value
  compile_opt idl2

  ; check data emptiness
  if self.data eq !NULL then begin
    print, 'Empty data!'
    return, self
  endif
  data = *(self.data)

  ; add the new tag
  data1 = create_struct(name, value, data)

  ; free old data and link new one
  ptr_free, self.data
  self.data = ptr_new(data1)
  
  return, self
end

;+
; :Description:
;			Remove tag
; Arguments:
;   name
;-
function info::RemTag, name
  compile_opt idl2

  ; check data emptiness
  if self.data eq !NULL then begin
    print, 'Empty data!'
    return, self
  endif
  data = *(self.data)

  ; remove the tag
  tags = TAG_NAMES(data)
  idx = where(strmatch(tags, name, /FOLD_CASE) eq 1, comp = idx1)
  if idx[0] eq 0 then begin
    print, 'Tag not found!'
    return, self
  endif
  newdata = create_struct(tags[idx1[0]], data.(idx1[0]))
  for i1 = 1, n_elements(idx1)-1 do begin
    newdata = create_struct(tags[idx1[i1]], data.(idx1[i1]), newdata)
  endfor
  
  ; free old data and link new one
  ptr_free, self.data
  self.data = ptr_new(newdata)
  
  return, self
end

;+
; :Description:
;			Import
; Arguments:
;   info
;-
function info::Import, infostr
  compile_opt idl2

  self.SetProperty, infostr=infostr
  
  return, self
end



;-------------------------------------------------------------------------------
;+
; :Description:
;     Class definition procedure
;-
pro info__define
  compile_opt idl2

  struct = {info, key: 0L, mission: '', data: PTR_NEW(), INHERITS IDL_Object}
end