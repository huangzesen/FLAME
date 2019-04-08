;+
; :Description:
;     Constructor
;-
function frinfo::Init, _EXTRA=extra
  compile_opt idl2

  if (ISA(extra)) then $
    self->frinfo::SetProperty, _EXTRA=extra
  return, 1
end

;+
; :Description:
;     Destructor
;-
pro frinfo::Cleanup
  compile_opt idl2

end

;+
; Procedure: GetProperty
; :Description:
;     Accessor
;-
pro frinfo::GetProperty, key=key, mission=mission, data=data, infostr=infostr, _REF_EXTRA=extra
  compile_opt idl2
  key = self.key
  mission = self.mission
  data = self.data
  datastr = *data
  extract_tags, infostr, datastr
end

;+
; Procedure: SetProperty
; :Description:
;     Mutator
; Keywords:
;   KEY: key of the info
;   MISSION: mission code, can be 'MVN' or 'VEX'
;   DATA: pointer to the data
;-
pro frinfo::SetProperty, key=key, mission=mission, data=data, infostr=infostr,  _EXTRA=extra
  compile_opt idl2
  
  if n_elements(infostr) then begin
    key = infostr.key
    mission = infostr.mission
    tags = tag_names(infostr)
    extract_tags, datastr, infostr
    data = ptr_new(datastr)
  endif
  if n_elements(key) then self.key = key
  if n_elements(mission) then self.mission = mission
  if n_elements(data) then self.data = data
end

;+
; Procedure: View
; :Description:
;			Viewer
;-
pro frinfo::View, figptr = figptr
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
  if figptr ne !NULL and file_test(*figptr) then $
    SPAWN, 'gnome-open ' + *figptr else $
    print, 'No figure saved!'
  
  return
end

;+
; Procedure: AddTag
; :Description:
;			Add tag
; Arguments:
;   name, value
; Keywords:
;   type
;-  
pro frinfo::AddTag, name, value, type=type
  compile_opt idl2

  ; check data emptiness
  if self.data eq !NULL then begin
    print, 'Empty data!'
    return
  endif
  data = *(self.data)

  ; add the new tag
  data1 = flm_info_addtag(data, name, vartype=type, defval=value)

  ; free old data and link new one
  ptr_free, self.data
  self.data = ptr_new(data1)
  
  return
end

;+
; Procedure: RemTag
; :Description:
;			Remove tag
; Arguments:
;   name
;-
pro frinfo::RemTag, name
  compile_opt idl2

  ; check data emptiness
  if self.data eq !NULL then begin
    print, 'Empty data!'
    return
  endif
  data = *(self.data)

  ; remove the tag
  tags = TAG_NAMES(data)
  idx = where(strmatch(tags, name, /FOLD_CASE) eq 1, comp = idx1)
  if idx[0] eq 0 then begin
    print, 'Tag not found!'
    return
  endif
  newdata = create_struct(tags[idx1[0]], data.(idx1[0]))
  for i1 = 1, n_elements(idx1)-1 do begin
    newdata = create_struct(tags[idx1[i1]], data.(idx1[i1]), newdata)
  endfor
  
  ; free old data and link new one
  ptr_free, self.data
  self.data = ptr_new(newdata)
  
  return
end

;+
; Procedure: Import
; :Description:
;			Import from a info structure
; Arguments:
;   infostr
;-
pro frinfo::Import, infostr
  compile_opt idl2

  self.SetProperty, infostr=infostr
  
  return
end

;+
; Procedure: ShowTags
; :Description:
;     Show tags
;-
pro frinfo::ShowTags
  compile_opt idl2
  
  tags = ['KEY', 'MISSION', 'DATA']
  for i1 = 0, n_elements(tags)-1 do print,tags[i1]
  
  return
end

;+
; Procedure: GetTags
; :Description:
;			Get info struct tags
; Keywords:
;   tags
;-
pro frinfo::GetTags, tags = tags
  compile_opt idl2

  tags = tag_names(*(self.data))

  return
end

;+
; Procedure: LSetVal
; :Description:
;			Set label value
; if success then success=1 else success=0
; Arguments:
;   lname, val
;-
pro frinfo::LSetVal, lname, value, success=success
  compile_opt idl2

  self.GetTags, tags = tags
  idx = where(tags.contains(lname, /FOLD_CASE) eq 1)
  if idx[0] eq -1 then success = 0 else begin
    (*(self.data)).(idx) = value
    success = 1 
  endelse
  return
end

;+
; Procedure: LGetVal
; :Description:
;			Get label value
; if success then success=1 else success=0
; Arguments:
;   lname, val
;-
pro frinfo::LGetVal, lname, value, success=success
  compile_opt idl2

  self.GetTags, tags = tags
  idx =where(tags.contains(lname, /FOLD_CASE) eq 1)
  if idx[0] eq -1 then success = 0 else begin
    value = (*(self.data)).(idx)
    success = 1
  endelse
  return
end

;+
; Function: GetKey
; :Description:
;			Get Key
;-
function frinfo::GetKey
  compile_opt idl2
  return, self.key
end


;+
; Function: FindInfo
; :Description:
;	  Find a info in infotable based on SELF.KEY
;   NOTE: might cause trouble if you are finding a info from a different mission.
;     e.g. find a MVN info in VEX infotable
; Arguments:
;   infotable
;-
function frinfo::FindInfo, infotable, success = success
  compile_opt idl2

  key = self.key
  keys = []
  for i1 = 0, n_elements(infotable)-1 do keys = [keys, (infotable[i1]).GetKey()]
  idx = where(keys eq key)
  if idx[0] eq -1 or n_elements(idx) ne 1 then success = 0 else success = 1
  return, idx
end

;+ 
; Procedure: FigUpdate
; :Description:
;			Update figure
; Keywords:
;   OVERWRITE: overwrite the old figure
;-
pro frinfo::FigUpdate, overwrite = overwrite
  compile_opt idl2

  if self.data eq !NULL then begin
    print, 'Empty data!'
    return
  endif
  data = *(self.data)

  tags = TAG_NAMES(data)
  if total(strmatch(tags, 'figptr', /FOLD_CASE)) ne 0 $ 
    and ~keyword_set(overwrite) then begin
    
    print, 'Already has a figure.'
    print, 'Overwrite?'
    strflag = ''
    read, 'Type YES to overwrite: ', strflag
    if strflag ne YES then return
  end 

  ; Update figure
  switch (self.mission) of
    ('VEX'): begin
      flm_plot_main, *(self.data), getptr = figptr
      self.data.figptr = figptr
      break
    end
    ('MVN'): begin
      print, 'Under construction...'
      break
    end
    else: begin 
      print, 'Mission not supported.'
      ; do nothing
    end
  endswitch
  
  return
  
end


;-------------------------------------------------------------------------------
;+
; :Description:
;     Class definition Procedure
;-
pro frinfo__define
  compile_opt idl2

  struct = {frinfo, key: 0L, mission: '', data: PTR_NEW(), INHERITS IDL_Object}
end