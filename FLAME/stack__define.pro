;+
; Init
; :Description:
;     Constructor
;-
function stack::Init, _EXTRA=extra
  compile_opt idl2

  if (ISA(extra)) then $
    self->stack::SetProperty, _EXTRA=extra

  return, 1
end

;+
; Cleanup
; (disabled, cause IDL crush)
; :Description:
;     Destructor
;-
;function stack::Cleanup
;  compile_opt idl2
;  elem = self.next
;  while (elem ne !NULL) do begin
;    elem = *elem
;    ptr_free, elem.data
;    elem = elem.next
;  endwhile
;  return, 1
;end

;+
; GetProperty()
; Keyword:
;   data, next
; :Description:
;     Accessor
;-
pro stack::GetProperty, data=data, next=next,  _REF_EXTRA=extra
  compile_opt idl2
  if self.data ne !NULL then data = *(self.data) else data = !NULL
  next = self.next
end

;+
; SetProperty()
; Keyword:
;   data, next
; :Description:
;     Mutator
;-
pro stack::SetProperty, data=data, next=next, _EXTRA=extra
  compile_opt idl2
  if n_elements(data) then self.data=ptr_new(data)
  if n_elements(next) then self.next=next
end

;+
; IsEmpty()
; :Description:
;     Check stack emptiness
;-
function stack::IsEmpty, _EXTRA=extra
  compile_opt idl2

  if self.next eq !NULL then return, 1 else return, 0

end

;+
; GetTop()
; :Description:
;     get the top element
;-
function stack::GetTop, _Extra=extra
  compile_opt idl2

  return, self.next
end

;+
; Push
; Arguments:
;   ptr: pointer to the new element
; :Description:
;     This procedure pushes a new elem to the stack
;-
pro stack::Push, data, _Extra=extra
  compile_opt idl2
  ptr = ptr_new(data)
  elem = obj_new('stack', data=ptr)
  elem.next = self.next
  self.next = ptr_new(elem)
  
  return
end

;+
; Pop()
; :Description:
;     This procedure pops the top element from the stack
;-
function stack::Pop, _Extra=extra
  compile_opt idl2

  if self.IsEmpty() then return, !NULL else begin
    elem = *(self.next)
    nextelem = *(self.next)
    self.next = nextelem.next
    return, *(elem.data)
  endelse
  
end

;+
; Save()
; Keyword: 
;   filename
; :Description:
;     This procedure saves the stack to filename
;-
function stack::Save, filename=filename, _Extra=extra
  compile_opt idl2
  
  if keyword_set(filename) then save, self, filename=filename
  
  return,1
end

;+
; len()
; :Description:
;			length of the stack
;-
function stack::Len
  compile_opt idl2

  len = 0
  elem = self
  while(elem.next ne !NULL) do begin
    elem = *(self.next)
    len = len + 1
  endwhile
  return, len
end


;-------------------------------------------------------------------------------
;+
; Class Stack
; Properties: data, next
; :Description:
;     Class definition procedure
;-
pro stack__define
  compile_opt idl2

  struct = {stack, data:ptr_new(), next:ptr_new(), inherits IDL_Object}
  return
end