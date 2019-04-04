PRO class1::print1
  print,self.data1
end

PRO class2::print2
  print,self.data1
  print,self.data2a, self.data2b
end

; Main 
struct = { class1, data1:0.0 }
struct = { class2, data2a:0, data2b:0L, INHERITS class1 }
A = OBJ_NEW('class1')
B = OBJ_NEW('class2') 
A.Print1
B->Print2
end