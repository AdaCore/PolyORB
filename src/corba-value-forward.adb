package body CORBA.Value.Forward is

   package body Convert is

      -------------------
      --  From_Forward --
      -------------------
      function From_Forward (The_Forward : in Value_Ref) return Ref_Type is
	 Result : Ref_Type;
      begin
	 Result.Ptr := The_Forward.Ptr;
	 return Result;
      end From_Forward;

      -----------------
      --  To_Forward --
      -----------------
      function To_Forward (The_Ref : in Ref_Type) return Value_Ref is
	 Result : Value_Ref;
      begin
	 Result.Ptr := The_Ref.Ptr;
	 return Result;
      end To_Forward;
      
   end Convert;
   
   
end CORBA.Value.Forward;
