package body PolyORB.CORBA_P.Policy is

   ---------------------
   -- Get_Policy_Type --
   ---------------------

   function Get_Policy_Type
     (Self : Policy_Object_Type)
     return CORBA.PolicyType is
   begin
      return Self.Policy;
   end Get_Policy_Type;

   ---------------------
   -- Set_Policy_Type --
   ---------------------

   procedure Set_Policy_Type
     (Self   : in out Policy_Object_Type;
      Policy :        CORBA.PolicyType) is
   begin
      Self.Policy := Policy;
   end Set_Policy_Type;

   ----------------------
   -- Get_Policy_Value --
   ----------------------

   function Get_Policy_Value
     (Self : Policy_Object_Type)
     return CORBA.Any is
   begin
      return Self.Value;
   end Get_Policy_Value;

   ----------------------
   -- Set_Policy_Value --
   ----------------------

   procedure Set_Policy_Value
     (Self  : in out Policy_Object_Type;
      Value :        CORBA.Any) is
   begin
      Self.Value := Value;
   end Set_Policy_Value;

end PolyORB.CORBA_P.Policy;
