package body CORBA.Value.Forward is

   package body Convert is

      -------------------
      --  From_Forward --
      -------------------

      function From_Forward
        (The_Forward : in Value_Ref)
        return Entity is
         Result : Entity;
      begin
         Set (Result, Object_Of (The_Forward));
         return Result;
      end From_Forward;

      -----------------
      --  To_Forward --
      -----------------

      function To_Forward
        (The_Ref : in Entity)
        return Value_Ref is
         Result : Value_Ref;
      begin
         Set (Result, Object_Of (The_Ref));
         return Result;
      end To_Forward;

   end Convert;

end CORBA.Value.Forward;
