with CORBA.Object;

package body CORBA.Forward is

   -------------
   -- Convert --
   -------------

   package body Convert is

      ------------------
      -- From_Forward --
      ------------------

      function From_Forward
        (The_Forward : in Ref)
         return Ref_Type
      is
         Result : Ref_Type;
      begin
         CORBA.Object.Ref (Result) := CORBA.Object.Ref (The_Forward);
         return Result;
      end From_Forward;

      ----------------
      -- To_Forward --
      ----------------

      function To_Forward
        (The_Ref : in Ref_Type)
         return Ref
      is
         Result : Ref;
      begin
         CORBA.Object.Ref (Result) := CORBA.Object.Ref (The_Ref);
         return Result;
      end To_Forward;

   end Convert;

end CORBA.Forward;

