package body Generic_Factory is

   -----------------
   -- New_Factory --
   -----------------

   function New_Factory return Factory_Access
   is
      F : Factory_Access := new Factory;
   begin
      return F;
   end New_Factory;

   ------------
   -- Create --
   ------------

   function Create (F    : Factory;
                    Enum : Enum_Type)
                   return Object_Access
   is
   begin
      return F (Enum);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register (F      : in out Factory;
                       Kind   :        Enum_Type;
                       Object :        Object_Access)
   is
   begin
      F (Kind) := Object;
   end Register;

end Generic_Factory;
