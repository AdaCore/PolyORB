generic
   type Object_Access is private;
   type Enum_Type is (<>);

package Generic_Factory is

   type Factory is array (Enum_Type'First .. Enum_Type'Last) of Object_Access;
   type Factory_Access is access all Factory;

   function New_Factory return Factory_Access;

   function Create (F    : Factory;
                    Enum : Enum_Type)
                   return Object_Access;

   procedure Register (F      : in out Factory;
                       Kind   :        Enum_Type;
                       Object :        Object_Access);

end Generic_Factory;
