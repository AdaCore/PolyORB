-- ==================================================== --
-- ===  Code automatically generated by IDL to Ada  === --
-- ===  compiler OrbAda-idl2ada                     === --
-- ===  Copyright Top Graph'X  1997                 === --
-- ==================================================== --
with Corba.Object ;
package Vehicle is

   type Ref is new Corba.Object.Ref with null record;

   function To_Vehicle (Self : in Corba.Object.Ref'class) return Ref'class;

   function To_Ref (From : in Corba.Any) return Ref;
   function To_Any (From : in Ref) return Corba.Any;

   function mark_Of
      (Self : in Ref) return Corba.String;

   procedure Set_mark
      (Self : in out Ref;
       To : in Corba.String);

   function can_drive
      ( Self : in Ref;
        age : in Corba.Unsigned_Short)
         return Corba.Boolean ;

   Null_Ref : constant Ref := (Corba.Object.Null_Ref with null record);

   Tgx_Service_Name : Corba.ObjectId := Corba.To_Unbounded_String
      ("vehicle") ;

   Vehicle_R_Id : constant Corba.RepositoryId :=
      Corba.To_Unbounded_String ("IDL:Vehicle:1.0") ;
end Vehicle;


