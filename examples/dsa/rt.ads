package RT is
   pragma Remote_Types;

   type Obj is abstract tagged limited private;
   procedure Method (Self : Obj) is abstract;
   procedure Method2 (Self : Obj; N : Integer) is abstract;
   procedure Method3 (Self : Obj; Other : Obj) is abstract;
   type RACW is access all Obj'Class;

private

   type Obj is abstract tagged limited null record;

end RT;
