-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around a C++ class whose name     ----
----   is BOA declared in file CORBA.h.                            ----
----     It provides two types of methods : the C functions        ----
----   of the Ada_OmniObject class and their equivalent in         ----
----   Ada. (the first ones have a C_ prefix.)                     ----
----                                                               ----
----                                                               ----
----                  package body Boa                             ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/24/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with System.Address_To_Access_Conversions ;

package body Corba.Boa is


   -- Object_Is_Ready
   -------------------
   procedure Object_Is_Ready(Self: in Object'Class ;
                             Obj: in Omniobject.Implemented_Object'Class ) is
   begin
      Omniobject.Object_Is_Ready(Obj) ;
   end ;

   -- Object_Is_Ready
   -------------------
   procedure Object_Is_Ready(Self: in Object'Class ;
                             Obj: in Corba.Object.Ref'Class ) is
   begin
      Corba.Object.Object_Is_Ready(Obj) ;
   end ;



   -- Address_To_Object
   --------------------
   package Address_To_Object is
     new System.Address_To_Access_Conversions (Object) ;
   -- needed to convert System.Address into Object


--   -- C_Get_Boa
--   ----------
--   function C_Get_Boa (Self: in Object'Class)
--                      return System.Address ;
--   pragma Import (C,C_Get_Boa,"toto") ;
--   -- wrapper around BOA function getBOA
--   -- (see CORBA.h)
--
--   -- Get_Boa
--   ----------
--   function Get_Boa (Self: in Object'Class)
--                    return Object'Class is
--      C_Result : System.Address ;
--   begin
--      -- calls the C function ...
--      C_Result := C_Get_Boa (Self) ;
--      -- ... and transforms the result into an Ada type
--      return Address_To_Object.To_Pointer(C_result).All ;
--   end ;
--
--
--   -- C_Dispose
--   ------------
--   procedure C_Dispose (Self : in Object'Class ;
--                        object : in System.Address) ;
--   pragma Import (C,C_Dispose,"toto") ;
--   -- wrapper around BOA function dispose
--   -- (see CORBA.h)
--
--   -- Dispose
--   ----------
--   procedure Dispose(Self: in Object'Class ;
--                     Obj : in Corba.Object.Ref'Class) is
--      C_Obj : System.Address ;
--   begin
--      -- transforms the arguments into a C type ...
--      C_Obj := Obj'Address ;
--      -- ... and calls the C procedure
--      C_Dispose (Self,C_Obj);
--   end ;


end Corba.Boa ;






