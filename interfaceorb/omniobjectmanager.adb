-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                                                               ----
----     This package is wrapped around the C class                ----
----   omniObjectManager declared in omniInternal.h.               ----
----     It provides the sames functions as the C class (ie        ----
----   one function : NilObjectManager).                           ----
----                                                               ----
----                                                               ----
----                  package body omniObjectManager               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/18/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with System.Address_To_Access_Conversions ;

package body OmniObjectManager is

   -- Address_To_OmniObjectManager
   -------------------------------
   package Address_To_OmniObjectManager is
     new System.Address_To_Access_Conversions (Object) ;
   -- needed to interface System.Address and Object


   -- C_Nil_Object_Manager
   -----------------------
   function C_Nil_Object_Manager return System.Address ;
   pragma Import (C,C_Nil_Object_Manager,"nilObjectManager__10omniObject") ;
   -- wrapper around static omniObjectManager* nilObjectManager();
   -- (see omniInternal.h L 514)
   -- called by the Ada equivalent : Nil_Object_Manager


   -- Nil_Object_Manager
   ---------------------
   function Nil_Object_Manager return Object is
      C_Result : System.Address ;
      Ada_Result_Ptr : Address_To_OmniObjectManager.Object_Pointer ;
   begin
      -- calls the C function C_Nil_Object_Manager ...
      C_Result := C_Nil_Object_Manager ;
      -- ... and tranforms the result in Ada type
      Ada_Result_Ptr := Address_To_OmniObjectManager.To_Pointer (C_Result) ;
      return Ada_Result_Ptr.all ;
   end;

end OmniObjectManager;
