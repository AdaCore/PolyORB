--  Base types for the various configuration axes (policies)
--  of the DROOPI Portable Object Adapter (libreally inspired from
--  the POA specification in CORBA).

--  $Id$

package body Droopi.POA_Policies is

   function Policy_Id (Self : Policy) return String is
   begin
      return Policy_Value'Image (Self.Value);
   end Policy_Id;

end Droopi.POA_Policies;
