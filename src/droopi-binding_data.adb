--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

--  $Id$

with Ada.Unchecked_Deallocation;

package body Droopi.Binding_Data is

   procedure Destroy_Profile (P : in out Profile_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Profile_Type'Class, Profile_Access);
   begin
      Free (P);
   end Destroy_Profile;

end Droopi.Binding_Data;
