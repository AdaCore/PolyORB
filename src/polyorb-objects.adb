--  Root type for concrete object implementations (servants).

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Utils;

package body PolyORB.Objects is

   procedure Free (X : in out Object_Id_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object_Id, Object_Id_Access);
   begin
      Free (X);
   end Free;

   function To_String (Oid : Object_Id) return String is
   begin
      return Utils.To_String (Stream_Element_Array (Oid));
   end To_String;

   function To_Oid (S : String) return Object_Id is
   begin
      return Object_Id (Utils.To_Stream_Element_Array (S));
   end To_Oid;

   function Image (Oid : Object_Id) return String
     renames To_String;

end PolyORB.Objects;
