--  Base data types for the whole middleware.

--  $Id$

package body PolyORB.Types is

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   function To_PolyORB_String
     (Source : Standard.String)
     return Types.String is
   begin
      return Types.String
        (Ada.Strings.Unbounded.To_Unbounded_String
         (Source));
   end To_PolyORB_String;

   function To_Standard_String
     (Source : Types.String)
     return Standard.String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (Source));
   end To_Standard_String;

   function To_PolyORB_Wide_String
     (Source : Standard.Wide_String)
     return Types.Wide_String is
   begin
      return Types.Wide_String
        (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String
         (Source));
   end To_PolyORB_Wide_String;

   function To_Standard_Wide_String
     (Source : Types.Wide_String)
     return Standard.Wide_String is
   begin
      return Ada.Strings.Wide_Unbounded.To_Wide_String
        (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String
         (Source));
   end To_Standard_Wide_String;


end PolyORB.Types;
