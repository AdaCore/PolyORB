------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . S O A P _ P . M E S S A G E . P A Y L O A D        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

package body PolyORB.SOAP_P.Message.Payload is

   -----------
   -- Build --
   -----------

   function Build
     (Procedure_Name : String;
      P_Set          : SOAP_P.Parameters.List;
      Name_Space     : String               := "")
     return Object is
   begin
      return (To_Unbounded_String (Name_Space),
              To_Unbounded_String (Procedure_Name),
              P_Set);
   end Build;

   --------------------
   -- Procedure_Name --
   --------------------

   function Procedure_Name (P : Object'Class) return String is
   begin
      return Wrapper_Name (P);
   end Procedure_Name;

   ------------------------
   -- Set_Procedure_Name --
   ------------------------

   procedure Set_Procedure_Name (P : in out Object'Class; Name : String) is
   begin
      Set_Wrapper_Name (P, Name);
   end Set_Procedure_Name;

end PolyORB.SOAP_P.Message.Payload;
