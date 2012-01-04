------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 S O A P . M E S S A G E . P A Y L O A D                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

package body SOAP.Message.Payload is

   -----------
   -- Build --
   -----------

   function Build
     (Procedure_Name : String;
      P_Set          : SOAP.Parameters.List;
      Name_Space     : String               := Default_Name_Space)
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

end SOAP.Message.Payload;
