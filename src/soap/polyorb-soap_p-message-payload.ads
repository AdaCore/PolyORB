------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . S O A P _ P . M E S S A G E . P A Y L O A D        --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Utils.Unchecked_Deallocation;
with PolyORB.SOAP_P.Parameters;

package PolyORB.SOAP_P.Message.Payload is

   type Object is new Message.Object with private;
   type Object_Access is access Object'Class;

   function Procedure_Name (P : Object'Class) return String;
   --  Retruns the Payload procedure name.

   procedure Set_Procedure_Name (P : in out Object'Class; Name : String);
   --  Set the payload procedure name.

   function Build
     (Procedure_Name : String;
      P_Set          : SOAP_P.Parameters.List;
      Name_Space     : String               := "")
     return Object;
   --  Retruns a Payload object initialized with the procedure name,
   --  parameters and name space.

   procedure Free (X : in out Object_Access);
private

   type Object is new Message.Object with null record;

   procedure Do_Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Object'Class,
      Name => Object_Access);
   procedure Free (X : in out Object_Access) renames Do_Free;

end PolyORB.SOAP_P.Message.Payload;
