------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . S O A P _ P . M E S S A G E . P A Y L O A D        --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Unchecked_Deallocation;
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

   procedure Do_Free is new Ada.Unchecked_Deallocation
     (Object'Class, Object_Access);
   procedure Free (X : in out Object_Access) renames Do_Free;

end PolyORB.SOAP_P.Message.Payload;
