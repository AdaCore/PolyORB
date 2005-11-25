------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . C O R B A _ P . P O L I C Y                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package body PolyORB.CORBA_P.Policy is

   ---------------------
   -- Get_Policy_Type --
   ---------------------

   function Get_Policy_Type
     (Self : Policy_Object_Type)
     return CORBA.PolicyType is
   begin
      return Self.Policy;
   end Get_Policy_Type;

   ---------------------
   -- Set_Policy_Type --
   ---------------------

   procedure Set_Policy_Type
     (Self   : in out Policy_Object_Type;
      Policy :        CORBA.PolicyType) is
   begin
      Self.Policy := Policy;
   end Set_Policy_Type;

   ----------------------
   -- Get_Policy_Value --
   ----------------------

   function Get_Policy_Value
     (Self : Policy_Object_Type)
     return CORBA.Any is
   begin
      return Self.Value;
   end Get_Policy_Value;

   ----------------------
   -- Set_Policy_Value --
   ----------------------

   procedure Set_Policy_Value
     (Self  : in out Policy_Object_Type;
      Value :        CORBA.Any) is
   begin
      Self.Value := Value;
   end Set_Policy_Value;

end PolyORB.CORBA_P.Policy;
