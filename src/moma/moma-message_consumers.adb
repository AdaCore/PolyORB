------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E _ C O N S U M E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body MOMA.Message_Consumers is

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      null;
   end Close;

   --------------------------
   -- Get_Message_Selector --
   --------------------------

   function Get_Message_Selector return String is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_Message_Selector;
      pragma Warnings (On);
   end Get_Message_Selector;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination (Self : Message_Consumer)
                             return MOMA.Destinations.Destination is
   begin
      return Self.Destination;
   end Get_Destination;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref (Self : Message_Consumer) return PolyORB.References.Ref is
   begin
      return Self.Ref;
   end Get_Ref;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination (Self : in out Message_Consumer'Class;
                              Dest : MOMA.Destinations.Destination) is
   begin
      Self.Destination := Dest;
   end Set_Destination;

   -------------
   -- Set_Ref --
   -------------

   procedure Set_Ref (Self : in out Message_Consumer;
                      Ref  : PolyORB.References.Ref) is
   begin
      Self.Ref := Ref;
   end Set_Ref;

end MOMA.Message_Consumers;

