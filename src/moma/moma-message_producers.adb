------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E _ P R O D U C E R S                --
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

package body MOMA.Message_Producers is

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      null;
   end Close;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination (Self : Message_Producer)
                             return MOMA.Destinations.Destination is
   begin
      return Self.Destination;
   end Get_Destination;

   --------------------
   -- Get_Persistent --
   --------------------

   function Get_Persistent (Self : Message_Producer) return Boolean is
   begin
      return Self.Persistent;
   end Get_Persistent;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (Self : Message_Producer) return Priority is
   begin
      return Self.Priority_Level;
   end Get_Priority;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref (Self : Message_Producer) return PolyORB.References.Ref is
   begin
      return Self.Ref;
   end Get_Ref;

   ----------------------
   -- Get_Time_To_Live --
   ----------------------

   function Get_Time_To_Live (Self : Message_Producer) return Time is
   begin
      return Self.TTL;
   end Get_Time_To_Live;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination (Self : in out Message_Producer'Class;
                              Dest : MOMA.Destinations.Destination) is
   begin
      Self.Destination := Dest;
   end Set_Destination;

   --------------------
   -- Set_Persistent --
   --------------------

   procedure Set_Persistent (Self : in out Message_Producer;
                             Persistent : Boolean) is
   begin
      Self.Persistent := Persistent;
   end Set_Persistent;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (Self : in out Message_Producer;
                           Value : Priority) is
   begin
      Self.Priority_Level := Value;
   end Set_Priority;

   -------------
   -- Set_Ref --
   -------------

   procedure Set_Ref (Self : in out Message_Producer;
                      Ref  : PolyORB.References.Ref) is
   begin
      Self.Ref := Ref;
   end Set_Ref;

   ----------------------
   -- Set_Time_To_Live --
   ----------------------

   procedure Set_Time_To_Live (Self : in out Message_Producer;
                               TTL : Time) is
   begin
      Self.TTL := TTL;
   end Set_Time_To_Live;

end MOMA.Message_Producers;
