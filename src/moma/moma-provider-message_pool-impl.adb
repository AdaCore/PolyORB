------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      M O M A . P R O V I D E R . M E S S A G E _ P O O L . I M P L       --
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

with MOMA.Provider.Message_Pool.Warehouse;
with PolyORB.Log;
with PolyORB.Any;

package body MOMA.Provider.Message_Pool.Impl is

   use MOMA.Provider.Message_Pool.Warehouse;
   use PolyORB.Any;
   use PolyORB.Log;

   package L is new
     PolyORB.Log.Facility_Log ("moma.provider.message_pool.impl");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   W : MOMA.Provider.Message_Pool.Warehouse.Warehouse;
   --  XXX up to now, we use one and only one Warehouse,
   --  more warehouse would require message analysis,
   --  => to be done later, after proper message definition

   Message_Id : Natural := 0;
   --  XXX Dummy counter for message_id, to be trashed ...

   Last_Read_Id : Natural := 0;
   --  XXX Dummy counter for message_id, to be trashed ...

   -------------
   -- Publish --
   -------------

   procedure Publish (Message : in PolyORB.Any.Any)
   is
      Temp : constant String := Integer'Image (Message_Id);
      Key  : constant String := "M" & Temp (2 .. Temp'Last);
      --  Dummy Key construction, should be analyzed from message
   begin
      pragma Debug (O ("Got new message " & Image (Message)
                       & " with Key " & Key
                       & " with Content " & Image (From_Any (Message))));

      Ensure_Initialization (W);

      Message_Id := Message_Id + 1;

      Register (W, Key, Message);
   end Publish;

   ---------
   -- Get --
   ---------

   function Get (Message_Id : in MOMA.Types.String)
                 return PolyORB.Any.Any is
      Result : PolyORB.Any.Any;
      Temp : constant String := Integer'Image (Last_Read_Id);
      Key  : constant String := "M" & Temp (2 .. Temp'Last);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Message_Id);
      pragma Warnings (On);
      --  XXX We only implement dummy message pool
      --  should be done with a FIFO when available

      Result := Lookup (W, Key);
      Last_Read_Id := Last_Read_Id + 1;

      pragma Debug (O ("Sending back message " & Image (Result)
                       & " with Key " & Key
                       & " with Content " & Image (From_Any (Result))));
      return Result;
   end Get;

end MOMA.Provider.Message_Pool.Impl;
