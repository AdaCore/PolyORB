------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        M O M A . M E S S A G E _ C O N S U M E R S . Q U E U E S         --
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

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Types;
with PolyORB.Requests;

package body MOMA.Message_Consumers.Queues is

   use PolyORB.Types;

   ------------------------
   -- Get_Queue Function --
   ------------------------

   function Get_Queue return MOMA.Destinations.Queues.Queue is
   begin
      pragma Warnings (Off);
      return Get_Queue;
      pragma Warnings (On);
   end Get_Queue;

   -------------
   -- Receive --
   -------------

   function Receive (Self : Queue)
                     return PolyORB.Types.String
   is
      Arg_Name_Mesg : PolyORB.Types.Identifier
       := PolyORB.Types.To_PolyORB_String ("Mesg");

      Argument_Mesg : PolyORB.Any.Any := PolyORB.Any.To_Any
        (To_PolyORB_String ("M0"));
      --  XXX Temporary hack, need to solve message_pool-warehouse

      Operation_Name : constant Standard.String := "Get";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   Arg_Name_Mesg,
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

      Result := (Name => PolyORB.Types.Identifier (Result_Name),
                 Argument => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_String),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Get_Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

      return PolyORB.Any.From_Any (Result.Argument);
   end Receive;

   function Receive (Timeout : Time) return MOMA.Messages.Message is
      Temp : MOMA.Messages.Message;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Timeout);
      pragma Warnings (On);

      return Temp;
   end Receive;

   ----------------------
   -- Receive _No_Wait --
   ----------------------

   function Receive_No_Wait return MOMA.Messages.Message is
   begin
      pragma Warnings (Off);
      return Receive_No_Wait;
      pragma Warnings (On);
   end Receive_No_Wait;



end MOMA.Message_Consumers.Queues;

