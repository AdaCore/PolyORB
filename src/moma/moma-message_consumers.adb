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

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Requests;
with PolyORB.Types;

package body MOMA.Message_Consumers is

   use MOMA.Messages;

   use PolyORB.Any;
   use PolyORB.Types;

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

   -------------
   -- Receive --
   -------------

   function Receive (Self : Message_Consumer)
      return MOMA.Messages.Message'Class
   is
      Argument_Mesg : PolyORB.Any.Any := PolyORB.Any.To_Any
        (To_PolyORB_String (""));
      --  XXX Temporary hack, should pass message filter ... or not ?

      Request        : PolyORB.Requests.Request_Access;
      Arg_List       : PolyORB.Any.NVList.Ref;
      Result         : PolyORB.Any.NamedValue;
      Result_Name    : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Message"),
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

      Result := (Name      => PolyORB.Types.Identifier (Result_Name),
                 Argument  => PolyORB.Any.Get_Empty_Any (TC_MOMA_Message),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Get_Ref (Self),
         Operation => "Get",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

      return MOMA.Messages.From_Any (Result.Argument);
   end Receive;

   function Receive (Timeout : Ada.Real_Time.Time)
      return MOMA.Messages.Message
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Receive (Timeout);
      pragma Warnings (On);
   end Receive;

   ---------------------
   -- Receive_No_Wait --
   ---------------------

   function Receive_No_Wait return MOMA.Messages.Message is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Receive_No_Wait;
      pragma Warnings (On);
   end Receive_No_Wait;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination (Self : in out Message_Consumer;
                              Dest : MOMA.Destinations.Destination)
   is
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

