------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . S E S S I O N S                         --
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

with PolyORB;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Requests;
with PolyORB.Types;

package body MOMA.Sessions is

   use MOMA.Types;

   use PolyORB.Types;

   ------------
   --  Close --
   ------------

   procedure Close is
   begin
      null;
      --  XXX Not Implemented
   end Close;

   -------------
   --  Commit --
   -------------

   procedure Commit is
   begin
      null;
      --  XXX Not Implemented
   end Commit;

   --------------------
   -- Create_Session --
   --------------------

   function Create_Session (Connection       : MOMA.Connections.Connection;
                            Transacted       : Boolean;
                            Acknowledge_Mode : MOMA.Types.Acknowledge_Type)
                            return Session
   is
      New_Session : Session;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Connection);
      pragma Warnings (On);
      --  XXX ??? Why
      New_Session.Transacted := Transacted;
      New_Session.Acknowledge_Mode := Acknowledge_Mode;
      return New_Session;
   end Create_Session;

   ---------------------
   --  Get_Transacted --
   ---------------------

   function Get_Transacted return Boolean is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_Transacted;
      pragma Warnings (On);
   end Get_Transacted;

   --------------
   --  Recover --
   --------------

   procedure Recover is
   begin
      null;
      --  XXX Not Implemented
   end Recover;

   ---------------
   --  Rollback --
   ---------------

   procedure Rollback is
   begin
      null;
      --  XXX Not Implemented
   end Rollback;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (Topic : MOMA.Destinations.Destination;
                        Pool  : MOMA.Destinations.Destination;
                        Sub   : Boolean := True)
   is
      use MOMA.Destinations;
      Arg_List  : PolyORB.Any.NVList.Ref;
      Request   : PolyORB.Requests.Request_Access;
      Result    : PolyORB.Any.NamedValue;
      Operation : MOMA.Types.String := To_MOMA_String ("Subscribe");
   begin
      if Get_Kind (Topic) /= MOMA.Types.Topic
      or else Get_Kind (Pool) /= MOMA.Types.Pool then
         raise Program_Error;
      end if;
      if not (Sub) then
         Operation := To_MOMA_String ("Unsubscribe");
      end if;
      PolyORB.Any.NVList.Create (Arg_List);
      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Topic"),
                                   To_Any (Topic),
                                   PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Pool"),
                                   To_Any (Pool),
                                   PolyORB.Any.ARG_IN);
      Result := (Name      => To_PolyORB_String ("Result"),
                 Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                 Arg_Modes => 0);
      PolyORB.Requests.Create_Request
        (Target    => Get_Ref (Topic),
         Operation => MOMA.Types.To_Standard_String (Operation),
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);
      PolyORB.Requests.Invoke (Request);
      PolyORB.Requests.Destroy_Request (Request);
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe (Topic : MOMA.Destinations.Destination;
                          Pool  : MOMA.Destinations.Destination)
   is
   begin
      Subscribe (Topic, Pool, False);
   end Unsubscribe;

end MOMA.Sessions;

