------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                M O M A . P R O V I D E R . R O U T E R S                 --
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

with MOMA.Destinations;
with MOMA.Messages;
with MOMA.Provider.Topic_Datas;

with PolyORB.Any.NVList;
with PolyORB.Any.ObjRef;
with PolyORB.Log;
with PolyORB.Requests;
with PolyORB.Types;

package body MOMA.Provider.Routers is

   use MOMA.Destinations;
   use MOMA.Messages;
   use MOMA.Types;

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("moma.provider.routers");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------------
   -- Get_Parameter_Profile --
   ---------------------------

   function Get_Parameter_Profile (Method : String)
     return PolyORB.Any.NVList.Ref
   is
      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      pragma Debug (O ("Parameter profile for " & Method & " requested."));

      if Method = "Publish" then
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Message"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (MOMA.Messages.TC_MOMA_Message),
              Arg_Modes => PolyORB.Any.ARG_IN));

      elsif Method = "Subscribe" then
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Topic_Id"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (PolyORB.Any.TypeCode.TC_String),
              Arg_Modes => PolyORB.Any.ARG_IN));
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Pool_Ref"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (PolyORB.Any.TypeCode.TC_Object),
              Arg_Modes => PolyORB.Any.ARG_IN));

      else
         raise Program_Error;
      end if;

      return Result;
   end Get_Parameter_Profile;

   ------------------------
   -- Get_Result_Profile --
   ------------------------

   function Get_Result_Profile (Method : String)
     return PolyORB.Any.Any
   is
      use PolyORB.Any;
   begin
      pragma Debug (O ("Result profile for " & Method & " requested."));
      if Method = "Publish" then
         return Get_Empty_Any (TypeCode.TC_Void);
      elsif Method = "Subscribe" then
         return Get_Empty_Any (TypeCode.TC_Void);
      else
         raise Program_Error;
      end if;
   end Get_Result_Profile;

   -------------
   -- If_Desc --
   -------------

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description is
   begin
      return
        (PP_Desc => Get_Parameter_Profile'Access,
         RP_Desc => Get_Result_Profile'Access);
   end If_Desc;

   -----------------
   --  Initialize --
   -----------------

   procedure Initialize (Self : access Router)
   is
   begin
      MOMA.Provider.Topic_Datas.Ensure_Initialization (Self.Topics);
   end Initialize;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self : access Router;
      Req  : PolyORB.Requests.Request_Access)
   is
      Args : PolyORB.Any.NVList.Ref;
   begin
      pragma Debug (O ("The router is executing the request:"
                    & PolyORB.Requests.Image (Req.all)));

      PolyORB.Any.NVList.Create (Args);

      Args := Get_Parameter_Profile (To_Standard_String (Req.all.Operation));
      PolyORB.Requests.Arguments (Req, Args);

      if Req.all.Operation = To_PolyORB_String ("Publish") then

         --  Publish

         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence  : constant NV_Sequence_Access := List_Of (Args);
            Message        : PolyORB.Any.Any :=
              NV_Sequence.Element_Of (Args_Sequence.all, 1).Argument;
         begin
            Publish (Self, Message);
         end;

      elsif Req.all.Operation = To_PolyORB_String ("Subscribe") then

         --  Subscribe

         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence  : constant NV_Sequence_Access := List_Of (Args);
            Topic_Id       : MOMA.Types.String :=
              PolyORB.Any.From_Any (NV_Sequence.Element_Of
                                    (Args_Sequence.all, 1).Argument);
            Pool           : PolyORB.References.Ref :=
               PolyORB.Any.ObjRef.From_Any
                  (PolyORB.Any.Get_Aggregate_Element
                     (NV_Sequence.Element_Of (Args_Sequence.all, 2).Argument,
                      PolyORB.Any.TypeCode.TC_Object,
                      PolyORB.Types.Unsigned_Long (1)));
         begin
            Subscribe (Self, Topic_Id, Pool);
         end;

      end if;
   end Invoke;

   -------------
   -- Publish --
   -------------

   procedure Publish (Self       : access Router;
                      Message    : PolyORB.Any.Any)
   is
      use MOMA.Provider.Topic_Datas.Ref_List;
      Subscribers : MOMA.Provider.Topic_Datas.Ref_List.List;
      I           : MOMA.Provider.Topic_Datas.Ref_List.Iterator;
      Topic_Id    : MOMA.Types.String;
      Destination : MOMA.Destinations.Destination;
   begin
      --  Check the destination is really a topic.

      Destination := Get_Destination (MOMA.Messages.From_Any (Message));
      if Get_Kind (Destination) /= MOMA.Types.Topic then
         raise Program_Error;
      end if;
      Topic_Id := Get_Name (Destination);

      --  Relay Message to other routers.
      --  XXX  Not implemented yet.

      --  Store Message into known pools subscribed to this topic.

      Subscribers := MOMA.Provider.Topic_Datas.Get_Subscribers (Self.Topics,
                                                                Topic_Id);
      I := First (Subscribers);
      while not (Last (I)) loop
         Store (Value (I).all, Message);
         Next (I);
      end loop;
      Deallocate (Subscribers);
   end Publish;

   -----------
   -- Store --
   -----------

   procedure Store (Pool      : Ref;
                    Message   : PolyORB.Any.Any)
   is
      Request     : PolyORB.Requests.Request_Access;
      Arg_List    : PolyORB.Any.NVList.Ref;
      Result      : PolyORB.Any.NamedValue;
   begin
      PolyORB.Any.NVList.Create (Arg_List);
      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Message"),
                                   Message,
                                   PolyORB.Any.ARG_IN);
      Result := (Name      => To_PolyORB_String ("Result"),
                 Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                 Arg_Modes => 0);
      PolyORB.Requests.Create_Request
        (Target    => Pool,
         Operation => "Publish",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);
      PolyORB.Requests.Invoke (Request);
      PolyORB.Requests.Destroy_Request (Request);
   end Store;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (Self     : access Router;
                        Topic_Id : MOMA.Types.String;
                        Pool     : PolyORB.References.Ref)
   is
   begin
      MOMA.Provider.Topic_Datas.Add_Subscriber (Self.Topics,
                                                Topic_Id,
                                                Pool);
   end Subscribe;

end MOMA.Provider.Routers;
