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

with MOMA.Messages;
with MOMA.Provider.Topic_Datas;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Requests;
with PolyORB.Types;

package body MOMA.Provider.Routers is

   use MOMA.Destinations;
   use MOMA.Messages;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Tasking.Rw_Locks;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("moma.provider.routers");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ----------------
   -- Add_Router --
   ----------------

   procedure Add_Router (Self    : in out Router;
                         Router  : MOMA.Destinations.Destination)
   is
   begin
      Lock_W (Self.Routers.L_Lock);
      Destination_List.Append (Self.Routers.List, Router);
      Unlock_W (Self.Routers.L_Lock);
      --  XXX It would be better to check first the router isn't already
      --  in the list.
   end Add_Router;

   ------------------------
   -- Create_Destination --
   ------------------------

   function Create_Destination (Self : Router)
      return MOMA.Destinations.Destination
   is
   begin
      return MOMA.Destinations.Create (Get_Id (Self),
                                       Get_Self_Ref (Self),
                                       MOMA.Types.Router);
   end Create_Destination;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Self : Router) return MOMA.Types.String
   is
   begin
      return Self.Id;
   end Get_Id;

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

      if       Method = "Publish"
      or else  Method = "Route" then
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Message"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (MOMA.Messages.TC_MOMA_Message),
              Arg_Modes => PolyORB.Any.ARG_IN));
         if Method = "Route" then
            PolyORB.Any.NVList.Add_Item
               (Result,
                (Name      => To_PolyORB_String ("From_Router_Id"),
                 Argument  => PolyORB.Any.Get_Empty_Any
                              (TypeCode.TC_String),
                 Arg_Modes => PolyORB.Any.ARG_IN));
         end if;

      elsif Method = "Register" then
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Router"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (MOMA.Destinations.TC_MOMA_Destination),
              Arg_Modes => PolyORB.Any.ARG_IN));

      elsif Method = "Subscribe" then
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Topic"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (MOMA.Destinations.TC_MOMA_Destination),
              Arg_Modes => PolyORB.Any.ARG_IN));
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Pool"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (MOMA.Destinations.TC_MOMA_Destination),
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

   -----------------
   -- Get_Routers --
   -----------------

   function Get_Routers (Self : Router) return Destination_List.List
   is
      Routers : Destination_List.List;
   begin
      Lock_R (Self.Routers.L_Lock);
      Routers := Destination_List.Duplicate (Self.Routers.List);
      Unlock_R (Self.Routers.L_Lock);
      return Routers;
   end Get_Routers;

   ------------------
   -- Get_Self_Ref --
   ------------------

   function Get_Self_Ref (Self : Router) return PolyORB.References.Ref
   is
   begin
      return Self.Self_Ref;
   end Get_Self_Ref;

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

   procedure Initialize (Self       : access Router;
                         Router_Ref : PolyORB.References.Ref)
   is
   begin
      MOMA.Provider.Topic_Datas.Ensure_Initialization (Self.Topics);
      if not (Self.Routers.L_Initialized) then
         PolyORB.Tasking.Rw_Locks.Create (Self.Routers.L_Lock);
         Self.Routers.L_Initialized := True;
      end if;
      if Router_Ref /= PolyORB.References.Nil_Ref then
         Register (Self, Router_Ref);
      end if;
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

      elsif Req.all.Operation = To_PolyORB_String ("Register") then

         --  Register

         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence  : constant NV_Sequence_Access := List_Of (Args);
            Router         : constant MOMA.Destinations.Destination :=
               MOMA.Destinations.From_Any
                  (NV_Sequence.Element_Of (Args_Sequence.all, 1).Argument);
         begin
            Req.Result.Argument := To_Any (Create_Destination (Self.all));
            Add_Router (Self.all, Router);
         end;

      elsif Req.all.Operation = To_PolyORB_String ("Route") then

         --  Route

         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence  : constant NV_Sequence_Access := List_Of (Args);
            Message        : constant PolyORB.Any.Any :=
              NV_Sequence.Element_Of (Args_Sequence.all, 1).Argument;
            From_Router_Id : constant MOMA.Types.String :=
              From_Any (NV_Sequence.Element_Of
                        (Args_Sequence.all, 2).Argument);
         begin
            Publish (Self, Message, From_Router_Id);
         end;

      elsif Req.all.Operation = To_PolyORB_String ("Subscribe") then

         --  Subscribe

         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence  : constant NV_Sequence_Access := List_Of (Args);
            Topic          : constant MOMA.Destinations.Destination :=
               MOMA.Destinations.From_Any
                  (NV_Sequence.Element_Of (Args_Sequence.all, 1).Argument);
            Pool           : constant MOMA.Destinations.Destination :=
               MOMA.Destinations.From_Any
                  (NV_Sequence.Element_Of (Args_Sequence.all, 2).Argument);
         begin
            Subscribe (Self, Topic, Pool);
         end;

      end if;
   end Invoke;

   -------------
   -- Publish --
   -------------

   procedure Publish (Self             : access Router;
                      Message          : PolyORB.Any.Any;
                      From_Router_Id   : MOMA.Types.String :=
                                            To_MOMA_String (""))
   is
      use MOMA.Provider.Topic_Datas.Ref_List;
      Subscribers : MOMA.Provider.Topic_Datas.Ref_List.List;
      I           : MOMA.Provider.Topic_Datas.Ref_List.Iterator;
      Topic_Id    : MOMA.Types.String;
      Destination : MOMA.Destinations.Destination;
      Routers     : Destination_List.List;
      J           : Destination_List.Iterator;
   begin
      --  Check the destination is really a topic.

      Destination := Get_Destination (MOMA.Messages.From_Any (Message));
      if Get_Kind (Destination) /= MOMA.Types.Topic then
         raise Program_Error;
      end if;
      Topic_Id := Get_Name (Destination);

      --  Relay Message to other routers.

      Routers := Get_Routers (Self.all);
      J := Destination_List.First (Routers);
      while not (Destination_List.Last (J)) loop
         if MOMA.Destinations.Get_Name (Destination_List.Value (J).all) /=
               From_Router_Id
         then
            Route (Self, Message, Destination_List.Value (J).all);
         end if;
         Destination_List.Next (J);
      end loop;
      Destination_List.Deallocate (Routers);

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

   --------------
   -- Register --
   --------------

   procedure Register (Self         : access Router;
                       Router_Ref   : PolyORB.References.Ref)
   is
      Request     : PolyORB.Requests.Request_Access;
      Arg_List    : PolyORB.Any.NVList.Ref;
      Result      : PolyORB.Any.NamedValue;
      Destination : constant MOMA.Destinations.Destination :=
                       Create_Destination (Self.all);
   begin
      PolyORB.Any.NVList.Create (Arg_List);
      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Router"),
                                   To_Any (Destination),
                                   PolyORB.Any.ARG_IN);
      Result := (Name      => To_PolyORB_String ("Result"),
                 Argument  => PolyORB.Any.Get_Empty_Any
                                 (MOMA.Destinations.TC_MOMA_Destination),
                 Arg_Modes => 0);
      PolyORB.Requests.Create_Request
        (Target    => Router_Ref,
         Operation => "Register",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);
      PolyORB.Requests.Invoke (Request);
      PolyORB.Requests.Destroy_Request (Request);
      Add_Router (Self.all,
                  MOMA.Destinations.From_Any (Result.Argument));
   end Register;

   -----------
   -- Route --
   -----------

   procedure Route (Self      : access Router;
                    Message   : PolyORB.Any.Any;
                    To_Router : MOMA.Destinations.Destination)
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
      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("From_Router_Id"),
                                   To_Any (Get_Id (Self.all)),
                                   PolyORB.Any.ARG_IN);
      Result := (Name      => To_PolyORB_String ("Result"),
                 Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                 Arg_Modes => 0);
      PolyORB.Requests.Create_Request
        (Target    => Get_Ref (To_Router),
         Operation => "Route",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);
      PolyORB.Requests.Invoke (Request);
      PolyORB.Requests.Destroy_Request (Request);
   end Route;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id (Self  : in out Router;
                     Id    : MOMA.Types.String)
   is
   begin
      Self.Id := Id;
   end Set_Id;

   -------------------
   --  Set_Self_Ref --
   -------------------

   procedure Set_Self_Ref (Self  : in out Router;
                           Ref   : PolyORB.References.Ref)
   is
   begin
      Self.Self_Ref := Ref;
   end Set_Self_Ref;

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
                        Topic    : MOMA.Destinations.Destination;
                        Pool     : MOMA.Destinations.Destination)
   is
   begin
      if Get_Kind (Topic) /= MOMA.Types.Topic
      or else Get_Kind (Pool) /= MOMA.Types.Pool then
         raise Program_Error;
      end if;
      MOMA.Provider.Topic_Datas.Add_Subscriber (Self.Topics,
                                                Get_Name (Topic),
                                                Get_Ref (Pool));
   end Subscribe;

end MOMA.Provider.Routers;
