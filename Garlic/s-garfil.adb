------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . F I L T E R S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;              use Ada.Streams;
with Ada.Unchecked_Deallocation;
with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.Heart;      use System.Garlic.Heart;
with System.Garlic.Name_Table; use System.Garlic.Name_Table;
with System.Garlic.Partitions; use System.Garlic.Partitions;
with System.Garlic.Streams;    use System.Garlic.Streams;
with System.Garlic.Table;
with System.Garlic.Types;      use System.Garlic.Types;
with System.Garlic.Utils;      use System.Garlic.Utils;

package body System.Garlic.Filters is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARFIL", "(s-garfil): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   --  This unit can be elaborated and used while its children (filters)
   --  are not elaborated. When System.Garlic.Elaboration is initialized, a
   --  filter can be assigned to some channels. A channel is a peer of
   --  partition and one of the partitions is the current partition. We
   --  append "'filter" to the name of the other partition and enter this
   --  name in the name table. The name id of the channel filter
   --  corresponds to the info of this composed name.

   --  Two special partition names are "others" and "declare". The first
   --  one, "others", corresponds to the default channel filter (with any
   --  unspecified partition). The second one, "declare", corresponds to
   --  the registration filter used to exchange public params when needed.

   Filter_Attribute_Name     : constant String := "'filter";
   Default_Filter_Name       : constant String := "others'filter";
   Registration_Filter_Name  : constant String := "declare'filter";

   Filter_ID_Increment : constant := 10;

   type Filter_Id is new Natural;
   Null_Filter    : constant Filter_Id := 0;
   First_Filter   : constant Filter_Id := 1_000;

   --  In an half channel is stored local params to use locally to filter
   --  an incoming or outgoing message. A remote param is a parameter to to
   --  use on the other side of the channel to filter an outgoing stream.

   type Half_Channel_Type is record
      Local  : Filter_Params_Access;
      Remote : Filter_Params_Access;
      Status : Status_Type;
   end record;

   Null_Half_Channel : constant Half_Channel_Type := (null, null, None);

   type Channel_Id is new Partition_ID;
   type Channel_Type is record
      Partition : Partition_ID;
      Filter    : Filter_Access;
      Installed : Boolean;
      Exchange  : Boolean;
      Incoming  : Half_Channel_Type;
      Outgoing  : Half_Channel_Type;
   end record;

   Null_Channel : constant Channel_Type :=
     (Partition => Null_PID,
      Filter    => null,
      Installed => False,
      Exchange  => False,
      Incoming  => Null_Half_Channel,
      Outgoing  => Null_Half_Channel);

   Default  : Filter_Access := null;
   --  Default filter to be used when no filter is assigned to a channel

   Register : Channel_Type  := Null_Channel;
   --  Registration filter to be used when filter params are exchanged

   type Request_Id is (Get_Params, Set_Params);
   type Request_Type is record
      Command   : Request_Id;
      Parameter : Filter_Params_Access;
   end record;

   Init : constant Request_Type := (Get_Params, null);

   type Registered_Filter_Type;
   type Registered_Filter_Access is access Registered_Filter_Type;
   type Registered_Filter_Type is
     record
        Name   : String_Access;
        Filter : Filter_Access;
        Next   : Registered_Filter_Access;
     end record;
   Registered_Filter_List : Registered_Filter_Access;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Registered_Filter_Type, Registered_Filter_Access);

   package Filters is new System.Garlic.Table.Complex
     (Index_Type     => Filter_Id,
      Null_Index     => Null_Filter,
      First_Index    => First_Filter,
      Initial_Size   => Filter_ID_Increment,
      Increment_Size => Filter_ID_Increment,
      Component_Type => Filter_Access,
      Null_Component => null);

   package Channels is new System.Garlic.Table.Complex
     (Index_Type     => Partition_ID,
      Null_Index     => Types.Null_PID,
      First_Index    => Types.First_PID,
      Initial_Size   => Natural (Partition_ID_Increment),
      Increment_Size => Natural (Partition_ID_Increment),
      Component_Type => Channel_Type,
      Null_Component => Null_Channel);

   procedure Get_Params_For_Incoming
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Error     : in out Error_Type);
   --  This procedure initializes and extracts the half incoming channel.

   procedure Get_Params_For_Outgoing
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Error     : in out Error_Type);
   --  This procedure initializes and extracts the half outgoing channel.

   procedure Get_Partition_Filter
     (Partition : in Partition_ID;
      Filter    : out Filter_Access;
      Error     : in out Error_Type);
   --  Retrieve the filter to apply on this channel. This procedure uses
   --  the naming convention. Info of name "partition name" + "'filter" is
   --  the name of the filter to apply on the channel.

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);
   --  Handle a remote request Get_Params and Set_Params. When needed, use
   --  registration filter.

   procedure Install_Channel
     (Partition : in Partition_ID;
      Channel   : in out Channel_Type;
      Error     : in out Error_Type);
   --  Find which filter to use and install it (but not the local and
   --  remote data for incoming and outgoing).

   function Name (P : Partition_ID) return String;

   procedure Send
     (Partition : in Partition_ID;
      Request   : in Request_Id;
      Channel   : in Channel_Type;
      Error     : in out Error_Type);
   --  Send a remote request Get_Params and Set_Params. When needed, use
   --  registration filter.

   procedure Set_Params_For_Outgoing
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Error     : in out Error_Type);
   --  This procedure initializes the half outgoing channel.

   ---------------------
   -- Filter_Incoming --
   ---------------------

   procedure Filter_Incoming
      (Partition : in Partition_ID;
       Opcode    : in Any_Opcode;
       Stream    : in Stream_Element_Access;
       Offset    : in Stream_Element_Offset;
       Result    : out Stream_Element_Access;
       Error     : in out Error_Type)
   is
      First   : constant Stream_Element_Offset := Stream'First + Offset;
      Last    : constant Stream_Element_Offset := Stream'Last;
      Channel : Channel_Type;
   begin
      --  Only remote calls are filtered

      if Opcode = Remote_Call then

         Channel := Channels.Get_Component (Partition);

         --  Check that this half channel is initialized

         if Channel.Incoming.Status /= Done then
            Get_Params_For_Incoming (Partition, Init, Error);
            if Found (Error) then
               return;
            end if;

            --  Channel has been updated in the meantime.

            Channel := Channels.Get_Component (Partition);
         end if;

         --  Check whether stream management is needed

         if Channel.Filter /= null then
            pragma Debug
              (D ("Partition " & Name (Partition) &
                  " incoming filter non null"));

            Result := Filter_Incoming
              (Channel.Filter.all, Channel.Incoming.Local, Stream, Offset);
            return;
         end if;
      end if;

      --  When possible, avoid unnecessary buffer copies

      Result := new Stream_Element_Array'(Stream (First .. Last));
   end Filter_Incoming;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   procedure Filter_Outgoing
      (Partition : in     Partition_ID;
       Opcode    : in     Any_Opcode;
       Stream    : access Params_Stream_Type;
       Result    : out    Stream_Element_Access;
       Error     : in out Error_Type)
   is
      Channel : Channel_Type;
   begin
      --  Only remote calls are filtered

      if Opcode = Remote_Call then

         Channel := Channels.Get_Component (Partition);

         --  Check  that this half channel is initialized

         if Channel.Outgoing.Status /= Done then
            Get_Params_For_Outgoing (Partition, Init, Error);
            if Found (Error) then
               return;
            end if;

            --  Channel has been updated in the meantime.

            Channel := Channels.Get_Component (Partition);
         end if;

         --  Check whether stream management is needed

         if Channel.Filter /= null then
            pragma Debug
              (D ("Partition " & Name (Partition) &
                  " outgoing filter non null"));

            Result := Filter_Outgoing
              (Channel.Filter.all, Channel.Outgoing.Local, Stream);
            return;
         end if;
      end if;

      --  When possible, avoid unnecessary buffer copies

      Result := To_Stream_Element_Access (Stream);
   end Filter_Outgoing;

   -----------------------------
   -- Get_Params_For_Incoming --
   -----------------------------

   procedure Get_Params_For_Incoming
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Error     : in out Error_Type)
   is
      Channel : Channel_Type;
   begin
      pragma Debug
        (D ("Get params for partition" & Partition'Img &
            " incoming filter"));

      Channels.Enter;

      Channel := Channels.Get_Component (Partition);

      if not Channel.Installed then
         Install_Channel (Partition, Channel, Error);
         if Found (Error) then
            Channels.Leave;
            return;
         end if;
      end if;

      if Channel.Incoming.Status = None then
         pragma Debug
           (D ("Generate params for partition" & Partition'Img &
               " incoming filter"));

         --  Always generate params. An incoming channel will always use
         --  the local part and will provide the remote part to the remote
         --  side on a Get_Params request (pull method).

         Generate_Params
           (Channel.Filter.all,
            Channel.Incoming.Remote,
            Channel.Incoming.Local,
            Channel.Exchange);
         Channel.Incoming.Status := Done;

         Channels.Set_Component (Partition, Channel);
      end if;

      Channels.Leave;
   end Get_Params_For_Incoming;

   -----------------------------
   -- Get_Params_For_Outgoing --
   -----------------------------

   procedure Get_Params_For_Outgoing
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Error     : in out Error_Type)
   is
      Channel : Channel_Type;
      Version : Version_Id;
      Waiting : Boolean;
   begin
      loop
         Waiting := False;

         Channels.Enter;
         Channel := Channels.Get_Component (Partition);

         if not Channel.Installed then
            Install_Channel (Partition, Channel, Error);
            if Found (Error) then
               Channels.Leave;
               return;
            end if;
         end if;

         if Channel.Outgoing.Status = None then

            --  Always generate params. An outgoing channel will not
            --  always use the local part but it needs to know if an
            --  param exchange is needed.

            Generate_Params
              (Channel.Filter.all,
               Channel.Outgoing.Remote,
               Channel.Outgoing.Local,
               Channel.Exchange);

            if not Channel.Exchange then
               pragma Debug
                 (D ("Exchange nothing for partition " & Name (Partition) &
                     " outgoing filter"));

               Channel.Outgoing.Status := Done;

            else

               --  A parameter exchange is needed. This partition should
               --  ask the other partition for its remote parameter (pull
               --  method). Previous remote and local params are discarded.

               Channel.Outgoing.Status := Busy;

               Free (Channel.Outgoing.Remote);
               Free (Channel.Outgoing.Local);

               Send (Partition, Get_Params, Channel, Error);
               if Found (Error) then
                  Channels.Leave;
                  return;
               end if;

               pragma Debug
                 (D ("Query params for partition " & Name (Partition) &
                     " outgoing filter"));

            end if;

            Channels.Set_Component (Partition, Channel);

         end if;

         Channels.Leave (Version);

         exit when Channel.Outgoing.Status = Done;

         Channels.Differ (Version);
      end loop;
   end Get_Params_For_Outgoing;

   --------------------------
   -- Get_Partition_Filter --
   --------------------------

   procedure Get_Partition_Filter
     (Partition : in Partition_ID;
      Filter    : out Filter_Access;
      Error     : in out Error_Type)
   is
      P : Name_Id;
      F : Name_Id;
   begin
      Get_Name (Partition, P, Error);

      if Found (Error) then
         return;
      end if;

      --  Info of "partition name" + "'filter" corresponds to the name of
      --  the filter to apply on this channel.

      F := To_Name_Id (Get_Info (Get (Get (P) & Filter_Attribute_Name)));
      if F /= Null_Name then
         pragma Debug
           (D ("Use filter " & Get (F) & " with partition " & Get (P)));

         Filter := Filters.Get_Component (Filters.Get_Index (Get (F)));

      else
         pragma Debug
           (D ("Use default filter with partition " & Get (P)));

         Filter := Default;
      end if;
   end Get_Partition_Filter;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
      --  We may have to filter the original stream. We will interpret the
      --  message once the stream has been filtered. That's why the job is
      --  done in Internal_Receive.

      S1, S2 : Stream_Element_Access;

      procedure Internal_Handler
        (Stream : access Params_Stream_Type;
         Error  : in out Error_Type);

      procedure Internal_Handler
        (Stream : access Params_Stream_Type;
         Error  : in out Error_Type)
      is
         Request : Request_Type;
         Filter  : Filter_Access;

         pragma Warnings (Off);

         --  ?????: This is a hack to force partition_data update.
         PName   : String := Name (Partition);

         pragma Warnings (On);

      begin
         Request_Id'Read (Stream, Request.Command);

         pragma Debug
           (D ("Recv "  & Request.Command'Img &
               " from " & PName & " -" & Partition'Img));

         if Request.Command = Set_Params then
            Get_Partition_Filter (Partition, Filter, Error);
            if Found (Error) then
               return;
            end if;

            Request.Parameter :=
              Filter_Params_Read (Filter.all,
                                  To_Stream_Element_Array (Stream));
            Set_Params_For_Outgoing (Partition, Request, Error);
            if Found (Error) then
               return;
            end if;

         else
            Get_Params_For_Incoming (Partition, Request, Error);
            if Found (Error) then
               return;
            end if;

            --  Provide the remote part to the other partition ouside
            --  from critical section because Send can also require to
            --  enter in a critical section.

            Send (Partition,
                  Set_Params,
                  Channels.Get_Component (Partition),
                  Error);
            if Found (Error) then
               return;
            end if;
         end if;
      exception when others =>
         Throw (Error, "Data error in Filters.Internal_Handler");
      end Internal_Handler;

   begin
      if Register.Filter /= null then
         S1 := To_Stream_Element_Access (Query);
         S2 := Filter_Incoming
           (Register.Filter.all, Register.Incoming.Local, S1, 0);
         declare
            Params : aliased Params_Stream_Type (S2'Length);
         begin
            Write (Params, S2.all);
            Free (S1);
            Free (S2);
            Internal_Handler (Params'Access, Error);
         end;

      else
         Internal_Handler (Query, Error);
      end if;
   end Handle_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      F : Name_Id;
      L : Registered_Filter_Access;
   begin
      Filters.Initialize;
      Channels.Initialize;

      --  We have postponed the filter registrations because this unit
      --  was not yet initialized and in particular, Filters and
      --  Channels were not initialized.

      --  We cannot Initialize this unit before having all the filter
      --  registration because the default filter and the registration
      --  filter would be installed without being registered.

      while Registered_Filter_List /= null loop
         L := Registered_Filter_List;
         Registered_Filter_List := L.Next;
         Filters.Set_Component (Filters.Get_Index (L.Name.all), L.Filter);
         Destroy (L.Name);
         Free (L);
      end loop;

      --  Initialize default filter

      F := To_Name_Id (Get_Info (Get (Default_Filter_Name)));
      if F /= Null_Name then
         pragma Debug (D ("Use default filter " & Get (F)));

         Default := Filters.Get_Component (Filters.Get_Index (Get (F)));
      end if;

      --  Initialize registration filter and create corresponding params
      --  when needed.

      F := To_Name_Id (Get_Info (Get (Registration_Filter_Name)));
      if F /= Null_Name then
         pragma Debug (D ("Use registration filter " & Get (F)));

         Register.Filter :=
           Filters.Get_Component (Filters.Get_Index (Get (F)));
         Generate_Params
           (Register.Filter.all,
            Register.Incoming.Remote,
            Register.Incoming.Local,
            Register.Exchange);

         if Register.Exchange then
            raise Program_Error;
         end if;
      end if;

      Register_Handler (Filtering_Service, Handle_Request'Access);
   end Initialize;

   ---------------------
   -- Install_Channel --
   ---------------------

   procedure Install_Channel
     (Partition : in Partition_ID;
      Channel   : in out Channel_Type;
      Error     : in out Error_Type)
   is
   begin
      pragma Debug
        (D ("Partition " & Name (Partition) &
            " filter installed"));

      Get_Partition_Filter (Partition, Channel.Filter, Error);
      if Found (Error) then
         return;
      end if;

      Channel.Installed := True;

      if Channel.Filter = null then
         Channel.Incoming.Status := Done;
         Channel.Outgoing.Status := Done;
      end if;

      Channels.Set_Component (Partition, Channel);
   end Install_Channel;

   ----------
   -- Name --
   ----------

   function Name (P : Partition_ID) return String is
      N : String_Access;
      E : Error_Type;
   begin
      Get_Name (P, N, E);
      if Found (E) then
         Catch (E);
         return "<error>";
      else
         return N.all;
      end if;
   end Name;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Filter : in Filter_Access;
      Name   : in String)
   is
      Registered_Filter : Registered_Filter_Access;
   begin
      --  We cannot yet register filters because this unit has not
      --  been initialized yet. Queue the registration request.

      Registered_Filter := new Registered_Filter_Type;
      Registered_Filter.Name   := new String'(Name);
      Registered_Filter.Filter := Filter;
      Registered_Filter.Next   := Registered_Filter_List;
      Registered_Filter_List   := Registered_Filter;
   end Register_Filter;

   ----------
   -- Send --
   ----------

   procedure Send
     (Partition : in Partition_ID;
      Request   : in Request_Id;
      Channel   : in Channel_Type;
      Error     : in out Error_Type)
   is
      Stream : aliased Params_Stream_Type (0);
      Params : Stream_Element_Access;
   begin
      pragma Debug (D ("Send " & Request'Img & " to "  & Partition'Img));

      Request_Id'Write (Stream'Access, Request);
      if Request = Set_Params then
         Params := Filter_Params_Write
           (Channel.Filter.all, Channel.Incoming.Remote);
         Write (Stream, Params.all);
         Free (Params);
      end if;

      --  When parameters are exchanged, stream may have to be filtered

      if Register.Filter /= null then
         Params := Filter_Outgoing
           (Register.Filter.all,
            Register.Outgoing.Local,
            Stream'Access);
         Write (Stream, Params.all);
         Free (Params);
      end if;

      Send (Partition, Filtering_Service, Stream'Access, Error);
   end Send;

   ------------------------
   -- Set_Channel_Filter --
   ------------------------

   procedure Set_Channel_Filter (Partition, Filter : in String) is
   begin
      Set_Info (Get (Partition & Filter_Attribute_Name),
                To_Natural (Get (Filter)));
   end Set_Channel_Filter;

   ------------------------
   -- Set_Default_Filter --
   ------------------------

   procedure Set_Default_Filter (Filter : in String) is
   begin
      Set_Info (Get (Default_Filter_Name), To_Natural (Get (Filter)));
   end Set_Default_Filter;

   -----------------------------
   -- Set_Params_For_Outgoing --
   -----------------------------

   procedure Set_Params_For_Outgoing
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Error     : in out Error_Type)
   is
      Channel : Channel_Type;
   begin
      pragma Debug
        (D ("Set params for partition" & Partition'Img & " outgoing filter"));

      Channels.Enter;
      Channel := Channels.Get_Component (Partition);

      if not Channel.Installed then
         Install_Channel (Partition, Channel, Error);
         if Found (Error) then
            Channels.Leave;
            return;
         end if;
      end if;

      --  If this request (Set_Params) is the answer to a remote
      --  request from this partition (Get_Params), then save the
      --  remote part (of the other partition) into the local part
      --  (of this partition).

      if Channel.Outgoing.Status /= Done then
         pragma Debug
           (D ("Save params for partition " & Name (Partition) &
               " outgoing filter"));

         Channel.Outgoing.Status := Done;
         Channel.Outgoing.Local  := Request.Parameter;

         Channels.Set_Component (Partition, Channel);
      end if;

      Channels.Leave;
   end Set_Params_For_Outgoing;

   -----------------------------
   -- Set_Registration_Filter --
   -----------------------------

   procedure Set_Registration_Filter (Filter : in String) is
   begin
      Set_Info (Get (Registration_Filter_Name), To_Natural (Get (Filter)));
   end Set_Registration_Filter;

end System.Garlic.Filters;
