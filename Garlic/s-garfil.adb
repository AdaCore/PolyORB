------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . F I L T E R S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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
with System.Garlic.Debug;      use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Heart;      use System.Garlic.Heart;
with System.Garlic.Name_Table; use System.Garlic.Name_Table;
with System.Garlic.Streams;    use System.Garlic.Streams;
with System.Garlic.Table;      use System.Garlic.Table;
with System.Garlic.Types;      use System.Garlic.Types;
with System.Garlic.Utils;      use System.Garlic.Utils;

package body System.Garlic.Filters is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARFIL", "(s-garfil): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   function Name (Partition : Partition_ID) return String
     renames Heart.Name;
   --  Renaming used to make sure that this function gets precedence over
   --  the System.Name type.

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

   Null_Partition    : constant Partition_ID
     := Null_Partition_ID;
   First_Partition   : constant Partition_ID
     := Partition_ID'Succ (Null_Partition);
   Last_Partition    : constant Partition_ID
     := Partition_ID'Last;
   Partition_ID_Size : constant Natural
     := Natural (Last_Partition) - Natural (First_Partition) + 1;

   Filter_ID_Size : constant := 8;

   type Filter_Id is new Natural;
   Null_Filter    : constant Filter_Id := 0;
   First_Filter   : constant Filter_Id := 1_000;

   --  In an half channel is stored local params to use locally to filter
   --  an incoming or outgoing message. A remote param is a parameter to to
   --  use on the other side of the channel to filter an outgoing stream.

   type Half_Channel_Type is record
      Local  : Filter_Params_Access;
      Remote : Filter_Params_Access;
      Ready  : Boolean;
   end record;

   Null_Half_Channel : constant Half_Channel_Type := (null, null, False);

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
     (Partition => Null_Partition_ID,
      Filter    => null,
      Installed => False,
      Exchange  => False,
      Incoming  => Null_Half_Channel,
      Outgoing  => Null_Half_Channel);

   Default  : Filter_Access := null;
   --  Default filter to be used when no filter is assigned to a channel

   Register : Channel_Type  := Null_Channel;
   --  Registration filter to be used when filter params are exchanged

   type Request_Id is (Initialize, Get_Params, Set_Params);
   type Request_Type is record
      Command   : Request_Id;
      Parameter : Filter_Params_Access;
   end record;

   Init_Request : constant Request_Type := (Initialize, null);

   package Filters is new Complex
     (Filter_Id, Null_Filter, First_Filter,
      Filter_ID_Size, 0, Filter_Access, null, Integer);

   package Channels is new Complex
     (Partition_ID, Null_Partition, First_Partition,
      Partition_ID_Size, 0, Channel_Type, Null_Channel, Request_Type);

   function Get_Partition_Filter
     (Partition : Partition_ID)
      return Filter_Access;
   --  Retrieve the filter to apply on this channel. This procedure uses
   --  the naming convention. Info of name "partition name" + "'filter" is
   --  the name of the filter to apply on the channel.

   procedure Incoming
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Channel   : in out Channel_Type;
      Status    : out Status_Type);
   --  Procedure to use with Filter.Apply. This procedure handles
   --  a request in a protected manner. Basically, this procedure
   --  initializes the half incoming channel.

   procedure Outgoing
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Channel   : in out Channel_Type;
      Status    : out Status_Type);
   --  Procedure to use with Filter.Apply. This procedure handles
   --  a request in a protected manner. Basically, this procedure
   --  initializes the half outgoing channel.

   procedure Receive_Message
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type);
   --  Handle a remote request Get_Params and Set_Params. When needed, use
   --  registration filter.

   procedure Send_Message
     (Partition : in Partition_ID;
      Request   : in Request_Id;
      Channel   : in Channel_Type);
   --  Send a remote request Get_Params and Set_Params. When needed, use
   --  registration filter.

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
      (Partition : in Types.Partition_ID;
       Operation : in System.Garlic.Heart.Opcode;
       Stream    : in Ada.Streams.Stream_Element_Array)
      return Streams.Stream_Element_Access is
   begin
      --  Only remote calls are filtered

      if Operation = Remote_Call then

         --  Check that this half channel is initialized

         if not Channels.Table (Partition).Incoming.Ready then
            pragma Debug
              (D (D_Debug,
                  "Partition " & Name (Partition) &
                  " incoming filter not initialized"));

            Channels.Apply (Partition, Init_Request, Incoming'Access);
         end if;

         --  Check whether stream management is needed

         if Channels.Table (Partition).Filter /= null then
            pragma Debug
              (D (D_Debug,
                  "Partition " & Name (Partition) &
                  " incoming filter non null"));

            return Filter_Incoming
              (Channels.Table (Partition).Filter.all,
               Channels.Table (Partition).Incoming.Local,
               Stream);
         end if;
      end if;

      --  When possible, avoid unnecessary buffer copies

      return new Stream_Element_Array'(Stream);
   end Filter_Incoming;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
      (Partition : in     Types.Partition_ID;
       Operation : in     System.Garlic.Heart.Opcode;
       Stream    : access Streams.Params_Stream_Type)
      return Streams.Stream_Element_Access is
   begin
      --  Only remote calls are filtered

      if Operation = Remote_Call then

         --  Check  that this half channel is initialized

         if not Channels.Table (Partition).Outgoing.Ready then
            pragma Debug
              (D (D_Debug,
                  "Partition " & Name (Partition) &
                  " outgoing filter not initialized"));

            Channels.Apply (Partition, Init_Request, Outgoing'Access);
         end if;

         --  Check whether stream management is needed

         if Channels.Table (Partition).Filter /= null then
            pragma Debug
              (D (D_Debug,
                  "Partition " & Name (Partition) &
                  " outgoing filter non null"));

            return Filter_Outgoing
              (Channels.Table (Partition).Filter.all,
               Channels.Table (Partition).Outgoing.Local,
               Stream);
         end if;
      end if;

      --  When possible, avoid unnecessary buffer copies

      return To_Stream_Element_Access (Stream);
   end Filter_Outgoing;

   --------------------------
   -- Get_Partition_Filter --
   --------------------------

   function Get_Partition_Filter
     (Partition : Partition_ID)
      return Filter_Access is
      P : Name_Id;
      F : Name_Id;

   begin
      P := Name (Partition);

      pragma Debug
        (D (D_Debug, "Looking for partition " & Get (P) & "'s filter"));

      --  Info of "partition name" + "'filter" corresponds to the name of
      --  the filter to apply on this channel.

      F := To_Name_Id (Get_Info (Get (Get (P) & Filter_Attribute_Name)));
      if F /= Null_Name then
         pragma Debug
           (D (D_Debug,
               "Use filter " & Get (F) & " with partition " & Get (P)));

         return Filters.Get_Component (Filters.Get_Index (Get (F)));
      end if;

      pragma Debug
        (D (D_Debug, "Use default filter with partition " & Get (P)));

      return Default;
   end Get_Partition_Filter;

   --------------
   -- Incoming --
   --------------

   procedure Incoming
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Channel   : in out Channel_Type;
      Status    : out Status_Type)
   is
   begin
      Status := Unmodified;

      if not Channel.Installed then
         pragma Debug
           (D (D_Debug, "Install filter of partition" & Partition'Img));

         Channel.Filter    := Get_Partition_Filter (Partition);
         Channel.Installed := True;

         if Channel.Filter = null then
            Channel.Incoming.Ready := True;
            Channel.Outgoing.Ready := True;
         end if;

         Status := Modified;
      end if;

      if not Channel.Incoming.Ready then
         pragma Debug
           (D (D_Debug,
               "Generate params for partition" & Partition'Img &
               " incoming filter"));

         --  Always generate params. An incoming channel will always use
         --  the local part and will provide the remote part to the remote
         --  side on a Get_Params request (pull method).

         Generate_Params
           (Channel.Filter.all,
            Channel.Incoming.Remote,
            Channel.Incoming.Local,
            Channel.Exchange);
         Channel.Incoming.Ready := True;

         Status := Modified;
      end if;
   end Incoming;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      F : Name_Id;
   begin
      --  Initialize default filter

      F := To_Name_Id (Get_Info (Get (Default_Filter_Name)));
      if F /= Null_Name then
         pragma Debug (D (D_Debug, "Use default filter " & Get (F)));

         Default := Filters.Get_Component (Filters.Get_Index (Get (F)));
      end if;

      --  Initialize registration filter and create corresponding params
      --  when needed.

      F := To_Name_Id (Get_Info (Get (Registration_Filter_Name)));
      if F /= Null_Name then
         pragma Debug (D (D_Debug, "Use registration filter " & Get (F)));

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

      Receive (Filtering, Receive_Message'Access);
   end Initialize;

   --------------
   -- Outgoing --
   --------------

   procedure Outgoing
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Channel   : in out Channel_Type;
      Status    : out Status_Type)
   is
   begin
      Status := Unmodified;

      if not Channel.Installed then
         pragma Debug
           (D (D_Debug,
               "Partition " & Name (Partition) &
               " filter initialized"));

         Channel.Filter    := Get_Partition_Filter (Partition);
         Channel.Installed := True;

         if Channel.Filter = null then
            Channel.Incoming.Ready := True;
            Channel.Outgoing.Ready := True;
         end if;

         Status := Modified;
      end if;

      if not Channel.Outgoing.Ready then

         --  If this request (Set_Params) is the answer to a remote request
         --  from this partition (Get_Params), then save the remote part
         --  (of the other partition) into the local part (of this
         --  partition).

         if Request.Command = Set_Params then
            pragma Debug
              (D (D_Debug,
                  "Save params for partition " & Name (Partition) &
                  " outgoing filter"));

            Channel.Outgoing.Ready := True;
            Channel.Outgoing.Local := Request.Parameter;
            Status := Modified;

         else

            --  Always generate params. An outgoing channel will not always
            --  use the local part but it needs to know if an param
            --  exchange is needed.

            Generate_Params
              (Channel.Filter.all,
               Channel.Outgoing.Remote,
               Channel.Outgoing.Local,
               Channel.Exchange);

            if not Channel.Exchange then
               pragma Debug
                 (D (D_Debug,
                     "Exchange no params for partition " &
                     Name (Partition) &
                     " outgoing filter"));

               Channel.Outgoing.Ready := True;
               Status := Modified;

            else

               --  A parameter exchange is needed. This partition should
               --  ask the other partition for its remote parameter (pull
               --  method). Previous remote and local params are discarded.

               Free (Channel.Outgoing.Remote);
               Free (Channel.Outgoing.Local);

               pragma Debug
                 (D (D_Debug,
                     "Query params for partition " & Name (Partition) &
                     " outgoing filter"));

               Send_Message (Partition, Get_Params, Channel);
               Status := Postponed;
            end if;
         end if;
      end if;
   end Outgoing;

   ---------------------
   -- Receive_Message --
   ---------------------

   procedure Receive_Message
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type)
   is
      --  We may have to filter the original stream. We will interpret the
      --  message once the stream has been filtered. That's why the job is
      --  done in Internal_Receive.

      S1, S2 : Stream_Element_Access;

      procedure Internal_Receive (P : access Params_Stream_Type);

      procedure Internal_Receive (P : access Params_Stream_Type) is
         Request : Request_Type;
         Filter  : Filter_Access;

         pragma Warnings (Off);

         --  This is a hack to force partition_data update.
         PName   : String := Name (Partition);

         pragma Warnings (On);

      begin
         Request_Id'Read (P, Request.Command);

         pragma Debug
           (D (D_Debug,
               "Recv "  & Request.Command'Img &
               " from " & PName & " -" & Partition'Img));

         if Request.Command = Set_Params then
            Filter := Get_Partition_Filter (Partition);
            Request.Parameter :=
              Filter_Params_Read (Filter.all, To_Stream_Element_Array (P));
            Channels.Apply (Partition, Request, Outgoing'Access);

         else
            Channels.Apply (Partition, Request, Incoming'Access);

            --  If this was a remote query, then provide the remote part to
            --  the other partition ouside from critical section because
            --  Send can also require to enter in a critical section.

            if Request.Command = Get_Params then
               pragma Debug
                 (D (D_Debug,
                     "Provide params for partition " & PName &
                     " incoming filter"));

               Send_Message (Partition, Set_Params,
                             Channels.Get_Component (Partition));
            end if;
         end if;
      end Internal_Receive;

   begin
      if Register.Filter /= null then
         S1 := To_Stream_Element_Access (Params);
         S2 := Filter_Incoming
           (Register.Filter.all,
            Register.Incoming.Local,
            S1.all);
         declare
            Params : aliased Params_Stream_Type (S2'Length);
         begin
            Write (Params, S2.all);
            Free (S1);
            Free (S2);
            Internal_Receive (Params'Access);
         end;

      else
         Internal_Receive (Params);
      end if;
   end Receive_Message;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Filter : in Filter_Access;
      Name   : in String) is
   begin
      Filters.Set_Component (Filters.Get_Index (Name), Filter);
   end Register_Filter;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (Partition : in Partition_ID;
      Request   : in Request_Id;
      Channel   : in Channel_Type)
   is
      Stream : aliased Params_Stream_Type (0);
      Params : Stream_Element_Access;
   begin
      pragma Debug
        (D (D_RNS,
            "Send " & Request'Img &
            " to "  & Partition'Img));

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

      Send (Partition, Filtering, Stream'Access);
   end Send_Message;

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
   -- Set_Registration_Filter --
   -----------------------------

   procedure Set_Registration_Filter (Filter : in String) is
   begin
      Set_Info (Get (Registration_Filter_Name), To_Natural (Get (Filter)));
   end Set_Registration_Filter;

end System.Garlic.Filters;
