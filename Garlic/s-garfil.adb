------------------------------------------------------------------------------
--                                                                          --
--                           GLADE COMPONENTS                               --
--                                                                          --
--               S Y S T E M . G A R L I C . F I L T E R S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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

-- This  package  is part of  the  transparent data filtering  extension to --
-- GARLIC  developed at  the  Software Engineering Laboratory of the  Swiss --
-- Federal Institute of Technology in Lausanne (EPFL).                      --

with Ada.Streams;
with System.Garlic;
with System.Garlic.Debug;   use System.Garlic.Debug;
with System.Garlic.Heart;   use System.Garlic.Heart;
with System.Garlic.Options; use System.Garlic.Options;
with System.Garlic.Streams; use System.Garlic.Streams;
with System.RPC;            use System.RPC;

package body System.Garlic.Filters is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("FILTER", "(s-garfil): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use System.Garlic.Streams;

   type String_Access is access all String;
   --  Partition names are stored in dynamically allocated memory!

   type Filter_Code is (Query_Name,          --  <empty>
                        Tell_Name,           --  Name, Public Params
                        Set_Session_Params,  --  Public (Name, Private)
                        Invalidate);         --  Private (Partition_ID)

   --  Declarations of locally used subroutines.

   function Default_Filter_Params_Write
      return Stream_Element_Access;
   --  Write this partition's public parameters for the default filter into
   --  a stream element array and return it.

   function Default_Filter_Params_Write
      (Partition : System.RPC.Partition_ID)
      return Stream_Element_Access;
   --  Write 'Partition's public parameters for the default filter into
   --  a stream element array and return it. If the public parameters of
   --  'Partition' are still unknown, ask for them!

   function Default_Filter_Params_Read
      (Buffer : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access;
   --  Read some parameters for the default filter from the given array.

   function Filter_Params_Write
      (To_Partition : System.RPC.Partition_ID;
       F_Params     : Filter_Params_Access)
      return Stream_Element_Access;
   --  Write some parameters for the filter to be used with 'From_Partition'.

   function Filter_Params_Read
      (From_Partition : System.RPC.Partition_ID;
       Buffer         : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access;
   --  Read some parameters for the filter to be used with 'From_Partition'.


   --  Filtering routines.

   function Default_Filter_Incoming
      (From_Partition : in System.RPC.Partition_ID;
       Stream         : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access;
   --  Note: 'From_Partition' is not needed, but passing it all the same
   --  makes that function's type compatible with 'Filter_Proc' above!

   function Default_Filter_Outgoing
      (To_Partition : in     System.RPC.Partition_ID;
       Stream       : access System.RPC.Params_Stream_Type)
      return Stream_Element_Access;

   function Filter_Incoming
      (From_Partition : in System.RPC.Partition_ID;
       Stream         : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access;

   function Filter_Outgoing
      (To_Partition : in     System.RPC.Partition_ID;
       Stream       : access System.RPC.Params_Stream_Type)
      return Stream_Element_Access;

   --  The following types and the protected table are used to keep track
   --  of the different filtering methods and their registration. The
   --  'Filter_Methods_Table' below provides the translation between filter
   --  names and objects.

   Max_Filters : constant := 32;
   --  This constant may be changed.

   subtype Filter_Idx is Positive range 1 .. Max_Filters;

   type Filter_Array is array (Filter_Idx) of Filter_Access;
   --  The table in which the registered filter types will be stored

   --------------------------
   -- Filter_Methods_Table --
   --------------------------

   protected Filter_Methods_Table is

      procedure Register_Filter (Filter : in Filter_Access);
      --  Adds a new filter into the filter table

      entry Wait_For_Filter (Name   : in  String;
                             Filter : out Filter_Access);
      --  Waits until a filter with the given name has been registered
      --  and then returns it.

   private

      Filters   : Filter_Array := (others => null);
      New_Entry : Boolean := False;     --  Local barrier

      entry Queue (Name   : in String;
                   Filter : out Filter_Access);
      --  Wait queue for 'Wait_For_Filter'.

   end Filter_Methods_Table;

   --------------------------
   -- Filter_Methods_Table --
   --------------------------

   protected body Filter_Methods_Table is

      ------------------------------------------
      -- Filter_Methods_Table.Register_Filter --
      ------------------------------------------

      procedure Register_Filter
         (Filter : in Filter_Access) is
         Name : String := Get_Name (Filter.all);
      begin
         --  Check whether that filter already is registered.
         for I in Filters'Range loop
            if Filters (I) /= null and then
               Name = Get_Name (Filters (I).all)
            then
               --  Watch it! Duplicate name!
               raise Filter_Already_Registered;
            end if;
         end loop;
         --  Now let's look for an empty table entry
         for I in Filters'Range loop
            if Filters (I) = null then
               pragma Debug (D (D_Table,
                  "Registering " & Name & Integer'Image (I)));
               --  This slot is free, lets grab it
               Filters (I) := Filter;
               if (Queue'Count > 0) then
                  New_Entry := True;      --  Open barrier
               end if;
               return;
            end if;
         end loop;
         --  If we come up to here, our table is full!
         raise Too_Many_Filters;
      end Register_Filter;

      ------------------------------------------
      -- Filter_Methods_Table.Wait_For_Filter --
      ------------------------------------------

      entry Wait_For_Filter (Name   : in String;
                             Filter : out Filter_Access)
         when not New_Entry is
      begin
         for I in Filters'Range loop
            if Filters (I) /= null and then
               Name = Get_Name (Filters (I).all)
            then
               Filter := Filters (I);
               return;
            end if;
         end loop;
         pragma Debug (D (D_Debug, "Waiting for filter '" & Name &
            "' to be registered"));
         requeue Queue with abort;
      end Wait_For_Filter;

      --  Private entries implementing wait queues below.

      entry Queue (Name   : in String;
                   Filter : out Filter_Access)
         when New_Entry is
      begin
         if Queue'Count = 0 then
            New_Entry := False;
         end if;
         requeue Wait_For_Filter with abort;
      end Queue;

   end Filter_Methods_Table;

   --  The following table maintains the information which filter is to be
   --  used for communication with which partition. Both partitions and
   --  Filters are identified by names (i.e., strings).
   --     The 'Partition_Filter_Table' below also stores the name of the
   --  default filter name.

   type Channel_Desc is
      record
         Dest   : String_Access := null;
         Method : String_Access := null;
      end record;

   type Partition_Filter_Array is array (Partition_ID) of Channel_Desc;

   protected Partition_Filter_Table is

      procedure Enter
        (Partition_Name, Filter_Name : in String);

      function Get
        (Partition_Name : String)
        return String_Access;
      --  If no entry for a partition with the given name is found,
      --  we just return the default filter name 'Default_Filter_Name'.

      procedure Set_Default
        (Default_Name : in String);
      --  Defines the default filter to use on channels.

      procedure Set_Register
        (Default_Name : in String);
      --  Defines the registration filter.

      function Get_Register return String_Access;
      --  Returns No_Filter_Name'Access if not set.

   private
      Channels             : Partition_Filter_Array;
      Default_Filter_Name  : String_Access := No_Filter_Name'Access;
      Register_Filter_Name : String_Access := No_Filter_Name'Access;
   end Partition_Filter_Table;

   ----------------------------
   -- Partition_Filter_Table --
   ----------------------------

   protected body Partition_Filter_Table is

      ----------------------------------
      -- Partition_Filter_Table.Enter --
      ----------------------------------

      procedure Enter (Partition_Name, Filter_Name : in String) is
      begin
         for I in Channels'Range loop
            if Channels (I).Dest /= null and then
               Channels (I).Dest.all = Partition_Name
            then
               if Channels (I).Method.all /= Filter_Name then
                  --  Only one filter method for a certain partition,
                  --  changing it on the fly is not allowed!
                  raise Program_Error;
               else
                  --  It's already in the table: nothing to do!
                  return;
               end if;
            end if;
         end loop;
         --  This partition hasn't been entered yet.
         for I in Channels'Range loop
            if Channels (I).Dest = null then
               --  Empty entry found, fill it.
               Channels (I).Dest   := new String'(Partition_Name);
               Channels (I).Method := new String'(Filter_Name);
               return;
            end if;
         end loop;
         raise Too_Many_Filters;
      end Enter;

      --------------------------------
      -- Partition_Filter_Table.Get --
      --------------------------------

      function Get
        (Partition_Name : String)
        return String_Access is
      begin
         for I in Channels'Range loop
            if Channels (I).Dest /= null and then
               Channels (I).Dest.all = Partition_Name
            then
               return Channels (I).Method;
            end if;
         end loop;
         pragma Debug (D (D_Debug, "No filter for partition '" &
            Partition_Name & "' found, using default"));
         --  If no entry for this partition name was found, we requeue and
         --  wait until it is entered.
         return Default_Filter_Name;
      end Get;

      -----------------------------------------
      -- Partition_Filter_Table.Set_Register --
      -----------------------------------------

      procedure Set_Register (Default_Name : in String) is
      begin
         Register_Filter_Name := new String'(Default_Name);
      end Set_Register;

      ----------------------------------------
      -- Partition_Filter_Table.Get_Default --
      ----------------------------------------

      function Get_Register return String_Access is
      begin
         return Register_Filter_Name;
      end Get_Register;

      -----------------------------------------
      -- Partition_Filter_Table.Set_Register --
      -----------------------------------------

      procedure Set_Default (Default_Name : in String) is
      begin
         Default_Filter_Name := new String'(Default_Name);
      end Set_Default;

   end Partition_Filter_Table;

   --  This table takes care of all translations between dynamic partition
   --  IDs and partition names.

   type State_Of_Data is (Unknown, Pending, Valid);

   type Partition_Map_Desc is
      record
         Name       : String_Access := null;
         State      : State_Of_Data := Unknown;
         Told       : Boolean       := False;
         Tell_State : State_Of_Data := Unknown;
      end record;

   type Partition_Map is array (Partition_ID) of Partition_Map_Desc;

   protected Partition_Name_Table is

      entry     To_Static (Dynamic : in  Partition_ID;
                           Name    : out String_Access);
      --  Maps a dynamic partition ID to that partition's name. If that
      --  name is still unknown, waits until it is entered.

      procedure Enter     (Dynamic : in Partition_ID;
                           Name    : in String);
      --  Enter the name of a partition.

      entry     Get_My_Name (Name : out String_Access);
      --  Returns the name of this partition, if - for some unknown
      --  reason - "s-garela" didn't yet enter the name, we'll wait
      --  for it to do so.

      procedure Set_My_Name (Name : in  String);
      --  Invoked by "s-garela".

      entry Did_I_Tell (Dynamic : in  Partition_ID;
                        Told    : out Boolean);

      procedure Told (Dynamic : in Partition_ID);

   private
      Map       : Partition_Map;
      New_Entry : Boolean := False;  --  Local barrier
      New_Tell  : Boolean := False;  --  Local barrier

      My_Name   : String_Access := null;

      entry Queue (Dynamic : in  Partition_ID;
                   Name    : out String_Access);
      --  Local wait queue for 'To_Static'.

      entry Tell_Queue (Dynamic : in  Partition_ID;
                        Told    : out Boolean);
      --  Local wait queue for 'Did_I_Tell'.

   end Partition_Name_Table;

   --------------------------
   -- Partition_Name_Table --
   --------------------------

   protected body Partition_Name_Table is

      ------------------------------------
      -- Partition_Name_Table.To_Static --
      ------------------------------------

      entry To_Static (Dynamic : in  Partition_ID;
                       Name    : out String_Access)
         when not New_Entry is
      begin
         if Map (Dynamic).State = Unknown then
            Map (Dynamic).State := Pending;
            Name := null;
         elsif Map (Dynamic).State = Valid then
            Name := Map (Dynamic).Name;
         else
            pragma Debug (D (D_Debug, "Waiting for name of partition" &
               Partition_ID'Image (Dynamic)));
            requeue Queue with abort;
         end if;
      end To_Static;

      --------------------------------
      -- Partition_Name_Table.Enter --
      --------------------------------

      procedure Enter (Dynamic : in Partition_ID;
                       Name    : in String) is
      begin
         if Map (Dynamic).State = Valid and then
            Name /= Map (Dynamic).Name.all
         then
            --  Uh-oh. While it is very well possible that we try to record
            --  a certain relation several times, we cannot tolerate that
            --  it be registered with different names!
            raise Program_Error;
         end if;
         pragma Debug (D (D_Debug,
            "Entered name-ID pair ('" & Name & "'," &
            Partition_ID'Image (Dynamic) & ") in table!"));
         Map (Dynamic).Name  := new String'(Name);
         Map (Dynamic).State := Valid;
         if Queue'Count > 0 then
            New_Entry := True;  --  Open barrier
         end if;
      end Enter;

      -------------------------------------
      -- Partition_Name_Table.Did_I_Tell --
      -------------------------------------

      entry Did_I_Tell (Dynamic : in  Partition_ID;
                        Told    : out Boolean)
         when not New_Tell is
      begin
         if Map (Dynamic).Tell_State = Unknown then
            Map (Dynamic).Tell_State := Pending;
            Told := False;
         elsif Map (Dynamic).Tell_State = Pending then
            requeue Tell_Queue with abort;
         else
            Told := Map (Dynamic).Told;
         end if;
      end Did_I_Tell;

      -------------------------------
      -- Partition_Name_Table.Told --
      -------------------------------

      procedure Told (Dynamic : in Partition_ID) is
      begin
         Map (Dynamic).Told := True;
         Map (Dynamic).Tell_State := Valid;
         if Tell_Queue'Count > 0 then
            New_Tell := True;
         end if;
      end Told;

      --------------------------------------
      -- Partition_Name_Table.Get_My_Name --
      --------------------------------------

      entry Get_My_Name (Name : out String_Access)
         when My_Name /= null is
      begin
         Name := My_Name;
      end Get_My_Name;

      --------------------------------------
      -- Partition_Name_Table.Set_My_Name --
      --------------------------------------

      procedure Set_My_Name (Name : in  String) is
      begin
         My_Name := new String'(Name);
      end Set_My_Name;

      --  Private entries implementing wait queues below.

      entry Queue (Dynamic : in  Partition_ID;
                   Name    : out String_Access)
         when New_Entry is
      begin
         if Queue'Count = 0 then
            New_Entry := False;
         end if;
         requeue To_Static with abort;
      end Queue;

      entry Tell_Queue (Dynamic : in  Partition_ID;
                        Told    : out Boolean)
         when New_Tell is
      begin
         if Tell_Queue'Count = 0 then
            New_Tell := False;
         end if;
         requeue Did_I_Tell with abort;
      end Tell_Queue;

   end Partition_Name_Table;

   function Partition_To_Filter
      (Dynamic_ID : Partition_ID)
      return Filter_Access;

   -------------------------
   -- Partition_To_Filter --
   -------------------------

   function Partition_To_Filter
      (Dynamic_ID : Partition_ID)
      return Filter_Access is
      Name   : String_Access;
      Filter : Filter_Access;
   begin
      Partition_Name_Table.To_Static (Dynamic_ID, Name);
      if Name = null then
         --  We don't know that name yet?? Go ask the partition!
         declare
            Params : aliased Params_Stream_Type (0);
         begin
            Filter_Code'Write (Params'Access, Query_Name);
            Send (Dynamic_ID, Filtering, Params'Access);
         end;
         Partition_Name_Table.To_Static (Dynamic_ID, Name);
      end if;
      Name := Partition_Filter_Table.Get (Name.all);
      Filter_Methods_Table.Wait_For_Filter (Name.all, Filter);
      return Filter;
   end Partition_To_Filter;

   --  The following types and the protected table are used to
   --  keep track of the mapping from Partition_ID to the
   --  filter and parameters that are used to communicate with it

   type Filter_Desc  is
      record
         Private_State   : State_Of_Data        := Unknown;
         Public_State    : State_Of_Data        := Unknown;
         Private_Params  : Filter_Params_Access := null;
         Public_Params   : Filter_Params_Access := null;
      end record;

   type Filter_Table is array (Partition_ID) of Filter_Desc;

   protected Partition_Filter_Params_Table is

      entry Wait_For_Params
         (Partition : in  Partition_ID;
          Params    : out Filter_Params_Access);
      --  Returns 'null' upon the first call, subsequent calls will wait
      --  until somebody enters the parameters for this partition by
      --  calling 'Set_Filter_Params'.

      entry Wait_For_Default_Params
         (Partition : in  Partition_ID;
          Params    : out Filter_Params_Access);
      --  Waits until somebody enters the partition's default parameters
      --  by calling 'Set_Default_Params', then returns these parameters.

      procedure Set_Filter_Params
         (Partition : in Partition_ID;
          Params    : in Filter_Params_Access);

      procedure Set_Default_Params
         (Partition : in Partition_ID;
          Params    : in Filter_Params_Access);

      procedure Set_My_Defaults
         (Filter          : in Filter_Access;
          Private_Params,
          Public_Params   : in Filter_Params_Access);

      entry Get_My_Defaults
         (Filter          : out Filter_Access;
          Private_Params,
          Public_Params   : out Filter_Params_Access);

   private

      Default_Params_Arrived : Boolean := False;
      Filters                : Filter_Table;
      Default_Params         : Filter_Desc;
      Default_Filter         : Filter_Access := null;

      New_Params             : Boolean := False;  --  Local barrier
      New_Default_Params     : Boolean := False;  --  Local barrier

      entry Queue (Partition : in  Partition_ID;
                   Params    : out Filter_Params_Access);
      --  Wait queue for 'Wait_For_Params'.

      entry Default_Queue (Partition : in  Partition_ID;
                           Params    : out Filter_Params_Access);
      --  Wait queue for 'Wait_For_Default_Params'.

   end Partition_Filter_Params_Table;

   -----------------------------------
   -- Partition_Filter_Params_Table --
   -----------------------------------

   protected body Partition_Filter_Params_Table is

      ---------------------------------------------------
      -- Partition_Filter_Params_Table.Wait_For_Params --
      ---------------------------------------------------

      entry Wait_For_Params
         (Partition : in  Partition_ID;
          Params    : out Filter_Params_Access)
         when not New_Params is
      begin
         if Filters (Partition).Private_State = Pending then
            --  Just wait for it to arrive
            requeue Queue with abort;
         elsif Filters (Partition).Private_State = Unknown then
            Filters (Partition).Private_State := Pending;
            Params := null;
         else
            Params := Filters (Partition).Private_Params;
         end if;
      end Wait_For_Params;

      ----------------------------------------------------------
      -- Partition_Filter_Params_Table.Wait_For_Default_Params --
      ----------------------------------------------------------

      entry Wait_For_Default_Params
         (Partition : in  Partition_ID;
          Params    : out Filter_Params_Access)
         when not New_Default_Params is
      begin
         if Filters (Partition).Public_State = Pending then
            requeue Default_Queue with abort;
         elsif Filters (Partition).Public_State = Unknown then
            Filters (Partition).Public_State := Pending;
            Params := null;
         else
            Params := Filters (Partition).Public_Params;
         end if;
      end Wait_For_Default_Params;

      -----------------------------------------------------
      -- Partition_Filter_Params_Table.Set_Filter_Params --
      -----------------------------------------------------

      procedure Set_Filter_Params
         (Partition : in System.RPC.Partition_ID;
          Params    : in Filter_Params_Access) is
      begin
         pragma Debug (D (D_Debug,
            "Entered params for partition " &
            Partition_ID'Image (Partition)));
         Filters (Partition).Private_Params := Params;
         Filters (Partition).Private_State  := Valid;
         if Queue'Count > 0 then
            New_Params := True;    --  Open barrier
         end if;
      end Set_Filter_Params;

      -----------------------------------------------------
      -- Partition_Filter_Params_Table.Set_Default_Params --
      -----------------------------------------------------

      procedure Set_Default_Params
         (Partition : in System.RPC.Partition_ID;
          Params    : in Filter_Params_Access) is
      begin
         pragma Debug (D (D_Debug,
            "Entered default parameters of partition" &
            Partition_ID'Image (Partition)));
         Filters (Partition).Public_Params := Params;
         Filters (Partition).Public_State  := Valid;
         if Default_Queue'Count > 0 then
            New_Default_Params := True;    --  Open barrier
         end if;
      end Set_Default_Params;

      ---------------------------------------------------
      -- Partition_Filter_Params_Table.Set_My_Defaults --
      ---------------------------------------------------

      procedure Set_My_Defaults
         (Filter          : in Filter_Access;
          Private_Params,
          Public_Params   : in Filter_Params_Access) is
      begin
         Default_Filter := Filter;
         Default_Params := (Valid, Valid, Private_Params, Public_Params);
      end Set_My_Defaults;

      ---------------------------------------------------
      -- Partition_Filter_Params_Table.Get_My_Defaults --
      ---------------------------------------------------

      entry Get_My_Defaults
         (Filter          : out Filter_Access;
          Private_Params,
          Public_Params   : out Filter_Params_Access)
         when Default_Params.Private_State /= Pending is
      begin
         Filter         := Default_Filter;
         Private_Params := Default_Params.Private_Params;
         Public_Params  := Default_Params.Public_Params;
         if Default_Params.Private_State = Unknown then
            Default_Params.Private_State := Pending;
         end if;
      end Get_My_Defaults;

      --  Private entries implementing wait queues below.

      entry Queue (Partition : in  Partition_ID;
                   Params    : out Filter_Params_Access)
         when New_Params is
      begin
         if Queue'Count = 0 then
            New_Params := False;
         end if;
         requeue Wait_For_Params with abort;
      end Queue;

      entry Default_Queue (Partition : in  Partition_ID;
                           Params    : out Filter_Params_Access)
         when New_Default_Params is
      begin
         if Default_Queue'Count = 0 then
            New_Default_Params := False;
         end if;
         requeue Wait_For_Default_Params with abort;
      end Default_Queue;

   end Partition_Filter_Params_Table;

   --  The following procedure handles all messages of class 'Filtering'.
   --  It is registered in the body with Garlic.Heart as the receiver for
   --  all messages of this kind.

   procedure Tell_My_Name (Partition : in Partition_ID);

   ------------------
   -- Tell_My_Name --
   ------------------

   procedure Tell_My_Name (Partition : in Partition_ID) is
      My_Name : String_Access;
      Answer  : aliased Params_Stream_Type (0);
   begin
      Partition_Name_Table.Get_My_Name (My_Name);
      Filter_Code'Write (Answer'Access, Tell_Name);
      String'Output (Answer'Access, My_Name.all);
      Ada.Streams.Stream_Element_Array'Output
         (Answer'Access, Default_Filter_Params_Write.all);
      Send (Partition, Filtering, Answer'Access);
   end Tell_My_Name;

   procedure Message_Handler
      (Partition : in     Partition_ID;
       Operation : in     Public_Opcode;
       Params    : access Params_Stream_Type);

   ---------------------
   -- Message_Handler --
   ---------------------

   procedure Message_Handler
      (Partition : in     Partition_ID;
       Operation : in     Public_Opcode;
       Params    : access Params_Stream_Type) is

      Opcode : Filter_Code;

   begin
      Filter_Code'Read (Params, Opcode);
      if not Opcode'Valid then
         pragma Debug (D (D_Debug, "Invalid filter code received"));
         raise Constraint_Error;
      end if;
      pragma Debug (D (D_Debug, "Got a message of type " &
         Filter_Code'Image (Opcode)));
      case Opcode is

         when Query_Name =>
            --  We're being asked for our name (and public parameters)
            --  Send back a 'Tell_Name' message.
            Tell_My_Name (Partition);

         when Tell_Name =>
            --  Somebody told us his name (and public parameters).
            declare
               Name : String := String'Input (Params);
            begin
               Partition_Name_Table.Enter (Partition, Name);
               Partition_Filter_Params_Table.Set_Default_Params
                 (Partition,
                  Default_Filter_Params_Read
                    (Ada.Streams.Stream_Element_Array'Input (Params)));
            end;

         when Set_Session_Params =>
            --  Somebody told us the session parameters to use on this
            --  channel. This message must be run through the public
            --  filter!
            declare
               Buffer          : Ada.Streams.Stream_Element_Array
                 := To_Stream_Element_Array (Params);
               Filtered_Data   : Stream_Element_Access
                 := Default_Filter_Incoming (Partition, Buffer);
               Filtered_Params : aliased Params_Stream_Type
                                            (Filtered_Data'Length);
            begin
               To_Params_Stream_Type (Filtered_Data.all,
                                      Filtered_Params'Access);
               Free (Filtered_Data);
               declare
                  Name : String := String'Input (Filtered_Params'Access);
               begin
                  Partition_Name_Table.Enter (Partition, Name);
                  Partition_Filter_Params_Table.Set_Filter_Params
                    (Partition,
                     Filter_Params_Read
                       (Partition, Ada.Streams.Stream_Element_Array'Input
                                      (Filtered_Params'Access)));
               end;
            end;

         when Invalidate =>
            --  Invalidate any information on the partition mentioned in
            --  the message. Must be run through the private filter for
            --  'Partition'!
            declare
               Buffer          : Ada.Streams.Stream_Element_Array
                 := To_Stream_Element_Array (Params);
               Filtered_Data   : Stream_Element_Access
                 := Filter_Incoming (Partition, Buffer);
               Filtered_Params : aliased Params_Stream_Type
                                            (Filtered_Data'Length);
            begin
               To_Params_Stream_Type (Filtered_Data.all,
                                      Filtered_Params'Access);
               Free (Filtered_Data);
               declare
                  Old_Partition : Partition_ID;
               begin
                  Partition_ID'Read (Filtered_Params'Access, Old_Partition);
                  --  Invalidate (Old_Partition);
               end;
            end;

      end case;
   exception
      when others =>
         pragma Debug (D (D_Debug, "Message_Handler: fatal error"));
         raise Communication_Error;
   end Message_Handler;

   --  The following table tells us for which opcodes we have to filter.

   type Do_Filter_Table is array (Opcode) of Boolean;

   Do_Filter : Do_Filter_Table :=
     (Set_Location => False,
      Filtering    => False,
      others       => True);

   --  The two exported routines for filtering data.

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
      (To_Partition : in     System.RPC.Partition_ID;
       Operation    : in     System.Garlic.Heart.Opcode;
       Params       : access System.RPC.Params_Stream_Type)
      return Stream_Element_Access is

      Told : Boolean;

   begin
      pragma Debug (D (D_Debug, "Generic filter outgoing"));
      if Do_Filter (Operation) then
         if Operation in Internal_Opcode then
            Partition_Name_Table.Did_I_Tell (To_Partition, Told);
            if not Told then
               Tell_My_Name (To_Partition);
               Partition_Name_Table.Told (To_Partition);
            end if;
            return Default_Filter_Outgoing (To_Partition, Params);
         elsif Operation in Public_Opcode then
            Partition_Name_Table.Did_I_Tell (To_Partition, Told);
            if not Told then
               Tell_My_Name (To_Partition);
               Partition_Name_Table.Told (To_Partition);
            end if;
            return Filter_Outgoing (To_Partition, Params);
         else
            raise Constraint_Error;
         end if;
      end if;
      return To_Stream_Element_Access (Params);
   end Filter_Outgoing;

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
      (From_Partition : in System.RPC.Partition_ID;
       Operation      : in System.Garlic.Heart.Opcode;
       Params         : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access is

      use type Ada.Streams.Stream_Element_Offset;

   begin
      pragma Debug (D (D_Debug, "Generic filter incoming"));
      if Do_Filter (Operation) then
         if Operation in Internal_Opcode then
            return Default_Filter_Incoming (From_Partition, Params);
         elsif Operation in Public_Opcode then
            return Filter_Incoming (From_Partition, Params);
         else
            raise Constraint_Error;
         end if;
      end if;
      return new Ada.Streams.Stream_Element_Array'(Params);
   end Filter_Incoming;

   ---------
   -- Dbg --
   ---------

   procedure Dbg (Stream : in Ada.Streams.Stream_Element_Array;
                  Msg    : in String);
   --  Only for debugging purposes. (Spec only to shut up GNAT's style
   --  warning!)


   procedure Dbg (Stream : in Ada.Streams.Stream_Element_Array;
                  Msg    : in String) is
      Max_Length : constant := 60;
      S          : String (1 .. Max_Length) := (others => ' ');
      I          : Natural := 0;
   begin
      pragma Debug (D (D_Dump, Msg));
      for J in Stream'Range loop
         declare
            E : String := Ada.Streams.Stream_Element'Image (Stream (J));
         begin
            if I + E'Length > Max_Length then
               --  Would overflow the buffer!
               --  (Assumption: E'Length < Max_Length)
               if I > 0 then
                  pragma Debug (D (D_Dump, S (1 .. I)));
                  null;
               end if;
               I := 0;
            end if;
            S (I + 1 .. I + E'Length) := E;
            I := I + E'Length;
         end;
      end loop;
      if I > 1 then
         pragma Debug (D (D_Dump, S (1 .. I)));
         null;
      end if;
   end Dbg;

   --  Standard filtering routines for outgoing and incoming data.

   function Filter_Data_Out
      (Method    : in     Filter_Access;
       Params    : in     Filter_Params_Access;
       Data      : access System.RPC.Params_Stream_Type;
       Msg       : in     String := "")
      return Stream_Element_Access;

   function Filter_Data_Out
      (Method    : in     Filter_Access;
       Params    : in     Filter_Params_Access;
       Data      : access System.RPC.Params_Stream_Type;
       Msg       : in     String := "")
      return Stream_Element_Access is
   begin
      pragma Debug (D (D_Debug, "Just before filtering..."));
      declare
         Filtered_Data : constant Stream_Element_Access
           := Filter_Outgoing (Method.all, Params, Data);
      begin
         pragma Debug (
            Dbg (Filtered_Data.all,
                 "Data after filtering (OUTGOING, " & Msg & ")"));
         return Filtered_Data;
      end;
   end Filter_Data_Out;

   function Filter_Data_In
      (Method    : in Filter_Access;
       Params    : in Filter_Params_Access;
       Data      : in Ada.Streams.Stream_Element_Array;
       Msg       : in String := "")
      return Stream_Element_Access;

   function Filter_Data_In
      (Method    : in Filter_Access;
       Params    : in Filter_Params_Access;
       Data      : in Ada.Streams.Stream_Element_Array;
       Msg       : in String := "")
      return Stream_Element_Access is
   begin
      pragma Debug (
            Dbg (Data,
                 "Data before filtering (INCOMING, " & Msg & ")"));
      declare
         Filtered_Data : constant Stream_Element_Access
           := Filter_Incoming (Method.all, Params, Data);
      begin
         pragma Debug (
               Dbg (Filtered_Data.all,
                    "Data after filtering (INCOMING, " & Msg & ")"));
         return Filtered_Data;
      end;
   end Filter_Data_In;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
      (To_Partition : in     System.RPC.Partition_ID;
       Stream       : access System.RPC.Params_Stream_Type)
      return Stream_Element_Access is

      Filter_Method         : Filter_Access;
      Params                : Filter_Params_Access;
      Private_Params        : Filter_Params_Access;
      Needs_Params_Exchange : Boolean;

   begin
      pragma Debug (D (D_Debug,
         "'Filter_Outgoing' dyn =" &
         System.RPC.Partition_ID'Image (To_Partition)));
      --  Get the Filter method from the table
      Filter_Method := Partition_To_Filter (To_Partition);
      --  Get the Params
      pragma Debug (D (D_Debug, "Got filter method..."));
      Partition_Filter_Params_Table.Wait_For_Params
          (To_Partition, Params);
      if Params = null then
         pragma Debug (D (D_Debug, "I have to create new params"));
         Generate_Params (Filter_Method.all, Params,
                          Private_Params, Needs_Params_Exchange);
         if Needs_Params_Exchange = True then
            pragma Debug (D (D_Debug, "I have to exchange parameters"));
            declare
               Buffer     : Stream_Element_Access :=
                 Filter_Params_Write (To_Partition, Params);
               Par_Stream : aliased Params_Stream_Type (0);
               My_Name    : String_Access;
            begin
               Partition_Name_Table.Get_My_Name (My_Name);
               String'Output (Par_Stream'Access, My_Name.all);
               Ada.Streams.Stream_Element_Array'Output
                  (Par_Stream'Access, Buffer.all);
               Free (Buffer);
               declare
                  Code_Buffer : Stream_Element_Access :=
                    Default_Filter_Outgoing (To_Partition, Par_Stream'Access);
                  Code_Stream : aliased Params_Stream_Type (0);
               begin
                  Filter_Code'Write (Code_Stream'Access, Set_Session_Params);
                  Ada.Streams.Stream_Element_Array'Output
                     (Code_Stream'Access, Code_Buffer.all);
                  Free (Code_Buffer);
                  Send (To_Partition, Filtering, Code_Stream'Access);
               end;
            end;
         end if;
         Partition_Filter_Params_Table.Set_Filter_Params
           (To_Partition, Params);
         pragma Debug (D (D_Debug,
            "Params to partition" & Partition_ID'Image (To_Partition) &
            " is now in the table"));
      end if;
      return Filter_Data_Out (Filter_Method, Params, Stream);
   end Filter_Outgoing;

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
      (From_Partition : in System.RPC.Partition_ID;
       Stream         : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access is

      Filter_Method            : Filter_Access;
      Params, Private_Params   : Filter_Params_Access;
      Needs_Params_Exchange    : Boolean;

   begin
      pragma Debug (D (D_Debug,
         "'Filter_Incoming' dyn =" &
         System.RPC.Partition_ID'Image (From_Partition)));
      --  Get the filter method from the table
      Filter_Method := Partition_To_Filter (From_Partition);

      --  Get the parameters
      Partition_Filter_Params_Table.Wait_For_Params
          (From_Partition, Params);

      if Params = null then
         Generate_Params (Filter_Method.all, Params,
                          Private_Params, Needs_Params_Exchange);
         if Needs_Params_Exchange = True then
            --  I guess we'd better wait until the parameters arrive -
            --  don't use the ones we just generated!
            Partition_Filter_Params_Table.Wait_For_Params
               (From_Partition, Params);
         else
            --  We have a filter that doesn't need a parameter transfer -
            --  in other words, our locally generated params are sufficient
            --  to filter the messages: enter the new parameters and then
            --  continue!
            Partition_Filter_Params_Table.Set_Filter_Params
               (From_Partition, Params);
         end if;
         pragma Debug (D (D_Debug,
            "Params from partition" & Partition_ID'Image (From_Partition) &
            " are now in the table"));
      end if;
      return Filter_Data_In (Filter_Method, Params, Stream);
   end Filter_Incoming;

   --  Internal auxiliary routine to properly initialize the default
   --  filter.

   function Get_Default_Filter return Filter_Access;

   function Get_Default_Filter return Filter_Access is
      Private_Params,
      Public_Params  : Filter_Params_Access;
      Default_Filter : Filter_Access;
      Default_Name   : String_Access;
      Must_Exchange  : Boolean;
   begin
      Partition_Filter_Params_Table.Get_My_Defaults
         (Default_Filter, Public_Params, Private_Params);
      if Default_Filter = null then
         --  Get the default filter's name
         Default_Name := Partition_Filter_Table.Get_Register;
         --  Get the default filter object, waiting until it is registered.
         Filter_Methods_Table.Wait_For_Filter
           (Default_Name.all, Default_Filter);
         --  Once we have the object, generate our parameters...
         Generate_Params (Default_Filter.all, Public_Params,
                          Private_Params, Must_Exchange);
         --  ... and then store them.
         Partition_Filter_Params_Table.Set_My_Defaults
            (Default_Filter, Public_Params, Private_Params);
         pragma Debug (D (D_Debug, "Generated default Params"));
         --  Just a sanity check
         if Must_Exchange then
            raise Not_A_Public_Params_Algorithm;
         end if;
      end if;
      return Default_Filter;
   end Get_Default_Filter;

   function Get_Default_Params (Partition : in Partition_ID)
      return Filter_Params_Access;

   ------------------------
   -- Get_Default_Params --
   ------------------------

   function Get_Default_Params (Partition : in Partition_ID)
      return Filter_Params_Access is

      Public_Params : Filter_Params_Access;

   begin
      Partition_Filter_Params_Table.Wait_For_Default_Params
         (Partition, Public_Params);

      if Public_Params = null then
         --  If we don't know them yet, ask that partition for its name and
         --  public parameters. Then wait until the answer arrives.
         declare
            Params : aliased Params_Stream_Type (0);
         begin
            Filter_Code'Write (Params'Access, Query_Name);
            Send (Partition, Filtering, Params'Access);
         end;
         Partition_Filter_Params_Table.Wait_For_Default_Params
            (Partition, Public_Params);
      end if;
      return Public_Params;
   end Get_Default_Params;

   --  Default filtering routines. Note: they will create our own private
   --  and public parameters, if that wasn't done yet. (BTW, the same also
   --  is true for reading and writing default filter parameters, see
   --  below.)

   -----------------------------
   -- Default_Filter_Outgoing --
   -----------------------------

   function Default_Filter_Outgoing
      (To_Partition : in     System.RPC.Partition_ID;
       Stream       : access System.RPC.Params_Stream_Type)
      return Stream_Element_Access is

      Public_Params    : Filter_Params_Access;  --  of 'To_Partition'
      Default_Filter   : Filter_Access;

   begin
      Default_Filter := Get_Default_Filter;

      pragma Debug (D (D_Debug,
         "Default_Filter_Outgoing a stream " &
         " with public Params of partition" &
         Partition_ID'Image (To_Partition)));

      --  Check if we already know the public Params of the
      --  receiver partition
      Public_Params := Get_Default_Params (To_Partition);

      pragma Debug (D (D_Debug,
         "Found public params for partition " &
         Partition_ID'Image (To_Partition)));

      return Filter_Data_Out (Default_Filter, Public_Params,
                              Stream, "default");
   end Default_Filter_Outgoing;

   ----------------------------
   -- Default_Filter_Incoming --
   ----------------------------

   function Default_Filter_Incoming
      (From_Partition : in System.RPC.Partition_ID;
       Stream         : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access is

      Public_Params,
      Private_Params   : Filter_Params_Access;  --  of myself
      Default_Filter   : Filter_Access;

   begin
      Default_Filter := Get_Default_Filter;
      Partition_Filter_Params_Table.Get_My_Defaults
         (Default_Filter, Public_Params, Private_Params);
      return Filter_Data_In (Default_Filter, Private_Params,
                             Stream, "default");
   end Default_Filter_Incoming;

   --  Subprograms for reading and writing filter parameters. There are two
   --  "write" functions for default parameters: one to write our own public
   --  parameters, and one to write some other partition's public parameters.

   ---------------------------------
   -- Default_Filter_Params_Write --
   ---------------------------------

   function Default_Filter_Params_Write
      return Stream_Element_Access is

      Private_Params,
      Public_Params    : Filter_Params_Access;
      Default_Filter   : Filter_Access;

   begin
      Default_Filter := Get_Default_Filter;
      pragma Debug (D (D_Debug, "Looking for my default params"));
      Partition_Filter_Params_Table.Get_My_Defaults
         (Default_Filter, Public_Params, Private_Params);
      pragma Debug (D (D_Debug, "Got my default params. Filter =" &
          Get_Name (Default_Filter.all)));
      return Filter_Params_Write (Default_Filter.all, Public_Params);
   end Default_Filter_Params_Write;

   ---------------------------------
   -- Default_Filter_Params_Write --
   ---------------------------------

   function Default_Filter_Params_Write
      (Partition : System.RPC.Partition_ID)
      return Stream_Element_Access is

      Public_Params    : Filter_Params_Access;  --  of 'Partition'
      Default_Filter   : Filter_Access;

   begin
      Default_Filter := Get_Default_Filter;
      --  Check if we already know the public Params of the
      --  receiver partition
      Public_Params := Get_Default_Params (Partition);
      pragma Debug (D (D_Debug,
         "Found default params for partition " &
         Partition_ID'Image (Partition)));
      return Filter_Params_Write (Default_Filter.all, Public_Params);
   end Default_Filter_Params_Write;

   -------------------------
   -- Filter_Params_Write --
   -------------------------

   function Filter_Params_Write
      (To_Partition : System.RPC.Partition_ID;
       F_Params     : Filter_Params_Access)
      return Stream_Element_Access is

      Filter_Method : Filter_Access;

   begin
      --  Get the Filter method from the table
      Filter_Method := Partition_To_Filter (To_Partition);
      return Filter_Params_Write (Filter_Method.all, F_Params);
   end Filter_Params_Write;

   --------------------------------
   -- Default_Filter_Params_Read --
   --------------------------------

   function Default_Filter_Params_Read
      (Buffer : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access is

      Default_Filter   : Filter_Access;

   begin
      Default_Filter := Get_Default_Filter;
      return Filter_Params_Read (Default_Filter.all, Buffer);
   end Default_Filter_Params_Read;

   ------------------------
   -- Filter_Params_Read --
   ------------------------

   function Filter_Params_Read
      (From_Partition : System.RPC.Partition_ID;
       Buffer         : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access is

      Filter_Method : Filter_Access;

   begin
      --  Get the Filter method from the table
      Filter_Method := Partition_To_Filter (From_Partition);
      return Filter_Params_Read (Filter_Method.all, Buffer);
   end Filter_Params_Read;

   --  Registration subprograms. 'Register_Filter' is called by each filter
   --  package to register itself (i.e., its filter object). The other
   --  procedures are called by "s-garela" and enter partition and filter
   --  names.

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter (Filter : in Filter_Access) is
   begin
      Filter_Methods_Table.Register_Filter (Filter);
   end Register_Filter;

   -----------------------------
   -- Set_Registration_Filter --
   -----------------------------

   procedure Set_Registration_Filter (Filter : in String) is
   begin
      pragma Debug (D (D_Debug, "Entering registration filter name"));
      Partition_Filter_Table.Set_Register (Filter);
   end Set_Registration_Filter;

   ------------------------
   -- Set_Default_Filter --
   ------------------------

   procedure Set_Default_Filter (Filter : in String) is
   begin
      pragma Debug (D (D_Debug, "Entering default filter for partition"));
      Partition_Filter_Table.Set_Default (Filter);
   end Set_Default_Filter;

   ------------------------
   -- Set_Channel_Filter --
   ------------------------

   procedure Set_Channel_Filter (Partition, Filter : in String) is
   begin
      pragma Debug (D (D_Debug, "Entering channel (" &
         Partition & ", " & Filter & ")"));
      Partition_Filter_Table.Enter (Partition, Filter);
   end Set_Channel_Filter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Partition_Name_Table.Set_My_Name (Options.Partition_Name.all);
      --  Register our message handler with Garlic.Heart to receive all
      --  messages of kind 'Filtering'.
      Receive (Filtering, Message_Handler'Access);
      pragma Debug (D (D_Debug, "Finished elaboration..."));
   end Initialize;

end System.Garlic.Filters;


