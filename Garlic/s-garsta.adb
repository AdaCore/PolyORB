------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . S T A R T U P                 --
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

with System.Garlic.Debug;             use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);

with System.Garlic.Elaboration;
pragma Elaborate_All (System.Garlic.Elaboration);

with System.Garlic.Exceptions;        use System.Garlic.Exceptions;
pragma Elaborate_All (System.Garlic.Exceptions);

with System.Garlic.Filters;
pragma Elaborate_All (System.Garlic.Filters);

with System.Garlic.Group;
pragma Elaborate_All (System.Garlic.Group);

with System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);

with System.Garlic.Name_Table;
pragma Elaborate_All (System.Garlic.Name_Table);
pragma Warnings (Off, System.Garlic.Name_Table);

with System.Garlic.Naming;
pragma Elaborate_All (System.Garlic.Naming);
pragma Warnings (Off, System.Garlic.Naming);

with System.Garlic.Options;           use System.Garlic.Options;
pragma Elaborate_All (System.Garlic.Options);

with System.Garlic.Partitions;
pragma Elaborate_All (System.Garlic.Partitions);

with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;

with System.Garlic.Protocols;         use System.Garlic.Protocols;
pragma Elaborate_All (System.Garlic.Protocols);

with System.Garlic.Storages.Config;   use System.Garlic.Storages.Config;
pragma Elaborate_All (System.Garlic.Storages.Config);

with System.Garlic.Protocols.Config;
pragma Elaborate_All (System.Garlic.Protocols.Config);

with System.Garlic.Services;
pragma Elaborate_All (System.Garlic.Services);

with System.Garlic.Streams;
pragma Elaborate_All (System.Garlic.Streams);

with System.Garlic.Termination;
pragma Elaborate_All (System.Garlic.Termination);

with System.Garlic.Trace;
pragma Elaborate_All (System.Garlic.Trace);

with System.Garlic.Types;             use System.Garlic.Types;
pragma Elaborate_All (System.Garlic.Types);

with System.Garlic.Units;
pragma Elaborate_All (System.Garlic.Units);

with System.Garlic.Utils;             use System.Garlic.Utils;
pragma Elaborate_All (System.Garlic.Utils);

package body System.Garlic.Startup is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARSTA", "(s-garsta): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   PID   : Partition_ID;
   Error : Error_Type;

   Self_Location : Location_Type;
   Self_Protocol : Protocol_Access;
   Self_Data     : String_Array_Access;

   Boot_Location : Location_Type;
   Boot_Protocol : Protocol_Access;
   Boot_Data     : String_Array_Access;

   N_Boot_Location : Natural := 0;
   N_Self_Location : Natural := 0;

   New_Boot_Location : String_Array_Access;
   New_Self_Location : String_Array_Access;

   Performed       : Boolean;

begin

   pragma Debug (D ("Entering partition startup phase"));

   --  At system startup, the operations occur in this order:

   --  (1) The elaboration code of System.Garlic.Startup initializes
   --      all the run-time options (command line and environment
   --      variables).

   System.Garlic.Options.Initialize_Default_Options;
   System.Garlic.Elaboration.Initialize;
   System.Garlic.Options.Initialize_User_Options;

   --  (2) The elaboration code of System.Garlic.Startup initializes
   --      all the soft links (the implementation of barrier, mutex and
   --      semaphores).
   --      The elaboration code of System.Garlic.Startup detaches the
   --      processus if needed.

   System.Garlic.Services.Initialize;

   --  (3) The elaboration code of System.Garlic.Startup initializes
   --      all the non-configurable units depending on soft links.
   --      - the stream storage pool
   --      - the core of the PCS
   --      - the partition id service
   --      - the group communication layer (for boot mirror partitions)

   System.Garlic.Streams.Initialize;
   System.Garlic.Heart.Initialize;
   System.Garlic.Partitions.Initialize;
   System.Garlic.Group.Initialize;

   --  (4) The elaboration code of System.Garlic.Startup initializes
   --      all the configurable units depending on soft links.

   System.Garlic.Termination.Initialize;

   --  (5) The elaboration code of System.Garlic.Startup executes
   --      and registers all the protocols the user wants to use.

   System.Garlic.Protocols.Config.Initialize;

   --  (5) The elaboration code of System.Garlic.Startup executes
   --      and registers all the storages the user wants to use.

   System.Garlic.Storages.Config.Initialize;

   --  (6) The elaboration code of System.Garlic.Startup initializes
   --      the boot locations. It declares all the boot locations on
   --      the boot partition. On another partition, it declares only
   --      the first usable boot location. That is the first location
   --      using a protocol present on the partition.

   if Options.Boot_Location /= null then
      for BL in Options.Boot_Location'Range loop
         Boot_Location := To_Location (Options.Boot_Location (BL).all);
         Boot_Protocol := Get_Protocol (Boot_Location);

         --  Is the protocol available?

         if Boot_Protocol /= null then
            Set_Boot_Data (Boot_Protocol, Get_Data (Boot_Location), Error);
            if Found (Error) then
               Raise_Communication_Error (Error);
            end if;

            N_Boot_Location := N_Boot_Location + 1;

            --  A boot partition has to set all its valid locations

            exit when not Options.Is_Boot_Server;
         end if;
      end loop;
   end if;

   --  (7) The elaboration code of System.Garlic.Startup initializes
   --      its self locations. Ignore self locations for which the
   --      protocol is not present. Then, initialize all the available
   --      protocols for which we have no self location but that may
   --      be needed to contact other partitions.

   if Options.Execution_Mode /= Replay_Mode then
      if Options.Self_Location /= null then
         for SL in Options.Self_Location'Range loop
            pragma Debug (D ("Should self protocol with " &
                             Options.Self_Location (SL).all &
                             " be activated"));

            Self_Location := To_Location (Options.Self_Location (SL).all);
            Self_Protocol := Get_Protocol (Self_Location);

            --  Is the protocol available?

            if Self_Protocol /= null then
               pragma Debug (D ("Activate self protocol with " &
                                Options.Self_Location (SL).all));

               Initialize
                 (Self_Protocol,
                  Get_Data (Self_Location),
                  True,
                  Performed,
                  Error);
               if Found (Error) then
                  Raise_Communication_Error (Error);
               end if;

               if Performed then
                  N_Self_Location := N_Self_Location + 1;
               end if;
            end if;
         end loop;
      end if;

      for P in First_Protocol .. Last_Protocol loop
         pragma Debug (D ("Activate loaded protocol " &
                          Get_Name (Protocol_Table (P))));

         Initialize (Protocol_Table (P), Null_String, False, Performed, Error);
         if Found (Error) then
            Raise_Communication_Error (Error);
         end if;

         --  Count the number of active protocols

         if Performed then
            N_Self_Location := N_Self_Location + 1;
         end if;
      end loop;

   --  For replay mode, ignore boot locations. Only replay protocol is
   --  now meaningful.

   else
      N_Boot_Location := 1;
      N_Self_Location := 1;
      Destroy (Options.Self_Location);
   end if;


   --  (8) The elaboration code of System.Garlic.Startup
   --      re-initializes Options.Boot_Locations. Size of
   --      New_Boot_Location may be to large because some
   --      locations may be duplicated. It is not a problem
   --      because null location will be ignored.

   New_Boot_Location := new String_Array (1 .. N_Boot_Location);
   N_Boot_Location := 0;

   for BL in Options.Boot_Location'Range loop
      Boot_Location := To_Location (Options.Boot_Location (BL).all);
      Boot_Protocol := Get_Protocol (Boot_Location);

      --  Is the protocol available?

      if Boot_Protocol /= null then
         if Options.Is_Boot_Server then
            Boot_Data := Get_Data (Boot_Protocol);
         else
            Boot_Data     := new String_Array (1 .. 1);
            Boot_Data (1) := new String'(Get_Data (Boot_Location));
         end if;

         --  Have to keep the boot locations in the order the user has
         --  specified them. We cannot append all the locations of
         --  this protocol. Just add the first missing location.

         Add_First_Missing_Location
           (New_Boot_Location, N_Boot_Location, Boot_Protocol, Boot_Data);

         Destroy (Boot_Data);
         exit when N_Boot_Location = New_Boot_Location'Last;
      end if;
   end loop;

   --  (9) The elaboration code of System.Garlic.Startup
   --      re-initializes Options.Self_Locations. Size of
   --      New_Self_Location may be to large because some locations may be
   --      duplicated. It is not a problem because null location will be
   --      ignored. We have to preserve the order specified by the user.
   --      Once this is done, add all the missing locations.

   New_Self_Location := new String_Array (1 .. N_Self_Location);
   N_Self_Location := 0;

   if Options.Self_Location /= null then
      for SL in Options.Self_Location'Range loop
         Self_Location := To_Location (Options.Self_Location (SL).all);
         Self_Protocol := Get_Protocol (Self_Location);

         --  Is this a potential location?

         if Self_Protocol /= null then
            Self_Data := Get_Data (Self_Protocol);

            --  Have to keep the self locations in the order the user
            --  has specified them. We cannot append all the locations
            --  of this protocol. Just add the first missing location.

            Add_First_Missing_Location
              (New_Self_Location, N_Self_Location, Self_Protocol, Self_Data);
         end if;

         Destroy (Self_Data);
         exit when N_Self_Location = New_Self_Location'Last;
      end loop;
   end if;

   --  (A) The elaboration code of System.Garlic.Startup
   --      re-initializes Options.Self_Locations. We add the remaining
   --      locations from all the protocols. This occurs when no
   --      self location was required. We may have also procotols
   --      that are always required and some that always add a self
   --      location even when none was required by the user.

   for P in First_Protocol .. Last_Protocol loop
      Self_Protocol := Protocols.Protocol_Table (P);
      Self_Data := Get_Data (Self_Protocol);
      if Self_Data /= null then
         Add_Missing_Locations
           (New_Self_Location, N_Self_Location, Self_Protocol);
      end if;
   end loop;

   --  (B) The elaboration code of System.Garlic.Startup replaces
   --      the old boot and self locations by the new ones.

   Destroy (Options.Self_Location);
   Options.Self_Location := New_Self_Location;

   pragma Debug (D ("Final self location is " &
                    Merge_String (Options.Self_Location)));

   Destroy (Options.Boot_Location);
   if Options.Is_Boot_Server then
      Destroy (New_Boot_Location);
      Options.Boot_Location := Options.Self_Location;
   else
      Options.Boot_Location := New_Boot_Location;
   end if;

   if Options.Boot_Location = null then
      Raise_Communication_Error ("cannot connect to boot partition");
   end if;

   Partitions.Set_Boot_Location (To_Location (Options.Boot_Location (1).all));

   pragma Debug (D ("Final boot location is " &
                    Merge_String (Options.Boot_Location)));

   --  (C) The elaboration code of System.Garlic.Startup initializes
   --      configurable units. They are not specifically needed by the
   --      previous code section. They do not belong to the core
   --      communication layer.

   Filters.Initialize;
   Units.Initialize;
   Trace.Initialize;

   pragma Debug (Partitions.Dump_Partition_Table (Private_Debug_Key));

   --  Let boot server know about this partition

   Heart.Get_My_Partition_ID (PID, Error);
   if PID = Null_PID then
      Raise_Communication_Error (Error);
   end if;

   pragma Debug (D ("Startup phase terminated"));

exception when others =>
   Heart.Activate_Shutdown;
   raise;
end System.Garlic.Startup;

