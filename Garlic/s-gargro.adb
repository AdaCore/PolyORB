------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . G R O U P                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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

with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.Heart;      use System.Garlic.Heart;
with System.Garlic.Options;
pragma Warnings (Off, System.Garlic.Options);
with System.Garlic.Partitions; use System.Garlic.Partitions;
with System.Garlic.Streams;    use System.Garlic.Streams;
with System.Garlic.Types;      use System.Garlic.Types;
with System.Garlic.Utils;      use System.Garlic.Utils;

package body System.Garlic.Group is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARGRO", "(s-gargro): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Group_Mutex : Mutex_Type;

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);

   procedure Send_Next_Mirror
     (Opcode : in Any_Opcode;
      Params : access Streams.Params_Stream_Type);

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast
     (Opcode : in Any_Opcode;
      Params : access Streams.Params_Stream_Type)
   is
   begin
      Enter (Group_Mutex);
      pragma Debug (D ("Broadcast facility is locked"));
      Insert (Params.all);
      Partition_ID'Write (Params, Self_PID);
      Any_Opcode'Write (Params, Opcode);
      Send_Next_Mirror (Group_Service, Params);
   end Broadcast;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type;
      Error     : in out Error_Type)
   is
      Inner_PID   : Partition_ID;
      Inner_Code  : Any_Opcode;
      Inner_Query : aliased Params_Stream_Type (Query.Count);
      Inner_Reply : aliased Params_Stream_Type (Query.Count);
   begin
      pragma Debug (D ("Handle broadcast request"));
      Copy (Query.all, Inner_Query);

      Partition_ID'Read (Inner_Query'Access, Inner_PID);
      Any_Opcode'Read   (Inner_Query'Access, Inner_Code);
      Handle_Any_Request
        (Inner_PID,
         Inner_Code,
         Inner_Query'Access,
         Inner_Reply'Access,
         Error);

      --  In the general case, a query receives a reply. But in the
      --  case of a group query, the reply is used on the querying
      --  partition only.  If the partition is not the querying
      --  partition, the query can be modified and the reply is
      --  not used.

      Deallocate (Inner_Reply);

      if Found (Error) then
         Deallocate (Inner_Query);
         return;
      end if;

      if Inner_PID = Self_PID then
         if Empty (Reply) then
            pragma Debug (D ("Broadcast facility is unlocked"));
            Leave (Group_Mutex);

         else
            pragma Debug (D ("Continue broacast for a second time"));

            Send_Next_Mirror (Group_Service, Reply);
         end if;
         Deallocate (Inner_Query);

      else
         if Empty (Inner_Query'Access) then
            pragma Debug (D ("Forward same query"));
            Copy (Query.all, Inner_Query);

         else
            pragma Debug (D ("Forward new query"));
            Insert (Inner_Query);
            Partition_ID'Write (Inner_Query'Access, Inner_PID);
            Any_Opcode'Write   (Inner_Query'Access, Inner_Code);
         end if;

         Send_Next_Mirror (Group_Service, Inner_Query'Access);
      end if;
   end Handle_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Group_Mutex);
      Register_Handler (Group_Service, Handle_Request'Access);
   end Initialize;

   ----------------------
   -- Send_Next_Mirror --
   ----------------------

   procedure Send_Next_Mirror
     (Opcode : in Any_Opcode;
      Params : access Streams.Params_Stream_Type)
   is
      Error  : Error_Type;
      PID    : Partition_ID;
   begin
      pragma Assert (Options.Is_Boot_Mirror);
      loop
         PID := Next_Boot_Mirror;
         Send (PID, Opcode, Params, Error);
         exit when not Found (Error);

         --  On error, retry from Self_PID. We know that this
         --  partition is already a boot mirror. If we can not find a
         --  candidate, at least, send the message to itself.

         Catch (Error);
      end loop;
   end Send_Next_Mirror;

end System.Garlic.Group;
