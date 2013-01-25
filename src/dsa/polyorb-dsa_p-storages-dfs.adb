------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . D S A _ P . S T O R A G E S . D F S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;

with System;

--  WAG:601
--  pragma Warnings (Off) with pattern not supported in that compiler version
--  so use plain pragma Warnings (Off/On) instead.
--  pragma Warnings (Off, "* is an internal GNAT unit");
--  pragma Warnings (Off, "use of this unit is non-portable*");

pragma Warnings (Off);
with System.File_IO;
with System.File_Control_Block;
pragma Warnings (On);

with PolyORB.Log;
with PolyORB.Buffers;
with PolyORB.Representations;
with PolyORB.DSA_P.Conversions;
with PolyORB.Errors;
with PolyORB.Setup;

package body PolyORB.DSA_P.Storages.DFS is

   package IOX renames Ada.IO_Exceptions;
   package FCB renames System.File_Control_Block;
   package SFI renames System.File_IO;

   use Ada.Streams;

   use PolyORB.Log;
   use PolyORB.Buffers;
   use PolyORB.Representations;
   use PolyORB.DSA_P.Conversions;
   use PolyORB.Errors;
   use PolyORB.Any;

   use type SIO.File_Mode;
   use type System.Global_Locks.Lock_Type;

   LRU_Head : DFS_Manager_Access;
   LRU_Tail : DFS_Manager_Access;

   Critical_Section : Mutex_Access;
   --  Global critical section

   type Request_Type is (Read, Write, Lock);

   ----------------------------------------------
   -- Variables for Shared Memory Access Files --
   ----------------------------------------------

   Max_Shared_Files_Open : constant := 20;
   --  Maximum number of lock files that can be open

   Shared_Files_Open : Natural := 0;
   --  Number of shared memory access files currently open

   Endianness : constant Endianness_Type := Big_Endian;
   --  Endianness for stream representation

   To_File_Mode : constant array (Read .. Write) of SIO.File_Mode
     := (Read => SIO.In_File, Write => SIO.Out_File);

   procedure Initiate_Request
     (Var_Data : access DFS_Manager_Type;
      Request  : Request_Type;
      Success  : out Boolean);
   --  Initiate an operation on a variable. This routine can be thread
   --  blocking in order to serialize several concurrent requests and
   --  should be protected against abortion. Success returns whether
   --  the request can be performed on the variable. Typically, if
   --  there is no storage available on a read operation, Success is
   --  set to False. Any exception must be caught inside the
   --  routine. Note also that primitives Read and Write must catch
   --  all the potential exceptions.

   procedure Complete_Request (Var_Data : access DFS_Manager_Type);
   --  Complete the request previously initiated by the routine above.

   function Lock_Name (Var_Data : DFS_Manager_Type) return String;

   -------------
   -- Logging --
   -------------

   package L is new Log.Facility_Log ("polyorb.dsa_p.storages.dfs");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
                renames L.Output;
   function C  (Level : Log_Level := Debug) return Boolean
                renames L.Enabled;

   --------------------------
   -- Unchecked_Conversion --
   --------------------------

   function To_AFCB_Ptr is
      new Ada.Unchecked_Conversion (SIO.File_Type, FCB.AFCB_Ptr);

   ------------
   -- Create --
   ------------

   overriding function Create
     (Manager_Factory : access DFS_Manager_Type;
      Full_Name       : String)
      return Shared_Data_Manager_RACW
   is
      Var : constant DFS_Manager_Access := new DFS_Manager_Type;

   begin
      pragma Debug (C, O ("create DFS manager for variable " & Full_Name));

      Var.Name  := new String'(Full_Name);
      Var.Self  := Var;
      Var.Count := 0;
      Var.Dir   := Manager_Factory.Dir;
      Var.Lock  := SGL.Null_Lock;
      Create (Var.Mutex);

      return Shared_Data_Manager_RACW (Var);
   end Create;

   ----------------------
   -- Complete_Request --
   ----------------------

   procedure Complete_Request (Var_Data : access DFS_Manager_Type) is
   begin
      if Var_Data.Count > 0 then
         Var_Data.Count := Var_Data.Count - 1;

         if Var_Data.Count = 0 then
            SGL.Release_Lock (Var_Data.Lock);
         end if;

         pragma Debug (C, O ("Lock count =" & Var_Data.Count'Img));
      end if;
   end Complete_Request;

   ----------------------
   -- Initiate_Request --
   ----------------------

   procedure Initiate_Request
     (Var_Data : access DFS_Manager_Type;
      Request  : Request_Type;
      Success  : out Boolean)
   is
      Done : Boolean := True;
      Free : DFS_Manager_Access;

   begin
      case Request is
         when Read | Write =>
            declare
               Fname : constant String := Var_Data.Dir.all & Var_Data.Name.all;
               Fmode : constant SIO.File_Mode   := To_File_Mode (Request);

            begin
               if not SIO.Is_Open (Var_Data.File) then
                  begin
                     SIO.Open (Var_Data.File, Fmode, Name => Fname);
                     SFI.Make_Unbuffered (To_AFCB_Ptr (Var_Data.File));

                     pragma Debug (C, O ("Open variable file " & Fname));

                  exception
                     when IOX.Name_Error =>

                        if Request = Read then
                           Done := False;

                        else
                           SIO.Create (Var_Data.File, Fmode, Name => Fname);
                           pragma Debug
                             (C, O ("Create variable file " & Fname));
                        end if;
                  end;

                  if Done then
                     Enter (Critical_Section);
                     Shared_Files_Open := Shared_Files_Open + 1;

                     if Shared_Files_Open = Max_Shared_Files_Open then
                        Free := LRU_Head;

                        if Free.Next /= null then
                           Free.Next.Prev := null;
                        end if;

                        LRU_Head  := Free.Next;
                        Free.Next := null;
                        Free.Prev := null;
                        SIO.Close (Free.File);

                        pragma Debug
                             (C, O ("Close variable file " & Free.Name.all));
                     end if;

                     --  Add new entry at end of LRU chain

                     if LRU_Head = null then
                        LRU_Head := Var_Data.Self;
                        LRU_Tail := Var_Data.Self;

                     else
                        Var_Data.Prev := LRU_Tail;
                        LRU_Tail.Next := Var_Data.Self;
                        LRU_Tail      := Var_Data.Self;
                     end if;
                     Leave (Critical_Section);
                  end if;

                  --  Here if file is already open, set file for reading

               else
                  if SIO.Mode (Var_Data.File) /= Fmode then
                     pragma Debug
                       (C, O ("Reset variable file " & Var_Data.Name.all &
                        " mode to " & Request'Img));

                     SIO.Set_Mode (Var_Data.File, Fmode);
                     SFI.Make_Unbuffered (To_AFCB_Ptr (Var_Data.File));
                  end if;

                  SIO.Set_Index (Var_Data.File, 1);
               end if;
            end;

         when Lock =>
            Var_Data.Count := Var_Data.Count + 1;
            if Var_Data.Lock = SGL.Null_Lock then
               SGL.Create_Lock (Var_Data.Lock, Lock_Name (Var_Data.all));
            end if;
            if Var_Data.Count = 1 then
               SGL.Acquire_Lock (Var_Data.Lock);
            end if;

            pragma Debug (C, O ("Lock count =" & Var_Data.Count'Img));

      end case;
      Success := Done;
   end Initiate_Request;

   ---------------
   -- Lock_Name --
   ---------------

   function Lock_Name (Var_Data : DFS_Manager_Type) return String is
   begin
      return Var_Data.Dir.all & ".entry";
   end Lock_Name;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : access DFS_Manager_Type;
      Var  : SDT.Any_Container_Ptr)
   is
      Rep : constant Representation_Access :=
        PolyORB.Setup.Default_Representation;

      Buffer  : Buffer_Access := new Buffer_Type;
      Success : Boolean;
      Error   : Error_Container;

   begin
      Enter (Self.Mutex);
      Initiate_Request (Self, Read, Success);

      if not Success then
         Release (Buffer);
         Leave (Self.Mutex);
         return;
      end if;

      declare
         Last   : Stream_Element_Offset;
         Stream : Stream_Element_Array
           (1 .. Stream_Element_Offset (SIO.Size (Self.File)));
      begin
         --  Fill buffer with file data

         SIO.Read (Self.File, Stream, Last);
         Initialize_Buffer
           (Buffer               => Buffer,
            Size                 => Stream'Last,
            Data                 => Stream'Address,
            Endianness           => Endianness,
            Initial_CDR_Position => 0);

         --  Unmarshall buffer into Any

         Unmarshall_To_Any (Rep, Buffer, DAC_To_AC (Var).all, Error);
      end;

      Complete_Request (Self);

      Release (Buffer);
      Leave (Self.Mutex);
   end Read;

   ------------------------------
   -- Register_Passive_Package --
   ------------------------------

   procedure Register_Passive_Package
     (Pkg_Name : String;
      Is_Owner : Boolean;
      Location : String)
   is
      pragma Unreferenced (Is_Owner);
      Factory : constant DFS_Manager_Access := new DFS_Manager_Type;

   begin
      pragma Debug (C, O ("Register DFS factory for package "
        & Pkg_Name));

      --  Location is a directory. Add a separator if the location is
      --  not empty.

      if Location'Length /= 0 then
         Factory.Dir := new String'(Location & OS.Directory_Separator);
      else
         Factory.Dir := new String'(Location);
      end if;

      Register_Factory (Pkg_Name, Shared_Data_Manager_RACW (Factory));
   end Register_Passive_Package;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : access DFS_Manager_Type;
      Var  : SDT.Any_Container_Ptr)
   is
      Rep : constant Representation_Access :=
        PolyORB.Setup.Default_Representation;

      Buffer  : Buffer_Access := new Buffer_Type;
      Success : Boolean;
      Error   : Error_Container;

   begin
      Enter (Self.Mutex);
      Initiate_Request (Self, Write, Success);

      if not Success then
         Release (Buffer);
         Leave (Self.Mutex);
         return;
      end if;

      --  Marshall Any into buffer

      Set_Endianness (Buffer, Endianness);
      Marshall_From_Any (Rep, Buffer, DAC_To_AC (Var).all, Error);

      --  Fill file with buffer data

      declare
         Stream : constant Stream_Element_Array :=
           To_Stream_Element_Array (Buffer.all);
      begin
         SIO.Write (Self.File, Stream);
      end;

      Complete_Request (Self);

      Release (Buffer);
      Leave (Self.Mutex);
   end Write;

   ----------
   -- Lock --
   ----------

   overriding procedure Lock   (Self : access DFS_Manager_Type)
   is
      Success : Boolean;
      pragma Unreferenced (Success);

   begin
      Enter (Self.Mutex);
      Initiate_Request (Self, Lock, Success);
   end Lock;

   ------------
   -- Unlock --
   ------------

   overriding procedure Unlock (Self : access DFS_Manager_Type) is
   begin
      Complete_Request (Self);
      Leave (Self.Mutex);
   end Unlock;

begin
   Create (Critical_Section);
end PolyORB.DSA_P.Storages.DFS;
