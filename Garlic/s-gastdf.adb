------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--          S Y S T E M . G A R L I C . S T O R A G E S . D F S             --
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

with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with System;
with System.File_Control_Block;
with System.File_IO;

with System.Garlic.Debug; use System.Garlic.Debug;
pragma Elaborate (System.Garlic.Debug);

with System.Garlic.Physical_Location;
use  System.Garlic.Physical_Location;

with System.Garlic.Options;    use System.Garlic.Options;
with System.Garlic.Soft_Links; use System.Garlic.Soft_Links;
with System.Garlic.Utils;      use System.Garlic.Utils;

package body System.Garlic.Storages.Dfs is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GASTDF", "(s-gastdf): ");

   Dfs_Storage_Name : constant String := "dfs";

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use type SIO.File_Mode;
   use type System.Global_Locks.Lock_Type;

   package IOX renames Ada.IO_Exceptions;

   package FCB renames System.File_Control_Block;

   package SFI renames System.File_IO;

   function To_AFCB_Ptr is
      new Ada.Unchecked_Conversion (SIO.File_Type, FCB.AFCB_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (DFS_Data_Type, DFS_Data_Access);

   Sep : Character renames OS.Directory_Separator;

   Root : DFS_Data_Access;

   LRU_Head : DFS_Data_Access;
   LRU_Tail : DFS_Data_Access;

   ----------------------------------------------
   -- Variables for Shared Memory Access Files --
   ----------------------------------------------

   Max_Shared_Files_Open : constant := 20;
   --  Maximum number of lock files that can be open

   Shared_Files_Open : Natural := 0;
   --  Number of shared memory access files currently open

   To_File_Mode : constant array (Read .. Write) of SIO.File_Mode
     := (Read => SIO.In_File, Write => SIO.Out_File);

   function  Lock_Name (Var_Data : DFS_Data_Type) return String;

   --------------------
   -- Create_Storage --
   --------------------

   procedure Create_Storage
     (Master   : in out DFS_Data_Type;
      Location : in     String;
      Storage  : out    Shared_Data_Access)
   is
      Result   : DFS_Data_Access;

   begin
      Result := new DFS_Data_Type;

      --  Location is a directory. Add a separator if the location is
      --  not empty.

      if Location'Length /= 0 then
         Result.Name := new String'(Location & Sep);
      else
         Result.Name := new String'(Location);
      end if;

      pragma Debug
        (D ("create storage dfs with data """ & Result.Name.all & """"));

      Storage := Shared_Data_Access (Result);
   end Create_Storage;

   --------------------
   -- Create_Package --
   --------------------

   procedure Create_Package
     (Storage  : in out DFS_Data_Type;
      Pkg_Name : in     String;
      Pkg_Data : out    Shared_Data_Access)
   is
      Result : DFS_Data_Access;

   begin
      pragma Debug (D ("create package file " & Pkg_Name &
                       " on support " & Storage.Name.all));

      Result      := new DFS_Data_Type;
      Result.Name := Storage.Name;
      Pkg_Data    := Shared_Data_Access (Result);
   end Create_Package;

   ---------------------
   -- Create_Variable --
   ---------------------

   procedure Create_Variable
     (Pkg_Data : in out DFS_Data_Type;
      Var_Name : in     String;
      Var_Data : out    Shared_Data_Access)
   is
      Var : DFS_Data_Access := new DFS_Data_Type;

   begin
      Var.Name  := new String'(Pkg_Data.Name.all & Var_Name);
      Var.Self  := Var;
      Var.Count := 0;
      Create (Var.Mutex);
      Var.Lock  := SGL.Null_Lock;
      Var_Data  := Shared_Data_Access (Var);
   end Create_Variable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Data_Dir : OS.String_Access;

   begin
      pragma Debug (D ("initialize DFS"));

      if Root = null then
         Root := new DFS_Data_Type;
         if Data_Location /= null
           and then Get_Support_Name (Data_Location (1).all) = Dfs_Storage_Name
         then
            Data_Dir := new String'(Get_Support_Data (Data_Location (1).all));
         end if;
         if Data_Dir = null then
            Data_Dir := OS.Getenv ("DFS_DATA_DIR");
         end if;
         if Data_Dir'Length = 0 then
            Root.Name := new String'(Data_Dir.all);
         else
            Root.Name := new String'(Data_Dir.all & Sep);
         end if;
         Free (Data_Dir);
         pragma Debug (D ("root data name is """ & Root.Name.all & """"));
         Register_Storage (Dfs_Storage_Name, Shared_Data_Access (Root));
      end if;
   end Initialize;

   ----------------------
   -- Initiate_Request --
   ----------------------

   procedure Initiate_Request
     (Var_Data : in out DFS_Data_Type;
      Request  : in Request_Type;
      Success  : out Boolean)
   is
      Done : Boolean := True;
      Free : DFS_Data_Access;

   begin
      Enter (Var_Data.Mutex);
      case Request is
         when Read | Write =>
            declare
               Fname : constant String := Var_Data.Name.all;
               Fmode : SIO.File_Mode   := To_File_Mode (Request);

            begin
               if not SIO.Is_Open (Var_Data.File) then
                  begin
                     SIO.Open (Var_Data.File, Fmode, Name => Fname);
                     SFI.Make_Unbuffered (To_AFCB_Ptr (Var_Data.File));

                     pragma Debug (D ("open variable file " & Fname));

                  exception
                     when IOX.Name_Error =>

                        if Request = Read then
                           Leave (Var_Data.Mutex);
                           Done := False;

                        else
                           SIO.Create (Var_Data.File, Fmode, Name => Fname);
                           pragma Debug (D ("create variable file " & Fname));
                        end if;
                  end;

                  if Done then
                     Enter_Critical_Section;
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
                          (D ("close variable file " & Free.Name.all));
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
                     Leave_Critical_Section;
                  end if;

                  --  Here if file is already open, set file for reading

               else
                  if SIO.Mode (Var_Data.File) /= Fmode then
                     pragma Debug
                       (D ("reset variable file " &
                           Var_Data.Name.all &
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
               SGL.Create_Lock (Var_Data.Lock, Lock_Name (Var_Data));
            end if;
            if Var_Data.Count = 1 then
               SGL.Acquire_Lock (Var_Data.Lock);
            end if;

            pragma Debug (D ("lock count =" & Var_Data.Count'Img));

      end case;
      Success := Done;
   end Initiate_Request;

   ----------------------
   -- Complete_Request --
   ----------------------

   procedure Complete_Request
     (Var_Data : in out DFS_Data_Type) is
   begin
      if Var_Data.Count > 0 then
         Var_Data.Count := Var_Data.Count - 1;

         if Var_Data.Count = 0 then
            SGL.Release_Lock (Var_Data.Lock);
         end if;

         pragma Debug (D ("lock count =" & Var_Data.Count'Img));
      end if;

      Leave (Var_Data.Mutex);
   end Complete_Request;

   ---------------
   -- Lock_Name --
   ---------------

   function Lock_Name (Var_Data : DFS_Data_Type) return String is
   begin
      return ".entry";
   end Lock_Name;

   ----------
   -- Read --
   ----------

   procedure Read
     (Data : in out DFS_Data_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      pragma Debug (D ("read variable file " & Data.Name.all));

      SIO.Read (Data.File, Item, Last);
   exception when others =>
      Last := Item'Last;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Data : in out DFS_Data_Type;
      Item : in Ada.Streams.Stream_Element_Array) is
   begin
      pragma Debug (D ("write variable " & Data.Name.all));

      SIO.Write (Data.File, Item);
   exception when others =>
      null;
   end Write;

end System.Garlic.Storages.Dfs;
