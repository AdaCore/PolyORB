------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . M O M A _ P . P R O V I D E R . W A R E H O U S E     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  A dynamic, protected dictionary of Any, indexed by Strings.

with Ada.Streams.Stream_IO;

package body PolyORB.MOMA_P.Provider.Warehouse is

   use Ada.Streams;
   use Ada.Streams.Stream_IO;

   use PolyORB.Any;
   use PolyORB.Tasking.Rw_Locks;

   use MOMA.Types;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization (W : in out Warehouse);
   pragma Inline (Ensure_Initialization);
   --  Ensure that T was initialized

   procedure Ensure_Initialization (W : in out Warehouse) is
   begin
      if W.T_Initialized then
         return;
      end if;

      Initialize (W.T);
      Create (W.T_Lock);
      W.T_Initialized := True;
   end Ensure_Initialization;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (W : Warehouse;
      K : String)
      return PolyORB.Any.Any
   is
      Result      : PolyORB.Any.Any;
--        Stream_File : Ada.Streams.Stream_IO.File_Type;
--        Buffer      : constant Buffer_Access := new Buffer_Type;
--        Data        : Opaque_Pointer;
--        Last        : Ada.Streams.Stream_Element_Offset;
--        Received    : Ada.Streams.Stream_Element_Count;
      Temp        : Warehouse := W;

   begin
      Ensure_Initialization (Temp);

      if W.T_Persistence = None then
         Lock_R (W.T_Lock);
         Result := Lookup (W.T, K, Result);
         Unlock_R (W.T_Lock);

         if Is_Empty (Result) then
            raise Key_Not_Found;
         end if;
      else
--           Ada.Streams.Stream_IO.Open (Stream_File, In_File, "message_" & K);
--           Allocate_And_Insert_Cooked_Data (Buffer, 1024, Data);
--           declare
--              Z_Addr : constant System.Address := Data;
--              Z : Stream_Element_Array (0 .. 1023);
--              for Z'Address use Z_Addr;
--              pragma Import (Ada, Z);
--           begin
--              Ada.Streams.Stream_IO.Read (Stream_File, Z, Last);
--           end;

--           Received := Last + 1;
--           Unuse_Allocation (Buffer, 1024 - Received);
--           Ada.Streams.Stream_IO.Close (Stream_File);

--           Set_Type (Result, TC_MOMA_Message);
--           Rewind (Buffer);
--           Unmarshall_To_Any (Buffer, Result);

         raise Program_Error;
         --  XXX This code is now deactivated (as of 9/17/04): We
         --  cannot rely on the Rewind primitive which is
         --  deprecated. Besides, we need to find an efficient way to
         --  store a buffer into a file.
      end if;

      return Result;
   end Lookup;

   function Lookup
     (W       : Warehouse;
      K       : String;
      Default : PolyORB.Any.Any)
     return PolyORB.Any.Any
   is
      V : PolyORB.Any.Any;
      Temp : Warehouse := W;
   begin
      Ensure_Initialization (Temp);

      Lock_R (W.T_Lock);
      V := Lookup (W.T, K, Default);
      Unlock_R (W.T_Lock);

      return V;
   end Lookup;

   --------------
   -- Register --
   --------------

   procedure Register
     (W : in out Warehouse;
      K :        String;
      V :        PolyORB.Any.Any)
   is
--        Stream_File : Ada.Streams.Stream_IO.File_Type;
--        Buffer      : constant Buffer_Access := new Buffer_Type;
   begin
      Ensure_Initialization (W);

      if W.T_Persistence = None then
         Lock_W (W.T_Lock);
         Insert (W.T, K, V);
         Unlock_W (W.T_Lock);

      else
--       Marshall_From_Any (Buffer, V);
--       Ada.Streams.Stream_IO.Create (Stream_File, Out_File, "message_" & K);

--       Ada.Streams.Stream_IO.Write (Stream_File,
--                                    To_Stream_Element_Array (Buffer));
--       Ada.Streams.Stream_IO.Close (Stream_File);

         --  XXX This code is now deactivated (as of 9/17/04): We
         --  cannot rely on the Rewind primitive which is
         --  deprecated. Besides, we need to find an efficient way to
         --  store a buffer into a file.

         raise Program_Error;
      end if;

   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (W : in out Warehouse;
      K :        String)
   is
      Stream_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ensure_Initialization (W);

      if W.T_Persistence = None then
         Lock_W (W.T_Lock);
         Delete (W.T, K);
         Unlock_W (W.T_Lock);

      else
         Ada.Streams.Stream_IO.Open (Stream_File, Out_File, "message_" & K);
         Ada.Streams.Stream_IO.Delete (Stream_File);
      end if;

   end Unregister;

   ---------------------
   -- Set_Persistence --
   ---------------------

   procedure Set_Persistence
     (W           : in out Warehouse;
      Persistence :        MOMA.Types.Persistence_Mode) is
   begin
      W.T_Persistence := Persistence;
   end Set_Persistence;

end PolyORB.MOMA_P.Provider.Warehouse;
