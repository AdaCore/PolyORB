------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              M O M A . P R O V I D E R . W A R E H O U S E               --
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

--  A dynamic, protected dictionnary of Any, indexed by Strings.

--  $Id$

with Ada.Streams;
with Ada.Streams.Stream_IO;

with PolyORB.Any;
with PolyORB.Buffers;
with PolyORB.Opaque;
with PolyORB.Representations.CDR;
with PolyORB.Tasking.Rw_Locks;
with PolyORB.Utils.HTables.Perfect;

with MOMA.Messages;

package body MOMA.Provider.Warehouse is

   use Ada.Streams;
   use Ada.Streams.Stream_IO;

   use PolyORB.Any;
   use PolyORB.Buffers;
   use PolyORB.Opaque;
   use PolyORB.Representations.CDR;
   use PolyORB.Tasking.Rw_Locks;

   use MOMA.Messages;
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
      Stream_File : Ada.Streams.Stream_IO.File_Type;
      Buffer      : constant Buffer_Access := new Buffer_Type;
      Data        : Opaque_Pointer;
      Last        : Ada.Streams.Stream_Element_Offset;
      Received    : Ada.Streams.Stream_Element_Count;
      Temp        : Warehouse := W;
   begin
      Ensure_Initialization (Temp);

      if W.T_Persistence = None then
         Lock_R (W.T_Lock);
         Result := Lookup (W.T, K);
         Unlock_R (W.T_Lock);

      else
         Ada.Streams.Stream_IO.Open (Stream_File, In_File, "message_" & K);
         Allocate_And_Insert_Cooked_Data (Buffer, 1024, Data);
         declare
            subtype Z_Type is Stream_Element_Array (0 .. 1023);
            Z : Z_Type;
            for Z'Address use Data;
         begin
            Ada.Streams.Stream_IO.Read (Stream_File, Z, Last);
         end;

         Received := Last + 1;
         Unuse_Allocation (Buffer, 1024 - Received);
         Ada.Streams.Stream_IO.Close (Stream_File);

         Set_Type (Result, TC_MOMA_Message);
         Rewind (Buffer);
         Unmarshall_To_Any (Buffer, Result);
      end if;

      return Result;
   exception
         when No_Key => raise Key_Not_Found;
   end Lookup;

   function Lookup
     (W : Warehouse;
      K : String;
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
      K : String;
      V : PolyORB.Any.Any)
   is
      Stream_File : Ada.Streams.Stream_IO.File_Type;
      Buffer      : constant Buffer_Access := new Buffer_Type;
   begin
      Ensure_Initialization (W);

      if W.T_Persistence = None then
         Lock_W (W.T_Lock);
         Insert (W.T, K, V);
         Unlock_W (W.T_Lock);

      else
         Marshall_From_Any (Buffer, V);

         Ada.Streams.Stream_IO.Create (Stream_File, Out_File, "message_" & K);
         Ada.Streams.Stream_IO.Write (Stream_File,
                                      To_Stream_Element_Array (Buffer));
         Ada.Streams.Stream_IO.Close (Stream_File);
      end if;

   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (W : in out Warehouse;
      K : String)
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

   exception
      when No_Key => raise Key_Not_Found;
   end Unregister;

   ---------------------
   -- Set_Persistence --
   ---------------------

   procedure Set_Persistence (W : in out Warehouse;
                             Persistence : MOMA.Types.Persistence_Mode) is
   begin
      W.T_Persistence := Persistence;
   end Set_Persistence;

end MOMA.Provider.Warehouse;
