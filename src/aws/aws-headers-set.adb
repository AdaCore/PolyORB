------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      A W S . H E A D E R S . S E T                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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

--  with Ada.Strings.Fixed;
--  with Ada.Text_IO;

with AWS.Containers.Tables.Set;
--  with AWS.Net.Buffered;

package body AWS.Headers.Set is

   use AWS.Containers;

   subtype P_List is Tables.Table_Type;

   Debug_Flag : Boolean := False;
   --  Set to True to output debug information to the standard output.

   ---------
   -- Add --
   ---------

   procedure Add (Headers : in out List; Name, Value : String) is
   begin
      Tables.Set.Add (P_List (Headers), Name, Value);
   end Add;

   -----------
   -- Debug --
   -----------

   procedure Debug (Activate : Boolean) is
   begin
      Debug_Flag := not Debug_Flag;  --  just to lure the compiler
      Debug_Flag := Activate;
   end Debug;

   ----------
   -- Free --
   ----------

   procedure Free (Headers : in out List) is
   begin
      Tables.Set.Free (P_List (Headers));
   end Free;

   ----------
   -- Read --
   ----------

--  procedure Read (Socket : Net.Socket_Type'Class; Headers : in out List)
--  is

--        procedure Parse_Header_Lines (Line : String);
--        --  Parse the Line eventually catenated with the next line if it is a
--        --  continuation line see [RFC 2616 - 4.2].

--        ------------------------
--        -- Parse_Header_Lines --
--        ------------------------

--        procedure Parse_Header_Lines (Line : String) is
--           End_Of_Message : constant String := "";
--        begin
--           if Line = End_Of_Message then
--              return;

--           else
--              declare
--                 use Ada.Strings;

--                 Next_Line       : constant String
--                   := Net.Buffered.Get_Line (Socket);
--                 Delimiter_Index : Natural;

--              begin
--                 if Next_Line /= End_Of_Message
--                      and then
--                   (Next_Line (1) = ' ' or else Next_Line (1) = ASCII.HT)
--                 then
--                --  Continuing value on the next line. Header fields can be
--                    --  extended over multiple lines by preceding each extra
--                    --  line with at least one SP or HT.
--                    Parse_Header_Lines (Line & Next_Line);

--                 else
--                    if Debug_Flag then
--                       Ada.Text_IO.Put_Line ('>' & Line);
--                    end if;

--                    --  Put name and value to the container separately.

--                    Delimiter_Index := Fixed.Index (Line, ":");

--                    if Delimiter_Index = 0 then
--                       --  No delimiter, this is not a valid Header Line
--                       raise Format_Error;
--                    end if;

--                    Add (Headers,
--                         Name  => Line (Line'First .. Delimiter_Index - 1),
--                         Value => Fixed.Trim
--                                    (Line (Delimiter_Index + 1 .. Line'Last),
--                                     Side => Both));

--                    --  Parse next header line.

--                    Parse_Header_Lines (Next_Line);
--                 end if;
--              end;
--           end if;
--        end Parse_Header_Lines;

--     begin
--        Reset (Headers);
--        Parse_Header_Lines (Net.Buffered.Get_Line (Socket));
--     end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (Headers : in out List) is
   begin
      Tables.Set.Reset (P_List (Headers));
      Tables.Set.Case_Sensitive (P_List (Headers), False);
   end Reset;

   ------------
   -- Update --
   ------------

   procedure Update
     (Headers : in out List;
      Name    : String;
      Value   : String;
      N       : Positive := 1) is
   begin
      Tables.Set.Update (P_List (Headers), Name, Value, N);
   end Update;

end AWS.Headers.Set;
