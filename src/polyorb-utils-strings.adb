------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S T R I N G S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2001 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  General-purpose string pointer.
--  Some useful functions for Polyorbloc.

--  $Id$

with Ada.Strings.Unbounded;

package body PolyORB.Utils.Strings is

   ---------
   -- "+" --
   ---------

   function "+"
     (S : Standard.String)
     return String_Ptr is
   begin
      return new Standard.String'(S);
   end "+";

   -------------------------------------
   -- Extract_String_Before_Delimiter --
   -------------------------------------

   procedure Extract_String_Before_Delimiter
     (Str   : in out Types.String;
      Delim : in     String;
      Value :    out Types.String)
   is
      use Ada.Strings.Unbounded;

      use PolyORB.Types;

      K : constant Natural := Index (Unbounded_String (Str), Delim);
   begin
      if K = 0 or Length (Str) < K + Delim'Last then
         Value := Types.String (Null_Unbounded_String);
         return;
      end if;
      Value := To_PolyORB_String (Slice (Unbounded_String (Str), 1, K - 1));
      Delete (Unbounded_String (Str), 1, K + Delim'Last - 1);
   end Extract_String_Before_Delimiter;

   ------------------------------
   -- Insert_Str_And_Delimiter --
   ------------------------------

   procedure Insert_Str_And_Delimiter
     (Str   : in out Types.String;
      Value : in     Types.String;
      Delim : in     String)
   is
      use Ada.Strings.Unbounded;

      use PolyORB.Types;

   begin
      if Index (Unbounded_String (Value), Delim) = 0 then
         Append (Str, Value & Delim);
      end if;
   end Insert_Str_And_Delimiter;

   procedure Insert_Str_And_Delimiter
     (Str   : in out Types.String;
      Value : in     String;
      Delim : in     String)
   is
      use PolyORB.Types;

   begin
      Insert_Str_And_Delimiter
        (Str,
         To_PolyORB_String (Value),
         Delim);
   end Insert_Str_And_Delimiter;

   -----------------------------
   -- Check_And_Remove_Header --
   -----------------------------

   procedure Check_And_Remove_Header
     (Str     : in out Types.String;
      Value   : in     Types.String;
      Success :    out Boolean)
   is
      use Ada.Strings.Unbounded;

      use PolyORB.Types;

   begin
      Success := False;
      if Index (Unbounded_String (Str), To_Standard_String (Value)) = 1 then
         Success := True;
         Str := To_PolyORB_String
           (To_String (Str)
            (Length (Value) + 1 .. Length (Str)));
      end if;
   end Check_And_Remove_Header;

end PolyORB.Utils.Strings;
