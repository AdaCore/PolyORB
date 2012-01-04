------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . U T I L S . S R P                     --
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

--  Utilities for the Simple Request Protocol.

with GNAT.Regpat;

package body PolyORB.Utils.SRP is

   use PolyORB.Any;

   --------------------
   -- Set_SRP_Method --
   --------------------

   procedure Set_SRP_Method (Method : Types.String;
                             SRP_Info : in out Split_SRP)
   is
   begin
      SRP_Info.Method := new Types.String'(Method);
   end Set_SRP_Method;

   -----------------
   -- Set_SRP_Oid --
   -----------------

   procedure Set_SRP_Oid (Oid : Object_Id; SRP_Info : in out Split_SRP)
   is
   begin
      SRP_Info.Oid := new Object_Id'(Oid);
   end Set_SRP_Oid;

   -----------------
   -- Set_SRP_Arg --
   -----------------

   procedure Set_SRP_Arg (Name : Types.String;
                          Value : Any.Any;
                          SRP_Info : in out Split_SRP)
   is
      Current_Arg : Arg_Info_Ptr := SRP_Info.Args;
   begin
      if Current_Arg /= null then
         while Current_Arg.all.Next /= null loop
            Current_Arg := Current_Arg.all.Next;
         end loop;
         --  ??? revoir la maniere d'obtenir Value

         Current_Arg.all.Next :=
           new Arg_Info'(new Types.String'(Name),
                         new Types.String'(From_Any (Value)),
                         null);
      else
         SRP_Info.Args := new Arg_Info'(new Types.String'(Name),
                                        new Types.String'(From_Any (Value)),
                                        null);
      end if;
   end Set_SRP_Arg;

   -----------
   -- Split --
   -----------

   function Split (S : Types.String) return Split_SRP
   is
      use GNAT.Regpat;

      Result  : Split_SRP;
      Args    : constant Arg_Info_Ptr := new Arg_Info;
      Current : Arg_Info_Ptr := Args;
      Last    : Arg_Info_Ptr;

      Matches         : Match_Array (1 .. 255);
      Regexp_Req_OID  : constant Standard.String := "(\w+) (\w+)\?(.*)";
      Regexp_Args     : constant Standard.String := "(\w+)=(\w+)&?(.*)";

      Args_Ptr : Types.String_Ptr;
   begin
      Match (Compile (Regexp_Req_OID), To_Standard_String (S), Matches);
      --  Stores the name of the function/procedure called
      Result.Method :=
        new Types.String'(To_PolyORB_String
                          (Slice (S, Matches (1).First, Matches (1).Last)));

      --  Stores the Object Id
      Result.Oid :=
        new Object_Id'(Hex_String_To_Oid (String'(To_Standard_String (S)
                                                   (Matches (2).First ..
                                                    Matches (2).Last))));

      --  Stores the last string containing the arguments
      Args_Ptr := new Types.String'(To_PolyORB_String
                                    (Slice (S,
                                            Matches (3).First,
                                            Matches (3).Last)));
      pragma Warnings (Off, Args_Ptr);
      --  We want Args_Ptr to be able to be null

      --  ??? Could be optimized
      while Args_Ptr.all /= "" loop
         Match (Compile (Regexp_Args),
                To_Standard_String (Args_Ptr.all),
                Matches);
         Current.Name := new Types.String'(To_PolyORB_String
                                           (Slice (Args_Ptr.all,
                                                   Matches (1).First,
                                                   Matches (1).Last)));
         Current.Value := new Types.String'(To_PolyORB_String
                                            (Slice (Args_Ptr.all,
                                                    Matches (2).First,
                                                    Matches (2).Last)));

         --  Create a new String with the remaining arguments
         Args_Ptr := new Types.String'(To_PolyORB_String
                                       (Slice (Args_Ptr.all,
                                               Matches (3).First,
                                               Matches (3).Last)));
         Current.Next := new Arg_Info;
         Last := Current;
         Current := Current.Next;
      end loop;

      Last.Next := null;

      --  Destroy the last record (is empty)
      Free_Arg_Info (Current);

      Result.Args := Args;
      return Result;
   end Split;

   ----------
   -- Join --
   ----------

   function Join (Data : Split_SRP) return Any.Any
   is
      URL : Types.String;
      --  Current_Arg : Arg_Info_Ptr := Data.Args;
   begin
      Append (URL, Data.Method.all & " "
              & Oid_To_Hex_String (Data.Oid.all) & "?");
      raise Program_Error;
--      while Current_Arg /= null loop
--          Append (URL, Current_Arg.all.Name.all & "=" &
--                  Current_Arg.all.Value.all);
--          if Current_Arg.all.Next /= null then
--             Append (URL, "&");
--          end if;
--         Current_Arg := Current_Arg.Next;
--      end loop;
      return Any.To_Any (Types.To_PolyORB_String (To_String (URL)));
   end Join;

end PolyORB.Utils.SRP;
