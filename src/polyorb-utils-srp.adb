------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . U T I L S . S R P                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If  --
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

--  Utilities for the Simple Request Protocol.

--  $Id$

with GNAT.Regpat;

with PolyORB.Any;
with PolyORB.Types;

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
      use Objects;

      Result  : Split_SRP;
      Args    : Arg_Info_Ptr := new Arg_Info;
      Current : Arg_Info_Ptr := Args;
      Last    : Arg_Info_Ptr;

      --  ???
      --  WARNING : we consider a restrictive form of the Object_Id
      --  Should be changed
      Matches         : Match_Array (1 .. 255);
      Regexp_Req_OID  : Standard.String := "(\w+) (\d+)\?(.*)";
      Regexp_Args     : Standard.String := "(\w+)=(\w+)&?(.*)";

      Args_Ptr : Types.String_Ptr;
   begin
      Match (Compile (Regexp_Req_OID), To_Standard_String (S), Matches);
      --  Stores the name of the function/procedure called
      Result.Method :=
        new Types.String'(To_PolyORB_String
                          (Slice (S, Matches (1).First, Matches (1).Last)));
      --  ???
      --  Put_Line (Result.Method.all);

      --  Stores the Object Id
      Result.Oid :=
        new Object_Id'(To_Oid (String'(To_Standard_String (S)
                                       (Matches (2).First ..
                                        Matches (2).Last))));
      --  ???
      --  Put_Line (To_String (Result.Oid.all));

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

   -----------
   -- Split --
   -----------

--    function Split (Data : Any.Any) return Split_SRP
--    is
--       Bidon : Split_SRP := (new Types.String'(To_PolyORB_String ("Bidon")),
--                             new Object_Id'(To_Oid ("01000000")),
--                             null);
--    begin
--       raise Not_Implemented;
--       return Bidon;
--  --      return Split (To_PolyORB_String (Any.From_Any (Data)));
--    end Split;

   ----------
   -- Join --
   ----------

   function Join (Data : Split_SRP) return Any.Any
   is
      URL : Types.String;
      Current_Arg : Arg_Info_Ptr := Data.Args;
   begin
      Append (URL, Data.Method.all & " " & To_String (Data.Oid.all) & "?");
      while Current_Arg /= null loop
         raise Not_Implemented;
--          Append (URL, Current_Arg.all.Name.all & "=" &
--                  Current_Arg.all.Value.all);
--          if Current_Arg.all.Next /= null then
--             Append (URL, "&");
--          end if;
--          Current_Arg := Current_Arg.Next;
      end loop;
      return Any.To_Any (Types.To_PolyORB_String (To_String (URL)));
   end Join;

end PolyORB.Utils.SRP;
