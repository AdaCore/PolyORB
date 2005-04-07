------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . P A R A M E T E R S . R E G I S T R Y           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Dynamic_Dict;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Parameters.Registry is

   use PolyORB.Utils.Strings;

   -------
   -- O --
   -------

   procedure O (S : String);
   pragma Inline (O);
   --  Output a diagnostic or error message.

   --  Note: We are currently initializing structures on which
   --  PolyORB.Log.Facility_Log depends. Thus we cannot instantiate
   --  this package and use PolyORB.Log.Internals.Put_Line instead.

   Debug : constant Boolean := True;

   procedure O (S : String) is
   begin
      if Debug then
         PolyORB.Log.Internals.Put_Line (S);
      end if;
   end O;

   function Get_Conf
     (Section, Key : String;
      Default      : String := "")
     return String;

   procedure Set_Conf
     (Section, Key : String;
      Value        : String);

   --------------------------------------------
   -- The configuration variables dictionary --
   --------------------------------------------

   package Variables is new PolyORB.Dynamic_Dict (Value => String_Ptr);

   function Fetch (Key : String) return String;
   --  Get the string from a file (if Key starts with file: and the file
   --  exists, otherwise it is an empty string), or the string itself
   --  otherwise.

   function Make_Global_Key (Section, Key : String) return String;
   --  Build Dynamic Dict key from (Section, Key) tuple

   function Make_Env_Name (Section, Key : String) return String;
   --  Build environment variable from (Section, Key) tuple

   ---------------------
   -- Make_Global_Key --
   ---------------------

   function Make_Global_Key (Section, Key : String) return String is
   begin
      return "[" & Section & "]" & Key;
   end Make_Global_Key;

   -------------------
   -- Make_Env_Name --
   -------------------

   function Make_Env_Name (Section, Key : String) return String is
      Result : String := "POLYORB_"
        & PolyORB.Utils.To_Upper (Section & "_" & Key);

   begin
      for J in Result'Range loop
         case Result (J) is
            when
              '0' .. '9' |
              'A' .. 'Z' |
              'a' .. 'z' |
              '_'        =>
               null;
            when others =>
               Result (J) := '_';
         end case;
      end loop;

      return Result;
   end Make_Env_Name;

   -----------
   -- Fetch --
   -----------

   function Fetch (Key : String) return String is
   begin
      if PolyORB.Utils.Has_Prefix (Key, "file:")
        and then Fetch_From_File_Hook /= null
      then
         return Fetch_From_File_Hook.all (Key);

      else
         return Key;
      end if;
   end Fetch;

   --------------
   -- Get_Conf --
   --------------

   function Get_Conf
     (Section, Key : String;
      Default      : String := "")
     return String
   is
      From_Env : constant String
        := Get_Env (Make_Env_Name (Section, Key));

      Default_Value : aliased String := Default;

   begin
      if From_Env /= "" then
         return Fetch (From_Env);
      else
         return Fetch
           (Variables.Lookup
            (Make_Global_Key (Section, Key),
             String_Ptr'(Default_Value'Unchecked_Access)).all);
      end if;
   end Get_Conf;

   --------------
   -- Set_Conf --
   --------------

   procedure Set_Conf
     (Section, Key : String;
      Value        : String)
   is
      K : constant String := Make_Global_Key (Section, Key);
      P : String_Ptr := Variables.Lookup (K, null);

   begin
      pragma Debug (O (K & "=" & Value));
      if P /= null then
         Variables.Unregister (K);
         Free (P);
      end if;

      Variables.Register (K, +Value);
   end Set_Conf;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Get_Conf_Hook := Get_Conf'Access;
      Set_Conf_Hook := Set_Conf'Access;
      Reset_Hook := Variables.Reset'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"parameters.registry",
       Conflicts => Empty,
       Depends   => +"log?",
       Provides  => +"parameters",
       Implicit  => True,
       Init      => Initialize'Access));
end PolyORB.Parameters.Registry;
