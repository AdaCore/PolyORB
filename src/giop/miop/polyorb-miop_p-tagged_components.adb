------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . M I O P _ P . T A G G E D _ C O M P O N E N T S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2003 Free Software Foundation, Inc.            --
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

--  MIOP specific tagged components

with Ada.Unchecked_Deallocation;

with PolyORB.Buffers;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Representations.CDR;
with PolyORB.Utils.Strings;

package body PolyORB.MIOP_P.Tagged_Components is

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Representations.CDR;

   procedure Free
   is new Ada.Unchecked_Deallocation (TC_Group_Info, TC_Group_Info_Access);

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.miop_p.tagged_components");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ----------------------
   -- Create_Component --
   ----------------------

   function Create_Component
     return Tagged_Component_Access;

   function Create_Component
     return Tagged_Component_Access is
   begin
      return new TC_Group_Info;
   end Create_Component;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (C      : access TC_Group_Info;
      Buffer : access Buffer_Type)
   is
      use PolyORB.Types;

   begin
      pragma Debug (O ("Marshall Group_Info"));
      pragma Debug (O ("Group : " & Image (C.G_I)));

      Marshall (Buffer, TC_Group_Info_Version_Major);
      Marshall (Buffer, TC_Group_Info_Version_Minor);
      Marshall (Buffer, C.G_I.Group_Domain_Id);
      Marshall (Buffer, C.G_I.Object_Group_Id);
      Marshall (Buffer, C.G_I.Object_Group_Ref_Version);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (C      : access TC_Group_Info;
      Buffer : access Buffer_Type)
   is
      use PolyORB.Types;

      Temp : Types.Octet;
   begin
      pragma Debug (O ("Unmarshall Group_Info"));
      Temp := Unmarshall (Buffer);
      pragma Assert (Temp = TC_Group_Info_Version_Major);

      Temp := Unmarshall (Buffer);
      pragma Assert (Temp = TC_Group_Info_Version_Minor);

      C.G_I.Group_Domain_Id := Unmarshall (Buffer);
      C.G_I.Object_Group_Id := Unmarshall (Buffer);
      C.G_I.Object_Group_Ref_Version := Unmarshall (Buffer);
      pragma Debug (O ("Group Info : " & Image (C.G_I)));
   end Unmarshall;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (C : access TC_Group_Info)
   is
      CC : TC_Group_Info_Access := TC_Group_Info_Access (C);
   begin
      Free (CC);
   end Release_Contents;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (C : access TC_Group_Info)
     return String
   is
      use PolyORB.Types;
      use PolyORB.Utils;

   begin
      pragma Debug (O ("To_String Group_Info"));
      pragma Debug (O ("Group : " & Image (C.G_I)));
      declare
         S : constant String :=
           Trimmed_Image (Integer (TC_Group_Info_Version_Major)) & "."
           & Trimmed_Image (Integer (TC_Group_Info_Version_Minor)) & "-"
           & To_Standard_String (C.G_I.Group_Domain_Id) & "-"
           & Trimmed_Image (Integer (C.G_I.Object_Group_Id));
         --  XXX not a long long conversion
      begin
         if C.G_I.Object_Group_Ref_Version /= 0 then
            return S & "-"
              & Trimmed_Image (Integer (C.G_I.Object_Group_Ref_Version));
         else
            return S;
         end if;
      end;
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String
     (S : String)
     return TC_Group_Info_Access
   is
      use PolyORB.Types;
      use PolyORB.Utils;
      use PolyORB.Utils.Strings;

      Index  : Integer := S'First;
      Index2 : Integer;
      G_I    : TC_Group_Info_Access;
   begin
      pragma Debug (O ("Extract Group_Info from string"));

      Index2 := Find (S, Index, '.');
      if Index2 = S'Last + 1 then
         return null;
      end if;

      if Types.Octet'Value (S (Index .. Index2 - 1))
        /= TC_Group_Info_Version_Major
      then
         return null;
      end if;
      Index := Index2 + 1;

      Index2 := Find (S, Index, '-');
      if Index2 = S'Last + 1 then
         return null;
      end if;

      if Types.Octet'Value (S (Index .. Index2 - 1))
        /= TC_Group_Info_Version_Minor
      then
         return null;
      end if;
      Index := Index2 + 1;

      Index2 := Find (S, Index, '-');
      if Index2 = S'Last + 1 then
         return null;
      end if;

      G_I := new TC_Group_Info;
      G_I.G_I.Group_Domain_Id := To_PolyORB_String (S (Index .. Index2 - 1));
      Index := Index2 + 1;

      Index2 := Find (S, Index, '-');
      if Index2 = S'Last + 1 then
         G_I.G_I.Object_Group_Id
           := Types.Unsigned_Long_Long'Value (S (Index .. S'Last));

      else
         G_I.G_I.Object_Group_Id
           := Types.Unsigned_Long_Long'Value (S (Index .. Index2 - 1));
         G_I.G_I.Object_Group_Ref_Version
           := Types.Unsigned_Long'Value (S (Index2 + 1 .. S'Last));
      end if;

      pragma Debug (O ("Group Info : " & Image (G_I.G_I)));
      return G_I;
   end From_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register (Tag_Group, Create_Component'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.miop",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Init      => Initialize'Access));

end PolyORB.MIOP_P.Tagged_Components;
