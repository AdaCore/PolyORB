------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . M I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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

--  MIOP specific tagged components

with Ada.Streams;

with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Strings;

package body PolyORB.MIOP_P.Tagged_Components is

   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.miop_p.tagged_components");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

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

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   procedure Marshall_Component_Data
     (Comp   : access TC_Group_Info;
      Buffer : access Buffer_Type)
   is
      use PolyORB.Types;
      Temp_Buf : Buffer_Access := new Buffer_Type;
   begin
      pragma Debug (C, O ("Marshall Group_Info"));
      pragma Debug (C, O ("Group : " & Image (Comp.G_I)));

      Start_Encapsulation (Temp_Buf);

      Marshall (Temp_Buf, TC_Group_Info_Version_Major);
      Marshall (Temp_Buf, TC_Group_Info_Version_Minor);
      Marshall (Temp_Buf, Types.Identifier (Comp.G_I.Group_Domain_Id));
      Marshall (Temp_Buf, Comp.G_I.Object_Group_Id);
      Marshall (Temp_Buf, Comp.G_I.Object_Group_Ref_Version);

      Marshall (Buffer, Encapsulate (Temp_Buf));
      Release (Temp_Buf);
   end Marshall_Component_Data;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   procedure Unmarshall_Component_Data
     (Comp   : access TC_Group_Info;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;
      use PolyORB.Types;
      use type Ada.Streams.Stream_Element_Offset;

      Tag_Body : aliased Encapsulation := Unmarshall (Buffer);
      Temp_Buf : Buffer_Access := new Buffer_Type;
      Temp : Types.Octet;

   begin
      Decapsulate (Tag_Body'Access, Temp_Buf);

      pragma Debug (C, O ("Unmarshall Group_Info"));
      Temp := Unmarshall (Temp_Buf);
      pragma Assert (Temp = TC_Group_Info_Version_Major);

      Temp := Unmarshall (Temp_Buf);
      pragma Assert (Temp = TC_Group_Info_Version_Minor);

      Comp.G_I.Group_Domain_Id :=
        Types.String (Types.Identifier'(Unmarshall (Temp_Buf)));
      Comp.G_I.Object_Group_Id := Unmarshall (Temp_Buf);
      Comp.G_I.Object_Group_Ref_Version := Unmarshall (Temp_Buf);
      pragma Debug (C, O ("Group Info : " & Image (Comp.G_I)));

      pragma Assert (Remaining (Temp_Buf) = 0);
      Release (Temp_Buf);

   exception
      when others =>
         Release (Temp_Buf);
         Throw (Error,
                Bad_Param_E,
                System_Exception_Members'(10, Completed_No));
   end Unmarshall_Component_Data;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (Comp : TC_Group_Info) return Tagged_Component_Access is
   begin
      return new TC_Group_Info'(Comp);
   end Duplicate;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (Comp : access TC_Group_Info) is
      pragma Unreferenced (Comp);
   begin
      null;
   end Release_Contents;

   ---------------
   -- To_String --
   ---------------

   function To_String (Comp : access TC_Group_Info) return String
   is
      use PolyORB.Types;
      use PolyORB.Utils;
   begin
      pragma Debug (C, O ("To_String Group_Info"));
      pragma Debug (C, O ("Group : " & Image (Comp.G_I)));
      declare
         S : constant String :=
           Trimmed_Image (Unsigned_Long_Long
                          (TC_Group_Info_Version_Major)) & "."
           & Trimmed_Image (Unsigned_Long_Long
                            (TC_Group_Info_Version_Minor)) & "-"
           & To_Standard_String (Comp.G_I.Group_Domain_Id) & "-"
           & Trimmed_Image (Comp.G_I.Object_Group_Id);
      begin
         if Comp.G_I.Object_Group_Ref_Version /= 0 then
            return S & "-"
              & Trimmed_Image (Unsigned_Long_Long
                               (Comp.G_I.Object_Group_Ref_Version));
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
      pragma Debug (C, O ("Extract Group_Info from string"));

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

      pragma Debug (C, O ("Group Info : " & Image (G_I.G_I)));
      return G_I;
   end From_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register (Tag_Group, Create_Component'Access, null);
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
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.MIOP_P.Tagged_Components;
