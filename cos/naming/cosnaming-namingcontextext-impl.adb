------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      C O S N A M I N G . N A M I N G C O N T E X T E X T . I M P L       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with PolyORB.Utils;

with CosNaming.NamingContextExt.Helper;
with CosNaming.NamingContextExt.Skel;
pragma Warnings (Off, CosNaming.NamingContextExt.Skel);

with PolyORB.CORBA_P.Naming_Tools;

package body CosNaming.NamingContextExt.Impl is

   use PolyORB.Utils;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : access Object;
      N : CosNaming.Name)
     return CosNaming.NamingContextExt.StringName
   is
      pragma Unreferenced (Self);

      Result : CosNaming.NamingContextExt.StringName;

   begin
      for J in 1 .. Length (N) loop
         if Get_Element (N, J).kind = "" then
            if Get_Element (N, J).id = "" then
               Result := Result & ".";
            else
               Result := Result
                 & StringName (Get_Element (N, J).id);
            end if;
         else
            Result := Result
              & To_CORBA_String
              (URI_Encode
               (To_Standard_String (StringName (Get_Element (N, J).id))))
              & "."
              & StringName (Get_Element (N, J).kind);
         end if;

         if J < Length (N) then
            Result := Result & "/";
         end if;

      end loop;

      return Result;
   end To_String;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (Self : access Object;
      Sn : CosNaming.NamingContextExt.StringName)
     return CosNaming.Name
   is
      pragma Unreferenced (Self);
   begin
      return PolyORB.CORBA_P.Naming_Tools.Parse_Name (To_String (Sn));
   end To_Name;

   ------------
   -- To_Url --
   ------------

   function To_Url
     (Self : access Object;
      Addr : CosNaming.NamingContextExt.Address;
      Sn : CosNaming.NamingContextExt.StringName)
     return CosNaming.NamingContextExt.URLString
   is
      pragma Unreferenced (Self);

      Result : CosNaming.NamingContextExt.URLString;
   begin
      if Addr = To_CORBA_String ("") then
         CosNaming.NamingContextExt.Helper.Raise_InvalidAddress
           (InvalidAddress_Members'
            (CORBA.IDL_Exception_Members with null record));
      end if;

      Result := To_CORBA_String (URI_Encode (To_Standard_String (Addr)))
        & "/" & To_CORBA_String (URI_Encode (To_Standard_String (Sn)));

      return Result;
   end To_Url;

   -----------------
   -- Resolve_Str --
   -----------------

   function Resolve_Str
     (Self : access Object;
      Sn : CosNaming.NamingContextExt.StringName)
     return CORBA.Object.Ref
   is
      Result : CORBA.Object.Ref;

   begin
      Result := CosNaming.NamingContext.Impl.Resolve
        (CosNaming.NamingContext.Impl.Object (Self.all)'Access,
         To_Name (Self, Sn));

      return Result;
   end Resolve_Str;

   ------------
   -- Create --
   ------------

   function Create return CosNaming.NamingContextExt.Impl.Object_Ptr is
      Obj : constant Object_Ptr := new Object;

   begin
      Initialize (Obj);

      return Obj;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : Object_Ptr) is
   begin
      CosNaming.NamingContext.Impl.Initialize
        (CosNaming.NamingContext.Impl.Object_Ptr (Self));
   end Initialize;

end CosNaming.NamingContextExt.Impl;
