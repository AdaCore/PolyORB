------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                               X E _ R E G                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with System;
with Interfaces.C;

package body XE_Reg is

   use System;

   REG_ERROR : exception;

   --  Win32 registry functions prototypes

   subtype ULONG   is Interfaces.C.unsigned_long;
   subtype LONG    is Interfaces.C.long;
   subtype DWORD   is ULONG;
   type    PULONG  is access all ULONG;
   subtype PDWORD  is PULONG;
   subtype LPDWORD is PDWORD;

   subtype HKEY is ULONG;
   type PHKEY   is access all HKEY;

   type ACCESS_MASK is new DWORD;
   type REGSAM      is new ACCESS_MASK;

   KEY_QUERY_VALUE    : constant := 1;
   KEY_SET_VALUE      : constant := 2;

   HKEY_LOCAL_MACHINE : constant HKEY := 16#80000002#;

   ERROR_SUCCESS      : constant := 0;

   ------------------
   -- RegOpenKeyEx --
   ------------------

   function RegOpenKeyEx (hKey       : XE_Reg.HKEY;
                          lpSubKey   : Address;
                          ulOptions  : DWORD;
                          samDesired : REGSAM;
                          phkResult  : PHKEY)
     return LONG;
   pragma Import (Stdcall, RegOpenKeyEx, "RegOpenKeyExA");

   ----------------------
   -- RegQueryValueExA --
   ----------------------

   function RegQueryValueEx (hKey        : XE_Reg.HKEY;
                             lpValueName : Address;
                             lpReserved  : LPDWORD;
                             lpType      : LPDWORD;
                             lpData      : Address;
                             lpcbData    : LPDWORD)
     return LONG;
   pragma Import (Stdcall, RegQueryValueEx, "RegQueryValueExA");

   -----------------
   -- RegCloseKey --
   -----------------

   function RegCloseKey (hKey : XE_Reg.HKEY) return LONG;
   pragma Import (Stdcall, RegCloseKey, "RegCloseKey");

   function Open_Key (From_Key : in HKEY; Name : in String)
     return HKEY;
   --  open the Name registry key and return its handle

   function Get_Key (Key  : in HKEY;
                     Name : in String) return String;
   --  key the key name value for Name in the registry Key.

   --------------
   -- Open_Key --
   --------------

   function Open_Key (From_Key : in HKEY; Name : in String)
                      return HKEY
   is
      use type LONG;

      C_Name  : constant String := Name & ASCII.NUL;
      New_Key : aliased HKEY;
      Result  : LONG;
   begin
      Result := RegOpenKeyEx (From_Key,
                              C_Name'Address,
                              0,
                              KEY_QUERY_VALUE + KEY_SET_VALUE,
                              New_Key'Unchecked_Access);

      if Result /= ERROR_SUCCESS then
         raise REG_ERROR;
      else
         return New_Key;
      end if;
   end Open_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Key  : in HKEY;
                     Name : in String) return String
   is
      use type ULONG;
      use type LONG;

      Value        : String (1 .. 100);
      pragma Warnings (Off, Value);

      Size_Value   : aliased ULONG;
      Type_Value   : aliased DWORD;

      C_Name       : constant String := Name & ASCII.NUL;
      Result       : LONG;

   begin
      --  read value of the GCC key
      Size_Value := Value'Length;

      Result := RegQueryValueEx (Key,
                                 C_Name'Address,
                                 null,
                                 Type_Value'Unchecked_Access,
                                 Value'Address,
                                 Size_Value'Unchecked_Access);

      if Result /= ERROR_SUCCESS then
         raise REG_ERROR;
      else
         Size_Value := Size_Value - 1;
         return Value (1 .. Integer (Size_Value));
      end if;

   end Get_Key;

   --------------------
   -- Get_GARLIC_Dir --
   --------------------

   function Get_GARLIC_Dir return String_Access is
      ACT_Key : HKEY;
      Result  : String_Access;
   begin

      --  open "HKEY_LOCAL_MACHINE\SOFTWARE\Free Software Foundation" key
      ACT_Key := Open_Key (HKEY_LOCAL_MACHINE,
                           "SOFTWARE\Ada Core Technologies");

      --  get GCC value
      Result := new String'(Get_Key (ACT_Key, "GCC") & "\lib\garlic");

      --  close key
      declare
         Result : LONG;
      begin
         Result := RegCloseKey (ACT_Key);
      end;

      return Result;

   exception
      when REG_ERROR =>
         return null;
   end Get_GARLIC_Dir;

end XE_Reg;
