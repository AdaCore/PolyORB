------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . H E L P E R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

--  $Id: //droopi/main/src/corba/corba-helper.adb#1 $

package body CORBA.Helper is

   function TC_RepositoryId return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := CORBA.TypeCode.TC_Alias;
      Name : constant CORBA.String
        := CORBA.To_CORBA_String ("RepositoryId");
      Id : constant CORBA.String := CORBA.To_CORBA_String
        ("IDL:omg.org/CORBA/RepositoryId:1.0");
   begin
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (CORBA.TC_String));
      return Result;
   end TC_RepositoryId;

   function From_Any (Item : in CORBA.Any)
      return CORBA.RepositoryId is
      Result : CORBA.String := CORBA.From_Any (Item);
   begin
      return CORBA.RepositoryId (Result);
   end From_Any;

   function To_Any
     (Item : in CORBA.RepositoryId)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Set_Type (Result, TC_RepositoryId);
      return Result;
   end To_Any;

   function TC_Identifier return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := CORBA.TypeCode.TC_Alias;
      Name : constant CORBA.String
        := CORBA.To_CORBA_String ("Identifier");
      Id : constant CORBA.String := CORBA.To_CORBA_String
        ("IDL:omg.org/CORBA/Identifier:1.0");
   begin
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (CORBA.TC_String));
      return Result;
   end TC_Identifier;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Identifier is
      Result : CORBA.String := CORBA.From_Any (Item);
   begin
      return CORBA.Identifier (Result);
   end From_Any;

   function To_Any
     (Item : in CORBA.Identifier)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Set_Type (Result, TC_Identifier);
      return Result;
   end To_Any;

   function TC_ScopedName return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object := CORBA.TypeCode.TC_Alias;
      Name : constant CORBA.String
        := CORBA.To_CORBA_String ("ScopedName");
      Id : constant CORBA.String := CORBA.To_CORBA_String
        ("IDL:omg.org/CORBA/ScopedName:1.0");
   begin
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter
        (Result, CORBA.To_Any (CORBA.TC_String));
      return Result;
   end TC_ScopedName;

   function From_Any (Item : in CORBA.Any)
      return CORBA.ScopedName is
      Result : CORBA.String := CORBA.From_Any (Item);
   begin
      return CORBA.ScopedName (Result);
   end From_Any;

   function To_Any
     (Item : in CORBA.ScopedName)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Set_Type (Result, TC_ScopedName);
      return Result;
   end To_Any;

end CORBA.Helper;
