------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . G I O P _ P . E X C E P T I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  $Id$

with PolyORB.Any;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.GIOP_P.Exceptions is

   use PolyORB.Any;
   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Types;
   use PolyORB.Utils;

   package L is new PolyORB.Log.Facility_Log ("polyorb.giop_p.exceptions");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------------------
   -- To_CORBA_Exception_TypeCode --
   ---------------------------------

   function To_CORBA_Exception_TypeCode
     (TC : PolyORB.Any.TypeCode.Object)
     return PolyORB.Any.TypeCode.Object;

   function To_CORBA_Exception_TypeCode
     (TC : PolyORB.Any.TypeCode.Object)
     return PolyORB.Any.TypeCode.Object
   is
      CORBA_Exc_NameSpace : constant String := "IDL:";
      CORBA_Root : constant PolyORB.Types.String
        := To_PolyORB_String ("CORBA/");
      CORBA_Exc_Version   : constant PolyORB.Types.String
        := To_PolyORB_String (":1.0");
      --  CORBA exceptions namespace and version.

      Name : constant String := To_Standard_String (From_Any
        (TypeCode.Get_Parameter (TC, PolyORB.Types.Unsigned_Long (1))));

      Colon1 : constant Integer := Find (Name, Name'First, '/');
      Colon2 : constant Integer := Find (Name, Colon1 + 1, ':');

      Internal_Name : constant String := Name (Colon1 + 1 .. Colon2 - 1);

      New_Name : PolyORB.Types.String;
      Result_TC : TypeCode.Object := TypeCode.TC_Except;
   begin
      pragma Debug (O ("Exception name was : " & Name));

      --  Construct CORBA exception typecode

      --  Name
      TypeCode.Add_Parameter (Result_TC, To_Any
                              (To_PolyORB_String (Internal_Name)));

      --  RepositoryId : 'IDL:<Name>:1.0'
      if Name (Name'First .. Name'First + CORBA_Exc_NameSpace'Length - 1)
        /= CORBA_Exc_NameSpace then
         New_Name := To_PolyORB_String (CORBA_Exc_NameSpace)
           & CORBA_Root
           & To_PolyORB_String (Internal_Name)
           & CORBA_Exc_Version;
      else
         pragma Debug (O ("No modification required"));
         return TC;
      end if;

      pragma Debug (O ("New exception name is : "
                       & To_Standard_String (New_Name)));
      TypeCode.Add_Parameter (Result_TC, To_Any (New_Name));

      --  Component 'minor'
      TypeCode.Add_Parameter
        (Result_TC, To_Any (TC_Unsigned_Long));
      TypeCode.Add_Parameter
        (Result_TC, To_Any (To_PolyORB_String ("minor")));

      --  Component 'completed'
      TypeCode.Add_Parameter
        (Result_TC, To_Any (TC_Completion_Status));
      TypeCode.Add_Parameter
        (Result_TC, To_Any (To_PolyORB_String ("completed")));

      return Result_TC;
   end To_CORBA_Exception_TypeCode;

   ------------------------
   -- To_CORBA_Exception --
   ------------------------

   function To_CORBA_Exception (Exc : PolyORB.Any.Any)
      return PolyORB.Any.Any
   is
      use PolyORB.Any.TypeCode;

      Exc_TC : constant PolyORB.Any.TypeCode.Object := Get_Type (Exc);
      Result_TC : PolyORB.Any.TypeCode.Object;

      Result : Any.Any;

   begin
      --  Construct exception typecode
      Result_TC := To_CORBA_Exception_TypeCode (Exc_TC);

      if Exc_TC /= Result_TC then
         Set_Type (Result, Result_TC);

         Result := Get_Empty_Any_Aggregate (Result_TC);
         Add_Aggregate_Element
           (Result,
            Get_Aggregate_Element (Exc,
                                   TypeCode.TC_Unsigned_Long,
                                   Types.Unsigned_Long (0)));

         Add_Aggregate_Element
           (Result,
            Get_Aggregate_Element (Exc,
                                   TC_Completion_Status,
                                   Types.Unsigned_Long (1)));

         return Result;
      else
         return Exc;
      end if;
   end To_CORBA_Exception;

end PolyORB.GIOP_P.Exceptions;
