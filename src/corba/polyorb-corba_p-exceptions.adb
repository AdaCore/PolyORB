------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  $Id: //droopi/main/src/corba/polyorb-corba_p-exceptions.adb#13 $

with Ada.Exceptions;

with CORBA;

with PolyORB.Any;
with PolyORB.Exceptions;
with PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.CORBA_P.Exceptions is

   use Ada.Exceptions;

   use PolyORB.Any;
   use PolyORB.Types;
   use PolyORB.Utils;

   ----------------------------
   -- To_CORBA_Internal_Name --
   ----------------------------

   function To_CORBA_Internal_Name (Name : String)
                           return String;

   function To_CORBA_Internal_Name (Name : String)
                           return String
   is
      use PolyORB.Utils;

      Colon1 : constant Integer := Find (Name, Name'First, ':');
      Colon2 : constant Integer := Find (Name, Colon1 + 1, '/');
      Colon3 : constant Integer := Find (Name, Colon2 + 1, ':');

      Root  : constant String   := Name (Name'First .. Colon2);

   begin
      if Root = "INTERNAL:POLYORB/" then
         return "CORBA/" & Name (Colon2 + 1 .. Colon3 - 1);
      else
         return Name (Colon1 + 1 .. Colon3 - 1);
      end if;
   end To_CORBA_Internal_Name;

   --------------------
   -- Raise_From_Any --
   --------------------

   procedure Raise_From_Any (Occurrence : Any.Any)
   is
      use PolyORB.Exceptions;

      Repository_Id : constant PolyORB.Types.RepositoryId
        := Any.TypeCode.Id (PolyORB.Any.Get_Type (Occurrence));

      Name : constant String
        := To_CORBA_Internal_Name (PolyORB.Types.To_Standard_String
                                   (Repository_Id));

      System_Id : Ada.Exceptions.Exception_Id;

      Is_CORBA_System_Exc : constant Boolean
        := Is_System_Exception (To_Standard_String (Repository_Id));
   begin
      PolyORB.Exceptions.Get_ExcepId_By_RepositoryId
        (Name, System_Id, CORBA.Unknown'Identity);

      if Is_CORBA_System_Exc then
         PolyORB.Exceptions.Raise_System_Exception_From_Any
           (System_Id, Occurrence);
      else
         PolyORB.Exceptions.Raise_User_Exception_From_Any
           (Repository_Id, Occurrence);
      end if;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Any;

end PolyORB.CORBA_P.Exceptions;
