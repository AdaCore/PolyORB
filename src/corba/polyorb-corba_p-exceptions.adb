------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

--  $Id: //droopi/main/src/corba/polyorb-corba_p-exceptions.adb#16 $

with Ada.Exceptions;

with PolyORB.Any;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.CORBA_P.Exceptions is

   use Ada.Exceptions;

   use PolyORB.Any;
   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Types;
   use PolyORB.Utils;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.exceptions");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ----------------------------
   -- To_CORBA_Internal_Name --
   ----------------------------

   function To_CORBA_Exception (Name : String) return String;
   --  Map PolyORB system exception Repository Ids into CORBA
   --  ones; leave other repository IDs unchanged.

   function To_CORBA_Exception
     (Name : String) return String
   is
      use PolyORB.Utils;

      Colon1 : constant Integer := Find (Name, Name'First, ':');
      Slash  : constant Integer := Find (Name, Colon1 + 1, '/');

      Root      : String renames Name (Name'First .. Slash);
      Base_Name : String renames Name (Slash + 1 .. Name'Last);
   begin
      pragma Debug (O ("To_CORBA_Exception: name was " & Name));

      if Root = PolyORB_Prefix then
         pragma Debug (O ("To_CORBA_Exception: new name is "
                          & "IDL:CORBA/" & Base_Name));

         return "IDL:CORBA/" & Base_Name;

      else
         pragma Debug (O (" not changed: " & Name));

         return Name;
      end if;
   end To_CORBA_Exception;

   --------------------
   -- Raise_From_Any --
   --------------------

   procedure Raise_From_Any
     (Occurrence : Any.Any)
   is
      Repository_Id : constant PolyORB.Types.RepositoryId
        := Any.TypeCode.Id (PolyORB.Any.Get_Type (Occurrence));

      EId : constant String := To_Standard_String (Repository_Id);

   begin
      pragma Debug (O ("Raise_From_Any: enter"));

      if Is_System_Exception (EId) then
         declare
            CORBA_Repository_Id : constant String
              := To_CORBA_Exception (EId);

            System_Id : constant Ada.Exceptions.Exception_Id
              := PolyORB.Exceptions.Get_ExcepId_By_RepositoryId
              (CORBA_Repository_Id);

         begin
            pragma Debug (O ("Raising "
                             & Ada.Exceptions.Exception_Name (System_Id)));

            PolyORB.Exceptions.Raise_System_Exception_From_Any
              (System_Id, Occurrence);
         end;
      else
         pragma Debug (O ("Raising " & EId));

         PolyORB.Exceptions.Raise_User_Exception_From_Any
           (Repository_Id, Occurrence);

      end if;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Any;

end PolyORB.CORBA_P.Exceptions;
