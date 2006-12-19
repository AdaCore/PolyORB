------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . E X C E P T I O N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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

with Ada.Exceptions;

with PolyORB.Exceptions;
with PolyORB.Types;

with System.RPC;

package body PolyORB.DSA_P.Exceptions is

   use Ada.Exceptions;

   use PolyORB.Errors;
   use PolyORB.Exceptions;
   use PolyORB.Types;

   --------------------
   -- Raise_From_Any --
   --------------------

   procedure Raise_From_Any
     (Occurrence : Any.Any;
      Message     : String := "")
   is
      Repository_Id : constant PolyORB.Types.RepositoryId
        := Any.TypeCode.Id (PolyORB.Any.Get_Type (Occurrence));

      EId : constant String := To_Standard_String (Repository_Id);

      Is_Error : Boolean;
      Id       : Error_Id;
   begin
      pragma Assert (not Any.Is_Empty (Occurrence));

      Exception_Name_To_Error_Id (EId, Is_Error, Id);

      if Is_Error then

         --  PolyORB errors should raise a DSA specific exception

         raise System.RPC.Communication_Error;
      else
         Ada.Exceptions.Raise_Exception
           (Get_ExcepId_By_Name (Exception_Name (EId)), Message);
      end if;
   end Raise_From_Any;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error : in out PolyORB.Errors.Error_Container) is
   begin
      pragma Assert (Is_Error (Error));
      Free (Error.Member);
      raise System.RPC.Communication_Error;
   end Raise_From_Error;
end PolyORB.DSA_P.Exceptions;
