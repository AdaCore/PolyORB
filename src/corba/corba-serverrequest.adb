------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . S E R V E R R E Q U E S T                   --
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

--  Mapping for the standard ServerRequest interface

--  $Id$

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body CORBA.ServerRequest is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("corba.serverrequest");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   function Operation (O : Object) return Identifier is
   begin
      return Identifier (O.Operation);
   end Operation;

   procedure Arguments (O : access Object; NV : in out NVList.Ref) is
      PolyORB_Args : PolyORB.Any.NVList.Ref
        := CORBA.NVList.To_PolyORB_Ref (NV);
   begin
      PolyORB.Requests.Arguments
        (PolyORB.Requests.Request_Access (O), PolyORB_Args);
      NV := CORBA.NVList.To_CORBA_Ref (PolyORB_Args);
   end Arguments;

   procedure Set_Result (O : access Object; Val : Any)
   is
   begin
      PolyORB.Requests.Set_Result
        (PolyORB.Requests.Request_Access (O), Val);
   end Set_Result;

   procedure Set_Exception (Obj : access Object; Val : Any)
   is
   begin
      pragma Debug
        (O ("Server notifies exception: " & Image (Val)));
      Obj.Exception_Info := Val;
   end Set_Exception;

end CORBA.ServerRequest;
