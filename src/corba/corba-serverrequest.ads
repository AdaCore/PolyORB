------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . S E R V E R R E Q U E S T                   --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Requests;

with CORBA.NVList;

package CORBA.ServerRequest is

   pragma Elaborate_Body;

--     interface ServerRequest { // PIDL
--         readonly attribute  Identifier operation;
--         void                arguments    (inout NVList nv);
--         Context             ctx();
--         void                set_result   (in any val);
--         void                set_exception(in any val);
--     };

   subtype Object is PolyORB.Requests.Request;
   subtype Object_Ptr is PolyORB.Requests.Request_Access;

   function Operation (O : Object) return Identifier;
   procedure Arguments (O : access Object; NV : in out NVList.Ref);
   --  function Ctx return Context;
   procedure Set_Result (O : access Object; Val : Any);
   procedure Set_Exception (Obj : access Object; Val : Any);

   --------------------------------------
   -- The following is PolyORB-specific --
   --------------------------------------

--    function To_PolyORB_Request
--      (O : access Object)
--      return PolyORB.Requests.Request_Access;

--    function To_CORBA_ServerRequest
--      (R : PolyORB.Requests.Request_Access)
--      return Object_Ptr;

--  private

--    type Object is new PolyORB.Requests.Request with null record;

--    pragma Inline (To_PolyORB_Request);
--    pragma Inline (To_CORBA_ServerRequest);

end CORBA.ServerRequest;
