------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      A W S . D I S P A T C H E R S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a service to build Callbacks which can support
--  user's data. It is possible to build a new dispatcher by inheriting the
--  handler type and to provides the Dispatch routine.

with Ada.Finalization;

with AWS.Response;
with AWS.Status;

package AWS.Dispatchers is

   type Handler is abstract new Ada.Finalization.Controlled with private;

   procedure Initialize (Dispatcher : in out Handler);
   procedure Adjust     (Dispatcher : in out Handler);
   procedure Finalize   (Dispatcher : in out Handler);
   --  Initialize/Adjust/Finalize is doing the reference counting, children
   --  should just call these routines if possible. It is possible to know if
   --  no more object are referenced by calling Ref_Counter below.

   function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data)
     return Response.Data is abstract;
   --  Call the appropriate inherited dispatcher

   function Ref_Counter (Dispatcher : Handler) return Natural;
   --  Returns the reference counter for Handler. If 0 is returned then this
   --  object is not referenced anymore, it is safe to deallocate ressources.

   type Handler_Class_Access is access all Handler'Class;

   procedure Free (Dispatcher : in out Handler_Class_Access);
   pragma Inline (Free);

private

   type Natural_Access is access all Natural;

   type Handler is abstract new Ada.Finalization.Controlled with record
      Ref_Counter : Natural_Access := null;
   end record;

end AWS.Dispatchers;
