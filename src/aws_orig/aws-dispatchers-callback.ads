------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             A W S . D I S P A T C H E R S . C A L L B A C K              --
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

--  Dispatch on a Callback procedure.

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;

package AWS.Dispatchers.Callback is

   type Handler is new Dispatchers.Handler with private;
   --  This is a simple wrapper around standard callback procedure (access to
   --  function). It will be used to build dispatchers services and for the
   --  main server callback.

   function Create
     (Callback : Response.Callback)
      return Handler;
   pragma Inline (Create);
   --  Build a dispatcher for the specified callback.

   function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data)
      return Response.Data;

private

   type Handler is new AWS.Dispatchers.Handler with record
      Callback : Response.Callback;
   end record;

end AWS.Dispatchers.Callback;
