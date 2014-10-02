------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . D S A _ P . C O N N E C T I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2011-2014, Free Software Foundation, Inc.          --
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

--  Connections management

pragma Ada_2005;

package PolyORB.DSA_P.Connections is

   generic
      type Object is abstract tagged limited private;
      type RACW is access all Object'Class;
      pragma Remote_Access_Type (RACW);
      --  Must be a remote access to class-wide type

   package Connection_Manager is
      function Is_Remote (X : RACW) return Boolean;
      --  True if X designates a remote object

      function Is_Connected (X : RACW) return Boolean;
      --  True if a valid connection to the partition of X is open (or X
      --  X designates a local object).

      procedure Disconnect (X : RACW);
      --  Forcibly close connection to the partition of X.
      --  Note that this connection is usually shared between all RACWs for
      --  objects on the same partition.

      procedure On_Disconnect
        (X        : RACW;
         Callback : access procedure (X : RACW));
      --  Call Callback (X) when connection to partition of X is closed (not
      --  implemented???)

      procedure Unchecked_Forget (X : in out RACW);
      --  Mark X as now unused by the application, causing associated stub
      --  resources (if any) to be deallocated. Note that it is erroneous to
      --  call this subprogram if the partition still holds other RACWs
      --  designating the same object.

   private
      subtype Stub_Type is RACW'Stub_Type;
   end Connection_Manager;

end PolyORB.DSA_P.Connections;
