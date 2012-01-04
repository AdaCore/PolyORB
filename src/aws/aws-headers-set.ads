------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      A W S . H E A D E R S . S E T                       --
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

package AWS.Headers.Set is

   procedure Add (Headers : in out List; Name, Value : String);
   pragma Inline (Add);
   --  Add HTTP header name/value at the end of the Headers container. Note
   --  that there is no check about validity of this header. This service is
   --  provided to be able to create user-defined headers.

   procedure Update
     (Headers : in out List;
      Name    : String;
      Value   : String;
      N       : Positive := 1);
   pragma Inline (Update);
   --  Update the N-th HTTP header Value with the given Name.
   --  The header could already have more than one value associated with
   --  this name. If there is M values with this Name, then if:
   --     N <= M      => update the value
   --     N  = M + 1  => the pair name=value is appended to the table
   --     N  > M + 1  => Constraint_Error raised

--   procedure Read (Socket : Net.Socket_Type'Class; Headers : in out List);
   --  Read and parse HTTP header from the socket.

   procedure Reset (Headers : in out List);
   pragma Inline (Reset);
   --  Removes all object from Headers. Headers will be reinitialized and will
   --  be ready for new use.

   procedure Free (Headers : in out List);
   pragma Inline (Free);
   --  Release all memory used by the List container.

   procedure Debug (Activate : Boolean);
   --  Turn on Debug output.

end AWS.Headers.Set;
