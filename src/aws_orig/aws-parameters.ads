------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A W S . P A R A M E T E R S                        --
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

with Ada.Strings.Unbounded;

with AWS.Containers.Tables;

package AWS.Parameters is

   type List is new AWS.Containers.Tables.Table_Type with private;

   subtype VString_Array is AWS.Containers.Tables.VString_Array;

   function URI_Format (Parameter_List : List) return String;
   --  Returns the list of parameters in the URI format. This can be added
   --  after the ressource to form the complete URI. The format is:
   --  "?name1=value1&name2=value2..."

   --  See AWS.Containers.Tables for inherited routines.

private
   --  A List must be initialized by calling AWS.Parameters.Set.Reset, Server
   --  is responsible for doing that.

   use Ada.Strings.Unbounded;

   type List is new AWS.Containers.Tables.Table_Type with record
      Parameters : Unbounded_String;
   end record;

end AWS.Parameters;
