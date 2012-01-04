------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A W S . P A R A M E T E R S . S E T                    --
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

package AWS.Parameters.Set is

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : String);
   --  Add a new Key/Value pair into the parameter set.

   procedure Add (Parameter_List : in out List; Parameters : String);
   --  Set parameters for the current request. This is used for a POST method
   --  because the parameters are found in the message body and are not known
   --  when we parse the request line. The Parameters string has the form
   --  "name1=value1&name2=value2...". The paramaters are added to the list.
   --  The parameters can start with a '?' (standard Web character separator)
   --  which is just ignored.

   procedure Case_Sensitive
     (Parameter_List : in out List;
      Mode           : Boolean);
   --  If Mode is True it will use all parameters with case sensitivity.

   procedure Reset (Parameter_List : in out List);
   --  Removes all object from the Set. Set will be reinitialized and will be
   --  ready for new use.

   procedure Free (Parameter_List : in out List);
   --  Release all memory used by the list.

end AWS.Parameters.Set;
