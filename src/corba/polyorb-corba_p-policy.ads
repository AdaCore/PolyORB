------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . C O R B A _ P . P O L I C Y                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with CORBA;

with PolyORB.Smart_Pointers.Controlled_Entities;

package PolyORB.CORBA_P.Policy is

   package PSPCE renames PolyORB.Smart_Pointers.Controlled_Entities;

   type Policy_Object_Type is new PSPCE.Entity with private;

   type Policy_Object_Ptr is access all Policy_Object_Type'Class;

   function Get_Policy_Type
     (Self : Policy_Object_Type)
     return CORBA.PolicyType;

   procedure Set_Policy_Type
     (Self   : in out Policy_Object_Type;
      Policy :        CORBA.PolicyType);

   function Get_Policy_Value
     (Self : Policy_Object_Type)
     return CORBA.Any;

   procedure Set_Policy_Value
     (Self  : in out Policy_Object_Type;
      Value :        CORBA.Any);

private

   type Policy_Object_Type is new PSPCE.Entity with record
      Policy : CORBA.PolicyType;
      Value  : CORBA.Any;
   end record;

end PolyORB.CORBA_P.Policy;
