------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            C O R B A . B O A                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.15 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package is wrapped around a C++ class whose name is BOA declared
--  in file CORBA.h.  It provides two types of methods : the C functions of
--  the BOA class and their equivalent in Ada. (the first ones have a C_
--  prefix.)

with AdaBroker.Sysdep;  use AdaBroker.Sysdep;
with AdaBroker.OmniORB; use AdaBroker.OmniORB;

package body CORBA.BOA is

   -------------------------------
   -- C_Implementation_Is_Ready --
   -------------------------------

   procedure C_Implementation_Is_Ready
     (Self                  : in Object;
      ImplementationDef_Ptr : in System.Address;
      Non_Blocking          : in Bool);
   pragma Import
     (CPP, C_Implementation_Is_Ready,
      "impl_is_ready__FPQ25CORBA3BOAPQ25CORBA17ImplementationDefb");
   --  Corresponds to Ada_CORBA_Boa method impl is ready see
   --  Ada_CORBA_Boa.hh

   -----------------------------
   -- Implementation_Is_Ready --
   -----------------------------
   procedure Implementation_Is_Ready
     (Self         : in Object;
      Non_Blocking : in Boolean := False)
   is
      Tmp : System.Address    := System.Null_Address;
      NB  : Bool := To_Bool (Non_Blocking);
   begin
      C_Implementation_Is_Ready (Self, Tmp, NB);
   end Implementation_Is_Ready;

   ---------------------
   -- Object_Is_Ready --
   ---------------------

   procedure Object_Is_Ready
     (Self : in Object;
      Obj  : in ImplObject'Class)
   is
   begin
      --  It does not take the BOA into account because thereis only one
      --  BOA in omniORB2. ( See corbaBoa.cc)
      Object_Is_Ready (Obj);
   end Object_Is_Ready;

   ---------------------
   -- Object_Is_Ready --
   ---------------------

   procedure Dispose_Object
     (Self : in Object;
      Obj  : in ImplObject'Class)
   is
   begin
      --  It does not take the BOA into account because thereis only one
      --  BOA in omniORB2. ( See corbaBoa.cc)
      Dispose_Object (Obj);
   end Dispose_Object;

end CORBA.BOA;
