------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         C O R B A . V A L U E                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with CORBA.AbstractBase;
with CORBA.Impl;

package CORBA.Value is

   type Base is abstract new CORBA.AbstractBase.Ref with null record;

   type Impl_Base is abstract new CORBA.Impl.Object with null record;

   --  FIXME:
   --  The standard definition of package CORBA.Value is ambiguous.
   --  It is unclear whether Impl_Base should be an
   --  "abstract tagged null record" or an
   --  "abstract new CORBA.Impl.Object with null record".
   --
   --  Inheriting from CORBA.Impl.Object seems to be the intended
   --  architecture, as CORBA.Impl.Object is defined to be used
   --  as "the root for all implementations, both of interfaces
   --  and value types. Value type implementations thus benefit
   --  of the reference-counting semantics of CORBA.Impl.OBject.
   --
   --  An issue against ptc/00-05-04 has been raised with OMG.

   --  I suggest that we define Is_A here to be able to
   --  implement To_Ref and To_Abstract_Ref for valuetypes.
   --  (Fabien)
   --  Shall we raise an issue to OMG ?

   function Is_A
     (Self : in Base;
      Logical_Type_Id : Standard.String)
      return CORBA.Boolean;


end CORBA.Value;
