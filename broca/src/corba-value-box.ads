------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                      C O R B A . V A L U E . B O X                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

generic
   type Boxed is private;
   type Boxed_Access is access all Boxed;

package CORBA.Value.Box is

   type Box_Ref is new CORBA.Value.Base with private;

   --  function Is_Null (The_Ref : in Box_Ref) return Boolean;
   --  inherited from corba.abstractbase.ref

   function Create (With_Value : in Boxed) return Box_Ref;
   function "+" (With_Value : in Boxed) return Box_Ref
     renames Create;

   function Contents (The_Boxed : in Box_Ref)
     return Boxed_Access;
   function "-" (The_Boxed : in Box_Ref) return Boxed_Access
     renames Contents;

   procedure Release (The_Ref : in out Box_Ref);

private

   type Box_Ref is new CORBA.Value.Base with null record;

   type Object is new CORBA.Value.Impl_Base with record
      Content : Boxed_Access;
   end record;
   type Object_Ptr is access all Object;

end CORBA.Value.Box;
