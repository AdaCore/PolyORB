------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       B R O C A . C D R . R E F S                        --
--                                                                          --
--                                 B o d y                                  --
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

with Broca.Exceptions;
with Broca.Refs; use Broca.Refs;
with Broca.Object;

package body Broca.CDR.Refs is

   ----------------
   --  Marshall  --
   ----------------
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Object.Ref'Class) is
   begin
      if CORBA.Object.Is_Nil (Data) then
         Broca.Exceptions.Raise_Marshal;
      end if;
      Marshall (Buffer,
                Broca.Refs.Ref_Type'Class (Data.Ptr.all));
   end Marshall;

   ----------------
   --  Marshall  --
   ----------------
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Impl.Object) is
   begin
      Marshall (Buffer,
                Broca.Refs.Ref_Type'Class (Data));
   end Marshall;

   -----------------
   --  Unmarshall --
   -----------------
   procedure Unmarshall (Buffer : access Buffer_Type;
                         Data : in out CORBA.Object.Ref'Class) is
      Obj : constant CORBA.Impl.Object_Ptr
        := new Broca.Object.Object_Type;
   begin
      Broca.Object.Unmarshall (Buffer, Broca.Object.Object_Type (Obj.all));
      CORBA.Object.Set (Data, Obj);
   end Unmarshall;

end Broca.CDR.Refs;
