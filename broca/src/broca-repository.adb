------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     B R O C A . R E P O S I T O R Y                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.11 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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

with CORBA; use CORBA;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Repository is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.repository");
   procedure O is new Broca.Debug.Output (Flag);

   --  Single linked list of all the factories.
   Factories : Factory_Ptr;

   --  Add a new factory to the repository.
   procedure Register (Factory : in Factory_Ptr) is
   begin
      pragma Debug
        (O ("Register new factory " &
            CORBA.To_Standard_String (CORBA.String (Factory.all.Type_Id))));

      Factory.Next := Factories;
      Factories := Factory;
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Type_Id : CORBA.RepositoryId)
     return CORBA.Object.Ref'Class
   is
      Factory   : Factory_Ptr;
      Reference : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Create new object of type " &
                       CORBA.To_Standard_String (CORBA.String (Type_Id))));

      Factory := Factories;
      while Factory /= null loop
         if Factory.Type_Id = Type_Id then
            return Create (Factory);
         end if;
         Factory := Factory.Next;
      end loop;

      --  Return a null object.
      pragma Debug (O ("No factory for this type."));
      CORBA.Object.Set (Reference, null);
      return Reference;

   end Create;

end Broca.Repository;



