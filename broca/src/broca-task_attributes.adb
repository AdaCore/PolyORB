------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                B R O C A . T A S K _ A T T R I B U T E S                 --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Task_Attributes;

package body Broca.Task_Attributes is

   type Task_Attribute is record
      Has_Context    : Boolean := False;
      Current_Object : PortableServer.ObjectId;
      Current_POA    : PortableServer.POA_Forward.Ref;
   end record;

   Nil_Attribute : Task_Attribute;

   package Attributes is new Ada.Task_Attributes
     (Attribute => Task_Attribute,
      Initial_Value => Nil_Attribute);

   function Has_Context return Boolean is
   begin
      return Attributes.Value.Has_Context;
   end Has_Context;

   function Current_Object return PortableServer.ObjectId is
   begin
      pragma Assert (Has_Context);
      return Attributes.Value.Current_Object;
   end Current_Object;

   function Current_POA return PortableServer.POA_Forward.Ref is
   begin
      pragma Assert (Has_Context);
      pragma Assert (False);
      --  FIXME:
      --  Current_POA is not currently supported in AdaBroker.
      --  Rationale: Cannot use a Ref as a task attribute,
      --  because Set on a task attribute is protected by
      --  a mutex, and assigning a ref causes a protected
      --  entry call which may cause a priority ceiling
      --  violation. This may or may not be a bug in GNAT 3.13p.
      return Attributes.Value.Current_POA;
   end Current_POA;

   procedure Set_Current_Object (Val : PortableServer.ObjectId) is
      Current_Attributes : Task_Attribute := Attributes.Value;
   begin
      Current_Attributes.Current_Object := Val;
      Attributes.Set_Value (Current_Attributes);
   end Set_Current_Object;

   procedure Set_Current_POA (Val : PortableServer.POA_Forward.Ref) is
      Current_Attributes : Task_Attribute := Attributes.Value;
   begin
      pragma Assert (False);
      --  Not supported (see above).
      Current_Attributes.Current_POA := Val;
      Attributes.Set_Value (Current_Attributes);
   end Set_Current_POA;

   procedure Set_Has_Context (Context : Boolean := True) is
      Current_Attributes : Task_Attribute := Attributes.Value;
   begin
      Current_Attributes.Has_Context := True;
      Attributes.Set_Value (Current_Attributes);
   end Set_Has_Context;

end Broca.Task_Attributes;
