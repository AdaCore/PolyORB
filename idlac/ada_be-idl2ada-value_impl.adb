------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               A D A _ B E . I D L 2 A D A . V A L U E _ I M P L          --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Debug;

with Errors;                use Errors;

with Ada_Be.Idl2Ada.Impl;

package body Ada_Be.Idl2Ada.Value_Impl is

   Flag : constant Natural := Ada_Be.Debug.Is_Active
     ("ada_be.idl2ada.value_impl");
   procedure O is new Ada_Be.Debug.Output (Flag);

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   ---------------------
   --  Gen_Scope_Spec --
   ---------------------
   procedure Gen_Node_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is
         when K_ValueType =>

            NL (CU);
            Put (CU, "type Object is new ");

            --  check parent
            if Parents (Node) = Nil_List then
               Add_With (CU, "CORBA.Value");
               Put (CU, "CORBA.Value.Impl_Base");
            else
               declare
                  First_Parent : Node_Id := Head (Parents (Node));
               begin
                  Add_With (CU, Ada_Full_Name (First_Parent));
                  Put (CU, Ada_Type_Name (First_Parent));
               end;
            end if;

            --  write members
            if Is_Empty (Contents (Node)) then
               PL (CU, " with null record;");
            else
               PL (CU, " with record");
               II (CU);
               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Contents (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Member_Node);
                     if Kind (Member_Node) = K_State_Member then
                        declare
                           Decl_Iterator : Node_Iterator;
                           Decl_Node : Node_Id;
                        begin
                           Init (Decl_Iterator,
                                 State_Declarators (Member_Node));
                           while not Is_End (Decl_Iterator) loop
                              Get_Next_Node (Decl_Iterator, Decl_Node);
                              if Kind (Decl_Node) = K_Declarator then
                                 Gen_Node_Stubs_Spec (CU, Decl_Node);
                                 Put (CU, " : ");
                                 Gen_Node_Stubs_Spec
                                   (CU, State_Type (Member_Node));
                                 PL (CU, ";");
                              end if;
                           end loop;
                        end;
                     end if;
                  end loop;
               end;

               DI (CU);
               PL (CU, "end record;");
               PL (CU, "type Object_Ptr is access all Object'Class;");
            end if;

         when K_Initializer =>
            Gen_Initializer_Profile (CU,
                                     "Object_Ptr",
                                     Node);
            PL (CU, ";");

         when K_Operation =>
            Ada_Be.Idl2Ada.Impl.Gen_Node_Spec (CU, Node);

         when K_State_Member =>
            null;

         when others =>
            Error
              (Node_Kind'Image (Kind (Node))
               & " should not generate code in package .Value_Impl",
               Fatal, Get_Location (Node));
      end case;
   end Gen_Node_Spec;

   ---------------------
   --  Gen_Scope_Body --
   ---------------------
   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Operation =>
            Ada_Be.Idl2Ada.Impl.Gen_Node_Body (CU, Node);

         when others =>
            null;

      end case;
   end Gen_Node_Body;


end Ada_Be.Idl2Ada.Value_Impl;
