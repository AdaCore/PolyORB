----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  ORB-specific matter, Broca version.
--  $Id: //depot/ciao/main/ciao-generator-broca.adb#5 $

with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

with CIAO.Nlists;     use CIAO.Nlists;
with CIAO.IDL_Syntax; use CIAO.IDL_Syntax;
with CIAO.IDL_Tree;   use CIAO.IDL_Tree;
with CIAO.Types;      use CIAO.Types;

package body CIAO.Generator.Broca is

   function Broca_Sequences_Package (N : Node_Id)
     return Wide_String is

      -----------------------
      -- Local subprograms --
      -----------------------

      function Sequence_Base_Package (N : Node_Id)
        return Wide_String;
      --  Return the base name for the sequence type denoted
      --  by N_Type_Dcl node N.

      function Count_Sequences (Root      : Node_Id;
                                Up_To     : Node_Id)
        return Integer;
      --  From Root, count recursively the sequence (<octet>) nodes
      --  up to, but not including, including Up_To. Root shall be a
      --  <module> or <specification> node.

      --------------------------------
      -- Local subprograms (bodies) --
      --------------------------------

      function Sequence_Base_Package (N : Node_Id)
        return Wide_String is
         Buffer : Unbounded_Wide_String
           := To_Unbounded_Wide_String ("IDL_");

         Current_Node : Node_Id := N;
      begin
         while Node_Kind (Current_Node) = N_Sequence_Type loop
            Append (Buffer, "SEQUENCE_");
            Current_Node := Specific_Type_Spec (Current_Node);
            if Node_Kind (Template_Type_Spec (Current_Node))
              = N_Sequence_Type then
               Current_Node := Template_Type_Spec (Current_Node);
            end if;
         end loop;

         --  Current_Node is now the innermost <simple_type_spec>
         --  of the <sequence_type>.

         Current_Node := Node1 (Current_Node);
         -- XXX UGLY abstraction violation.

         case Node_Kind (Current_Node) is
            when N_Scoped_Name                  =>
               Append (Buffer, Get_Name (Current_Node));

               -- XXX The following cases should be tested!
            when N_Base_Type_Boolean            =>
               Append (Buffer, "Boolean");
            when N_Base_Type_Long               =>
               Append (Buffer, "Long");
            when N_Base_Type_Double             =>
               Append (Buffer, "Double");
            when N_Base_Type_Unsigned_Long      =>
               Append (Buffer, "Unsigned_Long_Long");
            when N_Base_Type_Long_Long          =>
               Append (Buffer, "Long_Long");
            when N_Base_Type_Long_Double        =>
               Append (Buffer, "Long_Double");
            when N_Base_Type_Unsigned_Long_Long =>
               Append (Buffer, "Unsigned_Long_Long");
            when N_Base_Type_String             =>
               Append (Buffer, "String");
            when N_Base_Type_Octet              =>
               Append (Buffer, "Octet");

            when others =>
               -- XXX ERROR should not happen
               raise Program_Error;
         end case;

         return To_Wide_String (Buffer);

      end Sequence_Base_Package;

      function Count_Sequences (Root      : Node_Id;
                                Up_To     : Node_Id)
        return Integer is
         Dummy : Boolean;
         Count : Integer := 0;

         procedure Count_Sequences (Root      : Node_Id;
                                    Up_To     : Node_Id;
                                    Count     : in out Integer;
                                    Stop_Here : out Boolean) is
            N : Node_Id;
         begin
            Stop_Here := False;
            case Node_Kind (Root) is
               when
                 N_Module        |
                 N_Specification =>
                  N := First (Definitions (Root));

              Module:
                  while Present (N) loop
                     if True
                       and then Node_Kind (N) = N_Type_Dcl
                       and then Node_Kind (Specific_Type_Spec
                                           (Type_Spec
                                            (Type_Declarator (N))))
                       = N_Simple_Type_Spec then
                        declare
                           TTS_Node : constant Node_Id
                             := Template_Type_Spec
                             (Specific_Type_Spec
                              (Type_Spec
                               (Type_Declarator (N))));
                        begin
                           if TTS_Node = Up_To then
                              Stop_Here := True;
                              return;
                           elsif Node_Kind (TTS_Node) = N_Sequence_Type then
                              Count := Count + 1;
                           end if;
                        end;
                     elsif True
                       and then Node_Kind (N) = N_Type_Dcl
                       and then Node_Kind (Specific_Type_Spec
                                           (Type_Spec
                                            (Type_Declarator (N))))
                       = N_Constr_Type_Spec
                       and then Node_Kind (Structure
                                           (Specific_Type_Spec
                                            (Type_Spec
                                             (Type_Declarator (N)))))
                       in N_Compound_Type then
                       Count_Sequences (Structure
                                        (Specific_Type_Spec
                                         (Type_Spec
                                          (Type_Declarator (N)))),
                                        Up_To, Count, Stop_Here);
                       exit Module when Stop_Here;
                     -- elsif Type_Dcl and Array_Dcl then Count...
                     elsif Node_Kind (N) = N_Module then
                        Count_Sequences (N, Up_To, Count, Stop_Here);
                        exit Module when Stop_Here;
                     end if;

                     N := Next (N);
                  end loop Module;

               when N_Struct_Type =>
                  N := First (Members (Root));

                  while Present (N) loop
                     if Node_Kind (Specific_Type_Spec
                                   (Type_Spec (N)))
                       = N_Simple_Type_Spec then
                        declare
                           TTS_Node : constant Node_Id
                             := Template_Type_Spec
                             (Specific_Type_Spec
                              (Type_Spec (N)));
                        begin
                           if TTS_Node = Up_To then
                              Stop_Here := True;
                              return;
                           elsif Node_Kind (TTS_Node) = N_Sequence_Type then
                              Count := Count + 1;
                           end if;
                        end;
                     end if;

                     N := Next (N);
                  end loop;

               when others =>
                  -- XXX ERROR should not happen
                  raise Program_Error;

            end case;
         end Count_Sequences;

      begin
         Count_Sequences (Root, Up_To, Count, Dummy);
         return Count;
      end Count_Sequences;

      Scope : Node_Id := N;

   begin
      -----------------------------
      -- Broca_Sequences_Package --
      -----------------------------

      pragma Assert (Node_Kind (N) = N_Sequence_Type);

      while Node_Kind (Scope) /= N_Specification loop
         Scope := Parent (Scope);
         pragma Assert (Node_Kind (Scope) /= N_Empty);
      end loop;

      declare
         Base_Name : constant Wide_String
           := Sequence_Base_Package (N);
         Pos : Integer := Count_Sequences (Scope, N);
      begin
         if Pos = 0 then
            return Base_Name;
         else
            declare
               Pos_Image : Wide_String
                 := Natural'Wide_Image (Pos);
            begin
               return Base_Name & "_"
                 & Pos_Image (Pos_Image'First + 1 .. Pos_Image'Last);
            end;
         end if;
      end;
   end Broca_Sequences_Package;

end CIAO.Generator.Broca;
