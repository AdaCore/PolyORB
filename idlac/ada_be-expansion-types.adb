with Idl_Fe.Types; use Idl_Fe.Types;

package body Ada_Be.Expansion.Types is


   function Get_Be_Kind (Node : Ben_Node_List) return Be_Node_Kind is
   begin
      return Bek_Node_List;
   end Get_Be_Kind;

   function Get_Be_Kind (Node : Ben_Idl_File) return Be_Node_Kind is
   begin
      return Bek_Idl_File;
   end Get_Be_Kind;



   -----------------------
   --  New_Ben_Idl_File --
   -----------------------

   function New_Ben_Idl_File (Filename : String_Ptr) return Ben_Idl_File_Acc is
      Result : Ben_Idl_File_Acc;
   begin
      Result := new Ben_Idl_File;
      Set_Old (Result.all,
               Idl_Fe.Types.Nil_Node);
      Result.Name := new String' (Filename.all
                                  & "_IDL_File");
      return Result;
   end New_Ben_Idl_File;

end Ada_Be.Expansion.Types;
