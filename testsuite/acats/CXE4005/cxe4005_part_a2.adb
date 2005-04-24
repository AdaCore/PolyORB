
-----------------------------------------------------------------------------

with Report;
with CXE4005_Part_B;
with CXE4005_Normal;
with CXE4005_Remote_Types;
package body CXE4005_Part_A2 is
  Root_Obj : aliased CXE4005_Common.Root_Tagged_Type;
  RT_Obj   : aliased CXE4005_Remote_Types.RT_Tagged_Type;
  Normal_Obj : aliased CXE4005_Normal.Cant_Use_In_Remote_Call;

  function Get_RACWT (Which_Type : Type_Selection) 
       return CXE4005_Part_A1.RACWT is
  begin
    case Which_Type is
      when Common_Spec => return Root_Obj'Access;
      when RT_Spec     => return RT_Obj'Access;
      when B_Body      => return null;
      when Normal_Spec => return Normal_Obj'Access;
    end case;
  end Get_RACWT;

begin
  Set_Serial_Number (Root_Tagged_Type(Root_Obj)'Access   , 201);
  Set_Serial_Number (Root_Tagged_Type(RT_Obj)'Access     , 206);
  -- no 207 object
  Set_Serial_Number (Root_Tagged_Type(Normal_Obj)'Access , 208);
end CXE4005_Part_A2;
