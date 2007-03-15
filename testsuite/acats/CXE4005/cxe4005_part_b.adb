
-----------------------------------------------------------------------------

with CXE4005_Part_A1;
with CXE4005_Part_A2;
with CXE4005_Normal;
with CXE4005_Remote_Types;
with Report;
package body CXE4005_Part_B is

  type Not_Available_For_Remote_Call is new CXE4005_Common.Root_Tagged_Type
       with null record;

  Root_Obj : aliased CXE4005_Common.Root_Tagged_Type;
  RT_Obj   : aliased CXE4005_Remote_Types.RT_Tagged_Type;
  Local_Only_Obj : aliased Not_Available_For_Remote_Call;
  Normal_Obj : aliased CXE4005_Normal.Cant_Use_In_Remote_Call; 

  -- provide access to a remote access value
  function Get_RACWT (Which_Type : Type_Selection) 
       return CXE4005_Part_A1.RACWT is
  begin
    case Which_Type is
      when Common_Spec => return Root_Obj'Access;
      when RT_Spec     => return RT_Obj'Access;
      when B_Body      => return Local_Only_Obj'Access;
      when Normal_Spec => return Normal_Obj'Access;
    end case;
  end Get_RACWT;

begin
  CXE4005_Common.Set_Serial_Number (
        Root_Tagged_Type(Root_Obj)'Access          , 301);
  CXE4005_Common.Set_Serial_Number (
        Root_Tagged_Type(RT_Obj)'Access            , 306);
  CXE4005_Common.Set_Serial_Number (
        Root_Tagged_Type(Local_Only_Obj)'Access    , 307);
  CXE4005_Common.Set_Serial_Number (
        Root_Tagged_Type(Normal_Obj)'Access        , 308);
end CXE4005_Part_B;
