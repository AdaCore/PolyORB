------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                        SOAP Representations of Any                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
------------------------------------------------------------------------------





with Droopi.Any;

package Droopi.Representations.SOAP.Any is

   procedure Any_To_XML_Components
       (Name     :  Types.Identifier;
        Param    :  Droopi.Any.Any;
        XML_Comp :  out XML_Component_Access);

   procedure Array_To_XML_Components
      (Param    : Droopi.Any.Any;
       XML_Comp : out XML_Component_Access);

   procedure Struct_To_XML_Components
     (Name     : Types.Identifier;
      Param    : Droopi.Any.Any;
      XML_Comp : out XML_Component_Access);



end Droopi.Representations.SOAP.Any;
