------------------------------------------------------------------------------
--                                                                          --
--                          PolyORB COMPONENTS                               --
--                                                                          --
--                        SOAP Representations of Any                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
------------------------------------------------------------------------------





with PolyORB.Any;

package PolyORB.Representations.SOAP.Any is

   procedure Any_To_XML_Components
       (Name     :  Types.Identifier;
        Param    :  PolyORB.Any.Any;
        XML_Comp :  out XML_Component_Access);

   procedure Array_To_XML_Components
      (Param    : PolyORB.Any.Any;
       XML_Comp : out XML_Component_Access);

   procedure Struct_To_XML_Components
     (Name     : Types.Identifier;
      Param    : PolyORB.Any.Any;
      XML_Comp : out XML_Component_Access);



end PolyORB.Representations.SOAP.Any;
