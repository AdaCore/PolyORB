package body CORBA.Object_Map.Sequence_Map is

   use Active_Object_Map;
   use Droopi.Objects;
   use CORBA.POA_Types;

   ----------------------
   -- Is_Servant_Equal --
   ----------------------

   function Is_Servant_Equal (Item : in Object_Map_Entry_Access;
                              To   : in Droopi.Objects.Servant_Access)
                             return Boolean
   is
   begin
      return (Item.Servant = To);
   end Is_Servant_Equal;

   ------------------------
   -- Is_Object_Id_Equal --
   ------------------------

   function Is_Object_Id_Equal
     (Item : in Object_Map_Entry_Access;
      To   : in CORBA.POA_Types.Unmarshalled_Oid_Access)
     return Boolean
   is
   begin
      return (Item.Oid = To);
   end Is_Object_Id_Equal;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Item : in Object_Map_Entry_Access)
                    return Boolean
   is
   begin
      return (Item = null);
   end Is_Null;

   ----------------
   -- Null_Entry --
   ----------------

   function Null_Entry return Object_Map_Entry_Access
   is
   begin
      return null;
   end Null_Entry;

   -------------
   -- New_Map --
   -------------

   function New_Map return Seq_Object_Map_Access
   is
      Map : Object_Map_Access;
   begin
      Map := new Seq_Object_Map;
      Seq_Object_Map_Access (Map).Map := new Active_Object_Map.Object_Map;
      return Seq_Object_Map_Access (Map);
   end New_Map;

   --------------
   -- Free_Map --
   --------------

   procedure Free_Map (S_Map : in out Seq_Object_Map_Access)
   is
      Map :  Active_Object_Map.Object_Map_Access := S_Map.Map;
   begin
      Free (Map);
      Free (S_Map);
   end Free_Map;

   ---------
   -- Add --
   ---------

   function Add (O_Map : in Object_Map_Access;
                 Obj   : in Object_Map_Entry_Access)
                return Integer
   is
      Map : Seq_Object_Map_Access := Seq_Object_Map_Access (O_Map);
   begin
      return Active_Object_Map.Add (Map.Map, Obj);
   end Add;

   -------------------
   -- Is_Servant_In --
   -------------------

   function Is_Servant_In (O_Map  : in Object_Map_Access;
                           Item   : in Droopi.Objects.Servant_Access)
                          return Boolean
   is
      Map : Seq_Object_Map_Access := Seq_Object_Map_Access (O_Map);
   begin
      return Active_Object_Map.Is_Servant_In (Map.Map, Item);
   end Is_Servant_In;

   ---------------------
   -- Is_Object_Id_In --
   ---------------------

   function Is_Object_Id_In
     (O_Map  : in Object_Map_Access;
      Item   : in CORBA.POA_Types.Unmarshalled_Oid_Access)
     return Boolean
   is
      Map : Seq_Object_Map_Access := Seq_Object_Map_Access (O_Map);
   begin
      return Active_Object_Map.Is_Object_Id_In (Map.Map, Item);
   end Is_Object_Id_In;

   ---------------
   -- Get_By_Id --
   ---------------

   function Get_By_Id (O_Map  : in Object_Map_Access;
                       Item   : in CORBA.POA_Types.Unmarshalled_Oid_Access)
                      return Object_Map_Entry_Access
   is
      Map : Seq_Object_Map_Access := Seq_Object_Map_Access (O_Map);
   begin
      return Active_Object_Map.Get_By_Id (Map.Map, Item);
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   function Get_By_Servant (O_Map  : in Object_Map_Access;
                            Item   : in Droopi.Objects.Servant_Access)
                           return Object_Map_Entry_Access
   is
      Map : Seq_Object_Map_Access := Seq_Object_Map_Access (O_Map);
   begin
      return Active_Object_Map.Get_By_Servant (Map.Map, Item);
   end Get_By_Servant;

   ------------------
   -- Get_By_Index --
   ------------------

   function Get_By_Index (O_Map : in Object_Map_Access;
                          Index : in Integer)
                         return Object_Map_Entry_Access
   is
      Map : Seq_Object_Map_Access := Seq_Object_Map_Access (O_Map);
   begin
      return Active_Object_Map.Get_By_Index (Map.Map, Index);
   end Get_By_Index;

   ------------
   -- Remove --
   ------------

   function Remove (O_Map  : in Object_Map_Access;
                    Item   : in CORBA.POA_Types.Unmarshalled_Oid_Access)
                   return Object_Map_Entry_Access
   is
      Map : Seq_Object_Map_Access := Seq_Object_Map_Access (O_Map);
   begin
      return Active_Object_Map.Remove (Map.Map, Item);
   end Remove;

   ---------------------
   -- Remove_By_Index --
   ---------------------

   function Remove_By_Index (O_Map : in Object_Map_Access;
                             Index : in Integer)
                            return Object_Map_Entry_Access
   is
      Map : Seq_Object_Map_Access := Seq_Object_Map_Access (O_Map);
   begin
      return Active_Object_Map.Remove_By_Index (Map.Map, Index);
   end Remove_By_Index;

end CORBA.Object_Map.Sequence_Map;
