package body Droopi.Object_Map is

   use Object_Map_Entry_Seqs;

   ---------
   -- Add --
   ---------

   function Add (O_Map : in Object_Map_Access;
                 Obj   : Map_Entry)
                return Integer
   is
      Elts  : constant Element_Array := To_Element_Array (O_Map.Map);
      Index : Integer := Elts'Last + 1;
   begin
      for I in Elts'Range loop
         if Is_Null (Elts (I)) = True then
            Replace_Element (O_Map.Map, 1 + I - Elts'First, Obj);
            Index := I;
            return Index;
         end if;
      end loop;

      if Index > Elts'Last then
         Append (O_Map.Map, Obj);
      end if;

      return Index;
   end Add;

   -------------------
   -- Is_Servant_In --
   -------------------

   function Is_Servant_In (O_Map  : in Object_Map_Access;
                           Item   : Servant)
                          return Boolean
   is
      An_Entry : Map_Entry;
   begin
      An_Entry := Get_By_Servant (O_Map, Item);
      return not Is_Null (An_Entry);
   end Is_Servant_In;

   ---------------------
   -- Is_Object_Id_In --
   ---------------------

   function Is_Object_Id_In (O_Map  : in Object_Map_Access;
                             Item : Object_Id)
                            return Boolean
   is
      An_Entry : Map_Entry;
   begin
      An_Entry := Get_By_Id (O_Map, Item);
      return not Is_Null (An_Entry);
   end Is_Object_Id_In;

   ---------------
   -- Get_By_Id --
   ---------------

   function Get_By_Id (O_Map  : in Object_Map_Access;
                       Item : in Object_Id)
                      return Map_Entry
   is
      An_Entry : Map_Entry;
   begin
      if Length (O_Map.Map) = 0 then
         return Null_Entry;
      end if;
      for I in 1 .. Length (O_Map.Map) loop
         An_Entry := Element_Of (O_Map.Map, I);
         if Is_Null (An_Entry) = False then
            if Is_Object_Id_Equal (An_Entry, Item) = True then
               return An_Entry;
            end if;
         end if;
      end loop;
      return Null_Entry;
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   function Get_By_Servant (O_Map  : in Object_Map_Access;
                            Item : in Servant)
                           return Map_Entry
   is
      An_Entry : Map_Entry;
   begin
      if Length (O_Map.Map) = 0 then
         return Null_Entry;
      end if;
      for I in 1 .. Length (O_Map.Map) loop
         An_Entry := Element_Of (O_Map.Map, I);
         if Is_Null (An_Entry) = False then
            if Is_Servant_Equal (An_Entry, Item) = True then
               return An_Entry;
            end if;
         end if;
      end loop;
      return Null_Entry;
   end Get_By_Servant;

   ------------------
   -- Get_By_Index --
   ------------------

   function Get_By_Index (O_Map : in Object_Map_Access;
                          Index : in Natural)
                         return Map_Entry
   is
      An_Entry : Map_Entry;
   begin
      if Length (O_Map.Map) = 0 then
         return Null_Entry;
      end if;
      An_Entry := Element_Of (O_Map.Map, Index);
      return An_Entry;
   end Get_By_Index;

   ------------
   -- Remove --
   ------------

   function Remove (O_Map  : in Object_Map_Access;
                    Item   : in Object_Id)
                   return Map_Entry
   is
      An_Entry  : Map_Entry;
      To_Remove : Map_Entry := Null_Entry;
      Index     : Natural;
   begin

      if Length (O_Map.Map) = 0 then
         return To_Remove;
      end if;

      for I in 1 .. Length (O_Map.Map) loop
         An_Entry := Element_Of (O_Map.Map, I);
         if Is_Null (An_Entry) = False then
            if Is_Object_Id_Equal (An_Entry, Item) = True then
               To_Remove := An_Entry;
               Index := I;
               exit;
            end if;
         end if;
      end loop;

      if Is_Null (To_Remove) = False then
         Replace_Element (O_Map.Map, Index, Null_Entry);
      end if;

      return To_Remove;
   end Remove;

   ---------------------
   -- Remove_By_Index --
   ---------------------

   function Remove_By_Index (O_Map : in Object_Map_Access;
                             Index : in Natural)
                            return Map_Entry
   is
      To_Remove : Map_Entry;
   begin
      if Length (O_Map.Map) = 0 then
         return Null_Entry;
      end if;

      To_Remove := Element_Of (O_Map.Map, Index);
      if Is_Null (To_Remove) = False then
         Replace_Element (O_Map.Map, Index, Null_Entry);
      end if;
      return To_Remove;
   end Remove_By_Index;

end Droopi.Object_Map;
