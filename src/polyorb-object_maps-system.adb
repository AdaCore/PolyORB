with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.Object_Maps.System is

   use Map_Entry_Seqs;

   use PolyORB.Log;
   use PolyORB.POA_Types;
   use PolyORB.Types;

   package L is new Log.Facility_Log ("polyorb.object_maps.system");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ---------
   -- Add --
   ---------

   function Add
     (O_Map : access System_Object_Map;
      Obj   : in     Object_Map_Entry_Access)
     return Integer is
   begin
      pragma Debug (O ("Add: enter"));

      if Obj.Oid /= null then
         raise Program_Error;
      end if;

      declare
         Elts  : constant Element_Array
           := To_Element_Array (O_Map.System_Map);

      begin
         for J in Elts'Range loop
            if Is_Null (Elts (J)) then
               pragma Debug (O ("Replacing element" & Integer'Image (J)));
               Replace_Element (O_Map.System_Map, 1 + J - Elts'First, Obj);
               return J;
            end if;
         end loop;

         pragma Debug (O ("Appending element"));
         Append (O_Map.System_Map, Obj);

         pragma Debug (O ("Add: leave"));
         return Elts'Last + 1;
      end;
   end Add;

   procedure Add
     (O_Map : access System_Object_Map;
      Obj   : in     Object_Map_Entry_Access;
      Index : in     Integer)
   is
      Elts  : constant Element_Array := To_Element_Array (O_Map.System_Map);

   begin
      pragma Debug (O ("Add: enter"));

      if False
        or else not Obj.Oid.System_Generated
        or else not Is_Null (Element_Of (O_Map.System_Map, Index)) then
         raise Program_Error;
      end if;

      Replace_Element (O_Map.System_Map, 1 + Index - Elts'First, Obj);
      pragma Debug (O ("Add: leave"));
   end Add;

   ---------------
   -- Get_By_Id --
   ---------------

   function Get_By_Id
     (O_Map : in System_Object_Map;
      Item  : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access is
   begin
      pragma Debug (O ("Get_By_Id: enter"));
      pragma Debug (O ("Looking for: " & To_Standard_String (Item.Id)));

      if not Item.System_Generated then
         raise Program_Error;
      end if;

      pragma Debug (O ("System generated OID, directly return element"));
      return Get_Element (O_Map.System_Map,
                          Integer'Value (To_Standard_String (Item.Id)));
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   function Get_By_Servant
     (O_Map  : in System_Object_Map;
      Item   : in PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access
   is
      use type PolyORB.Servants.Servant_Access;

      Elts : constant Element_Array
        := To_Element_Array (O_Map.System_Map);

   begin
      pragma Debug (O ("Get_By_Servant: enter"));

      pragma Debug (O ("Object_Map'Size: " & Integer'Image (Elts'Last)));
      for J in Elts'Range loop
         if not Is_Null (Elts (J)) then
            pragma Debug (O ("Examinating elt: "
                             & To_Standard_String (Elts (J).Oid.Id)));

            if Elts (J).Servant = Item then
               pragma Debug (O ("Found !"));
               return Elts (J);
            end if;
         end if;
      end loop;

      pragma Debug (O ("Not Found !"));
      return null;

   end Get_By_Servant;

   ------------------
   -- Remove_By_Id --
   ------------------

   function Remove_By_Id
     (O_Map : access System_Object_Map;
      Item  : in     PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access
   is
      Old_Entry : Object_Map_Entry_Access;

   begin
      pragma Debug (O ("Remove_By_Id: enter"));
      pragma Debug (O ("Looking for: " & To_Standard_String (Item.Id)));

      if not Item.System_Generated then
         raise Program_Error;
      end if;

      pragma Debug (O ("System generated OID, directly remove element"));

      declare
         Index : constant Integer
           := Integer'Value (To_Standard_String (Item.Id));

      begin
         Old_Entry := Element_Of (O_Map.System_Map, Index);
         Replace_Element (O_Map.System_Map, Index, null);
         return Old_Entry;
      end;

   end Remove_By_Id;

end PolyORB.Object_Maps.System;
