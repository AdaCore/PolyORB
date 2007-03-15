with Locations; use Locations;
with Types;     use Types;

package Errors is

   Not_Yet_Implemented : exception;
   --  Raised when code is not ready yet

   Internal_Error : exception;
   --  Raised when idlac reaches an internal inconsistent state

   Fatal_Error : exception;
   --  Raised when idlac has detected an external inconsistent state

   type Error_Kind is (K_Error, K_Warning, K_None);

   procedure Display_Error (S : String; K : Error_Kind := K_Error);
   procedure DE (S : String; K : Error_Kind := K_Error) renames Display_Error;
   --  Display an error and output error message S. S may include
   --  meta-characters. '%' designates a string representing
   --  Error_Name (N) where N is the number of '%' and '#' in the
   --  substring. '#' designates a quoted string representing
   --  Error_Name (N). '!' designates a location representing
   --  Error_Loc (L) where L is the number of '!' in the
   --  substring. '$' designates an integer representing Error_Int (I)
   --  where I is the number of '$' in the substring.

   procedure Initialize;

   Error_Loc  : array (1 .. 2) of Location;
   Error_Int  : array (1 .. 2) of Int;
   Error_Name : array (1 .. 2) of Name_Id;

   N_Errors   : Int := 0;
   N_Warnings : Int := 0;

end Errors;
