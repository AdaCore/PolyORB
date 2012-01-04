------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . W E B . M I M E                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

package PolyORB.Web.MIME is

   pragma Preelaborate;

   Text_HTML           : aliased constant String := "text/html";
   Text_XML            : aliased constant String := "text/xml";
   Text_Plain          : aliased constant String := "text/plain";

   Image_Gif           : aliased constant String := "image/gif";
   Image_Jpeg          : aliased constant String := "image/jpeg";
   Image_Png           : aliased constant String := "image/png";

   Appl_Postscript     : aliased constant String := "application/postscript";
   Appl_Pdf            : aliased constant String := "application/pdf";
   Appl_Zip            : aliased constant String := "application/zip";
   Appl_Octet_Stream   : aliased constant String := "application/octet-stream";

   Appl_Form_Data      : aliased constant String
     := "application/x-www-form-urlencoded";

   Multipart_Form_Data : aliased constant String := "multipart/form-data";

   Multipart_Mixed_Replace : constant String := "multipart/x-mixed-replace";

   function Content_Type (Filename : String) return String;
   --  Determine the MIME Content Type from the file's type extension.
   --  Returns "application/octet-stream" if the file type is unknown.

end PolyORB.Web.MIME;
