--  AUTOMATICALLY GENERATED, DO NOT EDIT!

pragma Style_Checks ("NM32766");
private package PolyORB.GIOP_P.Code_Sets.Data is

   Info : constant array (Positive range <>) of Code_Set_Info_Record :=
     ((16#00010001#,  1,  1),  --  ISO 8859-1:1987; Latin Alphabet No. 1
      (16#00010002#,  2,  2),  --  ISO 8859-2:1987; Latin Alphabet No. 2
      (16#00010003#,  3,  3),  --  ISO 8859-3:1988; Latin Alphabet No. 3
      (16#00010004#,  4,  4),  --  ISO 8859-4:1988; Latin Alphabet No. 4
      (16#00010005#,  5,  5),  --  ISO/IEC 8859-5:1988; Latin-Cyrillic Alphabet
      (16#00010006#,  6,  6),  --  ISO 8859-6:1987; Latin-Arabic Alphabet
      (16#00010007#,  7,  7),  --  ISO 8859-7:1987; Latin-Greek Alphabet
      (16#00010008#,  8,  8),  --  ISO 8859-8:1988; Latin-Hebrew Alphabet
      (16#00010009#,  9,  9),  --  ISO/IEC 8859-9:1989; Latin Alphabet No. 5
      (16#0001000A#, 10, 10),  --  ISO/IEC 8859-10:1992; Latin Alphabet No. 6
      (16#0001000F#,  1,  1),  --  ISO/IEC 8859-15:1999; Latin Alphabet No. 9
      (16#00010020#, 11, 11),  --  ISO 646:1991 IRV (International Reference Version)
      (16#00010100#, 12, 12),  --  ISO/IEC 10646-1:1993; UCS-2, Level 1
      (16#00010101#, 12, 12),  --  ISO/IEC 10646-1:1993; UCS-2, Level 2
      (16#00010102#, 12, 12),  --  ISO/IEC 10646-1:1993; UCS-2, Level 3
      (16#00010104#, 12, 12),  --  ISO/IEC 10646-1:1993; UCS-4, Level 1
      (16#00010105#, 12, 12),  --  ISO/IEC 10646-1:1993; UCS-4, Level 2
      (16#00010106#, 12, 12),  --  ISO/IEC 10646-1:1993; UCS-4, Level 3
      (16#00010108#, 12, 12),  --  ISO/IEC 10646-1:1993; UTF-1, UCS Transformation Format 1
      (16#00010109#, 12, 12),  --  ISO/IEC 10646-1:1993; UTF-16, UCS Transformation Format 16-bit form
      (16#00030001#, 13, 13),  --  JIS X0201:1976; Japanese phonetic characters
      (16#00030004#, 14, 14),  --  JIS X0208:1978 Japanese Kanji Graphic Characters
      (16#00030005#, 14, 14),  --  JIS X0208:1983 Japanese Kanji Graphic Characters
      (16#00030006#, 14, 14),  --  JIS X0208:1990 Japanese Kanji Graphic Characters
      (16#0003000A#, 15, 15),  --  JIS X0212:1990; Supplementary Japanese Kanji Graphic Chars
      (16#00030010#, 16, 19),  --  JIS eucJP:1993; Japanese EUC
      (16#00040001#, 20, 20),  --  KS C5601:1987; Korean Hangul and Hanja Graphic Characters
      (16#00040002#, 21, 21),  --  KS C5657:1991; Supplementary Korean Graphic Characters
      (16#0004000A#, 22, 24),  --  KS eucKR:1991; Korean EUC
      (16#00050001#, 25, 25),  --  CNS 11643:1986; Taiwanese Hanzi Graphic Characters
      (16#00050002#, 26, 26),  --  CNS 11643:1992; Taiwanese Extended Hanzi Graphic Chars
      (16#0005000A#, 27, 28),  --  CNS eucTW:1991; Taiwanese EUC
      (16#00050010#, 29, 30),  --  CNS eucTW:1993; Taiwanese EUC
      (16#000B0001#, 31, 31),  --  TIS 620-2529, Thai characters
      (16#000D0001#, 25, 25),  --  TTB CCDC:1984; Chinese Code for Data Communications
      (16#05000010#, 32, 34),  --  OSF Japanese UJIS
      (16#05000011#, 32, 34),  --  OSF Japanese SJIS-1
      (16#05000012#, 32, 34),  --  OSF Japanese SJIS-2
      (16#05010001#, 12, 12),  --  X/Open UTF-8; UCS Transformation Format 8 (UTF-8)
      (16#05020001#, 35, 38),  --  JVC_eucJP
      (16#05020002#, 32, 34),  --  JVC_SJIS
      (16#10000001#, 16, 18),  --  DEC Kanji
      (16#10000002#, 16, 19),  --  Super DEC Kanji
      (16#10000003#, 16, 18),  --  DEC Shift JIS
      (16#10010001#,  1,  1),  --  HP roman8; English and Western European languages
      (16#10010002#, 13, 13),  --  HP kana8; Japanese katakana (incl JIS X0201:1976)
      (16#10010003#,  6,  6),  --  HP arabic8; Arabic
      (16#10010004#,  7,  7),  --  HP greek8; Greek
      (16#10010005#,  8,  8),  --  HP hebrew8; Hebrew
      (16#10010006#, 39, 40),  --  HP turkish8; Turkish
      (16#10010007#, 41, 42),  --  HP15CN; encoding method for Simplified Chinese
      (16#10010008#, 27, 28),  --  HP big5; encoding method for Traditional Chinese
      (16#10010009#, 32, 34),  --  HP japanese15 (sjis); Shift-JIS for mainframe (incl JIS X0208:1990)
      (16#1001000A#, 32, 34),  --  HP sjishi; Shift-JIS for HP user (incl JIS X0208:1990)
      (16#1001000B#, 32, 34),  --  HP sjispc; Shift-JIS for PC (incl JIS X0208:1990)
      (16#1001000C#, 32, 34),  --  HP ujis; EUC (incl JIS X0208:1990)
      (16#10020025#,  1,  1),  --  IBM-037 (CCSID 00037); CECP for USA, Canada, NL, Ptgl, Brazil, Australia, NZ
      (16#10020111#,  1,  1),  --  IBM-273 (CCSID 00273); CECP for Austria, Germany
      (16#10020115#,  1,  1),  --  IBM-277 (CCSID 00277); CECP for Denmark, Norway
      (16#10020116#,  1,  1),  --  IBM-278 (CCSID 00278); CECP for Finland, Sweden
      (16#10020118#,  1,  1),  --  IBM-280 (CCSID 00280); CECP for Italy
      (16#1002011A#,  1,  1),  --  IBM-282 (CCSID 00282); CECP for Portugal
      (16#1002011C#,  1,  1),  --  IBM-284 (CCSID 00284); CECP for Spain, Latin America (Spanish)
      (16#1002011D#,  1,  1),  --  IBM-285 (CCSID 00285); CECP for United Kingdom
      (16#10020122#, 13, 13),  --  IBM-290 (CCSID 00290); Japanese Katakana Host Ext SBCS
      (16#10020129#,  1,  1),  --  IBM-297 (CCSID 00297); CECP for France
      (16#1002012C#, 14, 14),  --  IBM-300 (CCSID 00300); Japanese Host DBCS incl 4370 UDC
      (16#1002012D#, 14, 14),  --  IBM-301 (CCSID 00301); Japanese PC Data DBCS incl 1880 UDC
      (16#100201A4#,  6,  6),  --  IBM-420 (CCSID 00420); Arabic (presentation shapes)
      (16#100201A8#,  8,  8),  --  IBM-424 (CCSID 00424); Hebrew
      (16#100201B5#,  1,  1),  --  IBM-437 (CCSID 00437); PC USA
      (16#100201F4#,  1,  1),  --  IBM-500 (CCSID 00500); CECP for Belgium, Switzerland
      (16#10020341#, 11, 11),  --  IBM-833 (CCSID 00833); Korean Host Extended SBCS
      (16#10020342#, 20, 20),  --  IBM-834 (CCSID 00834); Korean Host DBCS incl 1227 UDC
      (16#10020343#, 25, 25),  --  IBM-835 (CCSID 00835); T-Ch Host DBCS incl 6204 UDC
      (16#10020344#, 11, 11),  --  IBM-836 (CCSID 00836); S-Ch Host Extended SBCS
      (16#10020345#, 42, 42),  --  IBM-837 (CCSID 00837); S-Ch Host DBCS incl 1880 UDC
      (16#10020346#, 31, 31),  --  IBM-838 (CCSID 00838); Thai Host Extended SBCS
      (16#10020347#, 31, 31),  --  IBM-839 (CCSID 00839); Thai Host DBCS incl 374 UDC
      (16#10020352#,  1,  1),  --  IBM-850 (CCSID 00850); Multilingual IBM PC Data-MLP 222
      (16#10020354#,  2,  2),  --  IBM-852 (CCSID 00852); Multilingual Latin-2
      (16#10020357#,  5,  5),  --  IBM-855 (CCSID 00855); Cyrillic PC Data
      (16#10020358#,  8,  8),  --  IBM-856 (CCSID 00856); Hebrew PC Data (extensions)
      (16#10020359#,  9,  9),  --  IBM-857 (CCSID 00857); Turkish Latin-5 PC Data
      (16#1002035D#,  1,  1),  --  IBM-861 (CCSID 00861); PC Data Iceland
      (16#1002035E#,  8,  8),  --  IBM-862 (CCSID 00862); PC Data Hebrew
      (16#1002035F#,  1,  1),  --  IBM-863 (CCSID 00863); PC Data Canadian French
      (16#10020360#,  6,  6),  --  IBM-864 (CCSID 00864); Arabic PC Data
      (16#10020362#,  5,  5),  --  IBM-866 (CCSID 00866); PC Data Cyrillic 2
      (16#10020364#,  6,  6),  --  IBM-868 (CCSID 00868); Urdu PC Data
      (16#10020365#,  7,  7),  --  IBM-869 (CCSID 00869); Greek PC Data
      (16#10020366#,  2,  2),  --  IBM-870 (CCSID 00870); Multilingual Latin-2 EBCDIC
      (16#10020367#,  1,  1),  --  IBM-871 (CCSID 00871); CECP for Iceland
      (16#1002036A#, 31, 31),  --  IBM-874 (CCSID 00874); Thai PC Display Extended SBCS
      (16#1002036B#,  7,  7),  --  IBM-875 (CCSID 00875); Greek
      (16#10020370#,  5,  5),  --  IBM-880 (CCSID 00880); Multilingual Cyrillic
      (16#1002037B#, 11, 11),  --  IBM-891 (CCSID 00891); Korean PC Data SBCS
      (16#10020380#, 13, 13),  --  IBM-896 (CCSID 00896); Japanese Katakana characters; superset of JIS X0201:1976
      (16#10020381#, 13, 13),  --  IBM-897 (CCSID 00897); PC Data Japanese SBCS (use with CP 00301)
      (16#10020387#, 11, 11),  --  IBM-903 (CCSID 00903); PC Data Simplified Chinese SBCS (use with  DBCS)
      (16#10020388#, 11, 11),  --  IBM-904 (CCSID 00904); PC Data Traditional Chinese SBCS (use with  DBCS)
      (16#10020396#,  6,  6),  --  IBM-918 (CCSID 00918); Urdu
      (16#10020399#, 10, 10),  --  IBM-921 (CCSID 00921); Baltic 8-Bit
      (16#1002039A#, 10, 10),  --  IBM-922 (CCSID 00922); Estonia 8-Bit
      (16#1002039E#, 20, 20),  --  IBM-926 (CCSID 00926); Korean PC Data DBCS incl 1880 UDC
      (16#1002039F#, 25, 25),  --  IBM-927 (CCSID 00927); T-Ch PC Data DBCS incl 6204 UDC
      (16#100203A0#, 42, 42),  --  IBM-928 (CCSID 00928); S-Ch PC Data DBCS incl 1880 UDC
      (16#100203A1#, 31, 31),  --  IBM-929 (CCSID 00929); Thai PC Data DBCS incl 374 UDC
      (16#100203A2#, 13, 14),  --  IBM-930 (CCSID 00930); Kat-Kanji Host MBCS Ext-SBCS
      (16#100203A4#, 13, 14),  --  IBM-932 (CCSID 00932); Japanese PC Data Mixed
      (16#100203A5#, 43, 44),  --  IBM-933 (CCSID 00933); Korean Host Extended SBCS
      (16#100203A6#, 43, 44),  --  IBM-934 (CCSID 00934); Korean PC Data Mixed
      (16#100203A7#, 41, 42),  --  IBM-935 (CCSID 00935); S-Ch Host Mixed
      (16#100203A8#, 41, 42),  --  IBM-936 (CCSID 00936); PC Data S-Ch MBCS
      (16#100203A9#, 27, 28),  --  IBM-937 (CCSID 00937); T-Ch Host Mixed
      (16#100203AA#, 27, 28),  --  IBM-938 (CCSID 00938); PC Data T-Ch MBCS
      (16#100203AB#, 13, 14),  --  IBM-939 (CCSID 00939); Latin-Kanji Host MBCS
      (16#100203AD#, 14, 14),  --  IBM-941 (CCSID 00941); Japanese PC DBCS for Open
      (16#100203AE#, 13, 14),  --  IBM-942 (CCSID 00942); Japanese PC Data Mixed
      (16#100203AF#, 13, 14),  --  IBM-943 (CCSID 00943); Japanese PC MBCS for Open
      (16#100203B2#, 41, 42),  --  IBM-946 (CCSID 00946); S-Ch PC Data Mixed
      (16#100203B3#, 25, 25),  --  IBM-947 (CCSID 00947); T-Ch PC Data DBCS incl 6204 UDC
      (16#100203B4#, 27, 28),  --  IBM-948 (CCSID 00948); T-Ch PC Data Mixed
      (16#100203B5#, 43, 44),  --  IBM-949 (CCSID 00949); IBM KS PC Data Mixed
      (16#100203B6#, 27, 28),  --  IBM-950 (CCSID 00950); T-Ch PC Data Mixed
      (16#100203B7#, 20, 20),  --  IBM-951 (CCSID 00951); IBM KS PC Data DBCS incl 1880 UDC
      (16#100203BB#, 14, 14),  --  IBM-955 (CCSID 00955); Japan Kanji characters; superset of JIS X0208:1978
      (16#100203C4#, 27, 28),  --  IBM-964 (CCSID 00964); T-Chinese EUC CNS1163 plane 1,2
      (16#100203CA#, 22, 24),  --  IBM-970 (CCSID 00970); Korean EUC
      (16#100203EE#,  6,  6),  --  IBM-1006 (CCSID 01006); Urdu 8-bit
      (16#10020401#,  5,  5),  --  IBM-1025 (CCSID 01025); Cyrillic Multilingual
      (16#10020402#,  9,  9),  --  IBM-1026 (CCSID 01026); Turkish Latin-5
      (16#10020403#, 13, 13),  --  IBM-1027 (CCSID 01027); Japanese Latin Host Ext SBCS
      (16#10020410#, 11, 11),  --  IBM-1040 (CCSID 01040); Korean PC Data Extended SBCS
      (16#10020411#, 13, 13),  --  IBM-1041 (CCSID 01041); Japanese PC Data Extended SBCS
      (16#10020413#, 11, 11),  --  IBM-1043 (CCSID 01043); T-Ch PC Data Extended SBCS
      (16#10020416#,  6,  6),  --  IBM-1046 (CCSID 01046); Arabic PC Data
      (16#10020417#,  1,  1),  --  IBM-1047 (CCSID 01047); Latin-1 Open System
      (16#10020440#, 11, 11),  --  IBM-1088 (CCSID 01088); IBM KS Code PC Data SBCS
      (16#10020449#,  6,  6),  --  IBM-1097 (CCSID 01097); Farsi
      (16#1002044A#,  6,  6),  --  IBM-1098 (CCSID 01098); Farsi PC Data
      (16#10020458#, 10, 10),  --  IBM-1112 (CCSID 01112); Baltic Multilingual
      (16#1002045A#, 11, 11),  --  IBM-1114 (CCSID 01114); T-Ch PC Data SBCS (IBM BIG-5)
      (16#1002045B#, 11, 11),  --  IBM-1115 (CCSID 01115); S-Ch PC Data SBCS (IBM GB)
      (16#10020462#, 10, 10),  --  IBM-1122 (CCSID 01122); Estonia
      (16#100204E2#,  2,  2),  --  IBM-1250 (CCSID 01250); MS Windows Latin-2
      (16#100204E3#,  5,  5),  --  IBM-1251 (CCSID 01251); MS Windows Cyrillic
      (16#100204E4#,  1,  1),  --  IBM-1252 (CCSID 01252); MS Windows Latin-1
      (16#100204E5#,  7,  7),  --  IBM-1253 (CCSID 01253); MS Windows Greek
      (16#100204E6#,  9,  9),  --  IBM-1254 (CCSID 01254); MS Windows Turkey
      (16#100204E7#,  8,  8),  --  IBM-1255 (CCSID 01255); MS Windows Hebrew
      (16#100204E8#,  6,  6),  --  IBM-1256 (CCSID 01256); MS Windows Arabic
      (16#100204E9#, 10, 10),  --  IBM-1257 (CCSID 01257); MS Windows Baltic
      (16#10020564#, 42, 42),  --  IBM-1380 (CCSID 01380); S-Ch PC Data DBCS incl 1880 UDC
      (16#10020565#, 41, 42),  --  IBM-1381 (CCSID 01381); S-Ch PC Data Mixed incl 1880 UDC
      (16#10020567#, 41, 42),  --  IBM-1383 (CCSID 01383); S-Ch EUC GB 2312-80 set (1382)
      (16#1002112C#, 14, 14),  --  IBM-300 (CCSID 04396); Japanese Host DBCS incl 1880 UDC
      (16#10021352#,  1,  1),  --  IBM-850 (CCSID 04946); Multilingual IBM PC Data-190
      (16#10021354#,  2,  2),  --  IBM-852 (CCSID 04948); Latin-2 Personal Computer
      (16#10021357#,  5,  5),  --  IBM-855 (CCSID 04951); Cyrillic Personal Computer
      (16#10021358#,  8,  8),  --  IBM-856 (CCSID 04952); Hebrew PC Data
      (16#10021359#,  9,  9),  --  IBM-857 (CCSID 04953); Turkish Latin-5 PC Data
      (16#10021360#,  6,  6),  --  IBM-864 (CCSID 04960); Arabic PC Data (all shapes)
      (16#10021364#,  6,  6),  --  IBM-868 (CCSID 04964); PC Data for Urdu
      (16#10021365#,  7,  7),  --  IBM-869 (CCSID 04965); Greek PC Data
      (16#100213A2#, 13, 14),  --  IBM-5026 (CCSID 05026); Japanese Katakana-Kanji Host Mixed
      (16#100213A7#, 41, 42),  --  IBM-5031 (CCSID 05031); S-Ch Host MBCS
      (16#100213AB#, 13, 14),  --  IBM-1027 and -300 (CCSID 05035); Japanese Latin-Kanji Host Mixed
      (16#100213B8#, 14, 14),  --  IBM-5048 (CCSID 05048); Japanese Kanji characters; superset of JIS X0208:1990 (and 1983)
      (16#100213B9#, 15, 15),  --  IBM-5049 (CCSID 05049); Japanese Kanji characters; superset of JIS X0212:1990
      (16#100213CB#, 20, 20),  --  IBM-5067 (CCSID 05067); Korean Hangul and Hanja; superset of KS C5601:1987
      (16#100221A4#,  6,  6),  --  IBM-420 (CCSID 08612); Arabic (base shapes only)
      (16#10022341#, 11, 11),  --  IBM-833 (CCSID 09025); Korean Host SBCS
      (16#10022342#, 20, 20),  --  IBM-834 (CCSID 09026); Korean Host DBCS incl 1880 UDC
      (16#10022346#, 31, 31),  --  IBM-838 (CCSID 09030); Thai Host Extended SBCS
      (16#10022360#,  6,  6),  --  IBM-864 (CCSID 09056); Arabic PC Data (unshaped)
      (16#1002236A#, 31, 31),  --  IBM-874 (CCSID 09066); Thai PC Display Extended SBCS
      (16#100223A5#, 43, 44),  --  IBM-9125 (CCSID 09125); Korean Host Mixed incl 1880 UDC
      (16#10026352#,  1,  1),  --  IBM-850 (CCSID 25426); Multilingual IBM PC Display-MLP
      (16#10026358#,  8,  8),  --  IBM-856 (CCSID 25432); Hebrew PC Display (extensions)
      (16#10026412#, 11, 11),  --  IBM-1042 (CCSID 25618); S-Ch PC Display Ext SBCS
      (16#10027025#, 11, 11),  --  IBM-037 (CCSID 28709); T-Ch Host Extended SBCS
      (16#10028358#,  8,  8),  --  IBM-856 (CCSID 33624); Hebrew PC Display
      (16#100283BA#, 13, 15),  --  IBM33722 (CCSID 33722); Japanese EUC JISx201,208,212
      (16#10030001#, 32, 34),  --  HTCsjis; Hitachi SJIS 90-1
      (16#10030002#, 32, 34),  --  HTCujis; Hitachi eucJP 90-1
      (16#10040001#, 32, 34),  --  Fujitsu U90; Japanese EUC
      (16#10040002#, 32, 34),  --  Fujitsu S90; Japanese EUC
      (16#10040003#, 32, 34),  --  Fujitsu R90; Fujitsu Shift JIS
      (16#10040004#, 45, 46),  --  EBCDIC(ASCII) and JEF; Japanese encoding method for mainframe
      (16#10040005#, 32, 34),  --  EBCDIC(Katakana) and JEF; Japanese encoding method for mainframe
      (16#10040006#, 45, 46)); --  EBCDIC(Japanese English) and JEF; Japanese encoding method for mainframe

   Character_Sets : constant Character_Set_Id_Array :=
     (16#0011#, 16#0012#, 16#0013#, 16#0014#, 16#0015#, 16#0016#, 16#0017#,
      16#0018#, 16#0019#, 16#001A#, 16#0001#, 16#1000#, 16#0080#, 16#0081#,
      16#0082#, 16#0011#, 16#0080#, 16#0081#, 16#0082#, 16#0100#, 16#0101#,
      16#0011#, 16#0100#, 16#0101#, 16#0180#, 16#0181#, 16#0001#, 16#0180#,
      16#0001#, 16#0181#, 16#0200#, 16#0001#, 16#0080#, 16#0081#, 16#0001#,
      16#0080#, 16#0081#, 16#0082#, 16#0013#, 16#0019#, 16#0001#, 16#0300#,
      16#0001#, 16#0100#, 16#0001#, 16#0081#);

end PolyORB.GIOP_P.Code_Sets.Data;