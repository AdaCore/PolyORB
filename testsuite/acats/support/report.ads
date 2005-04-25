pragma Style_Checks (Off);

PACKAGE REPORT IS

     SUBTYPE FILE_NUM IS INTEGER RANGE 1..5;

  -- THE REPORT ROUTINES.

     PROCEDURE TEST           -- THIS ROUTINE MUST BE INVOKED AT THE
                              -- START OF A TEST, BEFORE ANY OF THE
                              -- OTHER REPORT ROUTINES ARE INVOKED.
                              -- IT SAVES THE TEST NAME AND OUTPUTS THE
                              -- NAME AND DESCRIPTION.
        ( NAME : STRING;      -- TEST NAME, E.G., "C23001A-AB".
          DESCR : STRING      -- BRIEF DESCRIPTION OF TEST, E.G.,
                              -- "UPPER/LOWER CASE EQUIVALENCE IN " &
                              -- "IDENTIFIERS".
        );

     PROCEDURE FAILED         -- OUTPUT A FAILURE MESSAGE.  SHOULD BE
                              -- INVOKED SEPARATELY TO REPORT THE
                              -- FAILURE OF EACH SUBTEST WITHIN A TEST.
        ( DESCR : STRING      -- BRIEF DESCRIPTION OF WHAT FAILED.
                              -- SHOULD BE PHRASED AS:
                              -- "(FAILED BECAUSE) ...REASON...".
        );

     PROCEDURE NOT_APPLICABLE -- OUTPUT A NOT-APPLICABLE MESSAGE.
                              -- SHOULD BE INVOKED SEPARATELY TO REPORT
                              -- THE NON-APPLICABILITY OF EACH SUBTEST
                              -- WITHIN A TEST.
        ( DESCR : STRING      -- BRIEF DESCRIPTION OF WHAT IS
                              -- NOT-APPLICABLE. SHOULD BE PHRASED AS:
                              -- "(NOT-APPLICABLE BECAUSE)...REASON...".
        );

     PROCEDURE SPECIAL_ACTION -- OUTPUT A MESSAGE DESCRIBING SPECIAL
                              -- ACTIONS TO BE TAKEN.
                              -- SHOULD BE INVOKED SEPARATELY TO GIVE
                              -- EACH SPECIAL ACTION.
        ( DESCR : STRING      -- BRIEF DESCRIPTION OF ACTION TO BE
                              -- TAKEN.
        );

     PROCEDURE COMMENT        -- OUTPUT A COMMENT MESSAGE.
        ( DESCR : STRING      -- THE MESSAGE.
        );

     PROCEDURE RESULT;        -- THIS ROUTINE MUST BE INVOKED AT THE
                              -- END OF A TEST.  IT OUTPUTS A MESSAGE
                              -- INDICATING WHETHER THE TEST AS A
                              -- WHOLE HAS PASSED, FAILED, IS
                              -- NOT-APPLICABLE, OR HAS TENTATIVELY
                              -- PASSED PENDING SPECIAL ACTIONS.

  -- THE DYNAMIC VALUE ROUTINES.

     -- EVEN WITH STATIC ARGUMENTS, THESE FUNCTIONS WILL HAVE DYNAMIC
     -- RESULTS.

     FUNCTION IDENT_INT       -- AN IDENTITY FUNCTION FOR TYPE INTEGER.
        ( X : INTEGER         -- THE ARGUMENT.
        ) RETURN INTEGER;     -- X.

     FUNCTION IDENT_CHAR      -- AN IDENTITY FUNCTION FOR TYPE
                              -- CHARACTER.
        ( X : CHARACTER       -- THE ARGUMENT.
        ) RETURN CHARACTER;   -- X.

     FUNCTION IDENT_WIDE_CHAR -- AN IDENTITY FUNCTION FOR TYPE
                              -- WIDE_CHARACTER.
        ( X : WIDE_CHARACTER  -- THE ARGUMENT.
        ) RETURN WIDE_CHARACTER; -- X.

     FUNCTION IDENT_BOOL      -- AN IDENTITY FUNCTION FOR TYPE BOOLEAN.
        ( X : BOOLEAN         -- THE ARGUMENT.
        ) RETURN BOOLEAN;     -- X.

     FUNCTION IDENT_STR       -- AN IDENTITY FUNCTION FOR TYPE STRING.
        ( X : STRING          -- THE ARGUMENT.
        ) RETURN STRING;      -- X.

     FUNCTION IDENT_WIDE_STR  -- AN IDENTITY FUNCTION FOR TYPE WIDE_STRING.
        ( X : WIDE_STRING     -- THE ARGUMENT.
        ) RETURN WIDE_STRING; -- X.

     FUNCTION EQUAL           -- A RECURSIVE EQUALITY FUNCTION FOR TYPE
                              -- INTEGER.
        ( X, Y : INTEGER      -- THE ARGUMENTS.
        ) RETURN BOOLEAN;     -- X = Y.

-- OTHER UTILITY ROUTINES.

     FUNCTION LEGAL_FILE_NAME -- A FUNCTION TO GENERATE LEGAL EXTERNAL
                              -- FILE NAMES.
        ( X : FILE_NUM := 1;  -- DETERMINES FIRST CHARACTER OF NAME.
          NAM : STRING := ""  -- DETERMINES REST OF NAME.
        ) RETURN STRING;      -- THE GENERATED NAME.

     FUNCTION TIME_STAMP      -- A FUNCTION TO GENERATE THE TIME AND
                              -- DATE TO PLACE IN THE OUTPUT OF AN ACVC
                              -- TEST.
          RETURN STRING;      -- THE TIME AND DATE.

END REPORT;
