*----------------------------------------------------------------------*
*       CLASS lc_Zsaplink_Program_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_zsaplink_program_test DEFINITION FOR TESTING
  " DURATION SHORT
  " RISK LEVEL HARMLESS
  "#AU Duration Medium
  "#AU Risk_Level Harmless
.
*?ï»¿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lc_Zsaplink_Program_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZSAPLINK_PROGRAM
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
* ================
    DATA:
      f_cut TYPE REF TO zsaplink_program.  "class under test

    METHODS: createstringfromobject FOR TESTING.
ENDCLASS.       "lc_Zsaplink_Program_Test


*----------------------------------------------------------------------*
*       CLASS lc_Zsaplink_Program_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_zsaplink_program_test IMPLEMENTATION.
* ==============================================

  METHOD createstringfromobject.
    CONSTANTS: object_name TYPE string VALUE 'SFLIGHT_DATA_GEN'.
    DATA: source_string TYPE string.

    CREATE OBJECT f_cut
      EXPORTING
        name = object_name.

    source_string = f_cut->createstringfromobject( ).
    cl_aunit_assert=>assert_not_initial(
        act = source_string               " Actual Data Object
        msg = 'No source string found'    " Message in Case of Error
    ).
  ENDMETHOD.       "createstringfromobject




ENDCLASS.       "lc_Zsaplink_Program_Test
