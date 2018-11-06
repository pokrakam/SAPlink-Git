*"* use this source file for your ABAP unit test classes
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

CLASS ltcl_check_enh_implementation DEFINITION FINAL FOR TESTING.
  " DURATION SHORT
  " RISK LEVEL HARMLESS
  "#AU Duration Medium
  "#AU Risk_Level Harmless
  PUBLIC SECTION.
    METHODS:
      check_010_non_existing   FOR TESTING RAISING cx_static_check,
      check_020_create_slinkee FOR TESTING RAISING cx_static_check.
  PRIVATE SECTION.
    DATA targetobject TYPE REF TO zsaplink.
    DATA ixml         TYPE REF TO if_ixml_document.
    DATA cx           TYPE REF TO cx_root.
    DATA msg  TYPE string.
    DATA name TYPE string.
    DATA devclass TYPE devclass.
    METHODS setup.
ENDCLASS.                    "ltcl_check_enh_implementation DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_check_enh_implementation IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_check_enh_implementation IMPLEMENTATION.

  METHOD setup.
  ENDMETHOD.                    "setup

  METHOD check_010_non_existing.
    name = 'ZDOES_NOT_EXIST'.
    CREATE OBJECT targetobject
      TYPE zsaplink_enh_implementation
      EXPORTING
        name = name.
    TRY.
        ixml = targetobject->createixmldocfromobject( ).
      CATCH zcx_saplink INTO cx.    " SAPlink exception class
        msg = cx->get_text( ).
        cl_aunit_assert=>assert_bound( act = cx msg = msg ).
    ENDTRY.

  ENDMETHOD.                    "CHECK_CHECK_NON_EXISTING

  METHOD check_020_create_slinkee.
    DATA: xmlstring TYPE string.
    name     = 'ZDUMMY_BADI'.
    devclass = '$TMP'.
    CREATE OBJECT targetobject
      TYPE zsaplink_enh_implementation
      EXPORTING
        name = name.
    TRY.
        ixml = targetobject->createixmldocfromobject( ).
      CATCH zcx_saplink INTO cx.    " SAPlink exception class
        msg = cx->get_text( ).
        cl_aunit_assert=>fail( msg = msg ).
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = ixml msg = 'iXML is not bound' ).

    xmlstring = targetobject->convertixmldoctostring( ixmldocument = ixml ).

    FREE: ixml, cx.

    ixml = targetobject->convertstringtoixmldoc( xmlstring = xmlstring ).

    " Test without overwrite
    TRY.

        targetobject->createobjectfromixmldoc(
          EXPORTING
            ixmldocument = ixml    " IF_IXML_DOCUMENT
            devclass     = devclass    " Development class/package
*         overwrite    = overwrite    " Overwrite original objects
*       RECEIVING
*         name         = name    " Installed object name
        ).
      CATCH zcx_saplink INTO cx.    " SAPlink exception class
        msg = cx->get_text( ).
        cl_aunit_assert=>assert_bound( act = cx msg = msg ).
    ENDTRY.
    " Test with overwrite
    TRY.
        targetobject->createobjectfromixmldoc(
          EXPORTING
            ixmldocument = ixml                 " IF_IXML_DOCUMENT
            devclass     = devclass    " Development class/package
            overwrite    = 'X'                  " Overwrite original objects
*       RECEIVING
*         name         = name    " Installed object name
        ).
      CATCH zcx_saplink INTO cx.    " SAPlink exception class
        msg = cx->get_text( ).
        cl_aunit_assert=>fail( msg = msg ).
    ENDTRY.
  ENDMETHOD.                    "check_030_xml_string

ENDCLASS.                    "ltcl_check_enh_implementation IMPLEMENTATION
