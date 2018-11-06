class ZSAPLINK_USER_PARAMETER definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.

  methods FREE .
  methods CHECK_AUTHORITY
    importing
      !IV_ACTVT type CHAR1
    raising
      ZCX_SAPLINK .
ENDCLASS.



CLASS ZSAPLINK_USER_PARAMETER IMPLEMENTATION.


METHOD checkexists.
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

  " Plugin created by:
  " Nicolas Busson
  " bussonnicolas@gmail.com

  DATA ls_para  TYPE tpara.

  SELECT SINGLE * FROM tpara INTO ls_para WHERE paramid = objname.
  IF sy-subrc = 0.
    exists = 'X'.
  ENDIF.

ENDMETHOD.


METHOD check_authority.
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

  " Plugin created by:
  " Nicolas Busson
  " bussonnicolas@gmail.com

  DATA lv_mode            TYPE char6.
  DATA lv_paramid         TYPE tpara-paramid.
  DATA lv_error           TYPE string.


  lv_paramid = objname.
  CASE iv_actvt.
    WHEN 'C'. " Create user parameter
      lv_mode = 'INSERT'.

    WHEN 'D'. " Delete user parameter
      lv_mode = 'MODIFY'.

    WHEN OTHERS.
      " Not supported
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
  ENDCASE.

  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      global_lock             = abap_true
      object                  = lv_paramid
      object_class            = 'PARA'
      mode                    = lv_mode
      language_upd_exit       = 'RS_PARAMETER_LANGUAGE_EXIT'
      suppress_language_check = space
    EXCEPTIONS
      canceled_in_corr        = 1
      OTHERS                  = 2.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO lv_error.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_error.
  ENDIF.


ENDMETHOD.


METHOD createixmldocfromobject.
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

  " Plugin created by:
  " Nicolas Busson
  " bussonnicolas@gmail.com

  DATA ls_para                TYPE tpara.
  DATA lt_parat               TYPE TABLE OF tparat.
  DATA ls_parat               TYPE tparat.
  DATA lv_error               TYPE string.
  DATA rc                     TYPE sysubrc.                 "#EC NEEDED

  DATA _objtype               TYPE string.
  DATA rootnode               TYPE REF TO if_ixml_element.
  DATA node                   TYPE REF TO if_ixml_element.



  " Read user parameter properties
  SELECT SINGLE * FROM tpara INTO ls_para  WHERE paramid = objname.
  SELECT * FROM tparat INTO TABLE lt_parat WHERE paramid = objname.

  IF ls_para IS INITIAL.
    MESSAGE e061(eu) WITH objname INTO lv_error.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_error.
  ENDIF.

  " Create XML
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = ls_para ).

  LOOP AT lt_parat INTO ls_parat.
    node = xmldoc->create_element( 'parat' ).
    setattributesfromstructure( node = node structure = ls_parat ).
    rc = rootnode->append_child( node ).
  ENDLOOP.

  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.

ENDMETHOD.


METHOD createobjectfromixmldoc.
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

  " Plugin created by:
  " Nicolas Busson
  " bussonnicolas@gmail.com

  DATA ls_para                TYPE tpara.
  DATA ls_parat               TYPE tparat.
  DATA lt_parat               TYPE TABLE OF tparat.
  DATA lv_error               TYPE string.

  DATA _objtype               TYPE string.
  DATA rootnode               TYPE REF TO if_ixml_element.
  DATA node                   TYPE REF TO if_ixml_element.
  DATA filter                 TYPE REF TO if_ixml_node_filter.
  DATA iterator               TYPE REF TO if_ixml_node_iterator.
  DATA checkexists            TYPE flag.


  _objtype = getobjecttype( ).
  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = ls_para.
  objname = ls_para-paramid.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.
      deleteobject( ).
    ENDIF.
  ENDIF.

  filter = xmldoc->create_filter_name( 'parat' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR ls_parat.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_parat.
    APPEND ls_parat TO lt_parat.
    node ?= iterator->get_next( ).
  ENDWHILE.

  " Create user parameter
  check_authority( 'C' ).
  CALL FUNCTION 'RS_CHARACTER_CHECK'
    EXPORTING
      objectname = ls_para-paramid
    EXCEPTIONS
      OTHERS     = 5.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO lv_error.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_error.
  ENDIF.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      global_lock         = abap_true
      object              = ls_para-paramid
      object_class        = 'PARA'
      mode                = 'I'
      master_language     = sy-langu
    EXCEPTIONS
      cancelled           = 01
      permission_failure  = 02
      unknown_objectclass = 03.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO lv_error.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_error.
  ENDIF.

  INSERT tpara  FROM ls_para.
  INSERT tparat FROM TABLE lt_parat.

  CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
    EXPORTING
      object    = ls_para-paramid
      operation = 'INSERT'
      type      = 'CR'.

  free( ).

  " successful install
  name = objname.


ENDMETHOD.


METHOD deleteobject.                                        "#EC NEEDED
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

  " Plugin created by:
  " Nicolas Busson
  " bussonnicolas@gmail.com

  DATA lv_error         TYPE string.
  DATA ls_para          TYPE tpara.


  check_authority( 'D' ).

  SELECT SINGLE * FROM tpara INTO ls_para WHERE paramid = objname.
  IF sy-subrc NE 0.
    MESSAGE e061(eu) WITH objname INTO lv_error.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_error.
  ENDIF.


  DELETE FROM tpara  WHERE paramid = objname.
  DELETE FROM tparat WHERE paramid = objname.
  CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
    EXPORTING
      object    = ls_para-paramid
      operation = 'DELETE'
      type      = 'CR'.

  free( ).

ENDMETHOD.


METHOD free.
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

  " Plugin created by:
  " Nicolas Busson
  " bussonnicolas@gmail.com

  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      mode         = 'FREE'
      object       = objname
      object_class = 'PARA'.

ENDMETHOD.


METHOD getobjecttype.
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

  " Plugin created by:
  " Nicolas Busson
  " bussonnicolas@gmail.com

  objecttype = 'PARA'. " User parameter

ENDMETHOD.
ENDCLASS.
