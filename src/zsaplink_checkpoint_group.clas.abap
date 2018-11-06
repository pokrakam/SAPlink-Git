class ZSAPLINK_CHECKPOINT_GROUP definition
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

  methods CHECK_AUTHORITY
    importing
      !IV_ACTVT type CHAR1
    raising
      ZCX_SAPLINK .
  methods CREATE
    importing
      !IS_PROP type AAB_ID_PROPT
    raising
      ZCX_SAPLINK .
  methods DEQUEUE .
  methods ENQUEUE
    raising
      ZCX_SAPLINK .
  methods GET_REF
    returning
      value(RR_ACID) type ref to CL_AAB_ID .
ENDCLASS.



CLASS ZSAPLINK_CHECKPOINT_GROUP IMPLEMENTATION.


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

  DATA lr_acid                 TYPE REF TO cl_aab_id.
  DATA lv_state                TYPE flag.


  lr_acid = get_ref( ).
  CALL METHOD lr_acid->get_state
    IMPORTING
      ex_state = lv_state.

  IF NOT lv_state IS INITIAL.
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

  DATA lr_acid                 TYPE REF TO cl_aab_id.


  CASE iv_actvt.
    WHEN 'C'. " Create checkpoint group
      IF cl_aab_id=>check_acid_authority( actvt = '01' ) EQ abap_false.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_authorized.
      ENDIF.

    WHEN 'D'. " Delete checkpoint group
      lr_acid = get_ref( ).
      IF lr_acid->check_authority( actvt = '06' ) NE abap_true.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_authorized.
      ENDIF.

    WHEN OTHERS.
      " Not supported
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
  ENDCASE.

ENDMETHOD.


METHOD create.
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

  DATA lr_acid                 TYPE REF TO cl_aab_id.
  DATA lv_error                TYPE string.


  lr_acid = get_ref( ).
  CALL METHOD lr_acid->set_descript
    EXPORTING
      im_descript      = is_prop-descript
    EXCEPTIONS
      no_authorization = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO lv_error.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_error.
  ENDIF.

  CALL METHOD lr_acid->save
    EXCEPTIONS
      no_descript_specified = 1
      no_changes_found      = 2
      prop_error            = 3
      propt_error           = 4
      act_error             = 5
      cts_error             = 6
      sync_attributes_error = 7
      OTHERS                = 8.
  IF sy-subrc <> 0.
    MESSAGE s011(saab) INTO lv_error.
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

  DATA ls_prop                TYPE aab_id_propt.
  DATA lv_string              TYPE string.
  DATA rc                     TYPE sysubrc.                 "#EC NEEDED

  DATA _objtype               TYPE string.
  DATA rootnode               TYPE REF TO if_ixml_element.



  " Read checkpoint group properties
  SELECT SINGLE * FROM aab_id_propt INTO ls_prop WHERE name EQ objname AND langu = sy-langu.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM aab_id_propt INTO ls_prop WHERE name EQ objname. "#EC WARNOK
  ENDIF.

  IF ls_prop-descript IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>system_error.
  ENDIF.


  " Create XML
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).

  " Checkpoint name is not the first component of structure
  " ls_prop so we need to add it 'manually' in first position
  " otherwise SAPLINK won't be able to fill-in
  " variable 'objname' correctly
  lv_string = ls_prop-name.
  rootnode->set_attribute( name = 'NAME'     value = lv_string ).
  lv_string = ls_prop-langu.
  rootnode->set_attribute( name = 'LANGU'    value = lv_string ).
  lv_string = ls_prop-descript.
  rootnode->set_attribute( name = 'DESCRIPT' value = lv_string ).

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

  DATA ls_prop                TYPE aab_id_propt.
  DATA _objtype               TYPE string.
  DATA rootnode               TYPE REF TO if_ixml_element.
  DATA checkexists            TYPE flag.



  _objtype = getobjecttype( ).
  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = ls_prop.
  objname = ls_prop-name.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.
      " Removed to avoid errors in case the checkkpoint
      " group is still in use... Description will
      " be updated anyway, deletion is therefore not mandatory

      " deleteobject( ).
    ENDIF.
  ENDIF.



  " Create checkpoint group
  check_authority( 'C' ).
  enqueue( ).
  create( ls_prop ).
  dequeue( ).

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

  DATA lr_acid          TYPE REF TO cl_aab_id.
  DATA lv_subrc         TYPE sysubrc.
  DATA lv_error         TYPE string.


  check_authority( 'D' ).
  enqueue( ).

  lr_acid = get_ref( ).
  lr_acid->delete( EXCEPTIONS prop_error       = 1
                              propt_error      = 2
                              act_error        = 3
                              cts_error        = 4
                              cts_devclass     = 5
                              id_not_found     = 6
                              no_authorization = 7
                              OTHERS           = 8 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO lv_error.
    dequeue( ).
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_error.
  ENDIF.
  dequeue( ).


ENDMETHOD.


METHOD dequeue.
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

  DATA lr_acid TYPE REF TO cl_aab_id.

  lr_acid = get_ref( ).
  lr_acid->dequeue( ).


ENDMETHOD.


METHOD enqueue.
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

  DATA lr_acid TYPE REF TO cl_aab_id.


  lr_acid = get_ref( ).
  CALL METHOD lr_acid->enqueue
    EXCEPTIONS
      foreign_lock = 1
      system_error = 2
      cts_error    = 3
      OTHERS       = 4.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>locked.
  ENDIF.


ENDMETHOD.


METHOD GETOBJECTTYPE.
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

  objecttype = 'ACID'. " Checkpoint group

ENDMETHOD.


METHOD get_ref.
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

  DATA lv_name TYPE aab_id_name.

  lv_name = objname.
  CREATE OBJECT rr_acid
    EXPORTING
      im_name          = lv_name
    EXCEPTIONS
      name_not_allowed = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.                                         "#EC NEEDED
    " Should never happen!
  ENDIF.

ENDMETHOD.
ENDCLASS.
