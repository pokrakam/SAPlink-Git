class ZSAPLINK_VIEW_CLUSTER definition
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
    raising
      ZCX_SAPLINK .
  methods CREATE_TRANSPORT
    returning
      value(RV_TRKORR) type TRKORR
    raising
      ZCX_SAPLINK .
  methods ENQUEUE
    importing
      !ACTION type C
    raising
      ZCX_SAPLINK .
ENDCLASS.



CLASS ZSAPLINK_VIEW_CLUSTER IMPLEMENTATION.


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

  SELECT SINGLE vclname INTO objname FROM vcldir WHERE vclname = objname.
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

  CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
    EXPORTING
      view_action                    = 'U'
      view_name                      = 'V_VCLDIR'
      no_warning_for_clientindep     = 'X'
    EXCEPTIONS
      no_authority                   = 1
      no_clientindependent_authority = 2
      OTHERS                         = 3.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>not_authorized.
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

  DATA lv_vclname             TYPE vcldir-vclname.
  DATA ls_vcldir              TYPE v_vcldir.
  DATA lt_vclstruc            TYPE TABLE OF v_vclstruc.
  DATA lt_vclstrudep          TYPE TABLE OF vclstrudep.
  DATA lt_vcltab              TYPE TABLE OF vclmf.
  DATA ls_vclstruc            TYPE v_vclstruc.
  DATA ls_vclstrudep          TYPE vclstrudep.
  DATA ls_vcltab              TYPE vclmf.
  DATA rc                     TYPE sysubrc.                 "#EC NEEDED

  DATA _objtype               TYPE string.
  DATA rootnode               TYPE REF TO if_ixml_element.
  DATA node                   TYPE REF TO if_ixml_element.



  " Read view cluster info
  lv_vclname = objname.
  CALL FUNCTION 'VIEWCLUSTER_GET_DEFINITION'
    EXPORTING
      vclname                = lv_vclname
    IMPORTING
      vcldir_entry           = ls_vcldir
    TABLES
      vclstruc_tab           = lt_vclstruc
      vclstrudep_tab         = lt_vclstrudep
      vclmf_tab              = lt_vcltab
    EXCEPTIONS
      viewcluster_not_found  = 1
      incomplete_viewcluster = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>not_found.
  ENDIF.


  " Create XML
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = ls_vcldir ).

  LOOP AT lt_vclstruc INTO ls_vclstruc.
    node = xmldoc->create_element( 'vclstruc' ).
    setattributesfromstructure( node = node structure = ls_vclstruc ).
    rc = rootnode->append_child( node ).
  ENDLOOP.

  LOOP AT lt_vclstrudep INTO ls_vclstrudep.
    node = xmldoc->create_element( 'vclstrudep' ).
    setattributesfromstructure( node = node structure = ls_vclstrudep ).
    rc = rootnode->append_child( node ).
  ENDLOOP.

  LOOP AT lt_vcltab INTO ls_vcltab.
    node = xmldoc->create_element( 'vcltab' ).
    setattributesfromstructure( node = node structure = ls_vcltab ).
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

  DATA lv_vclname             TYPE vcldir-vclname.
  DATA ls_vcldir              TYPE v_vcldir.
  DATA lt_vclstruc            TYPE TABLE OF v_vclstruc.
  DATA lt_vclstrudep          TYPE TABLE OF vclstrudep.
  DATA lt_vcltab              TYPE TABLE OF vclmf.
  DATA ls_vclstruc            TYPE v_vclstruc.
  DATA ls_vclstrudep          TYPE vclstrudep.
  DATA ls_vcltab              TYPE vclmf.
  DATA checkexists            TYPE flag.
  DATA lv_trkorr              TYPE trkorr.

  DATA _objtype               TYPE string.
  DATA rootnode               TYPE REF TO if_ixml_element.
  DATA node                   TYPE REF TO if_ixml_element.
  DATA filter                 TYPE REF TO if_ixml_node_filter.
  DATA iterator               TYPE REF TO if_ixml_node_iterator.



  _objtype = getobjecttype( ).
  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = ls_vcldir.

  objname = ls_vcldir-vclname.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.                                                   "#EC NEEDED
      " Object will be overwritten automatically
      " No need for deletion
    ENDIF.
  ENDIF.


  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'vclstruc' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR ls_vclstruc.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_vclstruc.
    APPEND ls_vclstruc TO lt_vclstruc.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'vclstrudep' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR ls_vclstrudep.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_vclstrudep.
    APPEND ls_vclstrudep TO lt_vclstrudep.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'vcltab' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR ls_vcltab.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_vcltab.
    APPEND ls_vcltab TO lt_vcltab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  check_authority( ).
  enqueue( action = 'E' ).
  lv_trkorr = create_transport( ).

  CALL FUNCTION 'VIEWCLUSTER_SAVE_DEFINITION'
    EXPORTING
      vcldir_entry   = ls_vcldir
    TABLES
      vclstruc_tab   = lt_vclstruc
      vclstrudep_tab = lt_vclstrudep
      vclmf_tab      = lt_vcltab.

  CALL FUNCTION 'OBJ_GENERATE'
    EXPORTING
      iv_korrnum       = lv_trkorr
      iv_objectname    = ls_vcldir-vclname
      iv_objecttype    = 'C'
      iv_maint_mode    = 'I'
    EXCEPTIONS
      illegal_call     = 1
      object_not_found = 2
      generate_error   = 3
      transport_error  = 4
      OTHERS           = 5.

  IF sy-subrc = 0.
    " successful install
    enqueue( action = 'D' ).
    name = ls_vcldir-vclname.
  ENDIF.


ENDMETHOD.


METHOD create_transport.
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

  DATA lt_ko200 TYPE TABLE OF ko200.
  DATA ls_ko200 TYPE ko200.
  DATA lv_trkorr  TYPE trkorr.


  ls_ko200-pgmid = 'R3TR'.
  ls_ko200-object = 'VCLS'.
  ls_ko200-obj_name = objname.
  APPEND ls_ko200 TO lt_ko200.

  CALL FUNCTION 'TR_OBJECTS_CHECK'
    IMPORTING
      we_order                = lv_trkorr
    TABLES
      wt_ko200                = lt_ko200
    EXCEPTIONS
      cancel_edit_other_error = 1
      show_only_other_error   = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>system_error.
  ENDIF.

  CALL FUNCTION 'TR_OBJECTS_INSERT'
    EXPORTING
      wi_order                = lv_trkorr
    IMPORTING
      we_order                = rv_trkorr
    TABLES
      wt_ko200                = lt_ko200
    EXCEPTIONS
      cancel_edit_other_error = 1
      show_only_other_error   = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>system_error.
  ENDIF.


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

* Do need to delete -> Object will be regenerated

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

  DATA: vcl_sellist TYPE TABLE OF vimsellist.
  DATA: vcl_sel     TYPE vimsellist.
  DATA: lv_error    TYPE string.
  DATA: lv_subrc    TYPE sysubrc.

  REFRESH vcl_sellist. CLEAR vcl_sellist.
  vcl_sel-viewfield = 'VCLNAME'.
  vcl_sel-operator  = 'EQ'.
  vcl_sel-value     = objname.
  vcl_sel-ddic      = 'S'.
  APPEND vcl_sel TO vcl_sellist.

  CALL FUNCTION 'VIEW_ENQUEUE'
    EXPORTING
      action         = action
      enqueue_mode   = 'E'
      view_name      = 'V_VCLDIR'
      enqueue_range  = 'X'
    TABLES
      sellist        = vcl_sellist
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2.
  IF sy-subrc <> 0 AND action = 'E'.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e049(sv) WITH sy-msgv1 INTO lv_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_error.
      WHEN 2.
        MESSAGE e050(sv) WITH 'V_VCLDIR' INTO lv_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'VIEW_ENQUEUE'
    EXPORTING
      action         = action
      enqueue_mode   = 'E'
      view_name      = 'V_VCLSTRUC'
      enqueue_range  = 'X'
    TABLES
      sellist        = vcl_sellist
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2.
  IF sy-subrc <> 0 AND action = 'E'.
    lv_subrc = sy-subrc.
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action        = 'D'
        enqueue_mode  = 'E'
        view_name     = 'V_VCLDIR'
        enqueue_range = 'X'
      TABLES
        sellist       = vcl_sellist.

    CASE lv_subrc.
      WHEN 1.
        MESSAGE e049(sv) WITH sy-msgv1 INTO lv_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_error.
      WHEN 2.
        MESSAGE e050(sv) WITH 'V_VCLSTRUC' INTO lv_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'VIEW_ENQUEUE'
    EXPORTING
      action         = action
      enqueue_mode   = 'E'
      view_name      = 'V_VCLSTDEP'
      enqueue_range  = 'X'
    TABLES
      sellist        = vcl_sellist
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2.
  IF sy-subrc <> 0 AND action = 'E'.
    lv_subrc = sy-subrc.
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action        = 'D'
        enqueue_mode  = 'E'
        view_name     = 'V_VCLDIR'
        enqueue_range = 'X'
      TABLES
        sellist       = vcl_sellist.
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action        = 'D'
        enqueue_mode  = 'E'
        view_name     = 'V_VCLSTRUC'
        enqueue_range = 'X'
      TABLES
        sellist       = vcl_sellist.

    CASE lv_subrc.
      WHEN 1.
        MESSAGE e049(sv) WITH sy-msgv1 INTO lv_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_error.
      WHEN 2.
        MESSAGE e050(sv) WITH 'V_VCLSTDEP' INTO lv_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'VIEW_ENQUEUE'
    EXPORTING
      action         = action
      enqueue_mode   = 'E'
      view_name      = 'V_VCLMF'
      enqueue_range  = 'X'
    TABLES
      sellist        = vcl_sellist
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2.
  IF sy-subrc <> 0 AND action = 'E'.
    lv_subrc = sy-subrc.
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action        = 'D'
        enqueue_mode  = 'E'
        view_name     = 'V_VCLDIR'
        enqueue_range = 'X'
      TABLES
        sellist       = vcl_sellist.
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action        = 'D'
        enqueue_mode  = 'E'
        view_name     = 'V_VCLSTRUC'
        enqueue_range = 'X'
      TABLES
        sellist       = vcl_sellist.
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action        = 'D'
        enqueue_mode  = 'E'
        view_name     = 'V_VCLSTDEP'
        enqueue_range = 'X'
      TABLES
        sellist       = vcl_sellist.
    CASE lv_subrc.
      WHEN 1.
        MESSAGE e049(sv) WITH sy-msgv1 INTO lv_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_error.
      WHEN 2.
        MESSAGE e050(sv) WITH 'V_VCLMF' INTO lv_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_error.
    ENDCASE.
  ENDIF.


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

  objecttype = 'VCLS'. " View cluster

ENDMETHOD.
ENDCLASS.
