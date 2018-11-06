class ZSAPLINK_SEARCH_HELPS definition
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
ENDCLASS.



CLASS ZSAPLINK_SEARCH_HELPS IMPLEMENTATION.


METHOD CHECKEXISTS.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: l_name TYPE ddobjname,
        dd30v_wa TYPE dd30v.
  l_name = objname.

  CALL FUNCTION 'DDIF_SHLP_GET'
    EXPORTING
      name          = l_name
    IMPORTING
      dd30v_wa      = dd30v_wa
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc = 0 AND dd30v_wa-SHLPNAME IS NOT INITIAL.
    exists = 'X'.
  ENDIF.


ENDMETHOD.


METHOD CREATEIXMLDOCFROMOBJECT.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: gotstate  TYPE ddgotstate,
        dd30v_wa  TYPE dd30v,
        dd31v_tab TYPE STANDARD TABLE OF dd31v,
        dd31v_wa  LIKE LINE OF dd31v_tab,
        dd32p_tab TYPE STANDARD TABLE OF dd32p,
        dd32p_wa  LIKE LINE OF dd32p_tab,
        dd33v_tab TYPE STANDARD TABLE OF dd33v,
        dd33v_wa  LIKE LINE OF dd33v_tab.

*xml nodes
  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA dd31v_node TYPE REF TO if_ixml_element.
  DATA dd32p_node TYPE REF TO if_ixml_element.
  DATA dd33v_node TYPE REF TO if_ixml_element.
  DATA rc         TYPE sysubrc.
  DATA _shlpname  TYPE ddobjname.
  _shlpname = objname.

  CALL FUNCTION 'DDIF_SHLP_GET'
    EXPORTING
      name          = _shlpname
      langu         = sy-langu
    IMPORTING
      gotstate      = gotstate
      dd30v_wa      = dd30v_wa
    TABLES
      dd31v_tab     = dd31v_tab
      dd32p_tab     = dd32p_tab
      dd33v_tab     = dd33v_tab
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0 OR dd30v_wa-SHLPNAME IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.

* Create parent node
  DATA _objtype TYPE string.
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = dd30v_wa ).

  LOOP AT dd31v_tab INTO dd31v_wa.
    dd31v_node = xmldoc->create_element( 'dd31v' ).
    setattributesfromstructure( node = dd31v_node structure = dd31v_wa ).
    rc = rootnode->append_child( dd31v_node ).
  ENDLOOP.

  LOOP AT dd32p_tab INTO dd32p_wa.
    dd32p_node = xmldoc->create_element( 'dd32p' ).
    setattributesfromstructure( node = dd32p_node structure = dd32p_wa ).
    rc = rootnode->append_child( dd32p_node ).
  ENDLOOP.

  LOOP AT dd33v_tab INTO dd33v_wa.
    dd33v_node = xmldoc->create_element( 'dd33v' ).
    setattributesfromstructure( node = dd33v_node structure = dd33v_wa ).
    rc = rootnode->append_child( dd33v_node ).
  ENDLOOP.

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
ENDMETHOD.


METHOD CREATEOBJECTFROMIXMLDOC.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: gotstate  TYPE ddgotstate,
        dd30v_wa  TYPE dd30v,
        dd31v_tab TYPE STANDARD TABLE OF dd31v,
        dd31v_wa  LIKE LINE OF dd31v_tab,
        dd32p_tab TYPE STANDARD TABLE OF dd32p,
        dd32p_wa  LIKE LINE OF dd32p_tab,
        dd33v_tab TYPE STANDARD TABLE OF dd33v,
        dd33v_wa  LIKE LINE OF dd33v_tab.

*xml nodes
  DATA rootnode    TYPE REF TO if_ixml_element.
  DATA dd31v_node  TYPE REF TO if_ixml_element.
  DATA dd32p_node  TYPE REF TO if_ixml_element.
  DATA dd33v_node  TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA rc          TYPE sysubrc.
  DATA _shlpname   TYPE ddobjname.
  DATA _devclass   TYPE devclass.
  DATA checkexists TYPE flag.
  DATA _objtype    TYPE string.

  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = dd30v_wa.

  objname = dd30v_wa-shlpname.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

* retrieve Tabl details
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd31v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd31v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd31v_wa.
    APPEND dd31v_wa TO dd31v_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd32p' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd32p_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd32p_wa.
    APPEND dd32p_wa TO dd32p_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd33v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd33v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd33v_wa.
    APPEND dd33v_wa TO dd33v_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  DATA : l_pgmid         TYPE tadir-pgmid,
           l_object      TYPE tadir-object,
           l_obj_name    TYPE tadir-obj_name,
           l_dd_objname  TYPE ddobjname,
           l_srcsystem   TYPE tadir-srcsystem,
           l_author      TYPE tadir-author,
           l_devclass    TYPE tadir-devclass,
           l_masterlang  TYPE tadir-masterlang.


  l_pgmid      = 'R3TR'.
  l_object     = _objtype.
  l_obj_name   = objname.
  l_dd_objname = objname.
  l_srcsystem  = sy-sysid.
  l_author     = sy-uname.
  l_devclass   = _devclass.
  l_masterlang = sy-langu.

  DATA: itadir TYPE tadir.
  itadir-pgmid      = l_pgmid.
  itadir-object     = l_object.
  itadir-obj_name   = l_obj_name.
  itadir-srcsystem  = l_srcsystem.
  itadir-author     = l_author.
  itadir-devclass   = l_devclass.
  itadir-masterlang = l_masterlang.
  MODIFY tadir FROM itadir.

  CALL FUNCTION 'TR_TADIR_INTERFACE'
    EXPORTING
      wi_test_modus                  = ' '
      wi_delete_tadir_entry          = 'X'
      wi_tadir_pgmid                 = l_pgmid
      wi_tadir_object                = l_object
      wi_tadir_obj_name              = l_obj_name
      wi_tadir_srcsystem             = l_srcsystem
      wi_tadir_author                = l_author
      wi_tadir_devclass              = l_devclass
      wi_tadir_masterlang            = l_masterlang
      iv_set_edtflag                 = ''
    EXCEPTIONS
      tadir_entry_not_existing       = 1
      tadir_entry_ill_type           = 2
      no_systemname                  = 3
      no_systemtype                  = 4
      original_system_conflict       = 5
      object_reserved_for_devclass   = 6
      object_exists_global           = 7
      object_exists_local            = 8
      object_is_distributed          = 9
      obj_specification_not_unique   = 10
      no_authorization_to_delete     = 11
      devclass_not_existing          = 12
      simultanious_set_remove_repair = 13
      order_missing                  = 14
      no_modification_of_head_syst   = 15
      pgmid_object_not_allowed       = 16
      masterlanguage_not_specified   = 17
      devclass_not_specified         = 18
      specify_owner_unique           = 19
      loc_priv_objs_no_repair        = 20
      gtadir_not_reached             = 21
      object_locked_for_order        = 22
      change_of_class_not_allowed    = 23
      no_change_from_sap_to_tmp      = 24
      OTHERS                         = 25.
  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
      WHEN 11 OR 23 OR 24.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_authorized.
      WHEN 22.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>locked.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'DDIF_SHLP_PUT'
    EXPORTING
      name              = l_dd_objname
      dd30v_wa          = dd30v_wa
    TABLES
      dd31v_tab         = dd31v_tab
      dd32p_tab         = dd32p_tab
      dd33v_tab         = dd33v_tab
    EXCEPTIONS
      shlp_not_found    = 1
      name_inconsistent = 2
      shlp_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>system_error.
  ENDIF.

  DATA: trobjtype  TYPE trobjtype,
        trobj_name TYPE trobj_name.
  trobjtype  = l_object.
  trobj_name = l_obj_name.
  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = trobjtype
      obj_name          = trobj_name
    EXCEPTIONS
      wrong_object_name = 1.

  name = objName.
ENDMETHOD.


method DELETEOBJECT.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

endmethod.


method GETOBJECTTYPE.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  objecttype = 'SHLP'.  "Search Helps
endmethod.
ENDCLASS.
