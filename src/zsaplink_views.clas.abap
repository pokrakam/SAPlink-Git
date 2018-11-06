class ZSAPLINK_VIEWS definition
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



CLASS ZSAPLINK_VIEWS IMPLEMENTATION.


METHOD CHECKEXISTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: l_name TYPE ddobjname,
        dd25v_wa TYPE dd25v.
  l_name = objname.

  CALL FUNCTION 'DDIF_VIEW_GET'
    EXPORTING
      name          = l_name
    IMPORTING
      dd25v_wa      = dd25v_wa
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc = 0 AND dd25v_wa-viewname IS NOT INITIAL.
    exists = 'X'.
  ENDIF.


ENDMETHOD.


METHOD CREATEIXMLDOCFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: gotstate  TYPE ddgotstate,
        dd25v_wa  TYPE dd25v,
        dd09l_wa  TYPE dd09v,
        dd26v_tab TYPE STANDARD TABLE OF dd26v,
        dd26v_wa  LIKE LINE OF dd26v_tab,
        dd27p_tab TYPE STANDARD TABLE OF dd27p,
        dd27p_wa  LIKE LINE OF dd27p_tab,
        dd28j_tab TYPE STANDARD TABLE OF dd28j,
        dd28j_wa  LIKE LINE OF dd28j_tab,
        dd28v_tab TYPE STANDARD TABLE OF dd28v,
        dd28v_wa  LIKE LINE OF dd28v_tab.


*xml nodes
  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA dd09l_node TYPE REF TO if_ixml_element.
  DATA dd26v_node TYPE REF TO if_ixml_element.
  DATA dd27p_node TYPE REF TO if_ixml_element.
  DATA dd28j_node TYPE REF TO if_ixml_element.
  DATA dd28v_node TYPE REF TO if_ixml_element.
  DATA rc         TYPE sysubrc.
  DATA _viewname  TYPE ddobjname.
  _viewname = objname.

  CALL FUNCTION 'DDIF_VIEW_GET'
    EXPORTING
      name          = _viewname
      langu         = sy-langu
    IMPORTING
      gotstate      = gotstate
      dd25v_wa      = dd25v_wa
      dd09l_wa      = dd09l_wa
    TABLES
      dd26v_tab     = dd26v_tab
      dd27p_tab     = dd27p_tab
      dd28j_tab     = dd28j_tab
      dd28v_tab     = dd28v_tab
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0 OR dd25v_wa-viewname IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.

* Create parent node
  DATA _objtype TYPE string.
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = dd25v_wa ).

  dd09l_node = xmldoc->create_element( 'dd09l' ).
  setattributesfromstructure( node = dd09l_node structure = dd09l_wa ).
  rc = rootnode->append_child( dd09l_node ).

  LOOP AT dd26v_tab INTO dd26v_wa.
    dd26v_node = xmldoc->create_element( 'dd26v' ).
    setattributesfromstructure( node = dd26v_node structure = dd26v_wa ).
    rc = rootnode->append_child( dd26v_node ).
  ENDLOOP.

  LOOP AT dd27p_tab INTO dd27p_wa.
    dd27p_node = xmldoc->create_element( 'dd27p' ).
    setattributesfromstructure( node = dd27p_node structure = dd27p_wa ).
    rc = rootnode->append_child( dd27p_node ).
  ENDLOOP.

  LOOP AT dd28j_tab INTO dd28j_wa.
    dd28j_node = xmldoc->create_element( 'dd28j' ).
    setattributesfromstructure( node = dd28j_node structure = dd28j_wa ).
    rc = rootnode->append_child( dd28j_node ).
  ENDLOOP.

  LOOP AT dd28v_tab INTO dd28v_wa.
    dd28v_node = xmldoc->create_element( 'dd28v' ).
    setattributesfromstructure( node = dd28v_node structure = dd28v_wa ).
    rc = rootnode->append_child( dd28v_node ).
  ENDLOOP.

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
ENDMETHOD.


METHOD CREATEOBJECTFROMIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: gotstate  TYPE ddgotstate,
        dd25v_wa  type dd25v,
        dd09l_wa  type dd09v,
        dd26v_tab type standard table of dd26v,
        dd26v_wa  like line of dd26v_tab,
        dd27p_tab type standard table of dd27p,
        dd27p_wa  like line of dd27p_tab,
        dd28j_tab type standard table of dd28j,
        dd28j_wa  like line of dd28j_tab,
        dd28v_tab type standard table of dd28v,
        dd28v_wa  like line of dd28v_tab.

*xml nodes
  DATA rootnode    TYPE REF TO if_ixml_element.
  data dd09l_node type ref to if_ixml_element.
  data dd26v_node type ref to if_ixml_element.
  data dd27p_node type ref to if_ixml_element.
  data dd28j_node type ref to if_ixml_element.
  data dd28v_node type ref to if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA rc          TYPE sysubrc.
  DATA _tablname   TYPE ddobjname.
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
      structure = dd25v_wa.

  objname = dd25v_wa-viewname.

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

  dd09l_node = xmldoc->find_from_name( 'dd09l' ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = dd09l_node
    CHANGING
      structure = dd09l_wa.

* retrieve Tabl details
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd26v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd26v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd26v_wa.
    APPEND dd26v_wa TO dd26v_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd27p' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd27p_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd27p_wa.
    APPEND dd27p_wa TO dd27p_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd28j' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd28j_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd28j_wa.
    APPEND dd28j_wa TO dd28j_tab.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd28v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR dd28v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = dd28v_wa.
    APPEND dd28v_wa TO dd28v_tab.
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

  CALL FUNCTION 'DDIF_VIEW_PUT'
    EXPORTING
      name              = l_dd_objname
      dd25v_wa          = dd25v_wa
      dd09l_wa          = dd09l_wa
    TABLES
      dd26v_tab         = dd26v_tab
      dd27p_tab         = dd27p_tab
      dd28j_tab         = dd28j_tab
      dd28v_tab         = dd28v_tab
    EXCEPTIONS
      view_not_found    = 1
      name_inconsistent = 2
      view_inconsistent = 3
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
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

endmethod.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  objecttype = 'VIEW'.  "Views
endmethod.
ENDCLASS.
