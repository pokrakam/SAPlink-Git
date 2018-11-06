class ZSAPLINK_TABLE_TYPES definition
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



CLASS ZSAPLINK_TABLE_TYPES IMPLEMENTATION.


METHOD CHECKEXISTS.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: l_name   TYPE ddobjname,
        dd40v_wa TYPE dd40v.
  l_name = objname.
  CALL FUNCTION 'DDIF_TTYP_GET'
    EXPORTING
      name          = l_name
    IMPORTING
      dd40v_wa      = dd40v_wa
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc = 0 AND dd40v_wa-typename IS NOT INITIAL.
    exists = 'X'.
  ENDIF.
ENDMETHOD.


METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: gotstate   TYPE ddgotstate,
        dd40v_wa   TYPE dd40v,
        lt_dd42v   TYPE STANDARD TABLE OF dd42v WITH NON-UNIQUE DEFAULT KEY,
        ls_dd42v   LIKE LINE OF lt_dd42v,
        lt_dd43v   TYPE STANDARD TABLE OF dd43v WITH NON-UNIQUE DEFAULT KEY,
        ls_dd43v   LIKE LINE OF lt_dd43v,

        _objtype   TYPE string,

*xml nodes
        rootnode   TYPE REF TO if_ixml_element,
        dd42v_node TYPE REF TO if_ixml_element,
        dd43v_node TYPE REF TO if_ixml_element,
        rc         TYPE sysubrc,
        _ttypname  TYPE ddobjname.


  _ttypname  = objname.

  CALL FUNCTION 'DDIF_TTYP_GET'
    EXPORTING
      name          = _ttypname
      langu         = sy-langu
    IMPORTING
      gotstate      = gotstate
      dd40v_wa      = dd40v_wa
    TABLES
      dd42v_tab     = lt_dd42v
      dd43v_tab     = lt_dd43v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0 OR dd40v_wa-typename IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>not_found.
  ENDIF.

* Create parent node
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = dd40v_wa ).

  LOOP AT lt_dd42v INTO ls_dd42v.
    dd42v_node = xmldoc->create_element( 'dd42v' ).
    setattributesfromstructure( node = dd42v_node structure = ls_dd42v ).
    rc = rootnode->append_child( dd42v_node ).
  ENDLOOP.

  LOOP AT lt_dd43v INTO ls_dd43v.
    dd43v_node = xmldoc->create_element( 'dd43v' ).
    setattributesfromstructure( node = dd43v_node structure = ls_dd43v ).
    rc = rootnode->append_child( dd43v_node ).
  ENDLOOP.

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
ENDMETHOD.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  DATA: gotstate     TYPE ddgotstate,
        dd40v_wa     TYPE dd40v,
        lt_dd42v     TYPE STANDARD TABLE OF dd42v WITH NON-UNIQUE DEFAULT KEY,
        ls_dd42v     LIKE LINE OF lt_dd42v,
        lt_dd43v     TYPE STANDARD TABLE OF dd43v WITH NON-UNIQUE DEFAULT KEY,
        ls_dd43v     LIKE LINE OF lt_dd43v,

*xml nodes
        rootnode     TYPE REF TO if_ixml_element,
        dd42v_node   TYPE REF TO if_ixml_element,
        dd43v_node   TYPE REF TO if_ixml_element,
        node         TYPE REF TO if_ixml_element,
        filter       TYPE REF TO if_ixml_node_filter,
        iterator     TYPE REF TO if_ixml_node_iterator,
        rc           TYPE sysubrc,
        _ttypname    TYPE ddobjname,
        _devclass    TYPE devclass,
        checkexists  TYPE flag,
        _objtype     TYPE string,
* putting object into ddic
        l_pgmid      TYPE tadir-pgmid,
        l_object     TYPE tadir-object,
        l_obj_name   TYPE tadir-obj_name,
        l_dd_objname TYPE ddobjname,
        l_srcsystem  TYPE tadir-srcsystem,
        l_author     TYPE tadir-author,
        l_devclass   TYPE tadir-devclass,
        l_masterlang TYPE tadir-masterlang.


  _devclass   = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = dd40v_wa.

  objname = dd40v_wa-typename.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

* retrieve table type details
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd42v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR dd42v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_dd42v.
    APPEND ls_dd42v TO lt_dd42v.
    node ?= iterator->get_next( ).
  ENDWHILE.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd43v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR dd43v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_dd43v.
    APPEND ls_dd43v TO lt_dd43v.
    node ?= iterator->get_next( ).
  ENDWHILE.


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
          EXPORTING
            textid = zcx_saplink=>not_authorized.
      WHEN 22.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>locked.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'DDIF_TTYP_PUT'
    EXPORTING
      name              = l_dd_objname
      dd40v_wa          = dd40v_wa
    TABLES
      dd42v_tab         = lt_dd42v
      dd43v_tab         = lt_dd43v
    EXCEPTIONS
      ttyp_not_found    = 1
      name_inconsistent = 2
      ttyp_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>system_error.
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

  name = objname.

ENDMETHOD.


METHOD DELETEOBJECT.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

ENDMETHOD.


METHOD GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  objecttype = 'TTYP'.  "Table Type
ENDMETHOD.
ENDCLASS.
