class ZSAPLINK_DOMAINS definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.

  methods CREATEOBJECTFROMIXMLDOC_NOLANG
    importing
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
      !DEVCLASS type DEVCLASS default '$TMP'
      !OVERWRITE type FLAG optional
    returning
      value(NAME) type STRING
    raising
      ZCX_SAPLINK .

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

  constants C_MULTILANGUAGESUPPORT type STRING value 'MultiLanguageSupport' ##NO_TEXT.
ENDCLASS.



CLASS ZSAPLINK_DOMAINS IMPLEMENTATION.


METHOD checkexists.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

  DATA: l_name TYPE ddobjname,
        dd01v_wa type dd01v.
  l_name = objname.
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = l_name
    IMPORTING
      dd01v_wa      = dd01v_wa
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc = 0 AND dd01v_wa-domname IS NOT INITIAL.
    exists = 'X'.
  ENDIF.
ENDMETHOD.


METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*
**      Plugin created by:
**      Thomas Jung
**      thomas.jung1@gmail.com

**      Stefan Schmöcker - July 2014
**      Changed to support multilinguitiy

  DATA: lt_ddlanguage TYPE STANDARD TABLE OF ddlanguage WITH NON-UNIQUE DEFAULT KEY,
        lo_rootnode   TYPE REF TO if_ixml_element,
        lo_langunode  TYPE REF TO if_ixml_element,
        lo_dd01vnode  TYPE REF TO if_ixml_element,
        lo_dd07vnode  TYPE REF TO if_ixml_element,
        lv_objecttype TYPE string,
        lv_value      TYPE string,
        lv_ddobjname  TYPE ddobjname,
        ls_dd01v      TYPE dd01v,
        lt_dd07v      TYPE STANDARD TABLE OF dd07v WITH NON-UNIQUE DEFAULT KEY.
  FIELD-SYMBOLS: <lv_ddlanguage> LIKE LINE OF lt_ddlanguage,
                 <ls_dd07v>      LIKE LINE OF lt_dd07v.

*--------------------------------------------------------------------*
* First determine all languages that we have to take into account
* This translation could have taken place
*       for the domain description  ( DD01T )
*       or for the fix-values       ( DD07T )
* Get a list of all languages
*--------------------------------------------------------------------*
  SELECT DISTINCT ddlanguage
    INTO TABLE lt_ddlanguage
    FROM dd01t
    WHERE domname = me->objname.
  SELECT DISTINCT ddlanguage
    APPENDING TABLE lt_ddlanguage
    FROM dd07t
    WHERE domname = me->objname.
  SORT lt_ddlanguage.
  DELETE ADJACENT DUPLICATES FROM lt_ddlanguage.

*--------------------------------------------------------------------*
* Build rootnode
*--------------------------------------------------------------------*
  lv_objecttype = getobjecttype( ).
  lo_rootnode   = xmldoc->create_element( lv_objecttype ).
  lo_rootnode->set_attribute(  name  = 'DOMNAME'
                               value = me->objname ).
  lo_rootnode->set_attribute( name  = c_multilanguagesupport
                              value = 'X' ).
*--------------------------------------------------------------------*
* For each language add a language-node,
* and then add the relevant data from DD01L and DD07L
*--------------------------------------------------------------------*
  lv_ddobjname = me->objname.
  LOOP AT lt_ddlanguage ASSIGNING <lv_ddlanguage>.

*--------------------------------------------------------------------*
    lo_langunode = xmldoc->create_element( 'DDLANGUAGE' ).
    lv_value     = <lv_ddlanguage>.
    lo_langunode->set_attribute(  name  = 'LANGU'
                                  value = lv_value ).

    CLEAR: ls_dd01v,
           lt_dd07v.
    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_ddobjname
        langu         = <lv_ddlanguage>
      IMPORTING
*      gotstate      = gotstate
        dd01v_wa      = ls_dd01v
      TABLES
        dd07v_tab     = lt_dd07v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0 OR ls_dd01v-domname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>not_found.
    ENDIF.

    lo_dd01vnode = xmldoc->create_element( 'DD01V' ).
    setattributesfromstructure( node      = lo_dd01vnode
                                structure = ls_dd01v ).
    lo_langunode->append_child( lo_dd01vnode ).

    LOOP AT lt_dd07v ASSIGNING <ls_dd07v>.
      lo_dd07vnode = xmldoc->create_element( 'DD07V' ).
      setattributesfromstructure( node      = lo_dd07vnode
                                  structure = <ls_dd07v> ).
      lo_langunode->append_child( lo_dd07vnode ).
    ENDLOOP.

    lo_rootnode->append_child( lo_langunode ).

  ENDLOOP.

  xmldoc->append_child( lo_rootnode ).
  ixmldocument = xmldoc.
*
*  DATA: gotstate TYPE ddgotstate,
*        dd01v_wa TYPE dd01v,
*        dd07v_tab TYPE STANDARD TABLE OF dd07v.
*
**xml nodes
*  DATA rootnode   TYPE REF TO if_ixml_element.
*  DATA dd07v_node TYPE REF TO if_ixml_element.
*  DATA rc         TYPE sysubrc.
*  DATA _domaname  TYPE ddobjname.
*  _domaname = objname.
*
*  CALL FUNCTION 'DDIF_DOMA_GET'
*    EXPORTING
*      name          = _domaname
*      langu         = sy-langu
*    IMPORTING
*      gotstate      = gotstate
*      dd01v_wa      = dd01v_wa
*    TABLES
*      dd07v_tab     = dd07v_tab
*    EXCEPTIONS
*      illegal_input = 1
*      OTHERS        = 2.
*  IF sy-subrc <> 0 OR dd01v_wa-domname IS INITIAL.
*    RAISE EXCEPTION TYPE zcx_saplink
*      EXPORTING textid = zcx_saplink=>not_found.
*  ENDIF.
*
** Create parent node
*  DATA _objtype TYPE string.
*  _objtype = getobjecttype( ).
*  rootnode = xmldoc->create_element( _objtype ).
*  setattributesfromstructure( node = rootnode structure = dd01v_wa ).
*
*  DATA: wa_dd07v LIKE LINE OF dd07v_tab.
*  LOOP AT dd07v_tab INTO wa_dd07v.
*    dd07v_node = xmldoc->create_element( 'dd07v' ).
*    setattributesfromstructure( node = dd07v_node structure = wa_dd07v ).
*    rc = rootnode->append_child( dd07v_node ).
*  ENDLOOP.

*\--------------------------------------------------------------------/
*  rc = xmldoc->append_child( lo_rootnode ).
*  ixmldocument = xmldoc.
ENDMETHOD.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

*      Stefan Schmöcker - July 2014
*      Changed to support multilinguitiy


  DATA: gotstate TYPE ddgotstate,
        dd01v_wa TYPE dd01v,
        dd07v_tab TYPE STANDARD TABLE OF dd07v.

*xml nodes
  DATA rootnode    TYPE REF TO if_ixml_element.
  DATA dd07v_node  TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA rc          TYPE sysubrc.
  DATA _domaname   TYPE ddobjname.
  DATA _devclass   TYPE devclass.
  DATA checkexists TYPE flag.
  DATA _objtype    TYPE string.

  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

* begin of insertion Multilinguality - check if nugget/slinkee was exported w/o support of multilinguality
  DATA: lv_multilanguagesupport TYPE flag.
  lv_multilanguagesupport = rootnode->get_attribute( name = c_multilanguagesupport ).
  IF lv_multilanguagesupport IS INITIAL.  " Not found or not set --> use old version of this class
    name = createobjectfromixmldoc_nolang(  ixmldocument =  ixmldocument
                                            devclass     =  devclass
                                            overwrite    =  overwrite    ).
    RETURN.
  ENDIF.
* end of insertion Multilinguality - check if nugget/slinkee was exported w/o support of multilinguality

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = dd01v_wa.

  objname = dd01v_wa-domname.

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

* begin of deletion Multilinguality - moved down and into loop
** retrieve Domain details
*  FREE: filter, iterator, node.
*  filter = xmldoc->create_filter_name( 'dd07v' ).
*  iterator = xmldoc->create_iterator_filtered( filter ).
*  node ?= iterator->get_next( ).
*
*  DATA: wa_dd07v LIKE LINE OF dd07v_tab.
*  WHILE node IS NOT INITIAL.
*    CLEAR dd07v_node.
*    CALL METHOD getstructurefromattributes
*      EXPORTING
*        node      = node
*      CHANGING
*        structure = wa_dd07v.
*    APPEND wa_dd07v TO dd07v_tab.
*    node ?= iterator->get_next( ).
*  ENDWHILE.
* end of deletion Multilinguality - moved down and into loop

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

* begin of deletion Multilinguality - moved into loop
*  CALL FUNCTION 'DDIF_DOMA_PUT'
*    EXPORTING
*      name              = l_dd_objname
*      dd01v_wa          = dd01v_wa
*    TABLES
*      dd07v_tab         = dd07v_tab
*    EXCEPTIONS
*      doma_not_found    = 1
*      name_inconsistent = 2
*      doma_inconsistent = 3
*      put_failure       = 4
*      put_refused       = 5
*      OTHERS            = 6.
*  IF sy-subrc <> 0.
*    RAISE EXCEPTION TYPE zcx_saplink
*      EXPORTING textid = zcx_saplink=>system_error.
*  ENDIF.
* begin of deletion Multilinguality -  moved into loop
* begin of insertion Multilinguality - loop at languages and insert them all
  DATA: lo_langunode  TYPE REF TO if_ixml_element,
        lo_dd01vnode  TYPE REF TO if_ixml_element,
        lo_dd07vnode  TYPE REF TO if_ixml_element,
        ls_dd01v      TYPE dd01v,
        lt_dd07v      TYPE STANDARD TABLE OF dd07v WITH NON-UNIQUE DEFAULT KEY.
  FIELD-SYMBOLS: <ls_dd07v>      LIKE LINE OF lt_dd07v.

  lo_langunode ?= rootnode->find_from_name( 'DDLANGUAGE' ).
  WHILE lo_langunode IS BOUND.  " No need to extract anything from languagenode since language is duplicated in DD01V and DD07V

    CLEAR: ls_dd01v,
           lt_dd07v.
    FREE:  lo_dd01vnode,
           lo_dd07vnode.

* DD01V
    lo_dd01vnode ?= lo_langunode->find_from_name( 'DD01V' ).
    IF lo_dd01vnode IS BOUND.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = lo_dd01vnode
        CHANGING
          structure = ls_dd01v.

* DD07V
      lo_dd07vnode ?= lo_langunode->find_from_name( 'DD07V' ).
      WHILE lo_dd07vnode IS BOUND.
        APPEND INITIAL LINE TO lt_dd07v ASSIGNING <ls_dd07v>.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = lo_dd07vnode
          CHANGING
            structure = <ls_dd07v>.
        lo_dd07vnode ?= lo_dd07vnode->get_next( ).
      ENDWHILE.


      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING
          name              = l_dd_objname
          dd01v_wa          = ls_dd01v
        TABLES
          dd07v_tab         = lt_dd07v
        EXCEPTIONS
          doma_not_found    = 1
          name_inconsistent = 2
          doma_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
      ENDIF.
    ENDIF.

    lo_langunode ?= lo_langunode->get_next( ).


  ENDWHILE.
* end of insertion Multilinguality - loop at languages and insert them all



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


method CREATEOBJECTFROMIXMLDOC_NOLANG.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

*      Stefan Schmöcker - July 2014
*      Changed to support multilinguitiy


  DATA: gotstate TYPE ddgotstate,
        dd01v_wa TYPE dd01v,
        dd07v_tab TYPE STANDARD TABLE OF dd07v.

*xml nodes
  DATA rootnode    TYPE REF TO if_ixml_element.
  DATA dd07v_node  TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA rc          TYPE sysubrc.
  DATA _domaname   TYPE ddobjname.
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
      structure = dd01v_wa.

  objname = dd01v_wa-domname.

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

* retrieve Domain details
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'dd07v' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  DATA: wa_dd07v LIKE LINE OF dd07v_tab.
  WHILE node IS NOT INITIAL.
    CLEAR dd07v_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_dd07v.
    APPEND wa_dd07v TO dd07v_tab.
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

  CALL FUNCTION 'DDIF_DOMA_PUT'
    EXPORTING
      name              = l_dd_objname
      dd01v_wa          = dd01v_wa
    TABLES
      dd07v_tab         = dd07v_tab
    EXCEPTIONS
      doma_not_found    = 1
      name_inconsistent = 2
      doma_inconsistent = 3
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

  name = objname.

ENDMETHOD.


method DELETEOBJECT.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

endmethod.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

  objecttype = 'DOMA'.  "Domain
endmethod.
ENDCLASS.
