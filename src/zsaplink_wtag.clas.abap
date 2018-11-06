class ZSAPLINK_WTAG definition
  public
  inheriting from ZSAPLINK
  create public .

public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.

  methods WTAG_SETATTRIBUTESFROMSTR
    importing
      !NODE type ref to IF_IXML_ELEMENT
      !STRUCTURE type DATA .
  methods WTAG_GETSTRUCTUREFROMATTR
    importing
      !NODE type ref to IF_IXML_ELEMENT
    changing
      !STRUCTURE type DATA .

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.

  methods CORR_INSERT
    importing
      !P_AUTHOR type SYUNAME
      !P_DEVCLASS type DEVCLASS
    raising
      ZCX_SAPLINK .
  class-methods ACCESS_PERMISSION
    importing
      !P_OBJECT_KEY type C
      !P_MODE type STRING default 'INSERT'
      !P_NO_DIALOG type CHAR1 default 'X'
      !P_OBJECTTYPE type TROBJTYPE
    exporting
      !P_DEVCLASS type DEVCLASS
      !P_TRANSPORT_KEY type TRKEY
      !P_CORR_NUM type TRKORR
      !P_MASTER_LANG type SYLANGU
      !P_MODIFY_LANG type SY-LANGU
    raising
      ZCX_SAPLINK .
  class-methods DELETE_FROM_WORKING_AREA
    importing
      !P_TLIBID type O2TLIBID
      !P_GLOBAL type XFELD
      !P_OBJECTTYPE type TROBJTYPE .
  class-methods UPDATE_OBJECT_LISTS
    importing
      !P_TLIBID type O2TLIBID
      !P_OPERATION type C .
ENDCLASS.



CLASS ZSAPLINK_WTAG IMPLEMENTATION.


METHOD ACCESS_PERMISSION .

*  DATA:
*    l_uname TYPE syuname,
*    l_uname2 TYPE syuname,
*    l_tlibid TYPE o2tlibid.

* check object name
  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      authority_check          = 'X'
      global_lock              = 'X'
      mode                     = p_mode
      object                   = p_object_key
      object_class             = p_objecttype
      suppress_language_check  = p_no_dialog
      suppress_language_dialog = p_no_dialog
    IMPORTING
      devclass                 = p_devclass
      new_master_language      = p_master_lang
      korrnum                  = p_corr_num
      transport_key            = p_transport_key
      modification_language    = p_modify_lang
    EXCEPTIONS
      canceled_in_corr         = 1
      enqueued_by_user         = 2
      enqueue_system_failure   = 3
      illegal_parameter_values = 4
      locked_by_author         = 5
      no_modify_permission     = 6
      no_show_permission       = 7
      permission_failure       = 8
      request_language_denied  = 9
      OTHERS                   = 10.

  case sy-subrc.
    when 0.
    when 2 or 5.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>locked.
    when 6 or 7 or 8 or 9.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>not_authorized.
    when others.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>system_error.
  endcase.
ENDMETHOD.                    "access_permission


method CHECKEXISTS .
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
*      Phil Young
*      ptyoun01@gmail.com


  data P_TLIBID TYPE O2TLIBID.

  P_TLIBID = objName.
  exists = cl_o2_taglib_manager=>exists_on_db( p_tlibid ).

endmethod.


method CORR_INSERT .
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

data myTROBJTYPE type TROBJTYPE.

myTROBJTYPE = getobjecttype( ).


  CALL FUNCTION 'RS_CORR_INSERT'
       EXPORTING
            AUTHOR              = p_author
            GLOBAL_LOCK         = 'X'
            OBJECT              = objName
            OBJECT_CLASS        = myTROBJTYPE
            DEVCLASS            = p_devClass
*            KORRNUM             = CORRNUMBER_LOCAL
            MASTER_LANGUAGE     = sy-langu
*            PROGRAM             = PROGRAM_LOCAL
            MODE                = 'INSERT'
*       IMPORTING
*            AUTHOR              = UNAME
*            KORRNUM             = CORRNUMBER_LOCAL
*            DEVCLASS            = DEVCLASS_LOCAL
       EXCEPTIONS
            CANCELLED           = 1
            PERMISSION_FAILURE  = 2
            UNKNOWN_OBJECTCLASS = 3.

  if sy-subrc <> 0.
    case sy-subrc.
      when 2.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>not_authorized.
      when others.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>system_error.
    endcase.
  endif.

endmethod.


method CREATEIXMLDOCFROMOBJECT .
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
*      Phil Young
*      ptyoun01@gmail.com


data rc type sysubrc.

data: my_wtag type ref to CL_O2_TAG_LIBRARY,
      l_tlibid TYPE o2tlibid.

data P_TLIB_EDIT TYPE O2TLIBEDIT.

*xml nodes
data rootNode type ref to if_ixml_element.

l_tlibid = objName.

CALL METHOD CL_O2_TAG_LIBRARY=>GET_INSTANCE
  EXPORTING
    P_TLIBID      = l_tlibid
    P_STATE       = 'A'
  RECEIVING
    P_TLIB_REF    = my_wtag
    .

CALL METHOD MY_WTAG->GET_TAGLIB
  RECEIVING
    P_TLIB_EDIT = P_TLIB_EDIT
    .

* Create parent node
  data _objType type string.
  _objType = getObjectType( ).
  rootNode = xmlDoc->create_element( _objType ).
  WTAG_SETATTRIBUTESFROMSTR( node = rootNode structure = P_TLIB_EDIT
  ).


*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootNode ).
  ixmlDocument = xmlDoc.
endmethod.


method CREATEOBJECTFROMIXMLDOC .
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
*      Phil Young
*      ptyoun01@gmail.com


  types:
    TT_TLINE type standard table of TLINE .
  types:
    t_t_tag TYPE HASHED TABLE OF o2tag
      WITH UNIQUE KEY tlibid tagid .
  types:
    t_t_tagt TYPE HASHED TABLE OF tagt
      WITH UNIQUE KEY tlibid tagid .
  types:
    t_t_tagatt TYPE HASHED TABLE OF o2tagatt
      WITH UNIQUE KEY tlibid tagid attid .
  types:
    t_t_tagattt TYPE HASHED TABLE OF tagattt
      WITH UNIQUE KEY tlibid tagid attid .
  types:
    t_t_tline TYPE STANDARD TABLE OF tline
      WITH DEFAULT KEY .

data rootNode type ref to if_ixml_element.
data _devclass type devclass.
data _objType type string.
data P_TLIB_EDIT TYPE O2TLIBEDIT.
data checkExists type flag.
data l_cx_ref TYPE REF TO zcx_saplink.

  _devclass = devclass.
  _objType = getObjectType( ).

  xmlDoc = ixmlDocument.
  rootNode = xmlDoc->find_from_name( _objType ).

*  call method GETSTRUCTUREFROMATTRIBUTES
  call method WTAG_GETSTRUCTUREFROMATTR
        exporting
          node = rootNode
        changing
          structure = P_TLIB_EDIT.

  objName = P_TLIB_EDIT-TLIBID.
  checkExists = checkexists( ).
  if checkExists is not initial.
    if overwrite is initial.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>existing.
    else.
*     delete object for new install
      TRY.
        deleteobject( ).

        CATCH ZCX_SAPLINK into l_cx_ref.
          raise exception l_cx_ref.
      ENDTRY.
    endif.
  endif.

*TYPES:
*
*    t_t_tag TYPE HASHED TABLE OF o2tag
*      WITH UNIQUE KEY tlibid tagid,
*
*    t_t_tagt TYPE HASHED TABLE OF tagt
*      WITH UNIQUE KEY tlibid tagid,
*
*    t_t_tagatt TYPE HASHED TABLE OF o2tagatt
*      WITH UNIQUE KEY tlibid tagid attid,
*
*    t_t_tagattt TYPE HASHED TABLE OF tagattt
*      WITH UNIQUE KEY tlibid tagid attid,
*
*    t_t_tline TYPE STANDARD TABLE OF tline
*      WITH DEFAULT KEY.
*
*TYPES: BEGIN OF t_s_docu,
*        tlibid TYPE o2tlibid,
*        tagid TYPE o2tagid,
*        dokhl TYPE dokhl,
*        dokil TYPE dokil,
*        docu TYPE t_t_tline,
*      END OF t_s_docu,
*
*      t_t_docu TYPE HASHED TABLE OF t_s_docu
*        WITH UNIQUE KEY tlibid tagid.

data wa_o2taglib type o2taglib.
data wa_taglibt type taglibt.
data it_tag type t_t_tag.
data wa_tag like line of it_tag.
data it_tagt type t_t_tagt.
data wa_tagt like line of it_tagt.
data it_tagatt type t_t_tagatt.
data wa_tagatt like line of it_tagatt.
data it_tagattt type t_t_tagattt.
data wa_tagattt like line of it_tagattt.

data wa_tags type O2TAGEDIT.
data wa_attributes type O2ATTEDIT.

data state type r3state value 'I'.

* build tables / structures

*extension
move-corresponding P_TLIB_EDIT to wa_o2taglib.
wa_o2taglib-state = state.
wa_o2taglib-author = sy-uname.
wa_o2taglib-createdon = sy-datum.
wa_o2taglib-changedby = sy-uname.
wa_o2taglib-changedon = sy-datum.
wa_o2taglib-stamp = sy-uzeit.

move-corresponding P_TLIB_EDIT to wa_taglibt.
wa_taglibt-state = state.
wa_taglibt-langu = sy-langu.

* elements
loop at P_TLIB_EDIT-TAGS into wa_tags.
  wa_tag-tlibid = P_TLIB_EDIT-TLIBID.
  move-corresponding wa_tags to wa_tag.
  wa_tag-state = state.
  wa_tag-author = sy-uname.
  wa_tag-createdon = sy-datum.
  wa_tag-changedby = sy-uname.
  wa_tag-changedon = sy-datum.
  insert wa_tag into table it_tag.

  move-corresponding wa_tag to wa_tagt.
  wa_tagt-langu = sy-langu.
  wa_tagt-descript = wa_tags-descript.
  insert wa_tagt into table it_tagt.

* attributes
  loop at wa_tags-attributes into wa_attributes.
    move-corresponding wa_attributes to wa_tagatt.
    wa_tagatt-tlibid = P_TLIB_EDIT-TLIBID.
    wa_tagatt-state = state.
    wa_tagatt-tagid = wa_tags-tagid.
    insert wa_tagatt into table it_tagatt.

    move-corresponding wa_tagatt to wa_tagattt.
    wa_tagattt-descript = wa_attributes-descript.
    wa_tagattt-langu = sy-langu.
    insert wa_tagattt into table it_tagattt.

    clear wa_tagattt.
    clear wa_tagatt.
  endloop.

  clear wa_tag.
  clear wa_tagt.
endloop.

* put bsp extension in worklist
data TROBJTYPE type TROBJTYPE.
data trobjName type trobj_name.

TROBJTYPE = GETOBJECTTYPE( ).
trobjName = objName.

TRY.
  CALL METHOD CORR_INSERT
    EXPORTING
      P_AUTHOR    = sy-uname
      P_DEVCLASS  = devclass
      .
  CATCH ZCX_SAPLINK into l_cx_ref.
    raise exception l_cx_ref.
ENDTRY.

TRY.
  CALL METHOD ZSAPLINK_WTAG=>ACCESS_PERMISSION
    EXPORTING
      P_OBJECT_KEY    = trobjName
      P_MODE          = 'INSERT'
      P_NO_DIALOG     = 'X'
      P_OBJECTTYPE    = TROBJTYPE
*    IMPORTING
*      P_DEVCLASS      =
*      P_TRANSPORT_KEY =
*      P_CORR_NUM      =
*      P_MASTER_LANG   =
*      P_MODIFY_LANG   =
      .
  CATCH ZCX_SAPLINK into l_cx_ref.
    raise exception l_cx_ref.
ENDTRY.



  call function 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = TROBJTYPE
      obj_name          = trobjName
    EXCEPTIONS
      wrong_object_name = 1.
  if sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  endif.


* update database tables

* extension
  INSERT INTO o2taglib VALUES wa_o2taglib.
  IF wa_taglibt IS NOT INITIAL.
    INSERT INTO taglibt VALUES wa_taglibt.
  ENDIF.
  IF sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  ENDIF.

* elements
  INSERT o2tag FROM TABLE it_tag.
  IF it_tagt[] IS NOT INITIAL.
    INSERT tagt FROM TABLE it_tagt.
  ENDIF.
  IF sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  ENDIF.

* element attributes
  IF it_tagatt[] IS NOT INITIAL.
    INSERT o2tagatt FROM TABLE it_tagatt.
  ENDIF.
  IF it_tagattt[] IS NOT INITIAL.
    INSERT tagattt FROM TABLE it_tagattt.
  ENDIF.
  IF sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  ENDIF.

  CALL METHOD ZSAPLINK_WTAG=>UPDATE_OBJECT_LISTS
    EXPORTING
      P_TLIBID    = P_TLIB_EDIT-TLIBID
      P_OPERATION = 'INSERT'
      .

  TRY.
    CALL METHOD ZSAPLINK_WTAG=>ACCESS_PERMISSION
      EXPORTING
        P_OBJECT_KEY    = trobjName
        P_MODE          = 'FREE'
        P_NO_DIALOG     = 'X'
        P_OBJECTTYPE    = TROBJTYPE
*      IMPORTING
*        P_DEVCLASS      =
*        P_TRANSPORT_KEY =
*        P_CORR_NUM      =
*        P_MASTER_LANG   =
*        P_MODIFY_LANG   =
        .
    CATCH ZCX_SAPLINK into l_cx_ref.
      raise exception l_cx_ref.
  ENDTRY.

  name = objName.

*CL_O2_TAGLIB_MANAGER-IMPORT_EXTENSION
endmethod.


method DELETEOBJECT .
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
*      Phil Young
*      ptyoun01@gmail.com

data myTLIBID type O2TLIBID.
data l_cx_ref TYPE REF TO zcx_saplink.
data TROBJTYPE type TROBJTYPE.
data trobjName type trobj_name.

TROBJTYPE = GETOBJECTTYPE( ).
trobjName = objName.

myTLIBID = objName.

TRY.
  CALL METHOD ZSAPLINK_WTAG=>ACCESS_PERMISSION
    EXPORTING
      P_OBJECT_KEY    = trobjName
      P_MODE          = 'MODIFY'
      P_NO_DIALOG     = 'X'
      P_OBJECTTYPE    = TROBJTYPE
*    IMPORTING
*      P_DEVCLASS      =
*      P_TRANSPORT_KEY =
*      P_CORR_NUM      =
*      P_MASTER_LANG   =
*      P_MODIFY_LANG   =
      .
  CATCH ZCX_SAPLINK into l_cx_ref.
    raise exception l_cx_ref.
ENDTRY.

* update database tables

* extension
delete from o2taglib where TLIBID = myTLIBID.
delete from taglibt where TLIBID = myTLIBID.

* elements
delete from o2tag where TLIBID = myTLIBID.
delete from tagt where TLIBID = myTLIBID.

* element attributes
delete from o2tagatt where TLIBID = myTLIBID.
delete from tagattt where TLIBID = myTLIBID.

TRY.
  CALL METHOD ZSAPLINK_WTAG=>ACCESS_PERMISSION
    EXPORTING
      P_OBJECT_KEY    = trobjName
      P_MODE          = 'FREE'
      P_NO_DIALOG     = 'X'
      P_OBJECTTYPE    = TROBJTYPE
*    IMPORTING
*      P_DEVCLASS      =
*      P_TRANSPORT_KEY =
*      P_CORR_NUM      =
*      P_MASTER_LANG   =
*      P_MODIFY_LANG   =
      .
  CATCH ZCX_SAPLINK into l_cx_ref.
    raise exception l_cx_ref.
ENDTRY.

* remove entry from working area
  delete_from_working_area( p_tlibid = myTLIBID
                            p_global = 'X'
                            p_objecttype = TROBJTYPE ).

* this call also deletes $TMP TADIR entries
  update_object_lists( p_tlibid    = myTLIBID
                       p_operation = 'DELETE' ).

endmethod.


METHOD DELETE_FROM_WORKING_AREA .

  DATA:
    l_obj_name TYPE trobj_name.

  CALL FUNCTION 'RS_WORKING_AREA_INIT'.

  l_obj_name = p_tlibid.

  CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
    EXPORTING
      object                 = p_objecttype
      obj_name               = l_obj_name
      immediate              = 'X'
      actualize_working_area = p_global.

ENDMETHOD.                    "delete_from_working_area


method GETOBJECTTYPE .
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

  objecttype = 'WTAG'. "BSP Extension

endmethod.


METHOD UPDATE_OBJECT_LISTS .

  DATA:
    l_tree_log TYPE REF TO cl_wb_tree_log,
    l_treelog_entry TYPE treelog,
    l_treelog_tab TYPE treelog_tab.

  CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
    EXPORTING
      object    = p_tlibid
      type      = swbm_c_type_o2_taglibrary
      operation = p_operation.

  CREATE OBJECT l_tree_log.

  CONCATENATE 'TL_' p_tlibid INTO l_treelog_entry-tname.
  l_treelog_entry-type = swbm_c_type_o2_taglibrary.
  l_treelog_entry-name = p_tlibid.
  APPEND l_treelog_entry TO l_treelog_tab.

  CALL METHOD l_tree_log->raise_entry_changed
    EXPORTING
      p_treelog = l_treelog_tab.

ENDMETHOD.                    "update_object_lists


method WTAG_GETSTRUCTUREFROMATTR .
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
data attributeList type ref to IF_IXML_NAMED_NODE_MAP.
data nodeIterator type ref to IF_IXML_NODE_ITERATOR.
data attributeNode type ref to if_ixml_node.
data value type string.
data name type string.
field-symbols <value> type any.
************************************************************************
data tableDescr type ref to cl_abap_tabledescr.
data structDescr type ref to cl_abap_structdescr.
data typeDescr type ref to cl_abap_typedescr.
data childNode type ref to if_ixml_element.
data filter type ref to if_ixml_node_filter.
data iterator type ref to if_ixml_node_iterator.
data childName type string.
data it_foo type ref to data.
data wa_foo type ref to data.
field-symbols: <tab> type any table.
field-symbols: <row> type any.

************************************************************************

  clear structure.
  attributeList = node->GET_ATTRIBUTES( ).
  nodeIterator = attributeList->create_iterator( ).
  attributeNode = nodeIterator->get_next( ).
  while attributeNode is not initial.
    name = attributeNode->get_name( ).
    if name = 'VERSION'.
      value = '0'.
    else.
      value = attributeNode->get_value( ).
    endif.
    assign component name of structure structure to <value>.
    if sy-subrc = 0.
************************************************************************
      typeDescr = cl_abap_typedescr=>DESCRIBE_BY_DATA(
        p_data = <value> ).
      try.
        tableDescr ?= typeDescr.
        catch CX_SY_MOVE_CAST_ERROR.
      endtry.
      try.
        structDescr ?= typeDescr.
        catch CX_SY_MOVE_CAST_ERROR.
      endtry.
      if tableDescr is initial and structDescr is initial.
        <value> = value.
      elseif tableDescr is initial.
        childNode ?= Node->get_first_child( ).
        while childNode is not initial.
          childName = childNode->GET_NAME( ).
          if childName = name.
            call method WTAG_GETSTRUCTUREFROMATTR
                  exporting
                    node = childNode
                  changing
                    structure = <value>.
            exit.
          endif.
          childNode ?= childNode->get_next( ).
        endwhile.
      else.
        create data it_foo type handle tableDescr.
        assign it_foo->* to <tab>.
        create data wa_foo like line of <tab>.
        assign wa_foo->* to <row>.

        free childNode.
        childNode ?= Node->get_first_child( ).
        while childNode is not initial.
          childName = childNode->GET_NAME( ).
          if childName = name.
            clear <row>.
            call method WTAG_GETSTRUCTUREFROMATTR
                  exporting
                    node = childNode
                  changing
                    structure = <row>.
            insert <row> into table <tab>.
          endif.
          childNode ?= childNode->get_next( ).
        endwhile.
        <value> = <tab>.
      endif.

      clear tableDescr.
      clear structDescr.
      clear typedescr.
      clear it_foo.
      clear wa_foo.
************************************************************************
    endif.
    attributeNode = nodeIterator->get_next( ).
  endwhile.

endmethod.


method WTAG_SETATTRIBUTESFROMSTR .
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

data int type i.
int = int.
data structDescr type ref to cl_abap_structDescr.
data aComponent type abap_compdescr.
field-symbols <fieldValue> type any.
data rc type sysubrc.
data sName type string.
data sValue type string.
****************************************************************
data tableDescr type ref to cl_abap_tabledescr.
data structDescr2 type ref to cl_abap_structdescr.
data typeDescr type ref to cl_abap_typedescr.
data childNode type ref to if_ixml_element.
data childName type string.
data it_foo type ref to data.
field-symbols: <tab> type any table.
field-symbols: <row> type any.
****************************************************************

  structDescr ?= cl_abap_structDescr=>describe_by_data( structure ).
  loop at structDescr->components into aComponent.
    assign component aComponent-name of structure
      structure to <fieldValue>.
    if sy-subrc = 0.
      sName = aComponent-name.
*      sValue = <fieldValue>.
*     for certain attributes, set to a standard for exporting
      case sName.
        when 'VERSION'. "version should always export as inactive
          sValue = '0'.
        when 'DEVCLASS'. "development class should always be $TMP
          sValue = '$TMP'.
        when others.
**********************************************************
  typeDescr = cl_abap_typedescr=>DESCRIBE_BY_DATA(
    p_data = <fieldValue> ).
  try.
    tableDescr ?= typeDescr.
    catch CX_SY_MOVE_CAST_ERROR.
  endtry.
  try.
    structDescr2 ?= typeDescr.
    catch CX_SY_MOVE_CAST_ERROR.
  endtry.
  if tableDescr is initial and structDescr2 is initial.
    sValue = <fieldValue>.
  elseif tableDescr is initial.
    sValue = '$$STRUCTURE$$'.
    childName = aComponent-name.
    childNode = xmlDoc->create_element( childName ).
    WTAG_SETATTRIBUTESFROMSTR( node = childNode
                                structure = <fieldValue> ).
    rc = Node->append_child( childNode ).

  else.
    sValue = '$$TABLE$$'.
    create data it_foo type handle tableDescr.
*    get reference of <fieldvalue> into it_foo.
    assign it_foo->* to <tab>.
    <tab> = <fieldValue>.
    loop at <tab> assigning <row>.
      childName = aComponent-name.
      childNode = xmlDoc->create_element( childName ).
      WTAG_SETATTRIBUTESFROMSTR( node = childNode
                                  structure = <row> ).
      rc = Node->append_child( childNode ).
    endloop.
  endif.
  clear it_foo.
  clear tableDescr.
  clear structDescr2.
  clear typedescr.

*          sValue = <fieldValue>.
**********************************************************

      endcase.
      if sValue is not initial.
        rc = Node->set_attribute( name = sName value = sValue ).
      endif.
    else.
* WHAT?>!??
    endif.
  endloop.
endmethod.
ENDCLASS.
