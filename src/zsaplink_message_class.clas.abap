class ZSAPLINK_MESSAGE_CLASS definition
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

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
ENDCLASS.



CLASS ZSAPLINK_MESSAGE_CLASS IMPLEMENTATION.


METHOD checkexists .
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
*      John Patterson
*      patterjo@gmail.com

  DATA: ls_t100a TYPE t100a,
        lv_msgid TYPE msgid.

  lv_msgid =  objname.

  SELECT SINGLE *
    INTO ls_t100a
    FROM t100a
    WHERE arbgb = lv_msgid.

  IF sy-subrc = 0.
    exists = 'X'.
  ENDIF.

ENDMETHOD.


METHOD createixmldocfromobject .
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
*      John Patterson
*      patterjo@gmail.com

  DATA: lt_t100  TYPE STANDARD TABLE OF t100,
        lt_t100t TYPE STANDARD TABLE OF t100t,
        ls_t100  LIKE LINE OF lt_t100,
        ls_t100t LIKE LINE OF lt_t100t,
        ls_t100a TYPE t100a.

  DATA: lv_msgid TYPE msgid,
        lv_rc    TYPE sysubrc,
        lv_objtype TYPE string.

*xml nodes
  DATA: lr_rootnode   TYPE REF TO if_ixml_element,
        lr_t100_node  TYPE REF TO if_ixml_element,
        lr_t100t_node TYPE REF TO if_ixml_element.

  lv_msgid = objname.

  SELECT SINGLE *
   INTO ls_t100a
   FROM t100a WHERE arbgb = lv_msgid.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_saplink
    EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.
  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      authority_check          = 'X'
      global_lock              = space
      mode                     = 'SHOW'
      object                   = lv_msgid
      object_class             = 'T100'
    EXCEPTIONS
      canceled_in_corr         = 01
      enqueued_by_user         = 02
      enqueue_system_failure   = 03
      illegal_parameter_values = 04
      locked_by_author         = 05
      no_modify_permission     = 06
      no_show_permission       = 07
      permission_failure       = 08.
  CASE sy-subrc.
    WHEN 0.
    WHEN 2 OR 5.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>locked.
    WHEN 6 OR 7 OR 8 OR 9.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>not_authorized.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>system_error.
  ENDCASE.

*--- get messages for all maintained languages
  SELECT *
   INTO TABLE lt_t100
   FROM t100
   WHERE arbgb = lv_msgid.

*--- get text for t100a
  SELECT  *
   INTO TABLE lt_t100t
   FROM t100t
   WHERE arbgb = lv_msgid.

*-- Create parent node
  lv_objtype = getobjecttype( ).
  lr_rootnode = xmldoc->create_element( lv_objtype ).
  setattributesfromstructure( node = lr_rootnode
                              structure = ls_t100a ).

*--- Create Elements for Messages
  LOOP AT lt_t100 INTO ls_t100.
    lr_t100_node = xmldoc->create_element( 't100' ).
    setattributesfromstructure( node = lr_t100_node
                                structure = ls_t100 ).
    lv_rc = lr_rootnode->append_child( lr_t100_node ).
  ENDLOOP.

*--- Create Elements for Texts
  LOOP AT lt_t100t INTO ls_t100t.
    lr_t100t_node = xmldoc->create_element( 't100t' ).
    setattributesfromstructure( node = lr_t100t_node
                                structure = ls_t100t ).
    lv_rc = lr_rootnode->append_child( lr_t100t_node ).
  ENDLOOP.

  lv_rc = xmldoc->append_child( lr_rootnode ).
  ixmldocument = xmldoc.



ENDMETHOD.


METHOD createobjectfromixmldoc .
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
*      John Patterson
*      patterjo@gmail.com

  DATA: lt_t100  TYPE STANDARD TABLE OF t100,
        lt_t100t TYPE STANDARD TABLE OF t100t,
        ls_t100  LIKE LINE OF lt_t100,
        ls_t100t LIKE LINE OF lt_t100t,
        ls_t100a TYPE t100a.

  DATA: lv_msgid TYPE msgid,
        lv_rc    TYPE sysubrc,
        lv_objtype TYPE string,
        lv_checkexists TYPE char1,
        lv_len TYPE i.

*--- xml data objects
  DATA: lr_rootnode   TYPE REF TO if_ixml_element,
        lr_t100_node  TYPE REF TO if_ixml_element,
        lr_t100t_node TYPE REF TO if_ixml_element,
        lr_filter     TYPE REF TO if_ixml_node_filter,
        lr_iterator   TYPE REF TO if_ixml_node_iterator.

  lv_msgid = objname.
  lv_objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  lr_rootnode = xmldoc->find_from_name( lv_objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = lr_rootnode
    CHANGING
      structure = ls_t100a.

  objname = ls_t100a-arbgb.

  lv_checkexists = checkexists( ).
  IF lv_checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
      deleteobject( ).
    ENDIF.
  ENDIF.

*--- Read Elements for Messages
  FREE: lr_filter, lr_iterator, lr_t100_node, lr_t100t_node.
  lr_filter = xmldoc->create_filter_name( 't100' ).
  lr_iterator = xmldoc->create_iterator_filtered( lr_filter ).
  lr_t100_node ?= lr_iterator->get_next( ).

  WHILE lr_t100_node IS NOT INITIAL.
    CLEAR ls_t100.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = lr_t100_node
      CHANGING
        structure = ls_t100.
    APPEND ls_t100 TO lt_t100.
    lr_t100_node ?= lr_iterator->get_next( ).
  ENDWHILE.

*--- Create Elements for Texts
  FREE: lr_filter, lr_iterator, lr_t100_node, lr_t100t_node.
  lr_filter = xmldoc->create_filter_name( 't100t' ).
  lr_iterator = xmldoc->create_iterator_filtered( lr_filter ).
  lr_t100t_node ?= lr_iterator->get_next( ).

  WHILE lr_t100t_node IS NOT INITIAL.
    CLEAR ls_t100t.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = lr_t100t_node
      CHANGING
        structure = ls_t100t.
    APPEND ls_t100t TO lt_t100t.
    lr_t100t_node ?= lr_iterator->get_next( ).
  ENDWHILE.

  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      mode         = 'FREE'
      object       = lv_msgid
      object_class = 'T100'.

*--- Check permission
  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      authority_check          = 'X'
      global_lock              = 'X'
      mode                     = 'INSERT'
      language_upd_exit        = 'UPDATE_MASTER'
      master_language          = ls_t100a-masterlang
      object                   = lv_msgid
      object_class             = 'T100'
      suppress_language_check  = ' '
    EXCEPTIONS
      canceled_in_corr         = 01
      enqueued_by_user         = 02
      enqueue_system_failure   = 03
      illegal_parameter_values = 04
      locked_by_author         = 05
      no_modify_permission     = 06
      no_show_permission       = 07
      permission_failure       = 08.
  CASE sy-subrc.
    WHEN 0.
    WHEN 2 OR 5.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>locked.
    WHEN 6 OR 7 OR 8 OR 9.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>not_authorized.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>system_error.
  ENDCASE.

  CALL FUNCTION 'RS_CORR_INSERT'
       EXPORTING
            author              = sy-uname
            global_lock         = 'X'
            object              = objname
            object_class        = 'T100'
            devclass            = devclass
            master_language     = sy-langu
            mode                = 'INSERT'
*       IMPORTING
*            AUTHOR              = UNAME
*            KORRNUM             = CORRNUMBER_LOCAL
*            DEVCLASS            = DEVCLASS_LOCAL
       EXCEPTIONS
            cancelled           = 1
            permission_failure  = 2
            unknown_objectclass = 3.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_authorized.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

*--- update message tables
  CLEAR: ls_t100a-applclass.

  ls_t100a-lastuser = sy-uname.
  ls_t100a-ldate = sy-datum.
  ls_t100a-ltime = sy-uzeit.

  MODIFY t100a FROM ls_t100a.
  MODIFY t100 FROM TABLE lt_t100.
  MODIFY t100t FROM TABLE lt_t100t.

  CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
    EXPORTING
      object    = lv_msgid
      operation = 'INSERT'
      type      = 'CN'.

  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      mode         = 'FREE'
      object       = lv_msgid
      object_class = 'T100'.

  name = objname.
ENDMETHOD.


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
*      John Patterson
*      patterjo@gmail.com
endmethod.


METHOD getobjecttype .
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
*      John Patterson
*      patterjo@gmail.com

  objecttype = 'MSAG'. "Message Class
ENDMETHOD.
ENDCLASS.
