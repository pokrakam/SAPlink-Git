class ZSAPLINK_DOCUMENTATION definition
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



CLASS ZSAPLINK_DOCUMENTATION IMPLEMENTATION.


METHOD checkexists.
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
*      Rich Heilman
*      rich.heilman.jr@gmail.com
  DATA xdokhl TYPE dokhl.
  DATA xdoc_object(62) TYPE c.
  DATA xdocu_id TYPE dokhl-id.
  DATA xdocu_obj TYPE dokhl-object.

  xdoc_object = objname.
  xdocu_id  = xdoc_object(2).
  xdocu_obj = xdoc_object+2(60).

  CLEAR xdokhl.
  SELECT SINGLE * INTO xdokhl FROM dokhl
             WHERE id = xdocu_id
               AND object = xdocu_obj.
  IF sy-subrc =  0.
    exists = 'X'.
  ENDIF.

ENDMETHOD.


METHOD createixmldocfromobject.
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
*      Rich Heilman
*      rich.heilman.jr@gmail.com

  DATA root_node     TYPE REF TO if_ixml_element.
  DATA txtlines_node TYPE REF TO if_ixml_element.
  DATA rc            TYPE sysubrc.
  DATA _objtype      TYPE string.

  DATA: BEGIN OF xdokhl,
        objname(62) TYPE c,
        id          TYPE dokhl-id,
        object      TYPE dokhl-object,
        typ         TYPE dokhl-typ,
        dokversion  TYPE dokhl-dokversion,
        END OF xdokhl.

  DATA ilines TYPE TABLE OF tline.
  DATA xlines LIKE LINE OF ilines.

  DATA _docuname TYPE e071-obj_name.
  DATA xdocu_id  TYPE dokhl-id.
  DATA xdocu_obj TYPE dokhl-object.

  _docuname = objname.

* Separate id and object from combined object name
  xdocu_id  = _docuname(2).
  xdocu_obj = _docuname+2(60).

* Check against database
  CLEAR xdokhl.
  SELECT SINGLE id object typ dokversion
        INTO (xdokhl-id, xdokhl-object, xdokhl-typ, xdokhl-dokversion)
           FROM dokhl
             WHERE id = xdocu_id
                AND object = xdocu_obj.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
     EXPORTING textid = zcx_saplink=>error_message
               msg    = `Documentation object not found, use full ` &
                        `object name including ID and Object(example DTZTEST`.
  ENDIF.

 xdokhl-objname = _docuname.

* Create parent node
  _objtype  = getobjecttype( ).
  root_node = xmldoc->create_element( _objtype ).
  me->setattributesfromstructure( node = root_node structure = xdokhl  ).

* Read the documentation text
  CALL FUNCTION 'DOCU_READ'
    EXPORTING
      id      = xdokhl-id
      langu   = sy-langu
      object  = xdokhl-object
      typ     = xdokhl-typ
      version = xdokhl-dokversion
    TABLES
      line    = ilines.

* Write records to XML node
  LOOP AT ilines INTO xlines.
    txtlines_node = xmldoc->create_element( `TextLines` ).
    me->setattributesfromstructure( node = txtlines_node structure = xlines ).
    rc = root_node->append_child( txtlines_node ).
  ENDLOOP.

* Add node
  rc = xmldoc->append_child( root_node ).
  ixmldocument = xmldoc.

ENDMETHOD.


METHOD createobjectfromixmldoc.
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
*      Rich Heilman
*      rich.heilman.jr@gmail.com

  DATA root_node        TYPE REF TO if_ixml_element.
  DATA txtline_node     TYPE REF TO if_ixml_element.
  DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
  DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

  DATA: BEGIN OF xdokhl,
        objname(62) TYPE c,
        id          TYPE dokhl-id,
        object      TYPE dokhl-object,
        typ         TYPE dokhl-typ,
        dokversion  TYPE dokhl-dokversion,
        END OF xdokhl.

  DATA: ilines  TYPE TABLE OF tline.
  DATA: objname TYPE e071-obj_name.
  DATA _objtype TYPE string.

  FIELD-SYMBOLS: <xlines_wa> LIKE LINE OF ilines.

  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  root_node = xmldoc->find_from_name( _objtype ).

* Get Root
  me->getstructurefromattributes(
          EXPORTING  node      = root_node
          CHANGING   structure = xdokhl ).

  objname = xdokhl-objname.

* Get TextLines from XML
  FREE: txtline_filter, txtline_iterator, txtline_node.
  txtline_filter = xmldoc->create_filter_name( `TextLines` ).
  txtline_iterator = xmldoc->create_iterator_filtered( txtline_filter ).
  txtline_node ?= txtline_iterator->get_next( ).
  WHILE txtline_node IS NOT INITIAL.
    APPEND INITIAL LINE TO ilines ASSIGNING <xlines_wa>.
    me->getstructurefromattributes(
            EXPORTING   node      = txtline_node
            CHANGING    structure = <xlines_wa> ).
    txtline_node ?= txtline_iterator->get_next( ).
  ENDWHILE.

  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      global_lock              = 'X'
      mode                     = 'INSERT'
      object                   = objname
      object_class             = 'DOCV'
    EXCEPTIONS
      canceled_in_corr         = 1
      enqueued_by_user         = 3
      enqueue_system_failure   = 4
      locked_by_author         = 5
      illegal_parameter_values = 6
      no_modify_permission     = 7
      no_show_permission       = 8
      permission_failure       = 9.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 7 OR 8 OR 9.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_authorized.
      WHEN 5.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'object locked'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

  DATA checkexists TYPE flag.
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

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object              = objname
      object_class        = 'DOCV'
      mode                = 'INSERT'
      global_lock         = 'X'
*     devclass            = devclass
      author              = sy-uname
      master_language     = sy-langu
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

  CALL FUNCTION 'DOCU_UPD'
    EXPORTING
      id       = xdokhl-id
      langu    = sy-langu
      object   = xdokhl-object
      typ      = xdokhl-typ
    TABLES
      line     = ilines
    EXCEPTIONS
      ret_code = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
     EXPORTING textid = zcx_saplink=>error_message
               msg    = `Documentation object import failed`.
  ENDIF.

  name = objname.

ENDMETHOD.


METHOD deleteobject.
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
*      Rich Heilman
*      rich.heilman.jr@gmail.com
  DATA xdokhl TYPE dokhl.
  DATA _docuname TYPE e071-obj_name.
  DATA xdocu_id  TYPE dokhl-id.
  DATA xdocu_obj TYPE dokhl-object.

  _docuname = objname.

  xdocu_id  = _docuname(2).
  xdocu_obj = _docuname+2(60).

  CLEAR xdokhl.
  SELECT SINGLE * INTO xdokhl
           FROM dokhl
             WHERE id = xdocu_id
               AND object = xdocu_obj.

  CALL FUNCTION 'DOCU_DEL'
    EXPORTING
      id       = xdokhl-id
      langu    = sy-langu
      object   = xdokhl-object
      typ      = xdokhl-typ
    EXCEPTIONS
      ret_code = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
     EXPORTING textid = zcx_saplink=>error_message
               msg    = `Error when deleting existing documentation object`.
  ENDIF.

ENDMETHOD.


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
*      Rich Heilman
*      rich.heilman.jr@gmail.com

  objecttype = 'DOCV'.  "Documentation

endmethod.
ENDCLASS.
