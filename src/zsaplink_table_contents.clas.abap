class ZSAPLINK_TABLE_CONTENTS definition
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



CLASS ZSAPLINK_TABLE_CONTENTS IMPLEMENTATION.


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

* No implementation

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

  TYPES: BEGIN OF ttabname,
          tabname TYPE dd02v-tabname,
         END OF ttabname.

  DATA xtabname     TYPE ttabname.
  DATA xdd02v       TYPE dd02v.

  DATA root_node    TYPE REF TO if_ixml_element.
  DATA datarow_node TYPE REF TO if_ixml_element.
  DATA rc           TYPE sysubrc.
  DATA _tablname    TYPE ddobjname.
  DATA _objtype     TYPE string.

  DATA dref_tab TYPE REF TO data.
  DATA dref_wa  TYPE REF TO data.

  FIELD-SYMBOLS: <dyn_tab> TYPE table.
  FIELD-SYMBOLS: <dyn_wa>  TYPE ANY.

* Check that table exits.
  _tablname = objname.

* Does the table exist?
  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = _tablname
    IMPORTING
      dd02v_wa      = xdd02v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0 OR xdd02v-tabname IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>error_message
                msg    = `Table not found`.
  ENDIF.

* Create parent node
  _objtype  = getobjecttype( ).
  root_node = xmldoc->create_element( _objtype ).
  xtabname-tabname = xdd02v-tabname.
  me->setattributesfromstructure( node = root_node structure = xtabname  ).

* Create dynamic internal table and work area
  CREATE DATA dref_tab TYPE TABLE OF (xdd02v-tabname).
  ASSIGN dref_tab->* TO <dyn_tab>.
  CREATE DATA dref_wa LIKE LINE OF <dyn_tab>.
  ASSIGN dref_wa->* TO <dyn_wa>.

* Select all data
  SELECT * INTO TABLE <dyn_tab> FROM (xdd02v-tabname).

* Write records to XML node
  LOOP AT <dyn_tab> ASSIGNING <dyn_wa>.
    datarow_node = xmldoc->create_element( `DataRow` ).
    me->setattributesfromstructure( node = datarow_node structure = <dyn_wa> ).
    rc = root_node->append_child( datarow_node ).
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

  TYPES: BEGIN OF ttabname,
          tabname TYPE dd02v-tabname,
         END OF ttabname.

  DATA xtabname    TYPE ttabname.
  DATA xdd02v      TYPE dd02v.
  DATA xtadir      TYPE tadir.

  DATA idd03p TYPE TABLE OF dd03p.
  DATA xdd03p LIKE LINE OF idd03p.

  DATA root_node        TYPE REF TO if_ixml_element.
  DATA datarow_node     TYPE REF TO if_ixml_element.
  DATA datarow_filter   TYPE REF TO if_ixml_node_filter.
  DATA datarow_iterator TYPE REF TO if_ixml_node_iterator.

  DATA _objtype           TYPE string.
  DATA l_answer           TYPE string.
  DATA l_nameclass        TYPE c.
  DATA l_client_dependent TYPE abap_bool.

  DATA dref_tab TYPE REF TO data.
  DATA dref_wa  TYPE REF TO data.

  FIELD-SYMBOLS: <dyn_tab>  TYPE table.
  FIELD-SYMBOLS: <dyn_wa>   TYPE ANY.
  FIELD-SYMBOLS: <fs_mandt> TYPE ANY.

  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  root_node = xmldoc->find_from_name( _objtype ).

* Get table name from XML.
  me->getstructurefromattributes(
          EXPORTING  node      = root_node
          CHANGING   structure = xtabname ).

  objname = xtabname-tabname.

* Check that table exists
  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = xtabname-tabname
    IMPORTING
      dd02v_wa      = xdd02v
    TABLES
      dd03p_tab     = idd03p
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0 OR xdd02v-tabname IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>error_message
                msg    = `Table not found`.
  ENDIF.

* Check for MANDT field, if found, then set client dependent
  READ TABLE idd03p INTO xdd03p WITH KEY fieldname = 'MANDT'.
  IF sy-subrc = 0.
    l_client_dependent = abap_true.
  ENDIF.

* Only allow tables in customer namespace
  CLEAR xtadir.
  SELECT SINGLE * FROM tadir INTO xtadir
              WHERE pgmid    = 'R3TR'
                AND object   = 'TABL'
                AND obj_name = xdd02v-tabname.
  CALL FUNCTION 'TRINT_OBJECT_NAMESPACE_INFO'
    EXPORTING
      iv_pgmid               = xtadir-pgmid
      iv_object              = xtadir-object
      iv_objname             = xtadir-obj_name
    IMPORTING
      ev_nameclass           = l_nameclass
    EXCEPTIONS
      namespace_not_existing = 1
      namespace_use_rejected = 2
      invalid_object         = 3
      OTHERS                 = 4.
  IF l_nameclass <> `C`.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>error_message
                msg    = 'Table is not within customer namespace'.
  ENDIF.

* Create dynamic internal table and work area
  CREATE DATA dref_tab TYPE TABLE OF (xdd02v-tabname).
  ASSIGN dref_tab->* TO <dyn_tab>.
  CREATE DATA dref_wa LIKE LINE OF <dyn_tab>.
  ASSIGN dref_wa->* TO <dyn_wa>.

* Build dynamic internal table from XML
  FREE: datarow_filter, datarow_iterator, datarow_node.
  datarow_filter = xmldoc->create_filter_name( `DataRow` ).
  datarow_iterator = xmldoc->create_iterator_filtered( datarow_filter ).
  datarow_node ?= datarow_iterator->get_next( ).
  WHILE datarow_node IS NOT INITIAL.
    APPEND INITIAL LINE TO <dyn_tab> ASSIGNING <dyn_wa>.
    me->getstructurefromattributes(
            EXPORTING   node      = datarow_node
            CHANGING    structure = <dyn_wa> ).
    datarow_node ?= datarow_iterator->get_next( ).
  ENDWHILE.

* Any records imported from XML, if not, give error.
  IF LINES( <dyn_tab> ) = 0.
    RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>error_message
                  msg    = `No data records present in XML document`.
  ENDIF.

* Change MANDT field to current client number
* Always add imported records to current client number
  IF l_client_dependent = abap_true.
    LOOP AT <dyn_tab> ASSIGNING <dyn_wa>.
      ASSIGN COMPONENT `MANDT` OF STRUCTURE <dyn_wa> TO <fs_mandt>.
      <fs_mandt> = sy-mandt.
    ENDLOOP.
  ENDIF.

* Check that db table is initial, if so, then insert data and exit
  DATA: l_count TYPE i.
  CASE l_client_dependent .
    WHEN abap_true.
      SELECT COUNT( * )  INTO l_count
             FROM (xdd02v-tabname) CLIENT SPECIFIED
                     WHERE mandt = sy-mandt.
    WHEN abap_false.
      SELECT COUNT( * )  INTO l_count
             FROM (xdd02v-tabname).
  ENDCASE.
  IF l_count = 0.
    INSERT (xdd02v-tabname) FROM TABLE <dyn_tab>.
    name = objname.
    RETURN.
  ENDIF.

* Still here, then ask user how he wants to handle the existing
* data, either modify it, of delete/insert
  DATA: text_question TYPE string.

  text_question = `Table contains data which may be modified, ` &
                  `would you like to modify existing records, ` &
                  `or delete existing data first and insert`.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = text_question
      text_button_1  = 'Modify Data'      " UPdate table via MODIFY
      icon_button_1  = 'ICON_CHANGE'
      text_button_2  = 'Del/Ins Data'     " Delete data first, then INSERT
      icon_button_2  = 'ICON_DELETE'
    IMPORTING
      answer         = l_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.
* Check answer
  CASE l_answer .
    WHEN  '1'.   "Modify existing rows, insert new rows based on key
      MODIFY (xdd02v-tabname) FROM TABLE <dyn_tab>.
    WHEN  '2'.   "Delete existing data first, then insert new data
      CASE l_client_dependent .
        WHEN abap_true.
          DELETE FROM (xdd02v-tabname) WHERE mandt = sy-mandt.
        WHEN abap_false.
          DELETE FROM (xdd02v-tabname).
      ENDCASE.
      INSERT (xdd02v-tabname) FROM TABLE <dyn_tab>.
    WHEN  'A'.   "Action has been cancelled
      RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>error_message
                msg    = `Action Cancelled`.
  ENDCASE.

  name = objname.

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
*      Rich Heilman
*      rich.heilman.jr@gmail.com

* No implementation

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
*      Rich Heilman
*      rich.heilman.jr@gmail.com

  objecttype = 'TABU'.  "Table Contents

endmethod.
ENDCLASS.
