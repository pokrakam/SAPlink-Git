class ZSAPLINK_WD_CONFIG_APPL definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.
  type-pools WDYN .
  type-pools WDYWB .

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



CLASS ZSAPLINK_WD_CONFIG_APPL IMPLEMENTATION.


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



  exists = abap_true.

  DATA: config_id TYPE WDY_CONFIG_ID.
  config_id = objname.
  TRANSLATE config_id TO UPPER CASE.

  SELECT SINGLE config_id FROM wdy_config_appl INTO config_id
    WHERE config_id = config_id.
  IF sy-subrc NE 0.
    CLEAR exists.
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


*xml nodes
  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA wdy_config_appl_node   TYPE REF TO if_ixml_element.
  DATA wdy_config_appt_node   TYPE REF TO if_ixml_element.
  DATA rc         TYPE sysubrc.

  DATA wdy_config_appl_table TYPE STANDARD TABLE OF wdy_config_appl.
  DATA wdy_config_appt_table TYPE STANDARD TABLE OF wdy_config_appt.
  FIELD-SYMBOLS <wdy_config_appl> TYPE wdy_config_appl.
  FIELD-SYMBOLS <wdy_config_appt> TYPE wdy_config_appt.

  DATA: config_id TYPE wdy_config_id.
  config_id = objname.
  TRANSLATE config_id TO UPPER CASE.

  SELECT * FROM wdy_config_appl INTO TABLE wdy_config_appl_table
    WHERE config_id = config_id.
  SELECT * FROM wdy_config_appt INTO TABLE wdy_config_appt_table
    WHERE config_id = config_id.

* Create parent node
  DATA _objtype TYPE string.
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  rc = rootnode->set_attribute( name = 'OBJNAME' value = objname ).

* copy wdy_config_appl
  LOOP AT wdy_config_appl_table ASSIGNING <wdy_config_appl>.
    wdy_config_appl_node = xmldoc->create_element( 'wdy_config_appl' ).
    setattributesfromstructure( node = wdy_config_appl_node structure = <wdy_config_appl> ).
    rc = rootnode->append_child( wdy_config_appl_node ).
  ENDLOOP.

* copy wdy_config_appt
  LOOP AT wdy_config_appt_table ASSIGNING <wdy_config_appt>.
    wdy_config_appt_node = xmldoc->create_element( 'wdy_config_appt' ).
    setattributesfromstructure( node = wdy_config_appt_node structure = <wdy_config_appt> ).
    rc = rootnode->append_child( wdy_config_appt_node ).
  ENDLOOP.

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
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
*
*
*xml nodes
  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA wdy_config_appl_node   TYPE REF TO if_ixml_element.
  DATA wdy_config_appt_node   TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA rc          TYPE sysubrc.
  DATA _devclass   TYPE devclass.
  DATA checkexists TYPE flag.
  DATA _objtype    TYPE string.


  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).



  DATA wdy_config_appl_table TYPE STANDARD TABLE OF wdy_config_appl.
  DATA wdy_config_appt_table TYPE STANDARD TABLE OF wdy_config_appt.
  DATA wdy_config_appl TYPE wdy_config_appl.
  DATA wdy_config_appt TYPE wdy_config_appt.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_config_appl' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_config_appl.
    APPEND wdy_config_appl TO wdy_config_appl_table.
    node ?= iterator->get_next( ).
  ENDWHILE.

  objname = wdy_config_appl-config_id.
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

  MODIFY wdy_config_appl FROM TABLE wdy_config_appl_table.

  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_config_appt' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_config_appt.
    APPEND wdy_config_appt TO wdy_config_appt_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_config_appt FROM TABLE wdy_config_appt_table.

  COMMIT WORK.
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
  DATA: config_id TYPE wdy_config_id.
  config_id = objname.
  TRANSLATE config_id TO UPPER CASE.
  DELETE FROM wdy_config_appl WHERE config_id = config_id.
  DELETE FROM wdy_config_appt WHERE config_id = config_id.

  COMMIT WORK.

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


  objecttype = 'WDCA'.

endmethod.
ENDCLASS.
