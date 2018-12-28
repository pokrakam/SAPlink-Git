class ZSAPLINK_WF_TEMPLATE definition
  public
  inheriting from ZSAPLINK_WF
  create public .

public section.
protected section.

  methods AFTER_SAVE
    redefinition .
  methods FILL_ROOT_NODE
    redefinition .
  methods GET_OBJECT_DESCRIPTION
    redefinition .
  methods GET_OTYPE
    redefinition .
private section.

  constants C_DEF_NODE_NAME type STRING value 'wf_definition' ##NO_TEXT.
ENDCLASS.



CLASS ZSAPLINK_WF_TEMPLATE IMPLEMENTATION.


METHOD after_save.

  DATA: ls_wfdkey TYPE swd_wfdkey,
        ls_object TYPE hrsobject,

        msg TYPE string,
        name TYPE string,
        len TYPE i,
        ind TYPE i,

        l_okcode TYPE swd_data-okcode,
        l_return TYPE swd_return,

        lt_hrs1000 TYPE TABLE OF hrs1000,

        l_task TYPE  swd_step_t,
        ls_head TYPE swd_ahead,

        lo_wfd_xml TYPE REF TO cl_xml_document_base,
        lo_wfd_import TYPE REF TO if_swf_pdef_import,
        lo_node TYPE REF TO if_ixml_node,
        lo_node_next TYPE REF TO if_ixml_node,
        lo_subdoc TYPE REF TO if_ixml_document,
        lo_ixml TYPE REF TO if_ixml.

  FIELD-SYMBOLS: <hrs1000> TYPE hrs1000.
  ls_object-otype = get_otype( ).
  ls_object-objid = get_objid( ).

  l_task = ls_object.
  CALL FUNCTION 'SWD_WORKFLOW_CREATE'
    EXPORTING
      im_task = l_task
    EXCEPTIONS
      OTHERS  = 0.

  lo_node = i_xmldoc->find_from_name( c_def_node_name ).
  WHILE lo_node IS BOUND.
    name = lo_node->get_name( ).
    lo_node_next = lo_node->get_next( ).

    IF name = c_def_node_name.
      IF lo_ixml IS NOT BOUND.
        lo_ixml = cl_ixml=>create( ).
      ENDIF.

      lo_subdoc = lo_ixml->create_document( ).
      lo_subdoc->append_child( lo_node ).

      CREATE OBJECT lo_wfd_xml
        EXPORTING
          document = lo_subdoc.

      l_okcode = 'NVRS'. "new viewrion
      PERFORM ssc_wd_store IN PROGRAM saplswdd
                  CHANGING
                     l_okcode
                     l_return.

      IF lo_wfd_import IS NOT BOUND.
        CREATE OBJECT lo_wfd_import TYPE cl_wfd_convert_ixml_to_def.
      ENDIF.

      lo_wfd_import->convert( xml_document  = lo_wfd_xml language = sy-langu ).
    ENDIF.

    lo_node = lo_node_next.
  ENDWHILE.

  CALL FUNCTION 'SWD_WORKFLOW_STORE'
    EXPORTING
      im_force_gen = abap_true
    IMPORTING
      ex_wfdkey    = ls_wfdkey
    EXCEPTIONS
      OTHERS       = 4.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'Error when saving Workflow definition ' && l_task.
  ENDIF.

  COMMIT WORK.

ENDMETHOD.


METHOD fill_root_node.
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
*      Sergey Korolev
*      slkorolev@gmail.com

  DATA: ls_wfdkey TYPE swd_wfdkey,

        lo_wfd_xml TYPE REF TO cl_xml_document_base,
        lo_wfd_export TYPE REF TO if_swf_pdef_export,
        lt_versions TYPE TABLE OF swd_versns,

        lo_node TYPE REF TO if_ixml_element.

  FIELD-SYMBOLS: <version> TYPE swd_versns.

  super->fill_root_node( i_node = i_node
                         is_object = is_object ).

  CALL FUNCTION 'SWD_GET_VERSIONS_OF_WORKFLOW'
    EXPORTING
      im_task          = is_object
      im_exetyp        = 'S'
    IMPORTING
      ex_active_wfdkey = ls_wfdkey
    TABLES
      ex_versions      = lt_versions.

  CREATE OBJECT lo_wfd_export TYPE cl_wfd_convert_def_to_ixml.

  SORT lt_versions BY version.
  LOOP AT lt_versions ASSIGNING <version>.

    ls_wfdkey-version = <version>-version.
    lo_wfd_xml = lo_wfd_export->convert( load_from_db = abap_true
                                         language = sy-langu
                                         wfd_key = ls_wfdkey ).
    lo_node = me->append_structure_node( i_node = i_node
                                         i_name    = c_def_node_name
                                         is_struct = ls_wfdkey ).

    lo_node->append_child( lo_wfd_xml->get_first_node( ) ).
    i_node->append_child( lo_node ).
  ENDLOOP.

  r_node = i_node.
ENDMETHOD.


METHOD get_object_description.
  r_value = 'Workflow template'.
ENDMETHOD.


METHOD GET_OTYPE.
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
*      Sergey Korolev
*      slkorolev@gmail.com

  r_otype = 'WS'.
ENDMETHOD.
ENDCLASS.
