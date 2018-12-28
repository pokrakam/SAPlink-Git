class ZSAPLINK_WF definition
  public
  inheriting from ZSAPLINK
  abstract
  create public .

public section.

  constants C_SWF_PERS_CLASS type CLASSNAME value 'CL_SWF_CNT_HRS_PERSISTENCE' ##NO_TEXT.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.

  data GO_CONTAINER type ref to IF_SWF_CNT_CONTAINER .
  data GS_OBJECT type HRSOBJECT .
  data G_MSG type STRING .

  methods AFTER_SAVE
    importing
      !I_XMLDOC type ref to IF_IXML_DOCUMENT
      !I_DEVCLASS type DEVCLASS default '$TMP'
    raising
      ZCX_SAPLINK .
  methods BEFORE_SAVE
    importing
      !I_XMLDOC type ref to IF_IXML_DOCUMENT
      !I_DEVCLASS type DEVCLASS default '$TMP' .
  methods CREATE_WF_CNT_INSTANCE
    returning
      value(R_INSTANCE) type ref to IF_SWF_CNT_CONTAINER .
  methods GET_OBJECT_DESCRIPTION
  abstract
    returning
      value(R_VALUE) type STRING .
  methods GET_WF_CNT_INSTANCE
    returning
      value(R_INSTANCE) type ref to IF_SWF_CNT_CONTAINER .
  methods APPEND_STRUCTURE_NODE
    importing
      !I_NODE type ref to IF_IXML_ELEMENT
      !I_NAME type STRING
      !IS_STRUCT type DATA
    returning
      value(R_NODE) type ref to IF_IXML_ELEMENT .
  methods APPEND_TABLE_NODE
    importing
      !I_NODE type ref to IF_IXML_ELEMENT
      !I_NAME type STRING
      !IT type ANY TABLE
    returning
      value(R_NODE) type ref to IF_IXML_ELEMENT .
  methods FILL_ROOT_NODE
    importing
      !I_NODE type ref to IF_IXML_ELEMENT
      !IS_OBJECT type HRSOBJECT
    returning
      value(R_NODE) type ref to IF_IXML_ELEMENT .
  methods GET_OBJID
    returning
      value(R_OBJID) type HRSOBJECT-OBJID .
  methods GET_OTYPE
  abstract
    returning
      value(R_OTYPE) type HRSOBJECT-OTYPE .
  methods GET_STRUCTURE_FROM_NODE
    importing
      !I_NODE type ref to IF_IXML_ELEMENT optional
      !I_NAME type STRING
    changing
      !CS_STRUCT type DATA .
  methods GET_TABLE_FROM_NODE
    importing
      !I_NODE type ref to IF_IXML_ELEMENT optional
      !I_NAME type STRING
    changing
      !CT type STANDARD TABLE .

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
ENDCLASS.



CLASS ZSAPLINK_WF IMPLEMENTATION.


method AFTER_SAVE.
endmethod.


METHOD APPEND_STRUCTURE_NODE.
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

  CHECK i_node IS BOUND.

  r_node = xmldoc->create_element( i_name ).
  i_node->append_child( r_node ).

  setattributesfromstructure( node = r_node
                              structure = is_struct ).
ENDMETHOD.


METHOD APPEND_TABLE_NODE.
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

  FIELD-SYMBOLS: <wa> TYPE any.

  DATA: item TYPE REF TO if_ixml_element,
        attr TYPE REF TO if_ixml_attribute.

  CHECK i_node IS BOUND AND lines( it ) > 0.

  r_node = xmldoc->create_element( i_name ).
  i_node->append_child( r_node ).

  LOOP AT it ASSIGNING <wa>.
    r_node->append_child( append_structure_node( i_node    = r_node
                                                 i_name    = 'item'
                                                 is_struct = <wa> ) ).
  ENDLOOP.

ENDMETHOD.


method BEFORE_SAVE.
endmethod.


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
*      Sergey Korolev
*      slkorolev@gmail.com

  DATA: ls_object TYPE hrsobject.

  ls_object-otype = get_otype( ).
  ls_object-objid = get_objid( ).

  CALL FUNCTION 'RH_CHECK_HRS_OBJECT_EXISTS'
    EXPORTING
      act_otype        = ls_object-otype
      act_objid        = ls_object-objid
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    CLEAR exists.
  ELSE.
    exists = abap_true.
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
*      Sergey Korolev
*      slkorolev@gmail.com

  DATA: ls_object TYPE hrsobject,
        ls_objec TYPE objec,
        l_tabname TYPE tabname,
        l_tabname_str TYPE string,
        lt_infty TYPE TABLE OF t787i-infty,

        ls_hrs1203 TYPE hrs1203,
        ls_hrs1216 TYPE hrscompdev,

        lo_container TYPE REF TO if_swf_cnt_container,
        lo_cnt_xml TYPE REF TO if_ixml_document,

        l_root_node TYPE REF TO if_ixml_element,

        lrt_infty_data TYPE REF TO data.

  DATA: task_por TYPE sibflpor.

  FIELD-SYMBOLS: <lt_infty> TYPE STANDARD TABLE,
                 <infty> TYPE t787i-infty.

  ls_objec-otype = ls_object-otype = get_otype( ).
  ls_objec-objid = ls_object-objid = get_objid( ).

  l_root_node = xmldoc->create_element( getobjecttype( ) ).

  SELECT DISTINCT infty FROM  t787i INTO TABLE lt_infty
         WHERE  otype  = ls_object-otype.

  DELETE lt_infty WHERE table_line = '1210' OR
                        table_line = '1211' OR
                        table_line = '1216' OR
                        table_line = '1001'.

  l_tabname(3) = 'HRS'.
  LOOP AT lt_infty ASSIGNING <infty>.

    l_tabname+3 = <infty>.

    UNASSIGN <lt_infty>.

    CREATE DATA lrt_infty_data TYPE STANDARD TABLE OF (l_tabname).
    ASSIGN lrt_infty_data->* TO <lt_infty>.

    CASE <infty>.
      WHEN '1000'.
        CALL FUNCTION 'RH_READ_HRS1000'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          TABLES
            act_hrs1000   = <lt_infty>
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
        ENDIF.

      WHEN '1002'.
        CALL FUNCTION 'RH_READ_HRS1002'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          TABLES
            act_hrs1002   = <lt_infty>
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
        ENDIF.

      WHEN '1200'.
        CALL FUNCTION 'RH_READ_HRS1200'
          EXPORTING
            act_otype        = ls_object-otype
            act_objid        = ls_object-objid
            rel_object_check = space
          TABLES
            act_hrs1200      = <lt_infty>
          EXCEPTIONS
            no_data_found    = 1
            OTHERS           = 2.

        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

      WHEN '1201'.
        DATA: ls_hrs1201 TYPE hrs1201.
        CALL FUNCTION 'RH_READ_HRS1201'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          IMPORTING
            act_hrs1201   = ls_hrs1201
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc = 0.
          APPEND ls_hrs1201 TO <lt_infty>.
        ENDIF.

      WHEN '1202'.
        CALL FUNCTION 'RH_READ_HRS1202'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          TABLES
            act_hrs1202   = <lt_infty>
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
        ENDIF.

      WHEN '1203'.
        CALL FUNCTION 'RH_READ_HRS1203'
          EXPORTING
            act_otype   = ls_object-otype
            act_objid   = ls_object-objid
          IMPORTING
            act_hrs1203 = ls_hrs1203
          EXCEPTIONS
            OTHERS      = 2.

        IF sy-subrc = 0.
          APPEND ls_hrs1203 TO <lt_infty>.
        ENDIF.

      WHEN '1205'.
        CALL FUNCTION 'RH_READ_HRS1205'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          TABLES
            act_hrs1205   = <lt_infty>
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
        ENDIF.

      WHEN '1206'.
        CALL FUNCTION 'RH_READ_HRS1206'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          TABLES
            act_hrs1206   = <lt_infty>
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
          " Implement suitable error handling here
        ENDIF.

      WHEN '1212'.
        CALL FUNCTION 'RH_READ_HRS1212'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          TABLES
            act_hrs1212   = <lt_infty>
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
          " Implement suitable error handling here
        ENDIF.

      WHEN '1213'.
        CALL FUNCTION 'RH_READ_HRS1213'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          TABLES
            act_hrs1213   = <lt_infty>
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
          "* Implement suitable error handling here
        ENDIF.

      WHEN '1214'.
        CALL FUNCTION 'RH_READ_HRS1214'
          EXPORTING
            act_otype     = ls_object-otype
            act_objid     = ls_object-objid
          TABLES
            act_hrs1214   = <lt_infty>
          EXCEPTIONS
            no_data_found = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
          "* Implement suitable error handling here
        ENDIF.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    l_tabname_str = l_tabname.
    me->append_table_node( i_node = l_root_node
                           i_name = l_tabname_str
                           it     = <lt_infty> ).

  ENDLOOP.

  lo_container = me->get_wf_cnt_instance( ).
  lo_container->to_xml( EXPORTING include_texts = 'X'
                                  include_extension_elements = 'X'
                                  include_initial_values = 'X'
                                  include_null_values = 'X'
                                  include_typenames = 'X'
                        IMPORTING xml_dom = lo_cnt_xml ).

  l_root_node->append_child( lo_cnt_xml->get_root_element( ) ).

  xmldoc->append_child( fill_root_node( i_node = l_root_node
                                        is_object = ls_object ) ).

  ixmldocument  = xmldoc.
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
*      Sergey Korolev
*      slkorolev@gmail.com

  DATA: ls_object TYPE hrsobject,
        ls_objec TYPE objec,
        l_tabname TYPE tabname,
        l_hrs_tabname TYPE tabname,
        l_tabname_str TYPE string,
        lt_infty TYPE TABLE OF t787i-infty,

        l_subroutine TYPE tabname,
        init_subrc TYPE sy-subrc,
        data_stored TYPE flag,
        l_fcode TYPE t77fc-fcode,

        l_object_guid32 TYPE guid_32,

        can_save TYPE abap_bool,

        l_langu TYPE sy-langu,

        l_root_node TYPE REF TO if_ixml_element,

        lrt_hrs_infty_data TYPE REF TO data,
        lrt_infty_data TYPE REF TO data.

  DATA: l_cnt_node TYPE REF TO if_ixml_element,
        lo_container TYPE REF TO if_swf_cnt_container,
        lo_cnt_xml TYPE REF TO if_ixml_document,
        l_subdoc TYPE REF TO if_ixml_document,
        l_ref_ixml TYPE REF TO if_ixml,
        ex TYPE REF TO cx_root.

  DATA: lt_i1000  TYPE TABLE OF hrs1000,
        ls_hrs1000 TYPE hrs1000,

        lt_i1002  TYPE TABLE OF hrs1002,
        lt_i1200  TYPE TABLE OF hrs1200,
        lt_i1201  TYPE TABLE OF hrs1201,

        lt_i1202  TYPE TABLE OF hrs1202,
        ls_i1202  TYPE hrs1202,

        lt_i1203  TYPE TABLE OF hrs1203,
        ls_i1203  TYPE hrs1203,

        lt_i1205  TYPE TABLE OF hrs1205,
        lt_i1206  TYPE TABLE OF hrs1206,
        lt_i1212  TYPE TABLE OF hrs1212,
        lt_i1213  TYPE TABLE OF hrs1213,
        lt_i1214  TYPE TABLE OF hrs1214,

        l_loc_tab_name TYPE tabname.

  DATA: task_por TYPE sibflpor.

  FIELD-SYMBOLS: <lt_hrs_infty> TYPE STANDARD TABLE,
                 <infty> TYPE t787i-infty.

  ls_objec-otype = ls_object-otype = get_otype( ).
  ls_objec-objid = ls_object-objid = get_objid( ).

  xmldoc = ixmldocument.

  SELECT DISTINCT infty FROM  t787i INTO TABLE lt_infty
         WHERE  otype  = ls_object-otype.

  DELETE lt_infty WHERE table_line = '1210' OR "old container
                        table_line = '1211' OR "old container
                        table_line = '1205' OR "WF definition version
                        table_line = '1216' OR
                        table_line = '1001'.

  l_tabname(3) = 'HRS'.
  l_hrs_tabname(4) = 'LT_I'.
  LOOP AT lt_infty ASSIGNING <infty>.

    l_tabname+3 = <infty>.
    l_hrs_tabname+4 = <infty>.

    UNASSIGN: <lt_hrs_infty>.
    ASSIGN (l_hrs_tabname) TO <lt_hrs_infty>.
    CHECK <lt_hrs_infty> IS ASSIGNED.

    l_tabname_str = l_tabname.
    me->get_table_from_node( EXPORTING i_name = l_tabname_str
                             CHANGING  ct     = <lt_hrs_infty> ).

  ENDLOOP.

  IF checkexists( ) = abap_false.
    CALL FUNCTION 'RH_HRSOBJECT_CORR_INSERT'
      EXPORTING
        act_otype    = ls_object-otype
        act_objid    = ls_object-objid
        act_mode     = ' '
        act_devclass = devclass
      IMPORTING
        act_maint    = can_save
      EXCEPTIONS
        OTHERS       = 4.

    IF sy-subrc NE 0.

      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO g_msg.

      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          msg = g_msg.

    ENDIF.

  ENDIF.

  READ TABLE lt_i1000 INTO ls_hrs1000 WITH KEY langu = sy-langu.
  IF sy-subrc = 0.
    DELETE lt_i1000 INDEX sy-tabix.
    INSERT ls_hrs1000 INTO lt_i1000 INDEX 1.
  ENDIF.

  CALL FUNCTION 'RH_HRSOBJECT_SAVE_FROM_EXPORT'
    EXPORTING
      act_hrs_otype           = ls_object-otype
      act_hrs_objid           = ls_object-objid
      act_langu               = sy-langu
      update_database         = abap_false
    TABLES
      i1000                   = lt_i1000
      i1002                   = lt_i1002
      i1200                   = lt_i1200
      i1201                   = lt_i1201
      i1205                   = lt_i1205
      i1206                   = lt_i1206
      i1212                   = lt_i1212
      i1213                   = lt_i1213
      i1214                   = lt_i1214
    EXCEPTIONS
      maintenance_not_allowed = 1
      no_corr_number          = 2
      error                   = 3
      OTHERS                  = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO g_msg.

    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        msg = g_msg.

* Implement suitable error handling here
  ENDIF.

  IF lines( lt_i1202 ) > 0.
    UNASSIGN: <lt_hrs_infty>.
    ASSIGN ('(SAPLRHWS)USE_HRS1202[]') TO <lt_hrs_infty>.
    IF <lt_hrs_infty> IS ASSIGNED.
      PERFORM upd_buf_hrs1202 IN PROGRAM saplrhws
                  USING
                      ls_object-otype
                      ls_object-objid
                      sy-langu
                      ls_i1202
                      'I'.
    ENDIF.
  ENDIF.

  IF lines( lt_i1203 ) > 0.
    READ TABLE lt_i1203 INTO ls_i1203 INDEX 1.
    PERFORM upd_buf_hrs1203 IN PROGRAM saplrhws
                USING
                    ls_object-otype
                    ls_object-objid
                    sy-langu
                    ls_i1203
                    'I'.
  ENDIF.

  me->before_save( i_xmldoc = ixmldocument
                   i_devclass = devclass ).

  l_cnt_node = xmldoc->find_from_name( name = 'CONTAINER' ).
  IF l_cnt_node IS BOUND.
    TRY .

        lo_container = me->get_wf_cnt_instance( ).

        l_ref_ixml = cl_ixml=>create( ).
        l_subdoc = l_ref_ixml->create_document( ).
        l_subdoc->append_child( l_cnt_node ).

        lo_container->import_from_xml( xml_dom = l_subdoc ).

        l_object_guid32 = ls_object.
        lo_container->set_guid( guid_32 = l_object_guid32 ).

        lo_container->save_to_database( ).

      CATCH cx_swf_cnt_invalid_por INTO ex.
        g_msg = 'Error when saving container' && ex->get_text( ).

        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            msg = g_msg.

    ENDTRY.
  ENDIF.

  CALL FUNCTION 'RH_STORE_HRS_DATA'. "It calls COMMIT

  me->after_save( i_xmldoc = ixmldocument
                  i_devclass = devclass ).

  name = get_object_description( ).

  CONCATENATE name ls_object INTO name SEPARATED BY space.

ENDMETHOD.


METHOD CREATE_WF_CNT_INSTANCE.
  DATA: task_por TYPE sibflpor,
        l_hr_task TYPE swd_step_t,
        ls_object TYPE hrsobject.


  ls_object-otype = get_otype( ).
  ls_object-objid = get_objid( ).
  l_hr_task = ls_object.

  TRY.

      CALL METHOD cl_swf_cnt_factory=>create_rh_task_container
        EXPORTING
          im_task_id               = l_hr_task
          im_persistence_classname = c_swf_pers_class
        IMPORTING
          ex_task_container        = r_instance.

    CATCH cx_swf_utl_obj_create_failed.
    CATCH cx_swf_utl_no_plan_variant.
    CATCH cx_swf_utl_task_not_found.
    CATCH cx_swf_utl_obj_invalid_ref.

  ENDTRY.
ENDMETHOD.


METHOD DELETEOBJECT.
ENDMETHOD.


METHOD FILL_ROOT_NODE.
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

  DATA: ls_taskid TYPE rhobjects.

  ls_taskid = is_object.
  setattributesfromstructure( node = i_node structure = ls_taskid ).

  r_node = i_node.
ENDMETHOD.


METHOD GETOBJECTTYPE.
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

  objecttype = 'PD' && get_otype( ).
ENDMETHOD.


METHOD GET_OBJID.
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

  IF objname(2) = get_otype( ).
    r_objid = objname+2.
  ELSE.
    r_objid = objname.
  ENDIF.
ENDMETHOD.


METHOD GET_STRUCTURE_FROM_NODE.
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

  DATA: lo_node	TYPE REF TO if_ixml_element.

  IF i_node IS BOUND.
    lo_node = i_node.
  ELSE.
    lo_node = xmldoc->get_root_element( ).
  ENDIF.

  lo_node = lo_node->find_from_name( name = i_name ).

  CHECK lo_node IS BOUND.

  getstructurefromattributes( EXPORTING node            = lo_node
                              CHANGING  structure       = cs_struct ).

ENDMETHOD.


METHOD GET_TABLE_FROM_NODE.
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

  DATA: lo_node	TYPE REF TO if_ixml_element,
        lo_item TYPE REF TO if_ixml_node,
        l_name TYPE string,
        l_value TYPE string,
        lo_attrs TYPE REF TO if_ixml_named_node_map,
        lo_attr TYPE REF TO if_ixml_node,
        lo_iterator TYPE REF TO if_ixml_node_iterator,
        lo_attr_iterator TYPE REF TO if_ixml_node_iterator,
        lo_filter TYPE REF TO if_ixml_node_filter.

  FIELD-SYMBOLS: <wa> TYPE any,
                 <f> TYPE any.

  IF i_node IS BOUND.
    lo_node = i_node.
  ELSE.
    lo_node = xmldoc->get_root_element( ).
  ENDIF.

  CHECK lo_node IS BOUND.
  lo_node = lo_node->find_from_name( name = i_name ).

  CHECK lo_node IS BOUND.
  lo_iterator = lo_node->create_iterator_filtered( lo_node->create_filter_name( 'item' ) ).

  DO.
    lo_item = lo_iterator->get_next( ).
    IF lo_item IS NOT BOUND.
      EXIT.
    ENDIF.

    l_name = lo_item->get_name( ).
    CHECK l_name = 'item'.

    lo_attrs = lo_item->get_attributes( ).
    CHECK lo_attrs IS BOUND.

    lo_attr_iterator = lo_attrs->create_iterator( ).
    UNASSIGN <wa>.

    DO.
      lo_attr = lo_attr_iterator->get_next( ).
      IF lo_attr IS NOT BOUND.
        EXIT.
      ENDIF.

      IF <wa> IS NOT ASSIGNED.
        APPEND INITIAL LINE TO ct ASSIGNING <wa>.
      ENDIF.

      l_name = lo_attr->get_name( ).
      l_value = lo_attr->get_value( ).

      UNASSIGN <f>.
      ASSIGN COMPONENT l_name OF STRUCTURE <wa> TO <f>.

      CHECK <f> IS ASSIGNED.
      <f> = l_value.
    ENDDO.

  ENDDO.


ENDMETHOD.


METHOD GET_WF_CNT_INSTANCE.
  DATA: task_por TYPE sibflpor,
        l_hr_task TYPE swd_step_t,
        ls_object TYPE hrsobject.


  IF go_container IS NOT BOUND.
    ls_object-otype = get_otype( ).
    ls_object-objid = get_objid( ).
    l_hr_task = ls_object.

* try to read the container from OO persistence
    TRY.

      task_por-typeid = c_swf_pers_class.
      task_por-instid = ls_object.

      CALL METHOD cl_swf_cnt_factory=>find_by_lpor
        EXPORTING
          lpor     = task_por
        RECEIVING
          instance = go_container.

    ENDTRY.

    IF go_container IS NOT BOUND.
* object has no persistent container yet

      go_container = me->create_wf_cnt_instance( ).

* get the texts of the container elements
      CALL METHOD go_container->refresh_element_texts.

    ENDIF.
  ENDIF.

  r_instance = go_container.
ENDMETHOD.
ENDCLASS.
