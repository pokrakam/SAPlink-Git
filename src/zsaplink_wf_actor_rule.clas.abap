class ZSAPLINK_WF_ACTOR_RULE definition
  public
  inheriting from ZSAPLINK_WF
  create public .

public section.

  types:
    BEGIN OF ts_rule_attr,
             act_short  TYPE  hrs1000-short,
             act_stext  TYPE  hrs1000-stext,
             execute_function TYPE  hrs1203-fname,
             execute_wegid  TYPE  hrs1203-wegid,
             execute_scenario TYPE  hrs1203-attrscn,
             execute_enforce  TYPE  hrs1203-enforce,
             sap_org_obj_based  TYPE  hrs1203-org_flag,
             sap_org_objtype  TYPE  hrs1203-objtype,
             responsible_flag TYPE  flag,
             personal_flag  TYPE  hrs1203-personal,
             prioseq_flag TYPE  hrs1203-prioseqn,
             roletype TYPE  hrroletype,
           END OF ts_rule_attr .
protected section.

  methods CREATE_WF_CNT_INSTANCE
    redefinition .
  methods FILL_ROOT_NODE
    redefinition .
  methods GET_OBJECT_DESCRIPTION
    redefinition .
  methods GET_OTYPE
    redefinition .
private section.
ENDCLASS.



CLASS ZSAPLINK_WF_ACTOR_RULE IMPLEMENTATION.


METHOD create_wf_cnt_instance.
*CALL METHOD SUPER->CREATE_WF_CNT_INSTANCE
*  RECEIVING
*    R_INSTANCE =
*    .
  DATA: task_por TYPE sibflpor,
        l_hr_task TYPE swd_step_t,
        ls_object TYPE hrsobject.


  ls_object-otype = get_otype( ).
  ls_object-objid = get_objid( ).
  l_hr_task = ls_object.

  TRY.

      CALL METHOD cl_swf_cnt_factory=>create_role_container
        EXPORTING
          im_role_id               = l_hr_task
          im_persistence_classname = c_swf_pers_class
        IMPORTING
          ex_role_container        = r_instance.

    CATCH cx_swf_utl_obj_create_failed.
    CATCH cx_swf_utl_no_plan_variant.
    CATCH cx_swf_utl_task_not_found.
    CATCH cx_swf_utl_obj_invalid_ref.

  ENDTRY.
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

  DATA: ls_attr TYPE ts_rule_attr,
        lt_cont	TYPE TABLE OF swcontdef.

  DATA: node TYPE REF TO if_ixml_element.

  r_node = super->fill_root_node( i_node    = i_node
                                  is_object = is_object ).

  CALL FUNCTION 'RH_GET_ACTOR_ATTRIBUTES'
    EXPORTING
      act_otype         = is_object-otype
      act_objid         = is_object-objid
      read_container    = abap_true
      authority_check   = abap_false
    IMPORTING
      act_short         = ls_attr-act_short
      act_stext         = ls_attr-act_stext
      execute_function  = ls_attr-execute_function
      execute_wegid     = ls_attr-execute_wegid
      execute_scenario  = ls_attr-execute_scenario
      execute_enforce   = ls_attr-execute_enforce
      sap_org_obj_based = ls_attr-sap_org_obj_based
      sap_org_objtype   = ls_attr-sap_org_objtype
      responsible_flag  = ls_attr-responsible_flag
      personal_flag     = ls_attr-personal_flag
      prioseq_flag      = ls_attr-prioseq_flag
      roletype          = ls_attr-roletype
    TABLES
      act_cont_def      = lt_cont
    EXCEPTIONS
      OTHERS            = 2.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  append_structure_node( i_node    = i_node
                         i_name    = 'rule_attributes'
                         is_struct = ls_attr ).

  append_table_node( i_node = i_node
                     i_name = 'rule_container'
                     it     = lt_cont ).
ENDMETHOD.


METHOD get_object_description.
  r_value = 'Agent determination rule'.
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

  r_otype = 'AC'.
ENDMETHOD.
ENDCLASS.
