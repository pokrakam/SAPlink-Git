class ZSAPLINK_WF_STASK definition
  public
  inheriting from ZSAPLINK_WF
  create public .

public section.
protected section.

  methods FILL_ROOT_NODE
    redefinition .
  methods GET_OBJECT_DESCRIPTION
    redefinition .
  methods GET_OTYPE
    redefinition .
private section.
ENDCLASS.



CLASS ZSAPLINK_WF_STASK IMPLEMENTATION.


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

  DATA: l_object TYPE rhobjects,

        ls_task_attributes  TYPE rhwftskatt,
        lt_init_bind  TYPE  swfbndptab,
        lt_om_bind  TYPE  swfbndptab,
        lt_def_role_bind  TYPE  swfbndptab,
        lt_not_role_bind  TYPE  swfbndptab,
        lt_dea_role_bind  TYPE  swfbndptab,
        lt_end_role_bind  TYPE  swfbndptab,
        lt_lat_role_bind  TYPE  swfbndptab,

        lt_act_cont_def	TYPE TABLE OF	swcontdef,
        lt_act_methods  TYPE TABLE OF	rhwf_meth,
        lt_start_events TYPE TABLE OF	hri1212,
        lt_term_events  TYPE TABLE OF	hri1212,
        lt_event_binding TYPE TABLE OF  hrs1212.

  super->fill_root_node( i_node    = i_node
                         is_object = is_object ).
  r_node = i_node.

  l_object = is_object.

  CALL FUNCTION 'RH_TASK_ATTRIBUTES_RUNTIME'
    EXPORTING
      act_object_ext     = l_object-object
    IMPORTING
      task_attributes    = ls_task_attributes
      task_init_bind     = lt_init_bind
      task_om_bind       = lt_om_bind
      task_def_role_bind = lt_def_role_bind
      task_not_role_bind = lt_not_role_bind
      task_dea_role_bind = lt_dea_role_bind
      task_end_role_bind = lt_end_role_bind
      task_lat_role_bind = lt_lat_role_bind
    TABLES
      act_cont_def       = lt_act_cont_def
      act_methods        = lt_act_methods
      start_events       = lt_start_events
      term_events        = lt_term_events
      event_binding      = lt_event_binding
    EXCEPTIONS
      OTHERS             = 3.

  append_structure_node( i_node    = i_node
                         i_name    = 'task_attributes'
                         is_struct = ls_task_attributes ).

  append_table_node( i_node = i_node i_name = 'task_init_bind' it = lt_init_bind ).
  append_table_node( i_node = i_node i_name = 'task_om_bind' it = lt_om_bind ).
  append_table_node( i_node = i_node i_name = 'task_def_role_bind' it = lt_def_role_bind ).
  append_table_node( i_node = i_node i_name = 'task_not_role_bind' it = lt_not_role_bind ).
  append_table_node( i_node = i_node i_name = 'task_dea_role_bind' it = lt_dea_role_bind ).
  append_table_node( i_node = i_node i_name = 'task_end_role_bind' it = lt_end_role_bind ).
  append_table_node( i_node = i_node i_name = 'task_lat_role_bind' it = lt_lat_role_bind ).
  append_table_node( i_node = i_node i_name = 'act_cont_def' it = lt_act_cont_def ).
  append_table_node( i_node = i_node i_name = 'act_methods' it = lt_act_methods ).
  append_table_node( i_node = i_node i_name = 'start_events' it = lt_start_events ).
  append_table_node( i_node = i_node i_name = 'term_events' it = lt_term_events ).
  append_table_node( i_node = i_node i_name = 'event_binding' it = lt_event_binding ).

  r_node = i_node.

ENDMETHOD.


METHOD get_object_description.
  r_value = 'Standard task'.
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

  r_otype = 'TS'.
ENDMETHOD.
ENDCLASS.
