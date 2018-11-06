class ZSAPLINK_WD_APPLICATION definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.
  type-pools WDYN .
  type-pools WDYWB .

  methods CONSTRUCTOR
    importing
      !NAME type STRING .

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

  methods GET_APPLICATION
    returning
      value(APPLICATION) type ref to IF_WDY_MD_APPLICATION
    raising
      CX_WDY_MD_NOT_EXISTING
      CX_WDY_MD_PERMISSION_FAILURE .
ENDCLASS.



CLASS ZSAPLINK_WD_APPLICATION IMPLEMENTATION.


method CHECKEXISTS.
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

  DATA: application_name TYPE wdy_application_name.
  data: application type wdy_application.
  application_name = objname.
  TRANSLATE application_name TO UPPER CASE.

  SELECT SINGLE * FROM wdy_application INTO application
    WHERE application_name = application_name.
  IF sy-subrc NE 0.
    CLEAR exists.
  ENDIF.


endmethod.


METHOD constructor.

  CALL METHOD super->constructor
    EXPORTING
      name = name.

  nugget_level = 99. " WD Application should load last

ENDMETHOD.


method CREATEIXMLDOCFROMOBJECT.
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
  DATA help_desc_node          TYPE REF TO if_ixml_element.
  DATA wdy_applicationt_node   TYPE REF TO if_ixml_element.
  DATA wdy_app_prop_node       TYPE REF TO if_ixml_element.
  DATA rc         TYPE sysubrc.

  DATA: application TYPE REF TO if_wdy_md_application.
  TRY.
      application = me->get_application( ).
    CATCH cx_wdy_md_not_existing.
      RAISE EXCEPTION TYPE zcx_saplink EXPORTING textid = zcx_saplink=>not_found.
      RETURN.
    CATCH cx_wdy_md_permission_failure.
      RAISE EXCEPTION TYPE zcx_saplink EXPORTING textid = zcx_saplink=>not_found.
      RETURN.
  ENDTRY.

  DATA definition TYPE wdy_application.

  application->if_wdy_md_object~get_definition(
     IMPORTING
       definition = definition ).

* Create parent node
  DATA _objtype TYPE string.
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = definition ).

  DATA help_desc TYPE wdy_md_translatable_text.
  help_desc = application->get_help_description( ).
  rc = rootnode->set_attribute( name = 'help_desc_txt' value = help_desc ).


* copy wdy_applicationt
  DATA wdy_applicationt_table TYPE STANDARD TABLE OF wdy_applicationt.
  FIELD-SYMBOLS: <wdy_applicationt> TYPE wdy_applicationt.
  SELECT * FROM wdy_applicationt INTO TABLE wdy_applicationt_table
    WHERE application_name = definition-application_name.
  LOOP AT wdy_applicationt_table ASSIGNING <wdy_applicationt>.
    wdy_applicationt_node = xmldoc->create_element( 'wdy_applicationt' ).
    setattributesfromstructure( node = wdy_applicationt_node structure = <wdy_applicationt> ).
    rc = rootnode->append_child( wdy_applicationt_node ).
  ENDLOOP.

* Application Properties
  DATA itab_wdy_app_property TYPE wdy_app_property_table.
  DATA prop TYPE REF TO if_wdy_md_application_property.
  FIELD-SYMBOLS: <wdy_app_prop> TYPE wdy_app_property.
  SELECT * FROM wdy_app_property INTO TABLE itab_wdy_app_property
      WHERE application_name = definition-application_name.
  LOOP AT itab_wdy_app_property ASSIGNING <wdy_app_prop>.
    wdy_app_prop_node = xmldoc->create_element( 'wdy_app_prop' ).
    setattributesfromstructure( node = wdy_app_prop_node structure = <wdy_app_prop> ).
    rc = rootnode->append_child( wdy_app_prop_node ).
  ENDLOOP.

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
endmethod.


method CREATEOBJECTFROMIXMLDOC.
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
*  DATA wdy_application TYPE wdy_application.
  DATA wdy_applicationt_node   TYPE REF TO if_ixml_element.
  DATA wdy_app_prop_node       TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA rc          TYPE sysubrc.
  DATA _devclass   TYPE devclass.
  DATA checkexists TYPE flag.
  DATA _objtype    TYPE string.
  DATA definition  TYPE wdy_application.

  DATA: application TYPE REF TO if_wdy_md_application.

  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node            = rootnode
      preserveversion = abap_true
    CHANGING
      structure       = definition.

  objname = definition-application_name.
  DATA help_desc TYPE wdy_md_translatable_text.
  help_desc = rootnode->get_attribute( 'help_desc_txt' ).

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

  application =  cl_wdy_md_application=>create(
  name = definition-application_name
  devclass = _devclass ).

* copy relevant parts of wdy_application
  DATA component TYPE REF TO if_wdy_md_component.
  CALL METHOD cl_wdy_md_component=>get_object_by_key
    EXPORTING
      name      = definition-component
    RECEIVING
      component = component.
  application->set_component( component ).


  application->set_help_description( help_desc ).
  application->set_help_link( definition-help_link ).
  application->set_message_display_mode( definition-msg_disp_mode ).
  DATA startup_plug TYPE REF TO if_wdy_md_inbound_plug.
  DATA startup_view TYPE REF TO if_wdy_md_abstract_view.
  startup_view = cl_wdy_md_view=>get_object_by_key(
      component_name = definition-component
      view_name      = definition-startup_view  ).
  startup_plug = startup_view->get_inbound_plug( definition-startup_plug ).
  application->set_startup_plug( startup_plug ).

  application->save_to_database( ).


  application->unlock( ).
  application->reload( ).

* copy wdy_applicationt
  DATA wdy_applicationt_table TYPE STANDARD TABLE OF wdy_applicationt.
  DATA wdy_applicationt TYPE wdy_applicationt.
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_applicationt' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_applicationt.
    APPEND wdy_applicationt TO wdy_applicationt_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_applicationt FROM TABLE wdy_applicationt_table.

* copy wdy_app_property
  DATA wdy_app_property_table TYPE STANDARD TABLE OF wdy_app_property.
  DATA wdy_app_property TYPE wdy_app_property.
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_app_prop' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_app_property.
    APPEND wdy_app_property TO wdy_app_property_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_app_property FROM TABLE wdy_app_property_table.

  cl_wdy_md_application=>generate_sicf(
    EXPORTING
      p_applname            = definition-display_name
      p_devclass            = _devclass
    EXCEPTIONS
      invalid_name          = 1
      parent_not_existing   = 2
      enqueue_error         = 3
      node_already_existing = 4
      transport_error       = 5
      tadir_error           = 6
      package_not_found     = 7
      alternate_name_exist  = 8
      error_occured         = 9 ).
  IF sy-subrc <> 0.
  ENDIF.


  COMMIT WORK.
  name = objname.
endmethod.


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

  DATA: application TYPE REF TO if_wdy_md_application.
  TRY.
      DATA: application_name TYPE wdy_application_name.
      application_name = objname.
      TRANSLATE application_name TO UPPER CASE.
      application = me->get_application( ).
    CATCH cx_wdy_md_not_existing.
      RAISE EXCEPTION TYPE zcx_saplink EXPORTING textid = zcx_saplink=>not_found.
      RETURN.
    CATCH cx_wdy_md_permission_failure.
      RAISE EXCEPTION TYPE zcx_saplink EXPORTING textid = zcx_saplink=>not_found.
      RETURN.
  ENDTRY.

  application->if_wdy_md_lockable_object~lock( ).
  application->if_wdy_md_object~delete( ).
*  cl_wdy_md_application=>delete_sicf( p_applname = application_name  ).
  application->if_wdy_md_lockable_object~save_to_database( ).
  application->if_wdy_md_lockable_object~unlock( ).
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


  objecttype = wdyn_r3tr_application.  "Web Dynpro Application

endmethod.


method GET_APPLICATION.

  DATA: application_name TYPE wdy_application_name.
  application_name = objname.
  TRANSLATE application_name TO UPPER CASE.

  DATA: tr_objtype     TYPE trobjtype,
         inactive_vers  TYPE char1,
         working_item   TYPE char1,
         l_obj_key      TYPE e071-obj_name.
  DATA: version TYPE r3state.

  CALL FUNCTION 'RS_WORKING_AREA_INIT'.

  tr_objtype = wdyn_r3tr_application.
  l_obj_key = application_name.
  TRANSLATE l_obj_key TO UPPER CASE.                      "#EC SYNTCHAR
  CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
    EXPORTING
      object                        = tr_objtype
      obj_name                      = l_obj_key
*     GLOBAL_CHECK                  = ' '
      mode                          = 'S' "Display
    IMPORTING
      object_is_work_item           = working_item
      object_inactive_version       = inactive_vers.


  IF working_item IS INITIAL.
    version = wdywb_version_active.
  ELSE.
    version = wdywb_version_inactive.
  ENDIF.

  TRY.
      CALL METHOD cl_wdy_md_application=>get_object_by_key
        EXPORTING
          name        = application_name
          version     = version
        RECEIVING
          application = application.
    CATCH cx_wdy_md_not_existing.
      IF version = wdywb_version_active.
        version     = wdywb_version_inactive.
      ELSE.
        version      = wdywb_version_active.
      ENDIF.
      TRY.
        CALL METHOD cl_wdy_md_application=>get_object_by_key
          EXPORTING
            name        = application_name
            version     = version
          RECEIVING
            application = application.
      ENDTRY.
  ENDTRY.
endmethod.
ENDCLASS.
