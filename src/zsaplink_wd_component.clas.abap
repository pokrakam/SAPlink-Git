class ZSAPLINK_WD_COMPONENT definition
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

  methods CONTROLLER_TO_XML
    importing
      !CONTROLLER type ref to IF_WDY_MD_CONTROLLER
      !XML_NODE type ref to IF_IXML_ELEMENT .
  methods XML_TO_CONTROLLER
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
    returning
      value(CONTROLLER) type ref to IF_WDY_MD_CONTROLLER
    raising
      CX_WDY_MD_ENQUEUE_FAILURE
      CX_WDY_MD_ALREADY_EXISTING .
  methods XML_TO_VIEW
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
    raising
      CX_WDY_MD_ENQUEUE_FAILURE .
  methods GET_COMPONENT
    returning
      value(COMPONENT) type ref to IF_WDY_MD_COMPONENT
    raising
      CX_WDY_MD_NOT_EXISTING
      CX_WDY_MD_PERMISSION_FAILURE .
  methods VIEW_TO_XML
    importing
      !VIEW type ref to IF_WDY_MD_ABSTRACT_VIEW
      !XML_NODE type ref to IF_IXML_ELEMENT
    exporting
      !VIEW_NODE type ref to IF_IXML_ELEMENT .
ENDCLASS.



CLASS ZSAPLINK_WD_COMPONENT IMPLEMENTATION.


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


  DATA: component TYPE REF TO if_wdy_md_component.
  exists = abap_true.
  TRY.
      component = me->get_component( ).
    CATCH cx_wdy_md_not_existing.
      CLEAR exists.
    CATCH cx_wdy_md_permission_failure.
      CLEAR exists.
  ENDTRY.



endmethod.


method CONTROLLER_TO_XML.
  DATA definition TYPE wdy_controller.
  DATA rc TYPE i.
  DATA controller_definition_node    TYPE REF TO if_ixml_element.
  DATA wdy_controllert_node          TYPE REF TO if_ixml_element.
  DATA wdy_ctlr_compo_node           TYPE REF TO if_ixml_element.
  DATA wdy_ctlr_compot_node          TYPE REF TO if_ixml_element.
  DATA wdy_ctlr_param_node           TYPE REF TO if_ixml_element.
  DATA wdy_ctlr_paramt_node          TYPE REF TO if_ixml_element.
  DATA wdy_ctlr_usage_node           TYPE REF TO if_ixml_element.
  DATA wdy_ctx_node_node             TYPE REF TO if_ixml_element.
  DATA wdy_ctx_attrib_node           TYPE REF TO if_ixml_element.
  data wdy_ctx_mapping_node          type ref to if_ixml_element.



  controller->if_wdy_md_object~get_definition(
     IMPORTING
       definition = definition ).

* Create parent node
  DATA _objtype TYPE string.
  _objtype = definition-controller_name.

* Controller Definition
  controller_definition_node = xmldoc->create_element( 'controller_definition' ).
  setattributesfromstructure( node = controller_definition_node structure = definition ).
  rc = xml_node->append_child( controller_definition_node ).

* copy wdy_controllert
  DATA wdy_controllert_table TYPE STANDARD TABLE OF wdy_controllert.
  FIELD-SYMBOLS: <wdy_controllert> TYPE wdy_controllert.
  SELECT * FROM wdy_controllert INTO TABLE wdy_controllert_table
    WHERE component_name = definition-component_name
      AND controller_name  = definition-controller_name.
  LOOP AT wdy_controllert_table ASSIGNING <wdy_controllert>.
    wdy_controllert_node = xmldoc->create_element( 'wdy_controllert' ).
    setattributesfromstructure( node = wdy_controllert_node structure = <wdy_controllert> ).
    rc = controller_definition_node->append_child( wdy_controllert_node ).
  ENDLOOP.

* copy components
  DATA it_wdy_ctlr_compo TYPE STANDARD TABLE OF wdy_ctlr_compo.
  FIELD-SYMBOLS: <wdy_ctlr_compo> TYPE wdy_ctlr_compo.
  SELECT * FROM wdy_ctlr_compo INTO TABLE it_wdy_ctlr_compo
    WHERE component_name   = definition-component_name
      AND controller_name  = definition-controller_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ctlr_compo ASSIGNING <wdy_ctlr_compo> WHERE version <> 'D'.
          IF <wdy_ctlr_compo>-version = 'A'.
            READ TABLE it_wdy_ctlr_compo TRANSPORTING NO FIELDS
              WITH KEY cmpname = <wdy_ctlr_compo>-cmpname
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ctlr_compo>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctlr_compo_node = xmldoc->create_element( 'wdy_ctlr_compo' ).
          setattributesfromstructure( node = wdy_ctlr_compo_node structure = <wdy_ctlr_compo> ).
          rc = controller_definition_node->append_child( wdy_ctlr_compo_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ctlr_compo ASSIGNING <wdy_ctlr_compo> WHERE version = 'A'.
          <wdy_ctlr_compo>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctlr_compo_node = xmldoc->create_element( 'wdy_ctlr_compo' ).
          setattributesfromstructure( node = wdy_ctlr_compo_node structure = <wdy_ctlr_compo> ).
          rc = controller_definition_node->append_child( wdy_ctlr_compo_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy wdy_ctlr_compot
  DATA wdy_ctlr_compot_table TYPE STANDARD TABLE OF wdy_ctlr_compot.
  FIELD-SYMBOLS: <wdy_ctlr_compot> TYPE wdy_ctlr_compot.
  SELECT * FROM wdy_ctlr_compot INTO TABLE wdy_ctlr_compot_table
    WHERE component_name = definition-component_name
      AND controller_name  = definition-controller_name.
  LOOP AT wdy_ctlr_compot_table ASSIGNING <wdy_ctlr_compot>.
    wdy_ctlr_compot_node = xmldoc->create_element( 'wdy_ctlr_compot' ).
    setattributesfromstructure( node = wdy_ctlr_compot_node structure = <wdy_ctlr_compot> ).
    rc = controller_definition_node->append_child( wdy_ctlr_compot_node ).
  ENDLOOP.

* copy parameters
  DATA it_wdy_ctlr_param TYPE STANDARD TABLE OF wdy_ctlr_param.
  FIELD-SYMBOLS: <wdy_ctlr_param> TYPE wdy_ctlr_param.
  SELECT * FROM wdy_ctlr_param INTO TABLE it_wdy_ctlr_param
    WHERE component_name   = definition-component_name
      AND controller_name  = definition-controller_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ctlr_param ASSIGNING <wdy_ctlr_param> WHERE version <> 'D'.
          IF <wdy_ctlr_param>-version = 'A'.
            READ TABLE it_wdy_ctlr_param TRANSPORTING NO FIELDS
              WITH KEY cmpname = <wdy_ctlr_param>-cmpname
                       parameter_name = <wdy_ctlr_param>-parameter_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ctlr_param>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctlr_param_node = xmldoc->create_element( 'wdy_ctlr_param' ).
          setattributesfromstructure( node = wdy_ctlr_param_node structure = <wdy_ctlr_param> ).
          rc = controller_definition_node->append_child( wdy_ctlr_param_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ctlr_param ASSIGNING <wdy_ctlr_param> WHERE version = 'A'.
          <wdy_ctlr_param>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctlr_param_node = xmldoc->create_element( 'wdy_ctlr_param' ).
          setattributesfromstructure( node = wdy_ctlr_param_node structure = <wdy_ctlr_param> ).
          rc = controller_definition_node->append_child( wdy_ctlr_param_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy wdy_ctlr_paramt
  DATA wdy_ctlr_paramt_table TYPE STANDARD TABLE OF wdy_ctlr_paramt.
  FIELD-SYMBOLS: <wdy_ctlr_paramt> TYPE wdy_ctlr_paramt.
  SELECT * FROM wdy_ctlr_paramt INTO TABLE wdy_ctlr_paramt_table
    WHERE component_name = definition-component_name
      AND controller_name  = definition-controller_name.
  LOOP AT wdy_ctlr_paramt_table ASSIGNING <wdy_ctlr_paramt>.
    wdy_ctlr_paramt_node = xmldoc->create_element( 'wdy_ctlr_paramt' ).
    setattributesfromstructure( node = wdy_ctlr_paramt_node structure = <wdy_ctlr_paramt> ).
    rc = controller_definition_node->append_child( wdy_ctlr_paramt_node ).
  ENDLOOP.

* copy controller usages
  DATA it_wdy_ctlr_usage TYPE STANDARD TABLE OF wdy_ctlr_usage.
  FIELD-SYMBOLS: <wdy_ctlr_usage> TYPE wdy_ctlr_usage.
  SELECT * FROM wdy_ctlr_usage INTO TABLE it_wdy_ctlr_usage
    WHERE component_name   = definition-component_name
      AND controller_name  = definition-controller_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ctlr_usage ASSIGNING <wdy_ctlr_usage> WHERE version <> 'D'.
          IF <wdy_ctlr_usage>-version = 'A'.
            READ TABLE it_wdy_ctlr_usage TRANSPORTING NO FIELDS
              WITH KEY ctlr_usage_name = <wdy_ctlr_usage>-ctlr_usage_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ctlr_usage>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctlr_usage_node = xmldoc->create_element( 'wdy_ctlr_usage' ).
          setattributesfromstructure( node = wdy_ctlr_usage_node structure = <wdy_ctlr_usage> ).
          rc = controller_definition_node->append_child( wdy_ctlr_usage_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ctlr_usage ASSIGNING <wdy_ctlr_usage> WHERE version = 'A'.
          <wdy_ctlr_usage>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctlr_usage_node = xmldoc->create_element( 'wdy_ctlr_usage' ).
          setattributesfromstructure( node = wdy_ctlr_usage_node structure = <wdy_ctlr_usage> ).
          rc = controller_definition_node->append_child( wdy_ctlr_usage_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy context nodes
  DATA it_wdy_ctx_node TYPE STANDARD TABLE OF wdy_ctx_node.
  FIELD-SYMBOLS: <wdy_ctx_node> TYPE wdy_ctx_node.
  SELECT * FROM wdy_ctx_node INTO TABLE it_wdy_ctx_node
    WHERE component_name   = definition-component_name
      AND controller_name  = definition-controller_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ctx_node ASSIGNING <wdy_ctx_node> WHERE version <> 'D'.
          IF <wdy_ctx_node>-version = 'A'.
            READ TABLE it_wdy_ctx_node TRANSPORTING NO FIELDS
              WITH KEY node_name = <wdy_ctx_node>-node_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ctx_node>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctx_node_node = xmldoc->create_element( 'wdy_ctx_node' ).
          setattributesfromstructure( node = wdy_ctx_node_node structure = <wdy_ctx_node> ).
          rc = controller_definition_node->append_child( wdy_ctx_node_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ctx_node ASSIGNING <wdy_ctx_node> WHERE version = 'A'.
          <wdy_ctx_node>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctx_node_node = xmldoc->create_element( 'wdy_ctx_node' ).
          setattributesfromstructure( node = wdy_ctx_node_node structure = <wdy_ctx_node> ).
          rc = controller_definition_node->append_child( wdy_ctx_node_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy context attributes
  DATA it_wdy_ctx_attrib TYPE STANDARD TABLE OF wdy_ctx_attrib.
  FIELD-SYMBOLS: <wdy_ctx_attrib> TYPE wdy_ctx_attrib.
  SELECT * FROM wdy_ctx_attrib INTO TABLE it_wdy_ctx_attrib
    WHERE component_name   = definition-component_name
      AND controller_name  = definition-controller_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ctx_attrib ASSIGNING <wdy_ctx_attrib> WHERE version <> 'D'.
          IF <wdy_ctx_attrib>-version = 'A'.
            READ TABLE it_wdy_ctx_attrib TRANSPORTING NO FIELDS
              WITH KEY node_name = <wdy_ctx_attrib>-node_name
                       attribute_name = <wdy_ctx_attrib>-attribute_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ctx_attrib>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctx_attrib_node = xmldoc->create_element( 'wdy_ctx_attrib' ).
          setattributesfromstructure( node = wdy_ctx_attrib_node structure = <wdy_ctx_attrib> ).
          rc = controller_definition_node->append_child( wdy_ctx_attrib_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ctx_attrib ASSIGNING <wdy_ctx_attrib> WHERE version = 'A'.
          <wdy_ctx_attrib>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ctx_attrib_node = xmldoc->create_element( 'wdy_ctx_attrib' ).
          setattributesfromstructure( node = wdy_ctx_attrib_node structure = <wdy_ctx_attrib> ).
          rc = controller_definition_node->append_child( wdy_ctx_attrib_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy context mapping
  data it_wdy_ctx_mapping type standard table of wdy_ctx_mapping.
  field-symbols: <wdy_ctx_mapping> type wdy_ctx_mapping.
  select * from wdy_ctx_mapping into table it_wdy_ctx_mapping
    where component_name   = definition-component_name
      and controller_name  = definition-controller_name.
  if sy-subrc = 0.
    case definition-version.
      when 'I'.
        loop at it_wdy_ctx_mapping assigning <wdy_ctx_mapping> where version <> 'D'.
          if <wdy_ctx_mapping>-version = 'A'.
            read table it_wdy_ctx_mapping transporting no fields
              with key ctlr_usage_name = <wdy_ctx_mapping>-ctlr_usage_name
                       ctx_mapp_name = <wdy_ctx_mapping>-ctx_mapp_name
                       version = 'I'.
            if sy-subrc = 0.
              continue.
            endif.
          endif.
          <wdy_ctx_mapping>-version = if_wdy_md_object=>co_version_inactive.
         wdy_ctx_mapping_node = xmldoc->create_element( 'wdy_ctx_mapping' ).
          setattributesfromstructure( node = wdy_ctx_mapping_node structure = <wdy_ctx_mapping> ).
          rc = controller_definition_node->append_child( wdy_ctx_mapping_node ).
        endloop.
      when 'A'.
        loop at it_wdy_ctx_mapping assigning <wdy_ctx_mapping> where version = 'A'.
          <wdy_ctx_mapping>-version = if_wdy_md_object=>co_version_inactive.
         wdy_ctx_mapping_node = xmldoc->create_element( 'wdy_ctx_mapping' ).
          setattributesfromstructure( node = wdy_ctx_mapping_node structure = <wdy_ctx_mapping> ).
          rc = controller_definition_node->append_child( wdy_ctx_mapping_node ).
        endloop.
    endcase.
  endif.



endmethod.


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
  DATA wdy_componentt_node   TYPE REF TO if_ixml_element.
  DATA wdy_compo_usage_node  TYPE REF TO if_ixml_element.
  DATA wdy_ext_ctlr_use_node TYPE REF TO if_ixml_element.
  DATA wdy_ext_ctx_map_node  TYPE REF TO if_ixml_element.
  DATA wdy_intf_implem_node  TYPE REF TO if_ixml_element.
  DATA view_node             TYPE REF TO if_ixml_element.
  DATA rc         TYPE sysubrc.

  DATA: component TYPE REF TO if_wdy_md_component.
  TRY.
      component = me->get_component( ).
    CATCH cx_wdy_md_not_existing.
      RAISE EXCEPTION TYPE zcx_saplink EXPORTING textid = zcx_saplink=>not_found.
      RETURN.
    CATCH cx_wdy_md_permission_failure.
      RAISE EXCEPTION TYPE zcx_saplink EXPORTING textid = zcx_saplink=>not_found.
      RETURN.
  ENDTRY.

  DATA definition TYPE wdy_component.

  component->if_wdy_md_object~get_definition(
     IMPORTING
       definition = definition ).

* Create parent node
  DATA _objtype TYPE string.
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = definition ).

* copy wdy_componentt
  DATA wdy_componentt_table TYPE STANDARD TABLE OF wdy_componentt.
  FIELD-SYMBOLS: <wdy_componentt> TYPE wdy_componentt.
  SELECT * FROM wdy_componentt INTO TABLE wdy_componentt_table
    WHERE component_name = definition-component_name.
  LOOP AT wdy_componentt_table ASSIGNING <wdy_componentt>.
    wdy_componentt_node = xmldoc->create_element( 'wdy_componentt' ).
    setattributesfromstructure( node = wdy_componentt_node structure = <wdy_componentt> ).
    rc = rootnode->append_child( wdy_componentt_node ).
  ENDLOOP.


* copy component usages
  DATA it_wdy_compo_usage TYPE STANDARD TABLE OF wdy_compo_usage.
  FIELD-SYMBOLS: <wdy_compo_usage> TYPE wdy_compo_usage.
  SELECT * FROM wdy_compo_usage INTO TABLE it_wdy_compo_usage
    WHERE component_name   = definition-component_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_compo_usage ASSIGNING <wdy_compo_usage> WHERE version <> 'D'.
          IF <wdy_compo_usage>-version = 'A'.
            READ TABLE it_wdy_compo_usage TRANSPORTING NO FIELDS
              WITH KEY compo_usage_name = <wdy_compo_usage>-compo_usage_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_compo_usage>-version = if_wdy_md_object=>co_version_inactive.

          wdy_compo_usage_node = xmldoc->create_element( 'wdy_compo_usage' ).
          setattributesfromstructure( node = wdy_compo_usage_node structure = <wdy_compo_usage> ).
          rc = rootnode->append_child( wdy_compo_usage_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_compo_usage ASSIGNING <wdy_compo_usage> WHERE version = 'A'.
          <wdy_compo_usage>-version = if_wdy_md_object=>co_version_inactive.
          wdy_compo_usage_node = xmldoc->create_element( 'wdy_compo_usage' ).
          setattributesfromstructure( node = wdy_compo_usage_node structure = <wdy_compo_usage> ).
          rc = rootnode->append_child( wdy_compo_usage_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy external controller usages
  DATA it_wdy_ext_ctlr_use TYPE STANDARD TABLE OF wdy_ext_ctlr_use.
  FIELD-SYMBOLS: <wdy_ext_ctlr_use> TYPE wdy_ext_ctlr_use.
  SELECT * FROM wdy_ext_ctlr_use INTO TABLE it_wdy_ext_ctlr_use
    WHERE component_name   = definition-component_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ext_ctlr_use ASSIGNING <wdy_ext_ctlr_use> WHERE version <> 'D'.
          IF <wdy_ext_ctlr_use>-version = 'A'.
            READ TABLE it_wdy_ext_ctlr_use TRANSPORTING NO FIELDS
              WITH KEY component_name = <wdy_ext_ctlr_use>-component_usage
                       ctlr_usage_name = <wdy_ext_ctlr_use>-ctlr_usage_name
                       comp_ctlr_usage = <wdy_ext_ctlr_use>-comp_ctlr_usage
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ext_ctlr_use>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ext_ctlr_use_node = xmldoc->create_element( 'wdy_ext_ctlr_use' ).
          setattributesfromstructure( node = wdy_ext_ctlr_use_node structure = <wdy_ext_ctlr_use> ).
          rc = rootnode->append_child( wdy_ext_ctlr_use_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ext_ctlr_use ASSIGNING <wdy_ext_ctlr_use> WHERE version = 'A'.
          <wdy_ext_ctlr_use>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ext_ctlr_use_node = xmldoc->create_element( 'wdy_ext_ctlr_use' ).
          setattributesfromstructure( node = wdy_ext_ctlr_use_node structure = <wdy_ext_ctlr_use> ).
          rc = rootnode->append_child( wdy_ext_ctlr_use_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy external context mappings
  DATA it_wdy_ext_ctx_map TYPE STANDARD TABLE OF wdy_ext_ctx_map.
  FIELD-SYMBOLS: <wdy_ext_ctx_map> TYPE wdy_ext_ctx_map.
  SELECT * FROM wdy_ext_ctx_map INTO TABLE it_wdy_ext_ctx_map
    WHERE component_name   = definition-component_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ext_ctx_map ASSIGNING <wdy_ext_ctx_map> WHERE version <> 'D'.
          IF <wdy_ext_ctx_map>-version = 'A'.
            READ TABLE it_wdy_ext_ctx_map TRANSPORTING NO FIELDS
              WITH KEY ctx_mapping_name = <wdy_ext_ctx_map>-ctx_mapping_name
                       ctlr_usage_name = <wdy_ext_ctx_map>-ctlr_usage_name
                       comp_ctlr_usage = <wdy_ext_ctx_map>-comp_ctlr_usage
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ext_ctx_map>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ext_ctx_map_node = xmldoc->create_element( 'wdy_ext_ctx_map' ).
          setattributesfromstructure( node = wdy_ext_ctx_map_node structure = <wdy_ext_ctx_map> ).
          rc = rootnode->append_child( wdy_ext_ctx_map_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ext_ctx_map ASSIGNING <wdy_ext_ctx_map> WHERE version = 'A'.
          <wdy_ext_ctx_map>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ext_ctx_map_node = xmldoc->create_element( 'wdy_ext_ctx_map' ).
          setattributesfromstructure( node = wdy_ext_ctx_map_node structure = <wdy_ext_ctx_map> ).
          rc = rootnode->append_child( wdy_ext_ctx_map_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy interface implementations
  DATA it_wdy_intf_implem TYPE STANDARD TABLE OF wdy_intf_implem.
  FIELD-SYMBOLS: <wdy_intf_implem> TYPE wdy_intf_implem.
  SELECT * FROM wdy_intf_implem INTO TABLE it_wdy_intf_implem
    WHERE component_name = definition-component_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_intf_implem ASSIGNING <wdy_intf_implem> WHERE version <> 'D'.
          IF <wdy_intf_implem>-version = 'A'.
            READ TABLE it_wdy_intf_implem TRANSPORTING NO FIELDS
              WITH KEY interface_name = <wdy_intf_implem>-interface_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_intf_implem>-version = if_wdy_md_object=>co_version_inactive.
          wdy_intf_implem_node = xmldoc->create_element( 'wdy_intf_implem' ).
          setattributesfromstructure( node = wdy_intf_implem_node structure = <wdy_intf_implem> ).
          rc = rootnode->append_child( wdy_intf_implem_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_intf_implem ASSIGNING <wdy_intf_implem> WHERE version = 'A'.
          <wdy_intf_implem>-version = if_wdy_md_object=>co_version_inactive.
          wdy_intf_implem_node = xmldoc->create_element( 'wdy_intf_implem' ).
          setattributesfromstructure( node = wdy_intf_implem_node structure = <wdy_intf_implem> ).
          rc = rootnode->append_child( wdy_intf_implem_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

  DATA: map TYPE REF TO if_object_map,
         iter TYPE REF TO if_object_collection_iterator,
         obj_name TYPE wdy_md_object_name,
         view TYPE REF TO if_wdy_md_abstract_view,
         controller TYPE REF TO if_wdy_md_controller.

  IF cl_wdy_md_component=>is_component_interface_def( definition-component_name ) = ' '.
*   copy views
    map ?= component->get_views( ).
    iter = map->get_values_iterator( ).
    WHILE iter->has_next( ) = 'X'.
      TRY.
        view ?= iter->get_next( ).
        IF definition-version = if_wdy_md_object=>co_version_active AND
           view->if_wdy_md_object~get_state( ) = if_wdy_md_object=>co_state_pseudo_active.
          CONTINUE.
        ENDIF.
        me->view_to_xml(
          EXPORTING
            view = view
            xml_node = rootnode
          IMPORTING
            view_node = view_node ).

        controller = view->get_view_controller( ).
        IF definition-version = if_wdy_md_object=>co_version_active AND
           controller->if_wdy_md_object~get_state( ) = if_wdy_md_object=>co_state_pseudo_active.
          CONTINUE.
        ENDIF.

        me->controller_to_xml(
           EXPORTING
              controller = controller
              xml_node   = view_node ).


      ENDTRY.
    ENDWHILE.

*   copy "window-views"
    map ?= component->get_windows( ).
    iter = map->get_values_iterator( ).
    WHILE iter->has_next( ) = 'X'.
      TRY.
        view ?= iter->get_next( ).
          me->view_to_xml(
            EXPORTING
              view = view
              xml_node = rootnode
            IMPORTING
              view_node = view_node ).

      controller = view->get_view_controller( ).
      IF definition-version = if_wdy_md_object=>co_version_active AND
         controller->if_wdy_md_object~get_state( ) = if_wdy_md_object=>co_state_pseudo_active.
        CONTINUE.
      ENDIF.

          me->controller_to_xml(
             EXPORTING
                controller = controller
                xml_node   = view_node ).
        CATCH cx_wdy_md_already_existing.
      ENDTRY.
    ENDWHILE.

  ELSE.
*   copy interface views
    DATA component_interface TYPE REF TO if_wdy_md_component_interface.
    component_interface ?= component->get_component_interface( ).
    map ?= component_interface->get_interface_views( ).
    iter = map->get_values_iterator( ).
    WHILE iter->has_next( ) = 'X'.
      TRY.
          view ?= iter->get_next( ).
          IF definition-version = if_wdy_md_object=>co_version_active AND
             view->if_wdy_md_object~get_state( ) = if_wdy_md_object=>co_state_pseudo_active.
            CONTINUE.
          ENDIF.
          me->view_to_xml(
            EXPORTING
              view = view
              xml_node = rootnode
            IMPORTING
              view_node = view_node ).

          controller = view->get_view_controller( ).
          IF definition-version = if_wdy_md_object=>co_version_active AND
             controller->if_wdy_md_object~get_state( ) = if_wdy_md_object=>co_state_pseudo_active.
            CONTINUE.
          ENDIF.
          me->controller_to_xml(
             EXPORTING
              controller = controller
              xml_node   = view_node ).
        CATCH cx_wdy_md_already_existing.
      ENDTRY.
    ENDWHILE.
  ENDIF.

* copy controllers of component
  map ?= component->get_controllers( ).
  iter = map->get_values_iterator( ).
  WHILE iter->has_next( ) = 'X'.
    TRY.
        controller ?= iter->get_next( ).
        IF controller->get_type( ) = wdyn_ctlr_type_cmp_config.
          CONTINUE.
        ENDIF.
        IF definition-version = if_wdy_md_object=>co_version_active AND
           controller->if_wdy_md_object~get_state( ) = if_wdy_md_object=>co_state_pseudo_active.
          CONTINUE.
        ENDIF.
        me->controller_to_xml(
           EXPORTING
            controller = controller
            xml_node   = rootnode ).

      CATCH cx_wdy_md_already_existing.
    ENDTRY.
  ENDWHILE.


*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
endmethod.


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


*xml nodes
  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA wdy_componentt_node   TYPE REF TO if_ixml_element.
  DATA wdy_compo_usage_node  TYPE REF TO if_ixml_element.
  DATA wdy_ext_ctlr_use_node TYPE REF TO if_ixml_element.
  DATA wdy_ext_ctx_map_node  TYPE REF TO if_ixml_element.
  DATA wdy_intf_implem_node  TYPE REF TO if_ixml_element.
  DATA view_node             TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA node2       TYPE REF TO if_ixml_element.
  DATA filter2     TYPE REF TO if_ixml_node_filter.
  DATA iterator2   TYPE REF TO if_ixml_node_iterator.
  DATA rc          TYPE sysubrc.
  DATA _devclass   TYPE devclass.
  DATA checkexists TYPE flag.
  DATA _objtype    TYPE string.
  DATA definition  TYPE wdy_component.

  DATA: component TYPE REF TO if_wdy_md_component.

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

  objname = definition-component_name.

  checkexists = checkexists( ).
  IF checkexists = abap_true.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.
      " delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

  cl_wdy_md_component=>create_complete(
    EXPORTING
      name          = definition-component_name
    IMPORTING
      component     = component
    CHANGING
      devclass      = _devclass
  ).

* copy relevant parts of wdy_component (concerning assistance class and configuration properties)
  DATA copy_def TYPE wdy_component.
  CALL METHOD component->if_wdy_md_object~get_definition
    IMPORTING
      definition = copy_def.
  copy_def-config_props = definition-config_props.
  copy_def-assistance_class = definition-assistance_class.
  MODIFY wdy_component FROM copy_def.

* copy wdy_componentt
  DATA wdy_componentt_table TYPE STANDARD TABLE OF wdy_componentt.
  DATA wdy_componentt TYPE wdy_componentt.
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_componentt' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_componentt.
    APPEND wdy_componentt TO wdy_componentt_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_componentt FROM TABLE wdy_componentt_table.

* copy component usages
  DATA it_wdy_compo_usage TYPE STANDARD TABLE OF wdy_compo_usage.
  DATA wdy_compo_usage TYPE wdy_compo_usage.
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_compo_usage' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_compo_usage.
    APPEND wdy_compo_usage TO it_wdy_compo_usage.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_compo_usage FROM TABLE it_wdy_compo_usage.

* copy external controller usages
  DATA it_wdy_ext_ctlr_use TYPE STANDARD TABLE OF wdy_ext_ctlr_use.
  DATA wdy_ext_ctlr_use TYPE wdy_ext_ctlr_use.
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_ext_ctlr_use' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ext_ctlr_use.
    APPEND wdy_ext_ctlr_use TO it_wdy_ext_ctlr_use.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ext_ctlr_use FROM TABLE it_wdy_ext_ctlr_use.

* copy external context mappings
  DATA it_wdy_ext_ctx_map TYPE STANDARD TABLE OF wdy_ext_ctx_map.
  DATA wdy_ext_ctx_map TYPE wdy_ext_ctx_map.
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_ext_ctx_map' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ext_ctx_map.
    APPEND wdy_ext_ctx_map TO it_wdy_ext_ctx_map.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ext_ctx_map FROM TABLE it_wdy_ext_ctx_map.

* copy interface implementations
  DATA it_wdy_intf_implem TYPE STANDARD TABLE OF wdy_intf_implem.
  DATA wdy_intf_implem TYPE wdy_intf_implem.
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'wdy_intf_implem' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_intf_implem.
    APPEND wdy_intf_implem TO it_wdy_intf_implem.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_intf_implem FROM TABLE it_wdy_intf_implem.

  TRY.
*   copy views
      FREE: filter, iterator, node.
      filter = xmldoc->create_filter_name( 'view_definition' ).
      iterator = xmldoc->create_iterator_filtered( filter ).
      node ?= iterator->get_next( ).
      WHILE node IS NOT INITIAL.
        me->xml_to_view(
          EXPORTING
            xml_node = node ).
        node ?= iterator->get_next( ).
      ENDWHILE.

      DATA controller TYPE REF TO if_wdy_md_controller.
      IF cl_wdy_md_component=>is_component_interface_def( definition-component_name ) = ' '.
        controller = component->get_controller( wdyn_component_controller_name ).
        controller->delete( ).
        controller->save_to_database( ).
      ENDIF.

* copy controllers of component
      FREE: filter, iterator, node.
      filter = xmldoc->create_filter_name( 'controller_definition' ).
      iterator = xmldoc->create_iterator_filtered( filter ).
      node ?= iterator->get_next( ).
      WHILE node IS NOT INITIAL.
        TRY.
            controller = me->xml_to_controller(
                xml_node = node ).
            node ?= iterator->get_next( ).
          CATCH cx_wdy_md_already_existing.
            node ?= iterator->get_next( ).
        ENDTRY.
      ENDWHILE.

    CATCH cx_wdy_md_enqueue_failure.
    CATCH cx_wdy_md_already_existing.

  ENDTRY.
*
*  DATA: trobjtype  TYPE trobjtype,
*        trobj_name TYPE trobj_name.
*  trobjtype  = l_object.
*  trobj_name = l_obj_name.
*  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
*    EXPORTING
*      object            = trobjtype
*      obj_name          = trobj_name
*    EXCEPTIONS
*      wrong_object_name = 1.

  component->unlock( ).
  component->reload( ).

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

  DATA: component TYPE REF TO if_wdy_md_component.
  TRY.
      component = me->get_component( ).
    CATCH cx_wdy_md_not_existing.
      RAISE EXCEPTION TYPE zcx_saplink EXPORTING textid = zcx_saplink=>not_found.
      RETURN.
    CATCH cx_wdy_md_permission_failure.
      RAISE EXCEPTION TYPE zcx_saplink EXPORTING textid = zcx_saplink=>not_found.
      RETURN.
  ENDTRY.

  component->if_wdy_md_lockable_object~lock( ).
  component->if_wdy_md_object~delete( ).
  component->if_wdy_md_lockable_object~save_to_database( ).
  component->if_wdy_md_lockable_object~unlock( ).
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


  objecttype = wdyn_r3tr_component.  "Web Dynpro Component

endmethod.


method GET_COMPONENT.

  DATA: component_name TYPE wdy_component_name.
  component_name = objname.
  TRANSLATE component_name TO UPPER CASE.

  DATA: tr_objtype     TYPE trobjtype,
         inactive_vers  TYPE char1,
         working_item   TYPE char1,
         l_obj_key      TYPE e071-obj_name.
  DATA: version TYPE r3state.

  CALL FUNCTION 'RS_WORKING_AREA_INIT'.

  tr_objtype = wdyn_limu_component_definition.
  l_obj_key = component_name.
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
      CALL METHOD cl_wdy_md_component=>get_object_by_key
        EXPORTING
          name      = component_name
          version   = version
        RECEIVING
          component = component.
    CATCH cx_wdy_md_not_existing.
      IF version = wdywb_version_active.
        version     = wdywb_version_inactive.
      ELSE.
        version      = wdywb_version_active.
      ENDIF.
      TRY.
        CALL METHOD cl_wdy_md_component=>get_object_by_key
          EXPORTING
            name      = component_name
            version   = version
          RECEIVING
            component = component.
      ENDTRY.
  ENDTRY.
endmethod.


method VIEW_TO_XML.
  DATA definition TYPE wdy_view.
  DATA rc TYPE i.
  DATA view_definition_node    TYPE REF TO if_ixml_element.
  DATA view_window_node        TYPE REF TO if_ixml_element.
  DATA wdy_viewt_node          TYPE REF TO if_ixml_element.
  DATA wdy_iobound_plug_node   TYPE REF TO if_ixml_element.
  DATA wdy_iobound_plgt_node   TYPE REF TO if_ixml_element.
  DATA wdy_ui_element_node     TYPE REF TO if_ixml_element.
  DATA wdy_ui_property_node    TYPE REF TO if_ixml_element.
  DATA wdy_view_cntr_node      TYPE REF TO if_ixml_element.
  DATA wdy_view_cntrt_node     TYPE REF TO if_ixml_element.
  DATA wdy_plug_param_node     TYPE REF TO if_ixml_element.
  DATA wdy_ui_ctx_bind_node    TYPE REF TO if_ixml_element.
  DATA wdy_ui_ddic_bind_node   TYPE REF TO if_ixml_element.
  DATA wdy_ui_evt_bind_node    TYPE REF TO if_ixml_element.
  DATA wdy_nav_link_node       TYPE REF TO if_ixml_element.
  DATA wdy_nav_targref_node    TYPE REF TO if_ixml_element.
  DATA wdy_vsh_node_node       TYPE REF TO if_ixml_element.
  DATA wdy_vsh_pholder_node    TYPE REF TO if_ixml_element.
  DATA wdy_vs_property_node    TYPE REF TO if_ixml_element.


  view->if_wdy_md_object~get_definition(
     IMPORTING
       definition = definition ).

* Create parent node
  DATA _objtype TYPE string.
  _objtype = definition-view_name.

* View Definition
  view_definition_node = xmldoc->create_element( 'view_definition' ).
  setattributesfromstructure( node = view_definition_node structure = definition ).
  rc = xml_node->append_child( view_definition_node ).
  view_node = view_definition_node.

* View Window
  TRY.
      DATA window TYPE REF TO if_wdy_md_window.
      window ?= view.
      DATA title TYPE wdy_md_translatable_text.
      title = window->get_title( ).
      view_window_node = xmldoc->create_element( 'view_window' ).
      rc = view_window_node->set_attribute( name = 'title' value = title ).
*   data def_root_node type ref to if_wdy_md_vset_hierarchy_node.
*   def_root_node ?= window->get_root_node( definition-def_root_node ).
      rc = view_definition_node->append_child( view_window_node ).
    CATCH cx_sy_move_cast_error.
  ENDTRY.

* copy wdy_viewt
  DATA wdy_viewt_table TYPE STANDARD TABLE OF wdy_viewt.
  FIELD-SYMBOLS: <wdy_viewt> TYPE wdy_viewt.
  SELECT * FROM wdy_viewt INTO TABLE wdy_viewt_table
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  LOOP AT wdy_viewt_table ASSIGNING <wdy_viewt>.
    wdy_viewt_node = xmldoc->create_element( 'wdy_viewt' ).
    setattributesfromstructure( node = wdy_viewt_node structure = <wdy_viewt> ).
    rc = view_definition_node->append_child( wdy_viewt_node ).
  ENDLOOP.

* copy wdy_iobound_plug.
  DATA it_wdy_iobound_plug TYPE STANDARD TABLE OF wdy_iobound_plug.
  DATA it_wdy_iobound_plug_copy TYPE STANDARD TABLE OF wdy_iobound_plug.
  FIELD-SYMBOLS: <wdy_iobound_plug> TYPE wdy_iobound_plug.
  SELECT * FROM wdy_iobound_plug INTO TABLE it_wdy_iobound_plug
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_iobound_plug ASSIGNING <wdy_iobound_plug> WHERE version <> 'D'.
          IF <wdy_iobound_plug>-version = 'A'.
            READ TABLE it_wdy_iobound_plug TRANSPORTING NO FIELDS
              WITH KEY plug_name = <wdy_iobound_plug>-plug_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_iobound_plug>-version   = if_wdy_md_object=>co_version_inactive.
          wdy_iobound_plug_node = xmldoc->create_element( 'wdy_iobound_plug' ).
          setattributesfromstructure( node = wdy_iobound_plug_node structure = <wdy_iobound_plug> ).
          rc = view_definition_node->append_child( wdy_iobound_plug_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_iobound_plug ASSIGNING <wdy_iobound_plug> WHERE version = 'A'.
          <wdy_iobound_plug>-version   = if_wdy_md_object=>co_version_inactive.
          wdy_iobound_plug_node = xmldoc->create_element( 'wdy_iobound_plug' ).
          setattributesfromstructure( node = wdy_iobound_plug_node structure = <wdy_iobound_plug> ).
          rc = view_definition_node->append_child( wdy_iobound_plug_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy wdy_iobound_plgt
  DATA it_wdy_iobound_plgt TYPE STANDARD TABLE OF wdy_iobound_plgt.
  FIELD-SYMBOLS: <plgt> TYPE wdy_iobound_plgt.
  SELECT * FROM wdy_iobound_plgt INTO TABLE it_wdy_iobound_plgt
    WHERE component_name = definition-component_name
      AND view_name = definition-view_name.
  IF sy-subrc = 0.
    LOOP AT it_wdy_iobound_plgt ASSIGNING <plgt>.
      wdy_iobound_plgt_node = xmldoc->create_element( 'wdy_iobound_plgt' ).
      setattributesfromstructure( node = wdy_iobound_plgt_node structure = <plgt> ).
      rc = view_definition_node->append_child( wdy_iobound_plgt_node ).
    ENDLOOP.
  ENDIF.

* copy WDY_UI_ELEMENT.
  DATA it_wdy_ui_element TYPE STANDARD TABLE OF wdy_ui_element.
  FIELD-SYMBOLS: <wdy_ui_element> TYPE wdy_ui_element.
  SELECT * FROM wdy_ui_element INTO TABLE it_wdy_ui_element
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ui_element ASSIGNING <wdy_ui_element> WHERE version <> 'D'.
          IF <wdy_ui_element>-version = 'A'.
            READ TABLE it_wdy_ui_element TRANSPORTING NO FIELDS
              WITH KEY element_name = <wdy_ui_element>-element_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ui_element>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_element_node = xmldoc->create_element( 'wdy_ui_element' ).
          setattributesfromstructure( node = wdy_ui_element_node structure = <wdy_ui_element> ).
          rc = view_definition_node->append_child( wdy_ui_element_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ui_element ASSIGNING <wdy_ui_element> WHERE version = 'A'.
          <wdy_ui_element>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_element_node = xmldoc->create_element( 'wdy_ui_element' ).
          setattributesfromstructure( node = wdy_ui_element_node structure = <wdy_ui_element> ).
          rc = view_definition_node->append_child( wdy_ui_element_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy WDY_UI_PROPERTY.
  DATA text TYPE string.
  DATA text_id TYPE wdy_md_translatable_text.
  DATA text_repository TYPE REF TO if_wdy_md_text_repository.
  text_repository = view->if_wdy_md_lockable_object~get_text_repository( ).
  DATA it_wdy_ui_property TYPE STANDARD TABLE OF wdy_ui_property.

  FIELD-SYMBOLS: <wdy_ui_property> TYPE wdy_ui_property.
  SELECT * FROM wdy_ui_property INTO TABLE it_wdy_ui_property
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  IF sy-subrc = 0.
    DATA str TYPE string.
    DATA header TYPE sotr_head.
    DATA concept TYPE sotr_head-concept.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ui_property ASSIGNING <wdy_ui_property> WHERE version <> 'D'.
          IF <wdy_ui_property>-version = 'A'.
            READ TABLE it_wdy_ui_property TRANSPORTING NO FIELDS
              WITH KEY element_name  = <wdy_ui_property>-element_name
                       property_name = <wdy_ui_property>-property_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ui_property>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_property_node = xmldoc->create_element( 'wdy_ui_property' ).
          setattributesfromstructure( node = wdy_ui_property_node structure = <wdy_ui_property> ).
          rc = view_definition_node->append_child( wdy_ui_property_node ).
          TRY.
              str = <wdy_ui_property>-property_value.
              text = text_repository->get_text( str ).
              rc = wdy_ui_property_node->set_attribute( name = 'PropText' value = text ).
            CATCH cx_root.
          ENDTRY.
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ui_property ASSIGNING <wdy_ui_property> WHERE version = 'A'.
          <wdy_ui_property>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_property_node = xmldoc->create_element( 'wdy_ui_property' ).
          setattributesfromstructure( node = wdy_ui_property_node structure = <wdy_ui_property> ).
          rc = view_definition_node->append_child( wdy_ui_property_node ).
          TRY.
              str = <wdy_ui_property>-property_value.
              text = text_repository->get_text( str ).
              rc = wdy_ui_property_node->set_attribute( name = 'PropText' value = text ).
            CATCH cx_root.
          ENDTRY.
        ENDLOOP.
    ENDCASE.
  ENDIF.

* copy WDY_VIEW_CNTR.
  DATA it_wdy_view_cntr TYPE STANDARD TABLE OF wdy_view_cntr.
  FIELD-SYMBOLS: <wdy_view_cntr> TYPE wdy_view_cntr.
  SELECT * FROM wdy_view_cntr INTO TABLE it_wdy_view_cntr
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_view_cntr ASSIGNING <wdy_view_cntr> WHERE version <> 'D'.
          IF <wdy_view_cntr>-version = 'A'.
            READ TABLE it_wdy_view_cntr TRANSPORTING NO FIELDS
              WITH KEY container_name  = <wdy_view_cntr>-container_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_view_cntr>-version = if_wdy_md_object=>co_version_inactive.
          wdy_view_cntr_node = xmldoc->create_element( 'wdy_view_cntr' ).
          setattributesfromstructure( node = wdy_view_cntr_node structure = <wdy_view_cntr> ).
          rc = view_definition_node->append_child( wdy_view_cntr_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_view_cntr ASSIGNING <wdy_view_cntr> WHERE version = 'A'.
          <wdy_view_cntr>-version = if_wdy_md_object=>co_version_inactive.
          wdy_view_cntr_node = xmldoc->create_element( 'wdy_view_cntr' ).
          setattributesfromstructure( node = wdy_view_cntr_node structure = <wdy_view_cntr> ).
          rc = view_definition_node->append_child( wdy_view_cntr_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.


* copy wdy_view_cntrt
  DATA wdy_view_cntrt_table TYPE STANDARD TABLE OF wdy_view_cntrt.
  FIELD-SYMBOLS: <wdy_view_cntrt> TYPE wdy_view_cntrt.
  SELECT * FROM wdy_view_cntrt INTO TABLE wdy_view_cntrt_table
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  LOOP AT wdy_view_cntrt_table ASSIGNING <wdy_view_cntrt>.
    wdy_view_cntrt_node = xmldoc->create_element( 'wdy_view_cntrt' ).
    setattributesfromstructure( node = wdy_view_cntrt_node structure = <wdy_view_cntrt> ).
    rc = view_definition_node->append_child( wdy_view_cntrt_node ).
  ENDLOOP.

*  copy WDY_PLUG_PARAM.
  DATA it_wdy_plug_param TYPE STANDARD TABLE OF wdy_plug_param.
  FIELD-SYMBOLS: <wdy_plug_param> TYPE wdy_plug_param.
  SELECT * FROM wdy_plug_param INTO TABLE it_wdy_plug_param
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_plug_param ASSIGNING <wdy_plug_param> WHERE version <> 'D'.
          IF <wdy_plug_param>-version = 'A'.
            READ TABLE it_wdy_plug_param TRANSPORTING NO FIELDS
              WITH KEY plug_name = <wdy_plug_param>-plug_name
                       parameter_name = <wdy_plug_param>-parameter_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_plug_param>-version = if_wdy_md_object=>co_version_inactive.
          wdy_plug_param_node = xmldoc->create_element( 'wdy_plug_param' ).
          setattributesfromstructure( node = wdy_plug_param_node structure = <wdy_plug_param> ).
          rc = view_definition_node->append_child( wdy_plug_param_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_plug_param ASSIGNING <wdy_plug_param> WHERE version = 'A'.
          <wdy_plug_param>-version = if_wdy_md_object=>co_version_inactive.
          wdy_plug_param_node = xmldoc->create_element( 'wdy_plug_param' ).
          setattributesfromstructure( node = wdy_plug_param_node structure = <wdy_plug_param> ).
          rc = view_definition_node->append_child( wdy_plug_param_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

*  copy WDY_UI_CTX_BIND.
  DATA it_wdy_ui_ctx_bind TYPE STANDARD TABLE OF wdy_ui_ctx_bind.
  FIELD-SYMBOLS: <wdy_ui_ctx_bind> TYPE wdy_ui_ctx_bind.
  SELECT * FROM wdy_ui_ctx_bind INTO TABLE it_wdy_ui_ctx_bind
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ui_ctx_bind ASSIGNING <wdy_ui_ctx_bind> WHERE version <> 'D'.
          IF <wdy_ui_ctx_bind>-version = 'A'.
            READ TABLE it_wdy_ui_ctx_bind TRANSPORTING NO FIELDS
              WITH KEY binding_name = <wdy_ui_ctx_bind>-binding_name
                       element_name = <wdy_ui_ctx_bind>-element_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ui_ctx_bind>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_ctx_bind_node = xmldoc->create_element( 'wdy_ui_ctx_bind' ).
          setattributesfromstructure( node = wdy_ui_ctx_bind_node structure = <wdy_ui_ctx_bind> ).
          rc = view_definition_node->append_child( wdy_ui_ctx_bind_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ui_ctx_bind ASSIGNING <wdy_ui_ctx_bind> WHERE version = 'A'.
          <wdy_ui_ctx_bind>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_ctx_bind_node = xmldoc->create_element( 'wdy_ui_ctx_bind' ).
          setattributesfromstructure( node = wdy_ui_ctx_bind_node structure = <wdy_ui_ctx_bind> ).
          rc = view_definition_node->append_child( wdy_ui_ctx_bind_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

*  copy WDY_UI_DDIC_BIND.
  DATA it_wdy_ui_ddic_bind TYPE STANDARD TABLE OF wdy_ui_ddic_bind.
  FIELD-SYMBOLS: <wdy_ui_ddic_bind> TYPE wdy_ui_ddic_bind.
  SELECT * FROM wdy_ui_ddic_bind INTO TABLE it_wdy_ui_ddic_bind
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ui_ddic_bind ASSIGNING <wdy_ui_ddic_bind> WHERE version <> 'D'.
          IF <wdy_ui_ddic_bind>-version = 'A'.
            READ TABLE it_wdy_ui_ddic_bind TRANSPORTING NO FIELDS
              WITH KEY binding_name = <wdy_ui_ddic_bind>-binding_name
                       element_name = <wdy_ui_ddic_bind>-element_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ui_ddic_bind>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_ddic_bind_node = xmldoc->create_element( 'wdy_ui_ddic_bind' ).
          setattributesfromstructure( node = wdy_ui_ddic_bind_node structure = <wdy_ui_ddic_bind> ).
          rc = view_definition_node->append_child( wdy_ui_ddic_bind_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ui_ddic_bind ASSIGNING <wdy_ui_ddic_bind> WHERE version = 'A'.
          <wdy_ui_ddic_bind>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_ddic_bind_node = xmldoc->create_element( 'wdy_ui_ddic_bind' ).
          setattributesfromstructure( node = wdy_ui_ddic_bind_node structure = <wdy_ui_ddic_bind> ).
          rc = view_definition_node->append_child( wdy_ui_ddic_bind_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

*  copy WDY_UI_EVT_BIND.
  DATA it_wdy_ui_evt_bind TYPE STANDARD TABLE OF wdy_ui_evt_bind.
  FIELD-SYMBOLS: <wdy_ui_evt_bind> TYPE wdy_ui_evt_bind.
  SELECT * FROM wdy_ui_evt_bind INTO TABLE it_wdy_ui_evt_bind
    WHERE component_name = definition-component_name
      AND view_name  = definition-view_name.
  IF sy-subrc = 0.
    CASE definition-version.
      WHEN 'I'.
        LOOP AT it_wdy_ui_evt_bind ASSIGNING <wdy_ui_evt_bind> WHERE version <> 'D'.
          IF <wdy_ui_evt_bind>-version = 'A'.
            READ TABLE it_wdy_ui_evt_bind TRANSPORTING NO FIELDS
              WITH KEY binding_name = <wdy_ui_evt_bind>-binding_name
                       element_name = <wdy_ui_evt_bind>-element_name
                       version = 'I'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          <wdy_ui_evt_bind>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_evt_bind_node = xmldoc->create_element( 'wdy_ui_evt_bind' ).
          setattributesfromstructure( node = wdy_ui_evt_bind_node structure = <wdy_ui_evt_bind> ).
          rc = view_definition_node->append_child( wdy_ui_evt_bind_node ).
        ENDLOOP.
      WHEN 'A'.
        LOOP AT it_wdy_ui_evt_bind ASSIGNING <wdy_ui_evt_bind> WHERE version = 'A'.
          <wdy_ui_evt_bind>-version = if_wdy_md_object=>co_version_inactive.
          wdy_ui_evt_bind_node = xmldoc->create_element( 'wdy_ui_evt_bind' ).
          setattributesfromstructure( node = wdy_ui_evt_bind_node structure = <wdy_ui_evt_bind> ).
          rc = view_definition_node->append_child( wdy_ui_evt_bind_node ).
        ENDLOOP.
    ENDCASE.
  ENDIF.

  IF definition-type = 'CL_WDY_MD_WINDOW'.                  "#EC NOTEXT
*   copy WDY_NAV_LINK.
    DATA it_wdy_nav_link TYPE STANDARD TABLE OF wdy_nav_link.
    FIELD-SYMBOLS: <wdy_nav_link> TYPE wdy_nav_link.
    SELECT * FROM wdy_nav_link INTO TABLE it_wdy_nav_link
      WHERE component_name = definition-component_name
        AND window_name  = definition-view_name.
    IF sy-subrc = 0.
      CASE definition-version.
        WHEN 'I'.
          LOOP AT it_wdy_nav_link ASSIGNING <wdy_nav_link> WHERE version <> 'D'.
            IF <wdy_nav_link>-version = 'A'.
              READ TABLE it_wdy_nav_link TRANSPORTING NO FIELDS
                WITH KEY nav_link_name = <wdy_nav_link>-nav_link_name
                         version = 'I'.
              IF sy-subrc = 0.
                CONTINUE.
              ENDIF.
            ENDIF.
            <wdy_nav_link>-version = if_wdy_md_object=>co_version_inactive.
            wdy_nav_link_node = xmldoc->create_element( 'wdy_nav_link' ).
            setattributesfromstructure( node = wdy_nav_link_node structure = <wdy_nav_link> ).
            rc = view_definition_node->append_child( wdy_nav_link_node ).
          ENDLOOP.
        WHEN 'A'.
          LOOP AT it_wdy_nav_link ASSIGNING <wdy_nav_link> WHERE version = 'A'.
            <wdy_nav_link>-version = if_wdy_md_object=>co_version_inactive.
            wdy_nav_link_node = xmldoc->create_element( 'wdy_nav_link' ).
            setattributesfromstructure( node = wdy_nav_link_node structure = <wdy_nav_link> ).
            rc = view_definition_node->append_child( wdy_nav_link_node ).
          ENDLOOP.
      ENDCASE.
    ENDIF.

*   copy WDY_NAV_TARGREF.
    DATA it_wdy_nav_targref TYPE STANDARD TABLE OF wdy_nav_targref.
    FIELD-SYMBOLS: <wdy_nav_targref> TYPE wdy_nav_targref.
    SELECT * FROM wdy_nav_targref INTO TABLE it_wdy_nav_targref
      WHERE component_name = definition-component_name
        AND window_name  = definition-view_name.
    IF sy-subrc = 0.
      CASE definition-version.
        WHEN 'I'.
          LOOP AT it_wdy_nav_targref ASSIGNING <wdy_nav_targref> WHERE version <> 'D'.
            IF <wdy_nav_targref>-version = 'A'.
              READ TABLE it_wdy_nav_targref TRANSPORTING NO FIELDS
                WITH KEY nav_link_name = <wdy_nav_targref>-nav_link_name
                         nav_targref_name = <wdy_nav_targref>-nav_targref_name
                         version = 'I'.
              IF sy-subrc = 0.
                CONTINUE.
              ENDIF.
            ENDIF.
            <wdy_nav_targref>-version = if_wdy_md_object=>co_version_inactive.
            wdy_nav_targref_node = xmldoc->create_element( 'wdy_nav_targref' ).
            setattributesfromstructure( node = wdy_nav_targref_node structure = <wdy_nav_targref> ).
            rc = view_definition_node->append_child( wdy_nav_targref_node ).
          ENDLOOP.
        WHEN 'A'.
          LOOP AT it_wdy_nav_targref ASSIGNING <wdy_nav_targref> WHERE version = 'A'.
            <wdy_nav_targref>-version = if_wdy_md_object=>co_version_inactive.
            wdy_nav_targref_node = xmldoc->create_element( 'wdy_nav_targref' ).
            setattributesfromstructure( node = wdy_nav_targref_node structure = <wdy_nav_targref> ).
            rc = view_definition_node->append_child( wdy_nav_targref_node ).
          ENDLOOP.
      ENDCASE.
    ENDIF.

*   copy WDY_VSH_NODE.
    DATA it_wdy_vsh_node TYPE STANDARD TABLE OF wdy_vsh_node.
    FIELD-SYMBOLS: <wdy_vsh_node> TYPE wdy_vsh_node.
    SELECT * FROM wdy_vsh_node INTO TABLE it_wdy_vsh_node
      WHERE component_name = definition-component_name
        AND window_name  = definition-view_name.
    IF sy-subrc = 0.
      CASE definition-version.
        WHEN 'I'.
          LOOP AT it_wdy_vsh_node ASSIGNING <wdy_vsh_node> WHERE version <> 'D'.
            IF <wdy_vsh_node>-version = 'A'.
              READ TABLE it_wdy_vsh_node TRANSPORTING NO FIELDS
                WITH KEY vsh_node_name = <wdy_vsh_node>-vsh_node_name
                         version = 'I'.
              IF sy-subrc = 0.
                CONTINUE.
              ENDIF.
            ENDIF.
            <wdy_vsh_node>-version = if_wdy_md_object=>co_version_inactive.
            wdy_vsh_node_node = xmldoc->create_element( 'wdy_vsh_node' ).
            setattributesfromstructure( node = wdy_vsh_node_node structure = <wdy_vsh_node> ).
            rc = view_definition_node->append_child( wdy_vsh_node_node ).
          ENDLOOP.
        WHEN 'A'.
          LOOP AT it_wdy_vsh_node ASSIGNING <wdy_vsh_node> WHERE version = 'A'.
            <wdy_vsh_node>-version = if_wdy_md_object=>co_version_inactive.
            wdy_vsh_node_node = xmldoc->create_element( 'wdy_vsh_node' ).
            setattributesfromstructure( node = wdy_vsh_node_node structure = <wdy_vsh_node> ).
            rc = view_definition_node->append_child( wdy_vsh_node_node ).
          ENDLOOP.
      ENDCASE.
    ENDIF.

*   copy WDY_VSH_PHOLDER.
    DATA it_wdy_vsh_pholder TYPE STANDARD TABLE OF wdy_vsh_pholder.
    FIELD-SYMBOLS: <wdy_vsh_pholder> TYPE wdy_vsh_pholder.
    SELECT * FROM wdy_vsh_pholder INTO TABLE it_wdy_vsh_pholder
      WHERE component_name = definition-component_name
        AND window_name  = definition-view_name.
    IF sy-subrc = 0.
      CASE definition-version.
        WHEN 'I'.
          LOOP AT it_wdy_vsh_pholder ASSIGNING <wdy_vsh_pholder> WHERE version <> 'D'.
            IF <wdy_vsh_pholder>-version = 'A'.
              READ TABLE it_wdy_vsh_pholder TRANSPORTING NO FIELDS
                WITH KEY vsh_node_name = <wdy_vsh_pholder>-vsh_node_name
                         vsh_pholder_name = <wdy_vsh_pholder>-vsh_pholder_name
                         version = 'I'.
              IF sy-subrc = 0.
                CONTINUE.
              ENDIF.
            ENDIF.
            <wdy_vsh_pholder>-version = if_wdy_md_object=>co_version_inactive.
            wdy_vsh_pholder_node = xmldoc->create_element( 'wdy_vsh_pholder' ).
            setattributesfromstructure( node = wdy_vsh_pholder_node structure = <wdy_vsh_pholder> ).
            rc = view_definition_node->append_child( wdy_vsh_pholder_node ).
          ENDLOOP.
        WHEN 'A'.
          LOOP AT it_wdy_vsh_pholder ASSIGNING <wdy_vsh_pholder> WHERE version = 'A'.
            <wdy_vsh_pholder>-version = if_wdy_md_object=>co_version_inactive.
            wdy_vsh_pholder_node = xmldoc->create_element( 'wdy_vsh_pholder' ).
            setattributesfromstructure( node = wdy_vsh_pholder_node structure = <wdy_vsh_pholder> ).
            rc = view_definition_node->append_child( wdy_vsh_pholder_node ).
          ENDLOOP.
      ENDCASE.
    ENDIF.

*   copy WDY_VS_PROPERTY.
    DATA it_wdy_vs_property TYPE STANDARD TABLE OF wdy_vs_property.
    FIELD-SYMBOLS: <wdy_vs_property> TYPE wdy_vs_property.
    SELECT * FROM wdy_vs_property INTO TABLE it_wdy_vs_property
      WHERE component_name = definition-component_name
        AND window_name  = definition-view_name.
    IF sy-subrc = 0.
      CASE definition-version.
        WHEN 'I'.
          LOOP AT it_wdy_vs_property ASSIGNING <wdy_vs_property> WHERE version <> 'D'.
            IF <wdy_vs_property>-version = 'A'.
              READ TABLE it_wdy_vs_property TRANSPORTING NO FIELDS
                WITH KEY vsh_node_name = <wdy_vs_property>-vsh_node_name
                         vs_prop_def_name = <wdy_vs_property>-vs_prop_def_name
                         version = 'I'.
              IF sy-subrc = 0.
                CONTINUE.
              ENDIF.
            ENDIF.
            <wdy_vs_property>-version = if_wdy_md_object=>co_version_inactive.
            wdy_vs_property_node = xmldoc->create_element( 'wdy_vs_property' ).
            setattributesfromstructure( node = wdy_vs_property_node structure = <wdy_vs_property> ).
            rc = view_definition_node->append_child( wdy_vs_property_node ).
          ENDLOOP.
        WHEN 'A'.
          LOOP AT it_wdy_vs_property ASSIGNING <wdy_vs_property> WHERE version = 'A'.
            <wdy_vs_property>-version = if_wdy_md_object=>co_version_inactive.
            wdy_vs_property_node = xmldoc->create_element( 'wdy_vs_property' ).
            setattributesfromstructure( node = wdy_vs_property_node structure = <wdy_vs_property> ).
            rc = view_definition_node->append_child( wdy_vs_property_node ).
          ENDLOOP.
      ENDCASE.
    ENDIF.

  ENDIF.

endmethod.


method XML_TO_CONTROLLER.
*xml nodes
  DATA definition TYPE wdy_controller.
  DATA rc TYPE i.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.


  CALL METHOD getstructurefromattributes
    EXPORTING
      node            = xml_node
      preserveversion = abap_true
    CHANGING
      structure       = definition.

  DATA: controller_key TYPE wdy_controller_key,
*         controller TYPE REF TO if_wdy_md_controller,
         context_node TYPE REF TO if_wdy_md_context_node.

  controller_key-component_name = definition-component_name.
  controller_key-controller_name = definition-controller_name.

  CALL METHOD cl_wdy_md_controller=>create
    EXPORTING
      component_name             = controller_key-component_name
      controller_name            = controller_key-controller_name
      suppress_access_permission = 'X'
    RECEIVING
      controller                 = controller.

  controller->set_type( definition-controller_type ).
  controller->save_to_database( ).
  TRANSLATE controller_key TO UPPER CASE.                "#EC TRANSLANG

  IF NOT definition-context IS INITIAL.
    UPDATE wdy_controller
      SET context = 'CONTEXT'
      WHERE component_name  = controller_key-component_name
        AND controller_name = controller_key-controller_name
        AND version         = if_wdy_md_object=>co_version_inactive.
    definition-context = 'CONTEXT'.
  ENDIF.

* copy wdy_controllert
  DATA wdy_controllert_table TYPE STANDARD TABLE OF wdy_controllert.
  DATA wdy_controllert TYPE wdy_controllert.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_controllert' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_controllert.
    APPEND wdy_controllert TO wdy_controllert_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_controllert FROM TABLE wdy_controllert_table.

* copy components
  DATA it_wdy_ctlr_compo TYPE STANDARD TABLE OF wdy_ctlr_compo.
  DATA wdy_ctlr_compo TYPE wdy_ctlr_compo.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ctlr_compo' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ctlr_compo.
    APPEND wdy_ctlr_compo TO it_wdy_ctlr_compo.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ctlr_compo FROM TABLE it_wdy_ctlr_compo.

* copy wdy_ctlr_compot
  DATA wdy_ctlr_compot_table TYPE STANDARD TABLE OF wdy_ctlr_compot.
  DATA wdy_ctlr_compot TYPE wdy_ctlr_compot.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ctlr_compot' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ctlr_compot.
    APPEND wdy_ctlr_compot TO wdy_ctlr_compot_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ctlr_compot FROM TABLE wdy_ctlr_compot_table.

* copy parameters
  DATA it_wdy_ctlr_param TYPE STANDARD TABLE OF wdy_ctlr_param.
  DATA wdy_ctlr_param TYPE wdy_ctlr_param.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ctlr_param' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ctlr_param.
    APPEND wdy_ctlr_param TO it_wdy_ctlr_param.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ctlr_param FROM TABLE it_wdy_ctlr_param.

* copy wdy_ctlr_paramt
  DATA wdy_ctlr_paramt_table TYPE STANDARD TABLE OF wdy_ctlr_paramt.
  DATA wdy_ctlr_paramt TYPE wdy_ctlr_paramt.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ctlr_paramt' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ctlr_paramt.
    APPEND wdy_ctlr_paramt TO wdy_ctlr_paramt_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ctlr_paramt FROM TABLE wdy_ctlr_paramt_table.

* copy controller usages
  DATA it_wdy_ctlr_usage TYPE STANDARD TABLE OF wdy_ctlr_usage.
  DATA wdy_ctlr_usage TYPE wdy_ctlr_usage.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ctlr_usage' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ctlr_usage.
    APPEND wdy_ctlr_usage TO it_wdy_ctlr_usage.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ctlr_usage FROM TABLE it_wdy_ctlr_usage.

* copy context nodes
  DATA it_wdy_ctx_node TYPE STANDARD TABLE OF wdy_ctx_node.
  DATA wdy_ctx_node TYPE wdy_ctx_node.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ctx_node' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ctx_node.
    APPEND wdy_ctx_node TO it_wdy_ctx_node.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ctx_node FROM TABLE it_wdy_ctx_node.

* copy context attributes
  DATA it_wdy_ctx_attrib TYPE STANDARD TABLE OF wdy_ctx_attrib.
  DATA wdy_ctx_attrib TYPE wdy_ctx_attrib.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ctx_attrib' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ctx_attrib.
    APPEND wdy_ctx_attrib TO it_wdy_ctx_attrib.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ctx_attrib FROM TABLE it_wdy_ctx_attrib.

* copy context mapping
  DATA it_wdy_ctx_mapping TYPE STANDARD TABLE OF wdy_ctx_mapping.
  DATA wdy_ctx_mapping TYPE wdy_ctx_mapping.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ctx_mapping' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ctx_mapping.
    APPEND wdy_ctx_mapping TO it_wdy_ctx_mapping.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_ctx_mapping FROM TABLE it_wdy_ctx_mapping.

* if the controller is a configuration
  IF definition-controller_type = wdyn_ctlr_type_custom AND
     cl_wdy_md_component=>get_config_controller_name( definition-component_name ) = definition-controller_name.
    DATA component TYPE REF TO cl_wdy_md_component.
    component ?= controller->if_wdy_md_object~get_parent( ).
    component->if_wdy_md_component~set_configuration_controller( controller ).
    component->save_to_db_definition_only( ).
  ENDIF.

* unlock
  controller->unlock( ).
  controller->reload( ).

endmethod.


METHOD xml_to_view.
*xml nodes
  DATA definition TYPE wdy_view.
  DATA rc TYPE i.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.


  CALL METHOD getstructurefromattributes
    EXPORTING
      node            = xml_node
      preserveversion = abap_true
    CHANGING
      structure       = definition.

  DATA: view     TYPE REF TO if_wdy_md_view,
        abstract_view TYPE REF TO if_wdy_md_abstract_view,
        view_key TYPE wdy_md_view_key,
        window   TYPE REF TO cl_wdy_md_window. "if_wdy_md_window.
  view_key-component_name = definition-component_name.
  view_key-view_name = definition-view_name.



  CALL METHOD cl_wdy_md_abstract_view=>create
    EXPORTING
      component_name             = view_key-component_name
      view_name                  = view_key-view_name
      suppress_access_permission = 'X'
      type                       = definition-type
    RECEIVING
      view                       = abstract_view.

  abstract_view->set_type( definition-view_type ).
  abstract_view->set_lifespan( definition-lifespan ).

  TRY.
      view ?= abstract_view.
      view->create_root_container( ).
    CATCH cx_sy_move_cast_error.
  ENDTRY.


  TRY.
      DATA def_root_node TYPE REF TO if_wdy_md_vset_hierarchy_node.
      DATA title TYPE wdy_md_translatable_text.
      window ?= abstract_view.
      title = definition-title.
      IF definition-def_root_node IS NOT INITIAL.
        def_root_node = window->find_viewset_hierarchy_node( name = definition-def_root_node ).
        IF def_root_node IS INITIAL.
          def_root_node = window->if_wdy_md_window~create_root_node( name = definition-def_root_node
                                                                      type = 'CL_WDY_MD_VIEW_USAGE' ).
        ENDIF.
        window->if_wdy_md_window~set_default_root_node( def_root_node ).
      ENDIF.

      IF NOT title IS INITIAL.
        window->if_wdy_md_window~set_title( title = title ).
      ENDIF.
    CATCH cx_sy_move_cast_error.
  ENDTRY.

  abstract_view->save_to_database( ).
  TRANSLATE view_key TO UPPER CASE.                      "#EC TRANSLANG

* copy wdy_viewt
  DATA wdy_viewt_table TYPE STANDARD TABLE OF wdy_viewt.
  DATA wdy_viewt TYPE wdy_viewt.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_viewt' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_viewt.
    APPEND wdy_viewt TO wdy_viewt_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_viewt FROM TABLE wdy_viewt_table.

* copy wdy_iobound_plug.
  DATA it_wdy_iobound_plug TYPE STANDARD TABLE OF wdy_iobound_plug.
  DATA wdy_iobound_plug TYPE wdy_iobound_plug.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_iobound_plug' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_iobound_plug.
    APPEND wdy_iobound_plug TO it_wdy_iobound_plug.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_iobound_plug FROM TABLE it_wdy_iobound_plug.

* copy wdy_iobound_plgt
  DATA it_wdy_iobound_plgt TYPE STANDARD TABLE OF wdy_iobound_plgt.
  DATA plgt TYPE wdy_iobound_plgt.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_iobound_plgt' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = plgt.
    APPEND plgt TO it_wdy_iobound_plgt.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_iobound_plgt FROM TABLE it_wdy_iobound_plgt.


* copy WDY_UI_ELEMENT.
  DATA it_wdy_ui_element TYPE STANDARD TABLE OF wdy_ui_element.
  DATA wdy_ui_element TYPE wdy_ui_element.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ui_element' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ui_element.
    APPEND wdy_ui_element TO it_wdy_ui_element.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_ui_element FROM TABLE it_wdy_ui_element.

* copy WDY_UI_PROPERTY.
  DATA text_repository_of_copy TYPE REF TO if_wdy_md_text_repository.
  DATA text TYPE string.
  DATA text_id TYPE wdy_md_translatable_text.
  DATA str TYPE string.
  DATA header TYPE sotr_head.
  DATA concept TYPE sotr_head-concept.
  text_repository_of_copy ?= abstract_view->if_wdy_md_lockable_object~get_text_repository( ).
  DATA it_wdy_ui_property TYPE STANDARD TABLE OF wdy_ui_property.
  DATA wdy_ui_property TYPE wdy_ui_property.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ui_property' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ui_property.
    TRY.
        str = wdy_ui_property-property_value.
        text = text_repository_of_copy->get_text( str ).
        IF NOT text IS INITIAL.
*               check if the guid corresponds to an otr alias, in that case DO NOT create a new one
          concept = str.
          CALL FUNCTION 'SOTR_GET_CONCEPT'
            EXPORTING
              concept = concept
            IMPORTING
              header  = header.
          IF header-alias_name IS INITIAL.
            wdy_ui_property-property_value = text_repository_of_copy->create_text( text ).
          ENDIF.
        ENDIF.
      CATCH cx_wb_text_not_existing.
        CLEAR str.
        str = node->get_attribute( name = 'PropText' ).
        IF str IS NOT INITIAL.
          wdy_ui_property-property_value = text_repository_of_copy->create_text( str ).
        ENDIF.
      CATCH cx_wb_text_create_exception.
        CLEAR wdy_ui_property-property_value.
    ENDTRY.
    APPEND wdy_ui_property TO it_wdy_ui_property.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_ui_property FROM TABLE it_wdy_ui_property.

* copy WDY_VIEW_CNTR.
  DATA it_wdy_view_cntr TYPE STANDARD TABLE OF wdy_view_cntr.
  DATA wdy_view_cntr TYPE wdy_view_cntr.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_view_cntr' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_view_cntr.
    APPEND wdy_view_cntr TO it_wdy_view_cntr.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_view_cntr FROM TABLE it_wdy_view_cntr.

* copy wdy_view_cntrt
  DATA wdy_view_cntrt_table TYPE STANDARD TABLE OF wdy_view_cntrt.
  DATA wdy_view_cntrt TYPE wdy_view_cntrt.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_view_cntrt' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_view_cntrt.
    APPEND wdy_view_cntrt TO wdy_view_cntrt_table.
    node ?= iterator->get_next( ).
  ENDWHILE.
  INSERT wdy_view_cntrt FROM TABLE wdy_view_cntrt_table.

*  copy WDY_PLUG_PARAM.
  DATA it_wdy_plug_param TYPE STANDARD TABLE OF wdy_plug_param.
  DATA wdy_plug_param TYPE wdy_plug_param.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_plug_param' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_plug_param.
    APPEND wdy_plug_param TO it_wdy_plug_param.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_plug_param FROM TABLE it_wdy_plug_param.

*  copy WDY_UI_CTX_BIND.
  DATA it_wdy_ui_ctx_bind TYPE STANDARD TABLE OF wdy_ui_ctx_bind.
  DATA wdy_ui_ctx_bind TYPE wdy_ui_ctx_bind.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ui_ctx_bind' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ui_ctx_bind.
    APPEND wdy_ui_ctx_bind TO it_wdy_ui_ctx_bind.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_ui_ctx_bind FROM TABLE it_wdy_ui_ctx_bind.

*  copy WDY_UI_DDIC_BIND.
  DATA it_wdy_ui_ddic_bind TYPE STANDARD TABLE OF wdy_ui_ddic_bind.
  DATA wdy_ui_ddic_bind TYPE wdy_ui_ddic_bind.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ui_ddic_bind' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ui_ddic_bind.
    APPEND wdy_ui_ddic_bind TO it_wdy_ui_ddic_bind.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_ui_ddic_bind FROM TABLE it_wdy_ui_ddic_bind.

*  copy WDY_UI_EVT_BIND.
  DATA it_wdy_ui_evt_bind TYPE STANDARD TABLE OF wdy_ui_evt_bind.
  DATA wdy_ui_evt_bind TYPE wdy_ui_evt_bind.
  FREE: filter, iterator, node.
  filter = xml_node->create_filter_name( 'wdy_ui_evt_bind' ).
  iterator = xml_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node            = node
        preserveversion = abap_true
      CHANGING
        structure       = wdy_ui_evt_bind.
    APPEND wdy_ui_evt_bind TO it_wdy_ui_evt_bind.
    node ?= iterator->get_next( ).
  ENDWHILE.
  MODIFY wdy_ui_evt_bind FROM TABLE it_wdy_ui_evt_bind.

  IF definition-type = 'CL_WDY_MD_WINDOW'.                  "#EC NOTEXT
*   copy WDY_NAV_LINK.
    DATA it_wdy_nav_link TYPE STANDARD TABLE OF wdy_nav_link.
    DATA wdy_nav_link TYPE wdy_nav_link.
    FREE: filter, iterator, node.
    filter = xml_node->create_filter_name( 'wdy_nav_link' ).
    iterator = xml_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node            = node
          preserveversion = abap_true
        CHANGING
          structure       = wdy_nav_link.
      APPEND wdy_nav_link TO it_wdy_nav_link.
      node ?= iterator->get_next( ).
    ENDWHILE.
    MODIFY wdy_nav_link FROM TABLE it_wdy_nav_link.

*   copy WDY_NAV_TARGREF.
    DATA it_wdy_nav_targref TYPE STANDARD TABLE OF wdy_nav_targref.
    DATA wdy_nav_targref TYPE wdy_nav_targref.
    FREE: filter, iterator, node.
    filter = xml_node->create_filter_name( 'wdy_nav_targref' ).
    iterator = xml_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node            = node
          preserveversion = abap_true
        CHANGING
          structure       = wdy_nav_targref.
      APPEND wdy_nav_targref TO it_wdy_nav_targref.
      node ?= iterator->get_next( ).
    ENDWHILE.
    MODIFY wdy_nav_targref FROM TABLE it_wdy_nav_targref.

*   copy WDY_VSH_NODE.
    DATA it_wdy_vsh_node TYPE STANDARD TABLE OF wdy_vsh_node.
    DATA wdy_vsh_node TYPE wdy_vsh_node.
    FREE: filter, iterator, node.
    filter = xml_node->create_filter_name( 'wdy_vsh_node' ).
    iterator = xml_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node            = node
          preserveversion = abap_true
        CHANGING
          structure       = wdy_vsh_node.
      APPEND wdy_vsh_node TO it_wdy_vsh_node.
      node ?= iterator->get_next( ).
    ENDWHILE.
    MODIFY wdy_vsh_node FROM TABLE it_wdy_vsh_node.

*   copy WDY_VSH_PHOLDER.
    DATA it_wdy_vsh_pholder TYPE STANDARD TABLE OF wdy_vsh_pholder.
    DATA wdy_vsh_pholder TYPE wdy_vsh_pholder.
    FREE: filter, iterator, node.
    filter = xml_node->create_filter_name( 'wdy_vsh_pholder' ).
    iterator = xml_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node            = node
          preserveversion = abap_true
        CHANGING
          structure       = wdy_vsh_pholder.
      APPEND wdy_vsh_pholder TO it_wdy_vsh_pholder.
      node ?= iterator->get_next( ).
    ENDWHILE.
    MODIFY wdy_vsh_pholder FROM TABLE it_wdy_vsh_pholder.

*   copy WDY_VS_PROPERTY.
    DATA it_wdy_vs_property TYPE STANDARD TABLE OF wdy_vs_property.
    DATA wdy_vs_property TYPE wdy_vs_property.
    FREE: filter, iterator, node.
    filter = xml_node->create_filter_name( 'wdy_vs_property' ).
    iterator = xml_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node            = node
          preserveversion = abap_true
        CHANGING
          structure       = wdy_vs_property.
      APPEND wdy_vs_property TO it_wdy_vs_property.
      node ?= iterator->get_next( ).
    ENDWHILE.
    MODIFY wdy_vs_property FROM TABLE it_wdy_vs_property.

  ENDIF.

  abstract_view->unlock( ).
  abstract_view->reload( ).

  TRY.
* copy controllers of component
      DATA  controller TYPE REF TO if_wdy_md_controller.
      FREE: filter, iterator, node.
      filter = xml_node->create_filter_name( 'controller_definition' ).
      iterator = xml_node->create_iterator_filtered( filter ).
      node ?= iterator->get_next( ).
      WHILE node IS NOT INITIAL.
        TRY.
            controller = me->xml_to_controller(
                xml_node = node ).
            node ?= iterator->get_next( ).
            abstract_view->set_view_controller( controller ).
          CATCH cx_wdy_md_already_existing.
            node ?= iterator->get_next( ).
        ENDTRY.
      ENDWHILE.
      abstract_view->if_wdy_md_lockable_object~save_to_database( ).
    CATCH cx_wdy_md_already_existing.
  ENDTRY.

ENDMETHOD.
ENDCLASS.
