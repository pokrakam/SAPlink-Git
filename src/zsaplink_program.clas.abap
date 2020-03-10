CLASS zsaplink_program DEFINITION
  PUBLIC
  INHERITING FROM zsaplink
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
    METHODS createstringfromobject
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    METHODS get_source
      RETURNING
        VALUE(progsource) TYPE rswsourcet .
    METHODS update_wb_tree .
    METHODS create_textpool
      IMPORTING
        !textpoolnode TYPE REF TO if_ixml_element .
    METHODS dequeue_abap
      RAISING
        zcx_saplink .
    METHODS get_textpool
      RETURNING
        VALUE(textnode) TYPE REF TO if_ixml_element .
    METHODS create_documentation
      IMPORTING
        !docnode TYPE REF TO if_ixml_element .
    METHODS create_source
      IMPORTING
        !source  TYPE table_of_strings
        !attribs TYPE trdir .
    METHODS enqueue_abap
      RAISING
        zcx_saplink .
    METHODS get_documentation
      RETURNING
        VALUE(docnode) TYPE REF TO if_ixml_element .
    METHODS transport_copy
      IMPORTING
        !author   TYPE syuname
        !devclass TYPE devclass
      RAISING
        zcx_saplink .
    METHODS get_dynpro
      RETURNING
        VALUE(dynp_node) TYPE REF TO if_ixml_element .
    METHODS create_dynpro
      IMPORTING
        !dynp_node TYPE REF TO if_ixml_element .
    METHODS get_pfstatus
      RETURNING
        VALUE(pfstat_node) TYPE REF TO if_ixml_element .
    METHODS create_pfstatus
      IMPORTING
        !pfstat_node TYPE REF TO if_ixml_element .
ENDCLASS.



CLASS zsaplink_program IMPLEMENTATION.


  METHOD checkexists.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

    SELECT SINGLE name FROM trdir INTO objname WHERE name = objname.
    IF sy-subrc = 0.
      exists = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA sourcenode TYPE REF TO if_ixml_element.
    DATA textpoolnode TYPE REF TO if_ixml_element.
    DATA docnode TYPE REF TO if_ixml_element.
    DATA dynpronode TYPE REF TO if_ixml_element.
    DATA statusnode TYPE REF TO if_ixml_element.
    DATA rc TYPE sysubrc.
    DATA progattribs TYPE trdir.
    DATA progsource TYPE rswsourcet.
    DATA sourcestring TYPE string.
    DATA _objtype TYPE string.

    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    sourcenode = xmldoc->create_element( 'source' ).
    SELECT SINGLE * FROM trdir INTO progattribs WHERE name = objname.
    IF sy-subrc = 0.
      setattributesfromstructure( node = rootnode structure =  progattribs ).
      progsource = me->get_source( ).
      sourcestring = buildsourcestring( sourcetable = progsource ).
      rc = sourcenode->if_ixml_node~set_value( sourcestring ).
      textpoolnode = get_textpool( ).
      rc = rootnode->append_child( textpoolnode ).
      docnode = get_documentation( ).
      rc = rootnode->append_child( docnode ).
      dynpronode = get_dynpro( ).
      rc = rootnode->append_child( dynpronode ).
      statusnode = get_pfstatus( ).
      rc = rootnode->append_child( statusnode ).
      rc = rootnode->append_child( sourcenode ).
      rc = xmldoc->append_child( rootnode ).
      ixmldocument = xmldoc.
    ELSE.
      CLEAR ixmldocument.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found
          object = objname.
    ENDIF.
  ENDMETHOD.


  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA progattribs TYPE trdir.
    DATA sourcenode TYPE REF TO if_ixml_element.
    DATA textnode TYPE REF TO if_ixml_element.
    DATA docnode TYPE REF TO if_ixml_element.
    DATA dynpnode TYPE REF TO if_ixml_element.
    DATA statnode TYPE REF TO if_ixml_element.
    DATA source TYPE string.
    DATA sourcetable TYPE table_of_strings.
    DATA _objname(30) TYPE c.
    DATA aobjname TYPE trobj_name.
    DATA _objtype TYPE string.
    DATA checkexists TYPE flag.

*if sy-uname <> 'USDWM01'.
*    _objType = getObjectType( ).
*    xmlDoc = ixmlDocument.
*    rootNode = xmlDoc->find_from_name( _objType ).
*    call method GETSTRUCTUREFROMATTRIBUTES
*          exporting
*            node = rootNode
*          changing
*            structure = progAttribs.
*    objName = progAttribs-NAME.
*
**   check existing
*    select single name from trdir into objName where NAME = objName.
*    if sy-subrc = 0.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>existing.
*    endif.
*
*    sourceNode = rootNode->find_from_name( 'source' ).
*    source = sourceNode->get_value( ).
*    sourceTable = BUILDTABLEFROMSTRING( source ).
*    insert report progAttribs-NAME from sourceTable.
*
*    commit work.
*
*    call function 'RS_INSERT_INTO_WORKING_AREA'
*      EXPORTING
*        object            = 'REPS'
*        obj_name          = aobjName
*      EXCEPTIONS
*        wrong_object_name = 1.
*    if sy-subrc <> 0.
*
*    endif.
*
*else.

    _objtype = getobjecttype( ).
    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = progattribs.
    objname = progattribs-name.

*  check if object exists
*  select single name from trdir into objName where NAME = objName.
*  if sy-subrc = 0 and overwrite <> 'X'.
*    raise exception type zcx_saplink
*      exporting textid = zcx_saplink=>existing.
*  endif.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.


    enqueue_abap( ).
    transport_copy( author = progattribs-cnam devclass = devclass ).
    sourcenode = rootnode->find_from_name( 'source' ).
    source = sourcenode->get_value( ).
    sourcetable = buildtablefromstring( source ).
    create_source( source = sourcetable attribs = progattribs ).
    textnode = rootnode->find_from_name( 'textPool' ).
    create_textpool( textnode ).
    docnode = rootnode->find_from_name( 'programDocumentation' ).
    create_documentation( docnode ).
    dynpnode = rootnode->find_from_name( 'dynpros' ).
    create_dynpro( dynpnode ).
    statnode = rootnode->find_from_name( 'pfstatus' ).
    create_pfstatus( statnode ).

    dequeue_abap( ).
    update_wb_tree( ).
*endif.

* successful install
    name = objname.

  ENDMETHOD.


  METHOD createstringfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA progsource TYPE rswsourcet.
    progsource = me->get_source( ).
    string = buildsourcestring( sourcetable = progsource ).
  ENDMETHOD.


  METHOD create_documentation.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    DATA lang_node     TYPE REF TO if_ixml_element.
    DATA lang_filter   TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator TYPE REF TO if_ixml_node_iterator.

    DATA obj_name TYPE dokhl-object.
    DATA prog_name TYPE string.
    DATA language  TYPE string.
    DATA obj_langu TYPE dokhl-langu.
    DATA lv_str TYPE string.
    DATA rc TYPE sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    IF docnode IS NOT BOUND.
      RETURN.
    ENDIF.

    prog_name = docnode->get_attribute( name = 'OBJECT' ).
    obj_name = prog_name.

* If no prog name, then there was no program documenation, just return.
    IF prog_name IS INITIAL.
      RETURN.
    ENDIF.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = docnode->create_filter_name( `language` ).
    lang_iterator = docnode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      REFRESH lt_lines.
      language = lang_node->get_attribute( name = 'SPRAS' ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( `textLine` ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'RE'   "<-- Report/program documentation
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'RE'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Program Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD create_dynpro.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    TYPES: BEGIN OF tdyn_head_temp.
             INCLUDE TYPE d020s.
             TYPES: dtext TYPE d020t-dtxt.
    TYPES: END OF tdyn_head_temp.

    DATA: idyn_fldl TYPE TABLE OF d021s,
          idyn_flow TYPE TABLE OF d022s,
          idyn_mcod TYPE TABLE OF d023s.

    DATA: xdyn_head TYPE  d020s,
          xdyn_fldl TYPE  d021s,
          xdyn_flow TYPE  d022s,
          xdyn_mcod TYPE  d023s.

    DATA: xdyn_text_string TYPE string.
    DATA: xdyn_text        TYPE d020t-dtxt .
    DATA: xdyn_head_temp   TYPE tdyn_head_temp.

    DATA _objname TYPE trobj_name.

    DATA dynpros_node       TYPE REF TO if_ixml_element.
    DATA dynpros_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynpros_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynpro_node        TYPE REF TO if_ixml_element.
    DATA dynpro_filter      TYPE REF TO if_ixml_node_filter.
    DATA dynpro_iterator    TYPE REF TO if_ixml_node_iterator.

    DATA dynfldl_node       TYPE REF TO if_ixml_element.
    DATA dynfldl_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynfldl_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynmcod_node       TYPE REF TO if_ixml_element.
    DATA dynmcod_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynmcod_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynflow_node       TYPE REF TO if_ixml_element.

    DATA xdynpro_flow_source TYPE string.
    DATA idynpro_flow_source TYPE table_of_strings.

    _objname = objname.

    dynpros_node =  dynp_node.
    CHECK dynpros_node IS NOT INITIAL.

    FREE: dynpro_filter, dynpro_iterator, dynpro_node.
    dynpro_filter = dynpros_node->create_filter_name( 'dynpro' ).
    dynpro_iterator =
          dynpros_node->create_iterator_filtered( dynpro_filter ).
    dynpro_node ?= dynpro_iterator->get_next( ).

    WHILE dynpro_node IS NOT INITIAL.

      CLEAR:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
      REFRESH:  idyn_fldl, idyn_flow, idyn_mcod.

* Get the header data for the screen.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = dynpro_node
        CHANGING
          structure = xdyn_head_temp.

      xdyn_head    = xdyn_head_temp.
      xdyn_text    = xdyn_head_temp-dtext.

* Retrieve field list
      FREE: dynfldl_filter, dynfldl_iterator, dynfldl_node.
      dynfldl_filter = dynpro_node->create_filter_name( 'dynprofield' ).
      dynfldl_iterator =
          dynpro_node->create_iterator_filtered( dynfldl_filter ).
      dynfldl_node ?= dynfldl_iterator->get_next( ).
      WHILE dynfldl_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = dynfldl_node
          CHANGING
            structure = xdyn_fldl.
        APPEND xdyn_fldl TO idyn_fldl.
        dynfldl_node ?= dynfldl_iterator->get_next( ).
      ENDWHILE.

* Retrieve matchcode data.
      FREE: dynmcod_filter, dynmcod_iterator, dynmcod_node.
      dynmcod_filter = dynpro_node->create_filter_name( 'dynprofield' ).
      dynmcod_iterator =
           dynpro_node->create_iterator_filtered( dynmcod_filter ).
      dynmcod_node ?= dynmcod_iterator->get_next( ).
      WHILE dynmcod_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = dynmcod_node
          CHANGING
            structure = xdyn_mcod.
        APPEND xdyn_mcod TO idyn_mcod.
        dynmcod_node ?= dynmcod_iterator->get_next( ).
      ENDWHILE.

* retieve flow logic source.
      CLEAR xdynpro_flow_source.  REFRESH idynpro_flow_source.
      CLEAR xdyn_flow.            REFRESH idyn_flow.
      FREE dynflow_node.
      dynflow_node = dynpro_node->find_from_name( 'dynproflowsource' ).
      xdynpro_flow_source  = dynflow_node->get_value( ).
      idynpro_flow_source = buildtablefromstring( xdynpro_flow_source ).
      LOOP AT idynpro_flow_source INTO xdyn_flow.
        APPEND xdyn_flow  TO idyn_flow.
      ENDLOOP.

* Build dynpro from data
      CALL FUNCTION 'RPY_DYNPRO_INSERT_NATIVE'
        EXPORTING
*         suppress_corr_checks           = ' '
*         CORRNUM            = ' '
          header             = xdyn_head
          dynprotext         = xdyn_text
*         SUPPRESS_EXIST_CHECKS          = ' '
*         USE_CORRNUM_IMMEDIATEDLY       = ' '
*         SUPPRESS_COMMIT_WORK           = ' '
        TABLES
          fieldlist          = idyn_fldl
          flowlogic          = idyn_flow
          params             = idyn_mcod
        EXCEPTIONS
          cancelled          = 1
          already_exists     = 2
          program_not_exists = 3
          not_executed       = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
      ENDIF.

      dynpro_node ?= dynpro_iterator->get_next( ).

    ENDWHILE.

  ENDMETHOD.


  METHOD create_pfstatus.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA: ista TYPE TABLE OF rsmpe_stat,
          ifun TYPE TABLE OF rsmpe_funt,
          imen TYPE TABLE OF rsmpe_men,
          imtx TYPE TABLE OF rsmpe_mnlt,
          iact TYPE TABLE OF rsmpe_act,
          ibut TYPE TABLE OF rsmpe_but,
          ipfk TYPE TABLE OF rsmpe_pfk,
          iset TYPE TABLE OF rsmpe_staf,
          idoc TYPE TABLE OF rsmpe_atrt,
          itit TYPE TABLE OF rsmpe_titt,
          ibiv TYPE TABLE OF rsmpe_buts.

    DATA: xsta TYPE rsmpe_stat,
          xfun TYPE rsmpe_funt,
          xmen TYPE rsmpe_men,
          xmtx TYPE rsmpe_mnlt,
          xact TYPE rsmpe_act,
          xbut TYPE rsmpe_but,
          xpfk TYPE rsmpe_pfk,
          xset TYPE rsmpe_staf,
          xdoc TYPE rsmpe_atrt,
          xtit TYPE rsmpe_titt,
          xbiv TYPE rsmpe_buts.

    DATA xtrkey TYPE trkey.
    DATA xadm   TYPE rsmpe_adm.
    DATA _program TYPE  trdir-name.
    DATA _objname TYPE trobj_name.

    DATA stat_node  TYPE REF TO if_ixml_element.
    DATA node       TYPE REF TO if_ixml_element.
    DATA filter     TYPE REF TO if_ixml_node_filter.
    DATA iterator   TYPE REF TO if_ixml_node_iterator.

    DATA: ls_iact TYPE rsmpe_act,
          ls_ipfk TYPE rsmpe_pfk,
          ls_imen TYPE rsmpe_men.

    _objname = objname.

    stat_node =  pfstat_node.
    CHECK stat_node IS NOT INITIAL.

* read pfstatus_sta node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_sta' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xsta.
      APPEND xsta TO ista.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_fun node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_fun' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xfun.
      APPEND xfun TO ifun.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_men node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_men' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xmen.
      APPEND xmen TO imen.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_mtx node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_mtx' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xmtx.
      APPEND xmtx TO imtx.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_act node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_act' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xact.
      APPEND xact TO iact.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_but node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_but' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xbut.
      APPEND xbut TO ibut.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_pfk node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_pfk' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xpfk.
      APPEND xpfk TO ipfk.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_set node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_set' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xset.
      APPEND xset TO iset.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_doc node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_doc' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xdoc.
      APPEND xdoc TO idoc.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_tit node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_tit' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xtit.
      APPEND xtit TO itit.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_biv node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_biv' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xbiv.
      APPEND xbiv TO ibiv.
      node ?= iterator->get_next( ).
    ENDWHILE.

* Update the gui status
    _program = _objname.

    xtrkey-obj_type = 'PROG'.
    xtrkey-obj_name = _program.
    xtrkey-sub_type = 'CUAD'.
    xtrkey-sub_name = _program.

    LOOP AT iact INTO ls_iact.
      xadm-actcode = ls_iact-code.
    ENDLOOP.
    LOOP AT ipfk INTO ls_ipfk.
      xadm-pfkcode = ls_ipfk-code.
    ENDLOOP.
    LOOP AT imen INTO ls_imen.
      xadm-mencode = ls_imen-code.
    ENDLOOP.

    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = _program
        language  = sy-langu
        tr_key    = xtrkey
        adm       = xadm
        state     = 'I'
      TABLES
        sta       = ista
        fun       = ifun
        men       = imen
        mtx       = imtx
        act       = iact
        but       = ibut
        pfk       = ipfk
        set       = iset
        doc       = idoc
        tit       = itit
        biv       = ibiv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

  ENDMETHOD.


  METHOD create_source.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

    DATA _objname TYPE trobj_name.
    DATA progline TYPE progdir.
    DATA titleinfo TYPE trdirti.
    DATA reportline TYPE string.
    DATA minireport TYPE table_of_strings.

    _objname = objname.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'REPS'
        obj_name          = _objname
      EXCEPTIONS
        wrong_object_name = 1.
    INSERT REPORT _objname FROM source STATE 'I'
      PROGRAM TYPE attribs-subc.  "added to handle includes, etc.
    MOVE 'I' TO progline-state.
    MOVE-CORRESPONDING attribs TO progline.
    progline-idate = sy-datum.
    progline-itime = sy-uzeit.
    progline-cdat  = sy-datum.
    progline-udat  = sy-datum.
    progline-sdate = sy-datum.
    MODIFY progdir FROM progline.
*  Are you kidding me?!?  No idea why you need to do this!!
    CONCATENATE 'REPORT' _objname '.' INTO reportline SEPARATED BY space.
    APPEND reportline TO minireport.
    INSERT REPORT _objname FROM minireport STATE 'A'
      PROGRAM TYPE attribs-subc. "added to handle includes, etc.
    MOVE 'A' TO progline-state.
    MODIFY progdir FROM progline.

  ENDMETHOD.


  METHOD create_textpool.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA langiterator TYPE REF TO if_ixml_node_iterator.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA textfilter TYPE REF TO if_ixml_node_filter.
    DATA textiterator TYPE REF TO if_ixml_node_iterator.
    DATA langnode TYPE REF TO if_ixml_element.
    DATA atextnode TYPE REF TO if_ixml_element.
    DATA _objname TYPE trobj_name.
    DATA lang TYPE spras.
    DATA langnodeexists TYPE flag.
    DATA logonlanguageexists TYPE flag.
    DATA _state(1) TYPE c.

    _objname = objname.
    CHECK textpoolnode IS NOT INITIAL.

    filter = textpoolnode->create_filter_name( 'language' ).
    langiterator = textpoolnode->create_iterator_filtered( filter ).
    langnode ?= langiterator->get_next( ).

    WHILE langnode IS NOT INITIAL.
      langnodeexists = 'X'.
      CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
        EXPORTING
          object   = 'REPT'
          obj_name = _objname
        EXCEPTIONS
          OTHERS   = 0.

      REFRESH textpooltable.
      textiterator = langnode->create_iterator( ).
      atextnode ?= textiterator->get_next( ).
*For some reason the 1st one is blank... not sure why.
      atextnode ?= textiterator->get_next( ).
      WHILE atextnode IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = atextnode
          CHANGING
            structure = textpoolrow.
        APPEND textpoolrow TO textpooltable.
        atextnode ?= textiterator->get_next( ).
      ENDWHILE.
      IF textpooltable IS NOT INITIAL.
        lang = langnode->get_attribute( 'SPRAS' ).
        IF lang = sy-langu.
          logonlanguageexists = 'X'.
          _state = 'I'.
        ELSE.
*       seems that if a textpool is inserted as inactive for language
*       other than the logon language, it is lost upon activation
*       not sure inserting as active is best solution,but seems to work
          _state = 'A'.
        ENDIF.
        INSERT TEXTPOOL _objname
          FROM textpooltable
          LANGUAGE lang
          STATE    _state.
      ENDIF.
      langnode ?= langiterator->get_next( ).
    ENDWHILE.
  ENDMETHOD.


  METHOD deleteobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA program TYPE sy-repid.

    program = objname.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
*       CORRNUMBER     =
        program        = program
*       SUPPRESS_CHECKS                  = ' '
*       SUPPRESS_COMMIT                  = ' '
        suppress_popup = 'X'
*       MASS_DELETE_CALL                 = ' '
*       WITH_CUA       = 'X'
*       WITH_DOCUMENTATION               = 'X'
*       WITH_DYNPRO    = 'X'
*       WITH_INCLUDES  = ' '
*       WITH_TEXTPOOL  = 'X'
*       WITH_VARIANTS  = 'X'
*       TADIR_DEVCLASS =
*       SKIP_PROGRESS_IND                = ' '
*       FORCE_DELETE_USED_INCLUDES       = ' '
* IMPORTING
*       CORRNUMBER     =
*       PROGRAM        =
* EXCEPTIONS
*       ENQUEUE_LOCK   = 1
*       OBJECT_NOT_FOUND                 = 2
*       PERMISSION_FAILURE               = 3
*       REJECT_DELETION                  = 4
*       OTHERS         = 5
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD dequeue_abap.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        global_lock              = 'X'
        mode                     = 'FREE'
        object                   = objname
        object_class             = 'ABAP'
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
              msg    = 'object locked'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD enqueue_abap.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
*       authority_check          = authority_check
        global_lock              = 'X'
        mode                     = 'INSERT'
*       master_language          = trdir-rload
        object                   = objname
        object_class             = 'ABAP'
*       importing
*       transport_key            = trkey_global
*       new_master_language      = trdir-rload
*       devclass                 = devclass_local
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
              msg    = 'object locked'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    objecttype = 'PROG'. "ABAP Program
  ENDMETHOD.


  METHOD get_documentation.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA languagenode   TYPE REF TO if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    TYPES: BEGIN OF t_dokhl,
             id         TYPE dokhl-id,
             object     TYPE dokhl-object,
             langu      TYPE dokhl-langu,
             typ        TYPE dokhl-typ,
             dokversion TYPE dokhl-dokversion,
           END OF t_dokhl.

    DATA lt_dokhl TYPE TABLE OF t_dokhl.
    DATA ls_dokhl LIKE LINE OF lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    DATA lv_str TYPE string.
    DATA _objname TYPE e071-obj_name.

    _objname = objname.

* Check against database
    SELECT  id object langu typ dokversion
          INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
             FROM dokhl
               WHERE id = 'RE'
                  AND object = _objname.

* Use only most recent version.
    SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

* Make sure there is at least one record here.
    CLEAR ls_dokhl.
    READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    docnode = xmldoc->create_element( 'programDocumentation' ).

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docnode->set_attribute( name = 'OBJECT' value = lv_str ).

    LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
      languagenode = xmldoc->create_element( 'language' ).
      lv_str = ls_dokhl-langu.
      rc = languagenode->set_attribute( name = 'SPRAS' value = lv_str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmldoc->create_element( `textLine` ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languagenode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docnode->append_child( languagenode ) .
    ENDLOOP.

  ENDMETHOD.


  METHOD get_dynpro.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    TYPES: BEGIN OF tdynp,
             prog TYPE d020s-prog,
             dnum TYPE d020s-dnum,
           END OF tdynp.

    DATA: idyn_fldl TYPE TABLE OF d021s,
          idyn_flow TYPE TABLE OF d022s,
          idyn_mcod TYPE TABLE OF d023s.

    DATA: xdyn_head TYPE  d020s,
          xdyn_fldl TYPE  d021s,
          xdyn_flow TYPE  d022s,
          xdyn_mcod TYPE  d023s.

    DATA idynp TYPE TABLE OF tdynp.
    DATA xdynp TYPE tdynp.

    DATA xdyn_text TYPE d020t-dtxt.
    DATA xdyn_text_string TYPE string.

    DATA _objname TYPE trobj_name.
    DATA rc TYPE sy-subrc .

    DATA iflowsource TYPE rswsourcet.
    DATA xflowsource LIKE LINE OF iflowsource.
    DATA flowsourcestring TYPE string.

    DATA dynnr_node TYPE REF TO if_ixml_element.
    DATA dynpromatchnode TYPE REF TO if_ixml_element.
    DATA dynprofieldsnode TYPE REF TO if_ixml_element.
    DATA dynproflownode TYPE REF TO if_ixml_element.

    _objname = objname.

* Get all dynpros for program object
    CLEAR xdynp.  REFRESH idynp.
    SELECT prog dnum INTO TABLE idynp
                  FROM d020s
                     WHERE prog = _objname
                       AND type <> 'S'    " No Selection Screens
                       AND type <> 'J'.   " No selection subscreens
    CHECK sy-subrc  = 0 .

    dynp_node = xmldoc->create_element( 'dynpros' ).

    LOOP AT idynp INTO xdynp.

* Retrieve dynpro imformation
      dynnr_node =  xmldoc->create_element( 'dynpro' ).

      CLEAR:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
      REFRESH:  idyn_fldl, idyn_flow, idyn_mcod.

      CALL FUNCTION 'RPY_DYNPRO_READ_NATIVE'
        EXPORTING
          progname         = xdynp-prog
          dynnr            = xdynp-dnum
*         SUPPRESS_EXIST_CHECKS       = ' '
*         SUPPRESS_CORR_CHECKS        = ' '
        IMPORTING
          header           = xdyn_head
          dynprotext       = xdyn_text
        TABLES
          fieldlist        = idyn_fldl
          flowlogic        = idyn_flow
          params           = idyn_mcod
*         FIELDTEXTS       =
        EXCEPTIONS
          cancelled        = 1
          not_found        = 2
          permission_error = 3
          OTHERS           = 4.

      CHECK sy-subrc = 0.

* Add heading information for screen.
      setattributesfromstructure(
                       node = dynnr_node structure =  xdyn_head  ).
* Add the dynpro text also.
      xdyn_text_string =  xdyn_text.
      rc = dynnr_node->set_attribute(
                 name = 'DTEXT'  value = xdyn_text_string ).
      rc = dynp_node->append_child( dynnr_node ).

* Add fields information for screen.
      IF NOT idyn_fldl[] IS INITIAL.
        LOOP AT idyn_fldl INTO xdyn_fldl.
          dynprofieldsnode = xmldoc->create_element( 'dynprofield' ).
          setattributesfromstructure(
                   node = dynprofieldsnode structure =  xdyn_fldl ).
          rc = dynnr_node->append_child( dynprofieldsnode ).
        ENDLOOP.
      ENDIF.

* Add flow logic of screen
      IF NOT idyn_flow[] IS INITIAL.
        CLEAR xflowsource. REFRESH  iflowsource.
        LOOP AT idyn_flow INTO xdyn_flow.
          xflowsource  = xdyn_flow.
          APPEND xflowsource TO iflowsource.
        ENDLOOP.

        dynproflownode = xmldoc->create_element( 'dynproflowsource' ).
        flowsourcestring = buildsourcestring( sourcetable = iflowsource ).
        rc = dynproflownode->if_ixml_node~set_value( flowsourcestring ).
        rc = dynnr_node->append_child( dynproflownode  ).
      ENDIF.

* Add matchcode information for screen.
      IF NOT idyn_mcod[] IS INITIAL.
        LOOP AT idyn_mcod INTO xdyn_mcod.
          CHECK NOT xdyn_mcod-type IS INITIAL
            AND NOT xdyn_mcod-content IS INITIAL.
          dynpromatchnode = xmldoc->create_element( 'dynpromatchcode' ).
          setattributesfromstructure(
                   node = dynpromatchnode structure =  xdyn_mcod ).
          rc = dynnr_node->append_child( dynpromatchnode ).
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_pfstatus.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA: ista TYPE TABLE OF rsmpe_stat,
          ifun TYPE TABLE OF rsmpe_funt,
          imen TYPE TABLE OF rsmpe_men,
          imtx TYPE TABLE OF rsmpe_mnlt,
          iact TYPE TABLE OF rsmpe_act,
          ibut TYPE TABLE OF rsmpe_but,
          ipfk TYPE TABLE OF rsmpe_pfk,
          iset TYPE TABLE OF rsmpe_staf,
          idoc TYPE TABLE OF rsmpe_atrt,
          itit TYPE TABLE OF rsmpe_titt,
          ibiv TYPE TABLE OF rsmpe_buts.

    DATA: xsta TYPE rsmpe_stat,
          xfun TYPE rsmpe_funt,
          xmen TYPE rsmpe_men,
          xmtx TYPE rsmpe_mnlt,
          xact TYPE rsmpe_act,
          xbut TYPE rsmpe_but,
          xpfk TYPE rsmpe_pfk,
          xset TYPE rsmpe_staf,
          xdoc TYPE rsmpe_atrt,
          xtit TYPE rsmpe_titt,
          xbiv TYPE rsmpe_buts.

    DATA sta_node TYPE REF TO if_ixml_element.
    DATA fun_node TYPE REF TO if_ixml_element.
    DATA men_node TYPE REF TO if_ixml_element.
    DATA mtx_node TYPE REF TO if_ixml_element.
    DATA act_node TYPE REF TO if_ixml_element.
    DATA but_node TYPE REF TO if_ixml_element.
    DATA pfk_node TYPE REF TO if_ixml_element.
    DATA set_node TYPE REF TO if_ixml_element.
    DATA doc_node TYPE REF TO if_ixml_element.
    DATA tit_node TYPE REF TO if_ixml_element.
    DATA biv_node TYPE REF TO if_ixml_element.

    DATA _objname TYPE trobj_name.
    DATA _program TYPE  trdir-name.
    DATA rc TYPE sy-subrc.

    _objname = objname.
    _program = objname.

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = _program
        language        = sy-langu
      TABLES
        sta             = ista
        fun             = ifun
        men             = imen
        mtx             = imtx
        act             = iact
        but             = ibut
        pfk             = ipfk
        set             = iset
        doc             = idoc
        tit             = itit
        biv             = ibiv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.

    CHECK sy-subrc = 0.

* if there is a gui status or gui title present, then
* create pfstatus node.
    IF ista[] IS NOT INITIAL
       OR itit[] IS NOT INITIAL.
      pfstat_node = xmldoc->create_element( 'pfstatus' ).
    ENDIF.


* if ista is filled, assume there are one or more
* gui statuses
    IF ista[] IS NOT INITIAL.

      LOOP AT ista INTO xsta.
        sta_node = xmldoc->create_element( 'pfstatus_sta' ).
        setattributesfromstructure(
                 node = sta_node
                 structure =  xsta ).
        rc = pfstat_node->append_child( sta_node ).
      ENDLOOP.

      LOOP AT ifun INTO xfun.
        fun_node = xmldoc->create_element( 'pfstatus_fun' ).
        setattributesfromstructure(
                 node = fun_node
                 structure =  xfun ).
        rc = pfstat_node->append_child( fun_node ).
      ENDLOOP.

      LOOP AT imen INTO xmen.
        men_node = xmldoc->create_element( 'pfstatus_men' ).
        setattributesfromstructure(
                 node = men_node
                 structure =  xmen ).
        rc = pfstat_node->append_child( men_node ).
      ENDLOOP.

      LOOP AT imtx INTO xmtx.
        mtx_node = xmldoc->create_element( 'pfstatus_mtx' ).
        setattributesfromstructure(
                 node = mtx_node
                 structure =  xmtx ).
        rc = pfstat_node->append_child( mtx_node ).
      ENDLOOP.

      LOOP AT iact INTO xact.
        act_node = xmldoc->create_element( 'pfstatus_act' ).
        setattributesfromstructure(
                 node = act_node
                 structure =  xact ).
        rc = pfstat_node->append_child( act_node ).
      ENDLOOP.

      LOOP AT ibut INTO xbut.
        but_node = xmldoc->create_element( 'pfstatus_but' ).
        setattributesfromstructure(
                 node = but_node
                 structure =  xbut ).
        rc = pfstat_node->append_child( but_node ).
      ENDLOOP.

      LOOP AT ipfk INTO xpfk.
        pfk_node = xmldoc->create_element( 'pfstatus_pfk' ).
        setattributesfromstructure(
                 node = pfk_node
                 structure =  xpfk ).
        rc = pfstat_node->append_child( pfk_node ).
      ENDLOOP.

      LOOP AT iset INTO xset.
        set_node = xmldoc->create_element( 'pfstatus_set' ).
        setattributesfromstructure(
                 node = set_node
                 structure =  xset ).
        rc = pfstat_node->append_child( set_node ).
      ENDLOOP.

      LOOP AT idoc INTO xdoc.
        doc_node = xmldoc->create_element( 'pfstatus_doc' ).
        setattributesfromstructure(
                 node = doc_node
                 structure =  xdoc ).
        rc = pfstat_node->append_child( doc_node ).
      ENDLOOP.


      LOOP AT ibiv INTO xbiv.
        biv_node = xmldoc->create_element( 'pfstatus_biv' ).
        setattributesfromstructure(
                 node = biv_node
                 structure =  xbiv ).
        rc = pfstat_node->append_child( biv_node ).
      ENDLOOP.

    ENDIF.


* It itit is filled, assume one or more titles
    IF itit[] IS NOT INITIAL.

      LOOP AT itit INTO xtit.
        tit_node = xmldoc->create_element( 'pfstatus_tit' ).
        setattributesfromstructure(
                 node = tit_node
                 structure =  xtit ).
        rc = pfstat_node->append_child( tit_node ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_source.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

    DATA _objname(30) TYPE c.

    _objname = me->objname.
    READ REPORT _objname INTO progsource.

  ENDMETHOD.


  METHOD get_textpool.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    DATA atext TYPE REF TO if_ixml_element.
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA languagelist TYPE instlang.
    DATA alanguage TYPE spras.
    DATA _objname(30) TYPE c.
    DATA rc TYPE i.
    DATA stemp TYPE string.
    DATA languagenode TYPE REF TO if_ixml_element.
    DATA firstloop TYPE flag.

    _objname = objname.


    CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
      CHANGING
        installed_languages = languagelist.

    firstloop = abap_true.

    LOOP AT languagelist INTO alanguage.
      READ TEXTPOOL _objname INTO textpooltable LANGUAGE alanguage.
      IF sy-subrc = 0.
        IF firstloop = abap_true.
          textnode = xmldoc->create_element( 'textPool' ).
          firstloop = abap_false.
        ENDIF.
        languagenode = xmldoc->create_element( 'language' ).
        stemp = alanguage.
        rc = languagenode->set_attribute( name = 'SPRAS' value = stemp ).
        LOOP AT textpooltable INTO textpoolrow.
          atext = xmldoc->create_element( 'textElement' ).
          setattributesfromstructure( node = atext structure =
          textpoolrow ).
          rc = languagenode->append_child( atext ).
        ENDLOOP.
        rc = textnode->append_child( languagenode ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD transport_copy.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        author              = author
        global_lock         = 'X'
        object              = objname
        object_class        = 'ABAP'
        devclass            = devclass
*       KORRNUM             = CORRNUMBER_LOCAL
        master_language     = sy-langu
*       PROGRAM             = PROGRAM_LOCAL
        mode                = 'INSERT'
*       IMPORTING
*       AUTHOR              = UNAME
*       KORRNUM             = CORRNUMBER_LOCAL
*       DEVCLASS            = DEVCLASS_LOCAL
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

  ENDMETHOD.


  METHOD update_wb_tree.

    DATA: BEGIN OF pname,
            root(3)     VALUE 'PG_',
            program(27),
          END OF pname.

    DATA: trdir TYPE trdir.

    pname-program = me->objname.

    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name = pname.

    trdir-name    = me->objname.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = trdir-name
        program   = trdir-name
        operation = 'INSERT'
        type      = 'CP'.

  ENDMETHOD.
ENDCLASS.
