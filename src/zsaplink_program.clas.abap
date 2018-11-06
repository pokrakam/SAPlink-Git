class ZSAPLINK_PROGRAM definition
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
  methods CREATESTRINGFROMOBJECT
    redefinition .
protected section.

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.

  methods GET_SOURCE
    returning
      value(PROGSOURCE) type RSWSOURCET .
  methods UPDATE_WB_TREE .
  methods CREATE_TEXTPOOL
    importing
      !TEXTPOOLNODE type ref to IF_IXML_ELEMENT .
  methods DEQUEUE_ABAP
    raising
      ZCX_SAPLINK .
  methods GET_TEXTPOOL
    returning
      value(TEXTNODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_DOCUMENTATION
    importing
      !DOCNODE type ref to IF_IXML_ELEMENT .
  methods CREATE_SOURCE
    importing
      !SOURCE type TABLE_OF_STRINGS
      !ATTRIBS type TRDIR .
  methods ENQUEUE_ABAP
    raising
      ZCX_SAPLINK .
  methods GET_DOCUMENTATION
    returning
      value(DOCNODE) type ref to IF_IXML_ELEMENT .
  methods TRANSPORT_COPY
    importing
      !AUTHOR type SYUNAME
      !DEVCLASS type DEVCLASS
    raising
      ZCX_SAPLINK .
  methods GET_DYNPRO
    returning
      value(DYNP_NODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_DYNPRO
    importing
      !DYNP_NODE type ref to IF_IXML_ELEMENT .
  methods GET_PFSTATUS
    returning
      value(PFSTAT_NODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_PFSTATUS
    importing
      !PFSTAT_NODE type ref to IF_IXML_ELEMENT .
ENDCLASS.



CLASS ZSAPLINK_PROGRAM IMPLEMENTATION.


method CHECKEXISTS.
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

  select single name from trdir into objName where NAME = objName.
  if sy-subrc = 0.
    exists = 'X'.
  endif.

endmethod.


method CREATEIXMLDOCFROMOBJECT.
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
data rootNode type ref to if_ixml_element.
data sourceNode type ref to if_ixml_element.
data textPoolNode type ref to if_ixml_element.
data docNode type ref to if_ixml_element.
data dynproNode type ref to if_ixml_element.
data statusNode type ref to if_ixml_element.
data rc type sysubrc.
data progAttribs type trdir.
data progSource type RSWSOURCET.
data sourceString type string.
data _objType type string.

  _objType = getObjectType( ).
  rootNode = xmlDoc->create_element( _objType ).
  sourceNode = xmlDoc->create_element( 'source' ).
  select single * from trdir into progAttribs where NAME = objName.
  if sy-subrc = 0.
    setAttributesFromStructure( node = rootNode structure =  progAttribs ).
    progSource = me->get_source( ).
    sourceString = buildSourceString( sourceTable = progSource ).
    rc = sourceNode->IF_IXML_NODE~SET_VALUE( sourceString ).
    textPoolNode = get_textPool( ).
    rc = rootNOde->append_child( textPoolNode ).
    docNode = get_documentation( ).
    rc = rootNOde->append_child( docNode ).
    dynproNode = get_dynpro( ).
    rc = rootNode->append_child( dynproNode ).
    statusNode = get_pfstatus( ).
    rc = rootNode->append_child( statusNode ).
    rc = rootNode->append_child( sourceNode ).
    rc = xmldoc->append_child( rootNode ).
    ixmlDocument = xmlDoc.
  else.
    clear ixmlDocument.
    raise exception type zcx_saplink
      exporting
        textid = zcx_saplink=>not_found
        object = objname.
  endif.
endmethod.


method CREATEOBJECTFROMIXMLDOC.
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
  data rootnode type ref to if_ixml_element.
  data progattribs type trdir.
  data sourcenode type ref to if_ixml_element.
  data textnode type ref to if_ixml_element.
  data docnode type ref to if_ixml_element.
  data dynpnode type ref to if_ixml_element.
  data statnode type ref to if_ixml_element.
  data source type string.
  data sourcetable type table_of_strings.
  data _objname(30) type c.
  data aobjname type trobj_name.
  data _objtype type string.
  data checkexists type flag.

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
  call method getstructurefromattributes
    exporting
      node      = rootnode
    changing
      structure = progattribs.
  objname = progattribs-name.

*  check if object exists
*  select single name from trdir into objName where NAME = objName.
*  if sy-subrc = 0 and overwrite <> 'X'.
*    raise exception type zcx_saplink
*      exporting textid = zcx_saplink=>existing.
*  endif.

  checkexists = checkexists( ).
  if checkexists is not initial.
    if overwrite is initial.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>existing.
    else.
*     delete object for new install
      deleteobject( ).
    endif.
  endif.


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

endmethod.


method CREATESTRINGFROMOBJECT.
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
  data progSource type RSWSOURCET.
  progsource = me->get_source( ).
  string = buildsourcestring( sourcetable = progsource ).
endmethod.


method CREATE_DOCUMENTATION.
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

  data obj_name type DOKHL-OBJECT.
  data prog_name type string.
  data language  type string.
  data obj_langu type DOKHL-LANGU.
  data lv_str type string.
  data rc type sy-subrc.

  DATA lt_lines  TYPE TABLE OF tline.
  FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

  if docnode is not bound.
    return.
  endif.

  prog_name = docNode->get_attribute( name = 'OBJECT' ).
  obj_name = prog_name.

* If no prog name, then there was no program documenation, just return.
  if prog_name is initial.
    return.
  endif.

* Get languages from XML
  FREE: lang_filter, lang_iterator, lang_node.
  lang_filter = docNode->create_filter_name( `language` ).
  lang_iterator = docNode->create_iterator_filtered( lang_filter ).
  lang_node ?= lang_iterator->get_next( ).
  WHILE lang_node IS NOT INITIAL.

    refresh lt_lines.
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

endmethod.


method CREATE_DYNPRO.
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
  types: begin of tdyn_head_temp.
         include type d020s.
  types: dtext type d020t-dtxt.
  types: end of tdyn_head_temp.

  data: idyn_fldl type table of d021s,
        idyn_flow type table of d022s,
        idyn_mcod type table of d023s.

  data: xdyn_head type  d020s,
        xdyn_fldl type  d021s,
        xdyn_flow type  d022s,
        xdyn_mcod type  d023s.

  data: xdyn_text_string type string.
  data: xdyn_text        type d020t-dtxt .
  data: xdyn_head_temp   type tdyn_head_temp.

  data _objname type trobj_name.

  data dynpros_node       type ref to if_ixml_element.
  data dynpros_filter     type ref to if_ixml_node_filter.
  data dynpros_iterator   type ref to if_ixml_node_iterator.

  data dynpro_node        type ref to if_ixml_element.
  data dynpro_filter      type ref to if_ixml_node_filter.
  data dynpro_iterator    type ref to if_ixml_node_iterator.

  data dynfldl_node       type ref to if_ixml_element.
  data dynfldl_filter     type ref to if_ixml_node_filter.
  data dynfldl_iterator   type ref to if_ixml_node_iterator.

  data dynmcod_node       type ref to if_ixml_element.
  data dynmcod_filter     type ref to if_ixml_node_filter.
  data dynmcod_iterator   type ref to if_ixml_node_iterator.

  data dynflow_node       type ref to if_ixml_element.

  data xdynpro_flow_source type string.
  data idynpro_flow_source type table_of_strings.

  _objname = objname.

  dynpros_node =  dynp_node.
  check dynpros_node is not initial.

  free: dynpro_filter, dynpro_iterator, dynpro_node.
  dynpro_filter = dynpros_node->create_filter_name( 'dynpro' ).
  dynpro_iterator =
        dynpros_node->create_iterator_filtered( dynpro_filter ).
  dynpro_node ?= dynpro_iterator->get_next( ).

  while dynpro_node is not initial.

    clear:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
    refresh:  idyn_fldl, idyn_flow, idyn_mcod.

* Get the header data for the screen.
    call method getstructurefromattributes
      exporting
        node      = dynpro_node
      changing
        structure = xdyn_head_temp.

    xdyn_head    = xdyn_head_temp.
    xdyn_text    = xdyn_head_temp-dtext.

* Retrieve field list
    free: dynfldl_filter, dynfldl_iterator, dynfldl_node.
    dynfldl_filter = dynpro_node->create_filter_name( 'dynprofield' ).
    dynfldl_iterator =
        dynpro_node->create_iterator_filtered( dynfldl_filter ).
    dynfldl_node ?= dynfldl_iterator->get_next( ).
    while dynfldl_node is not initial.
      call method getstructurefromattributes
        exporting
          node      = dynfldl_node
        changing
          structure = xdyn_fldl.
      append xdyn_fldl to idyn_fldl.
      dynfldl_node ?= dynfldl_iterator->get_next( ).
    endwhile.

* Retrieve matchcode data.
    free: dynmcod_filter, dynmcod_iterator, dynmcod_node.
    dynmcod_filter = dynpro_node->create_filter_name( 'dynprofield' ).
    dynmcod_iterator =
         dynpro_node->create_iterator_filtered( dynmcod_filter ).
    dynmcod_node ?= dynmcod_iterator->get_next( ).
    while dynmcod_node is not initial.
      call method getstructurefromattributes
        exporting
          node      = dynmcod_node
        changing
          structure = xdyn_mcod.
      append xdyn_mcod to idyn_mcod.
      dynmcod_node ?= dynmcod_iterator->get_next( ).
    endwhile.

* retieve flow logic source.
    clear xdynpro_flow_source.  refresh idynpro_flow_source.
    clear xdyn_flow.            refresh idyn_flow.
    free dynflow_node.
    dynflow_node = dynpro_node->find_from_name( 'dynproflowsource' ).
    xdynpro_flow_source  = dynflow_node->get_value( ).
    idynpro_flow_source = buildtablefromstring( xdynpro_flow_source ).
    loop at idynpro_flow_source into xdyn_flow.
      append xdyn_flow  to idyn_flow.
    endloop.

* Build dynpro from data
    call function 'RPY_DYNPRO_INSERT_NATIVE'
      exporting
*       suppress_corr_checks           = ' '
*       CORRNUM                        = ' '
        header                         = xdyn_head
        dynprotext                     = xdyn_text
*       SUPPRESS_EXIST_CHECKS          = ' '
*       USE_CORRNUM_IMMEDIATEDLY       = ' '
*       SUPPRESS_COMMIT_WORK           = ' '
      tables
        fieldlist                      = idyn_fldl
        flowlogic                      = idyn_flow
        params                         = idyn_mcod
     exceptions
        cancelled                      = 1
        already_exists                 = 2
        program_not_exists             = 3
        not_executed                   = 4
        others                         = 5.
    if sy-subrc <> 0.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>system_error.
    endif.

    dynpro_node ?= dynpro_iterator->get_next( ).

  endwhile.

endmethod.


method CREATE_PFSTATUS.
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
  data: ista type table of rsmpe_stat,
        ifun type table of rsmpe_funt,
        imen type table of rsmpe_men,
        imtx type table of rsmpe_mnlt,
        iact type table of rsmpe_act,
        ibut type table of rsmpe_but,
        ipfk type table of rsmpe_pfk,
        iset type table of rsmpe_staf,
        idoc type table of rsmpe_atrt,
        itit type table of rsmpe_titt,
        ibiv type table of rsmpe_buts.

  data: xsta type rsmpe_stat,
        xfun type rsmpe_funt,
        xmen type rsmpe_men,
        xmtx type rsmpe_mnlt,
        xact type rsmpe_act,
        xbut type rsmpe_but,
        xpfk type rsmpe_pfk,
        xset type rsmpe_staf,
        xdoc type rsmpe_atrt,
        xtit type rsmpe_titt,
        xbiv type rsmpe_buts.

  data xtrkey type trkey.
  data xadm   type rsmpe_adm.
  data _program type  trdir-name.
  data _objname type trobj_name.

  data stat_node  type ref to if_ixml_element.
  data node       type ref to if_ixml_element.
  data filter     type ref to if_ixml_node_filter.
  data iterator   type ref to if_ixml_node_iterator.

  _objname = objname.

  stat_node =  pfstat_node.
  check stat_node is not initial.

* read pfstatus_sta node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_sta' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xsta.
    append xsta to ista.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_fun node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_fun' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xfun.
    append xfun to ifun.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_men node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_men' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xmen.
    append xmen to imen.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_mtx node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_mtx' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xmtx.
    append xmtx to imtx.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_act node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_act' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xact.
    append xact to iact.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_but node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_but' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xbut.
    append xbut to ibut.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_pfk node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_pfk' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xpfk.
    append xpfk to ipfk.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_set node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_set' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xset.
    append xset to iset.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_doc node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_doc' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xdoc.
    append xdoc to idoc.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_tit node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_tit' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xtit.
    append xtit to itit.
    node ?= iterator->get_next( ).
  endwhile.

* read pfstatus_biv node
  free: filter, iterator, node.
  filter = stat_node->create_filter_name( 'pfstatus_biv' ).
  iterator = stat_node->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  while node is not initial.
    call method getstructurefromattributes
      exporting
        node      = node
      changing
        structure = xbiv.
    append xbiv to ibiv.
    node ?= iterator->get_next( ).
  endwhile.

* Update the gui status
  _program = _objname.

  xtrkey-obj_type = 'PROG'.
  xtrkey-obj_name = _program.
  xtrkey-sub_type = 'CUAD'.
  xtrkey-sub_name = _program.

  xadm-actcode = 'X'. " Issues #276

  call function 'RS_CUA_INTERNAL_WRITE'
    exporting
      program   = _program
      language  = sy-langu
      tr_key    = xtrkey
      adm       = xadm
      state     = 'I'
    tables
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
    exceptions
      not_found = 1
      others    = 2.

  if sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  endif.

endmethod.


method CREATE_SOURCE.
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

  data _objName type TROBJ_NAME.
  data progLine type PROGDIR.
  data titleInfo type trdirti.
  data reportLine type string.
  data miniReport type table_of_strings.

  _objName = objName.
  call function 'RS_INSERT_INTO_WORKING_AREA'
        exporting
             OBJECT   = 'REPS'
             OBJ_NAME = _objName
        exceptions
             WRONG_OBJECT_NAME = 1.
   INSERT REPORT _objName FROM source STATE 'I'
     program type attribs-subc.  "added to handle includes, etc.
   MOVE 'I' TO progline-STATE.
   move-corresponding attribs to progline.
   progline-idate = sy-datum.
   progline-itime = sy-uzeit.
   progline-CDAT  = sy-datum.
   progline-UDAT  = sy-datum.
   progline-SDATE = sy-datum.
   modify progdir from progline.
*  Are you kidding me?!?  No idea why you need to do this!!
   CONCATENATE 'REPORT' _objName '.' INTO reportLine SEPARATED BY SPACE.
   append reportline to miniReport.
   INSERT REPORT _objName FROM miniReport STATE 'A'
     program type attribs-subc. "added to handle includes, etc.
   MOVE 'A' TO progline-STATE.
   modify progdir from progline.

endmethod.


method CREATE_TEXTPOOL.
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
  data textPoolTable type standard table of textPool.
  data textPoolRow type textPool.
  data langIterator type ref to if_ixml_node_iterator.
  data filter type ref to if_ixml_node_filter.
  data textFilter type ref to if_ixml_node_filter.
  data textIterator type ref to if_ixml_node_iterator.
  data langNode type ref to if_ixml_element.
  data aTextNode type ref to if_ixml_element.
  data _objName type TROBJ_NAME.
  data lang type spras.
  data langNodeExists type flag.
  data logonLanguageExists type flag.
  data _state(1) type c.

  _objName = objName.
  CHECK textPoolNode IS NOT INITIAL.

  filter = textPoolNode->create_filter_name( 'language' ).
  langIterator = textPoolNode->create_iterator_filtered( filter ).
  langNode ?= langIterator->get_next( ).

  while langNode is not initial.
    langNodeExists = 'X'.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
         EXPORTING
              OBJECT   = 'REPT'
              OBJ_NAME = _objName
         EXCEPTIONS
              OTHERS   = 0.

    refresh textPoolTable.
    textIterator = langNode->create_iterator( ).
    aTextNode ?= textIterator->get_next( ).
*For some reason the 1st one is blank... not sure why.
    aTextNode ?= textIterator->get_next( ).
    while aTextNode is not initial.
      call method GETSTRUCTUREFROMATTRIBUTES
            exporting
              node = aTextNode
            changing
              structure = textPoolRow.
      append textPoolRow to textPoolTable.
      aTextNode ?= textIterator->get_next( ).
    endwhile.
    if textPoolTable is not initial.
      lang = langNode->get_attribute( 'SPRAS' ).
      if lang = sy-langu.
        logonLanguageExists = 'X'.
        _state = 'I'.
      else.
*       seems that if a textpool is inserted as inactive for language
*       other than the logon language, it is lost upon activation
*       not sure inserting as active is best solution,but seems to work
        _state = 'A'.
      endif.
      insert textpool _objName
        from textPooltable
        language lang
        state    _state.
    endif.
    langNode ?= langIterator->get_next( ).
  endwhile.
endmethod.


method DELETEOBJECT.
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
data program type sy-repid.

program = objName.

CALL FUNCTION 'RS_DELETE_PROGRAM'
  EXPORTING
*   CORRNUMBER                       =
    program                          = program
*   SUPPRESS_CHECKS                  = ' '
*   SUPPRESS_COMMIT                  = ' '
    SUPPRESS_POPUP                   = 'X'
*   MASS_DELETE_CALL                 = ' '
*   WITH_CUA                         = 'X'
*   WITH_DOCUMENTATION               = 'X'
*   WITH_DYNPRO                      = 'X'
*   WITH_INCLUDES                    = ' '
*   WITH_TEXTPOOL                    = 'X'
*   WITH_VARIANTS                    = 'X'
*   TADIR_DEVCLASS                   =
*   SKIP_PROGRESS_IND                = ' '
*   FORCE_DELETE_USED_INCLUDES       = ' '
* IMPORTING
*   CORRNUMBER                       =
*   PROGRAM                          =
* EXCEPTIONS
*   ENQUEUE_LOCK                     = 1
*   OBJECT_NOT_FOUND                 = 2
*   PERMISSION_FAILURE               = 3
*   REJECT_DELETION                  = 4
*   OTHERS                           = 5
          .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

endmethod.


method DEQUEUE_ABAP.
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
  call function 'RS_ACCESS_PERMISSION'
       exporting
            global_lock              = 'X'
            mode                     = 'FREE'
            object                   = objName
            object_class             = 'ABAP'
       exceptions
            canceled_in_corr         = 1
            enqueued_by_user         = 3
            enqueue_system_failure   = 4
            locked_by_author         = 5
            illegal_parameter_values = 6
            no_modify_permission     = 7
            no_show_permission       = 8
            permission_failure       = 9.

  if sy-subrc <> 0.
    case sy-subrc.
      when 7 or 8 or 9.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>not_authorized.
      when 5.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>error_message
            msg = 'object locked'.
      when others.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>system_error.
    endcase.
  endif.

endmethod.


method ENQUEUE_ABAP.
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
  call function 'RS_ACCESS_PERMISSION'
       exporting
*            authority_check          = authority_check
            global_lock              = 'X'
            mode                     = 'INSERT'
*            master_language          = trdir-rload
            object                   = objName
            object_class             = 'ABAP'
*       importing
*            transport_key            = trkey_global
*            new_master_language      = trdir-rload
*            devclass                 = devclass_local
       exceptions
            canceled_in_corr         = 1
            enqueued_by_user         = 3
            enqueue_system_failure   = 4
            locked_by_author         = 5
            illegal_parameter_values = 6
            no_modify_permission     = 7
            no_show_permission       = 8
            permission_failure       = 9.

  if sy-subrc <> 0.
    case sy-subrc.
      when 7 or 8 or 9.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>not_authorized.
      when 5.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>error_message
            msg = 'object locked'.
      when others.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>system_error.
    endcase.
  endif.

endmethod.


method GETOBJECTTYPE.
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
  objectType = 'PROG'. "ABAP Program
endmethod.


method GET_DOCUMENTATION.
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
  data languageNode   type ref to if_ixml_element.
  DATA txtlines_node TYPE REF TO if_ixml_element.
  DATA rc            TYPE sysubrc.
  DATA _objtype      TYPE string.

  Types: BEGIN OF t_dokhl,
          id          TYPE dokhl-id,
          object      TYPE dokhl-object,
          langu       type dokhl-langu,
          typ         TYPE dokhl-typ,
          dokversion  TYPE dokhl-dokversion,
         END OF t_dokhl.

  data lt_dokhl type table of t_dokhl.
  data ls_dokhl like line of lt_dokhl.

  DATA lt_lines TYPE TABLE OF tline.
  DATA ls_lines LIKE LINE OF lt_lines.

  data lv_str type string.
  DATA _objname TYPE e071-obj_name.

  _objname = objname.

* Check against database
  SELECT  id object langu typ dokversion
        INTO corresponding fields of table lt_dokhl
           FROM dokhl
             WHERE id = 'RE'
                AND object = _objname.

* Use only most recent version.
  sort lt_dokhl by id object langu typ ascending dokversion descending.
  delete adjacent duplicates from lt_dokhl comparing id object typ langu.

* Make sure there is at least one record here.
  clear ls_dokhl.
  read table lt_dokhl into ls_dokhl index 1.
  if sy-subrc <> 0.
    return.
  endif.

  docNode = xmlDoc->create_element( 'programDocumentation' ).

* Set docNode object attribute
  lv_str = ls_dokhl-object.
  rc = docNode->set_attribute( name = 'OBJECT' value = lv_Str ).

  Loop at lt_dokhl into ls_dokhl.

* Create language node, and set attribute
    languageNode = xmlDoc->create_element( 'language' ).
    lv_str = ls_dokhl-langu.
    rc = languageNode->set_attribute( name = 'SPRAS' value = lv_Str ).

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
      txtlines_node = xmlDoc->create_element( `textLine` ).
      me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
      rc = languageNode->append_child( txtlines_node ).
    ENDLOOP.
    rc = docNode->append_child( languageNode ) .
  Endloop.

endmethod.


method GET_DYNPRO.
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
  types: begin of tdynp,
         prog type d020s-prog,
         dnum type d020s-dnum,
         end of tdynp.

  data: idyn_fldl type table of d021s,
        idyn_flow type table of d022s,
        idyn_mcod type table of d023s.

  data: xdyn_head type  d020s,
        xdyn_fldl type  d021s,
        xdyn_flow type  d022s,
        xdyn_mcod type  d023s.

  data idynp type table of tdynp.
  data xdynp type tdynp.

  data xdyn_text type d020t-dtxt.
  data xdyn_text_string type string.

  data _objname type trobj_name.
  data rc type sy-subrc .

  data iflowsource type rswsourcet.
  data xflowsource like line of iflowsource.
  data flowsourcestring type string.

  data dynnr_node type ref to if_ixml_element.
  data dynpromatchnode type ref to if_ixml_element.
  data dynprofieldsnode type ref to if_ixml_element.
  data dynproflownode type ref to if_ixml_element.

  _objname = objname.

* Get all dynpros for program object
  clear xdynp.  refresh idynp.
  select prog dnum into table idynp
                from d020s
                   where prog = _objname
                     and type <> 'S'    " No Selection Screens
                     and type <> 'J'.   " No selection subscreens
  check sy-subrc  = 0 .

  dynp_node = xmldoc->create_element( 'dynpros' ).

  loop at idynp into xdynp.

* Retrieve dynpro imformation
    dynnr_node =  xmldoc->create_element( 'dynpro' ).

    clear:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
    refresh:  idyn_fldl, idyn_flow, idyn_mcod.

    call function 'RPY_DYNPRO_READ_NATIVE'
      exporting
        progname                    = xdynp-prog
        dynnr                       = xdynp-dnum
*       SUPPRESS_EXIST_CHECKS       = ' '
*       SUPPRESS_CORR_CHECKS        = ' '
    importing
        HEADER                      = xdyn_head
        dynprotext                  = xdyn_text
     tables
        fieldlist                   = idyn_fldl
        flowlogic                   = idyn_flow
        params                      = idyn_mcod
*       FIELDTEXTS                  =
     exceptions
        cancelled                   = 1
        not_found                   = 2
        permission_error            = 3
        others                      = 4.

    check sy-subrc = 0.

* Add heading information for screen.
    setattributesfromstructure(
                     node = dynnr_node structure =  xdyn_head  ).
* Add the dynpro text also.
    xdyn_text_string =  xdyn_text.
    rc = dynnr_node->set_attribute(
               name = 'DTEXT'  value = xdyn_text_string ).
    rc = dynp_node->append_child( dynnr_node ).

* Add fields information for screen.
    if not idyn_fldl[] is initial.
      loop at idyn_fldl into xdyn_fldl.
        dynprofieldsnode = xmldoc->create_element( 'dynprofield' ).
        setattributesfromstructure(
                 node = dynprofieldsnode structure =  xdyn_fldl ).
        rc = dynnr_node->append_child( dynprofieldsnode ).
      endloop.
    endif.

* Add flow logic of screen
    if not idyn_flow[] is initial.
      clear xflowsource. refresh  iflowsource.
      loop at idyn_flow into xdyn_flow.
        xflowsource  = xdyn_flow.
        append xflowsource to iflowsource.
      endloop.

      dynproflownode = xmldoc->create_element( 'dynproflowsource' ).
      flowsourcestring = buildsourcestring( sourcetable = iflowsource ).
      rc = dynproflownode->if_ixml_node~set_value( flowsourcestring ).
      rc = dynnr_node->append_child( dynproflownode  ).
    endif.

* Add matchcode information for screen.
    if not idyn_mcod[] is initial.
      loop at idyn_mcod into xdyn_mcod.
        check not xdyn_mcod-type is initial
          and not xdyn_mcod-content is initial.
        dynpromatchnode = xmldoc->create_element( 'dynpromatchcode' ).
        setattributesfromstructure(
                 node = dynpromatchnode structure =  xdyn_mcod ).
        rc = dynnr_node->append_child( dynpromatchnode ).
      endloop.
    endif.

  endloop.

endmethod.


method GET_PFSTATUS.
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
  data: ista type table of rsmpe_stat,
        ifun type table of rsmpe_funt,
        imen type table of rsmpe_men,
        imtx type table of rsmpe_mnlt,
        iact type table of rsmpe_act,
        ibut type table of rsmpe_but,
        ipfk type table of rsmpe_pfk,
        iset type table of rsmpe_staf,
        idoc type table of rsmpe_atrt,
        itit type table of rsmpe_titt,
        ibiv type table of rsmpe_buts.

  data: xsta type rsmpe_stat,
        xfun type rsmpe_funt,
        xmen type rsmpe_men,
        xmtx type rsmpe_mnlt,
        xact type rsmpe_act,
        xbut type rsmpe_but,
        xpfk type rsmpe_pfk,
        xset type rsmpe_staf,
        xdoc type rsmpe_atrt,
        xtit type rsmpe_titt,
        xbiv type rsmpe_buts.

  data sta_node type ref to if_ixml_element.
  data fun_node type ref to if_ixml_element.
  data men_node type ref to if_ixml_element.
  data mtx_node type ref to if_ixml_element.
  data act_node type ref to if_ixml_element.
  data but_node type ref to if_ixml_element.
  data pfk_node type ref to if_ixml_element.
  data set_node type ref to if_ixml_element.
  data doc_node type ref to if_ixml_element.
  data tit_node type ref to if_ixml_element.
  data biv_node type ref to if_ixml_element.

  data _objname type trobj_name.
  data _program type  trdir-name.
  data rc type sy-subrc.

  _objname = objname.
  _program = objname.

  call function 'RS_CUA_INTERNAL_FETCH'
    exporting
      program         = _program
      language        = sy-langu
    tables
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
    exceptions
      not_found       = 1
      unknown_version = 2
      others          = 3.

  check sy-subrc = 0.

* if there is a gui status or gui title present, then
* create pfstatus node.
  if ista[] is not initial
     or itit[] is not initial.
    pfstat_node = xmldoc->create_element( 'pfstatus' ).
  endif.


* if ista is filled, assume there are one or more
* gui statuses
  if ista[] is not initial.

    loop at ista into xsta.
      sta_node = xmldoc->create_element( 'pfstatus_sta' ).
      setattributesfromstructure(
               node = sta_node
               structure =  xsta ).
      rc = pfstat_node->append_child( sta_node ).
    endloop.

    loop at ifun into xfun.
      fun_node = xmldoc->create_element( 'pfstatus_fun' ).
      setattributesfromstructure(
               node = fun_node
               structure =  xfun ).
      rc = pfstat_node->append_child( fun_node ).
    endloop.

    loop at imen into xmen.
      men_node = xmldoc->create_element( 'pfstatus_men' ).
      setattributesfromstructure(
               node = men_node
               structure =  xmen ).
      rc = pfstat_node->append_child( men_node ).
    endloop.

    loop at imtx into xmtx.
      mtx_node = xmldoc->create_element( 'pfstatus_mtx' ).
      setattributesfromstructure(
               node = mtx_node
               structure =  xmtx ).
      rc = pfstat_node->append_child( mtx_node ).
    endloop.

    loop at iact into xact.
      act_node = xmldoc->create_element( 'pfstatus_act' ).
      setattributesfromstructure(
               node = act_node
               structure =  xact ).
      rc = pfstat_node->append_child( act_node ).
    endloop.

    loop at ibut into xbut.
      but_node = xmldoc->create_element( 'pfstatus_but' ).
      setattributesfromstructure(
               node = but_node
               structure =  xbut ).
      rc = pfstat_node->append_child( but_node ).
    endloop.

    loop at ipfk into xpfk.
      pfk_node = xmldoc->create_element( 'pfstatus_pfk' ).
      setattributesfromstructure(
               node = pfk_node
               structure =  xpfk ).
      rc = pfstat_node->append_child( pfk_node ).
    endloop.

    loop at iset into xset.
      set_node = xmldoc->create_element( 'pfstatus_set' ).
      setattributesfromstructure(
               node = set_node
               structure =  xset ).
      rc = pfstat_node->append_child( set_node ).
    endloop.

    loop at idoc into xdoc.
      doc_node = xmldoc->create_element( 'pfstatus_doc' ).
      setattributesfromstructure(
               node = doc_node
               structure =  xdoc ).
      rc = pfstat_node->append_child( doc_node ).
    endloop.


    loop at ibiv into xbiv.
      biv_node = xmldoc->create_element( 'pfstatus_biv' ).
      setattributesfromstructure(
               node = biv_node
               structure =  xbiv ).
      rc = pfstat_node->append_child( biv_node ).
    endloop.

  endif.


* It itit is filled, assume one or more titles
  if itit[] is not initial.

    loop at itit into xtit.
      tit_node = xmldoc->create_element( 'pfstatus_tit' ).
      setattributesfromstructure(
               node = tit_node
               structure =  xtit ).
      rc = pfstat_node->append_child( tit_node ).
    endloop.

  endif.

endmethod.


method GET_SOURCE.
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

  data _objName(30) type c.

  _objName = me->objName.
  read report _objName into progSource.

endmethod.


method GET_TEXTPOOL.
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
data aText type ref to if_ixml_element.
data textPoolTable type standard table of TEXTPOOL.
data textPoolRow type textPool.
data languageList type instLang.
data aLanguage type SPRAS.
data _objName(30) type c.
data rc type i.
data sTemp type string.
data languageNode type ref to if_ixml_element.
data firstLoop type flag.

  _objName = objName.


  CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
        changing
          INSTALLED_LANGUAGES = languageList.

  firstLoop = abap_true.

  loop at languageList into aLanguage.
    read textpool _objName into textPoolTable language aLanguage.
    if sy-subrc = 0.
      if firstLoop = abap_true.
        textNode = xmlDoc->create_element( 'textPool' ).
        firstLoop = abap_false.
      endif.
      languageNode = xmlDoc->create_Element( 'language' ).
      sTemp = aLanguage.
      rc = languageNode->set_attribute( name = 'SPRAS' value = sTemp ).
      loop at textPoolTable into textPoolRow.
        aText = xmlDoc->create_element( 'textElement' ).
        setAttributesFromStructure( node = aText structure =
        textPoolRow ).
        rc = languageNode->append_child( aText ).
      endloop.
      rc = textNode->append_child( languageNode ).
    endif.
  endloop.

endmethod.


method TRANSPORT_COPY.
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
            AUTHOR              = author
            GLOBAL_LOCK         = 'X'
            OBJECT              = objName
            OBJECT_CLASS        = 'ABAP'
            DEVCLASS            = devClass
*            KORRNUM             = CORRNUMBER_LOCAL
            MASTER_LANGUAGE     = sy-langu
*            PROGRAM             = PROGRAM_LOCAL
            MODE                = 'INSERT'
*       IMPORTING
*            AUTHOR              = UNAME
*            KORRNUM             = CORRNUMBER_LOCAL
*            DEVCLASS            = DEVCLASS_LOCAL
       EXCEPTIONS
            CANCELLED           = 1
            PERMISSION_FAILURE  = 2
            UNKNOWN_OBJECTCLASS = 3.

  if sy-subrc <> 0.
    case sy-subrc.
      when 2.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>not_authorized.
      when others.
        raise exception type zcx_saplink
          exporting
            textid = zcx_saplink=>system_error.
    endcase.
  endif.

endmethod.


method UPDATE_WB_TREE.

  DATA: BEGIN OF pname,
          root(3) VALUE 'PG_',
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

endmethod.
ENDCLASS.
