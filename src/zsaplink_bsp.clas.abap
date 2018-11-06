class ZSAPLINK_BSP definition
  public
  inheriting from ZSAPLINK
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



CLASS ZSAPLINK_BSP IMPLEMENTATION.


method CHECKEXISTS.
*/---------------------------------------------------------------------\
*|   This file is part of the SAPlink-plugins.                         |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

data _bspName TYPE O2APPLNAME.

  _bspName = objName.
  call method cl_o2_api_application=>check_exist
    exporting
      p_application = _bspName
    importing
      p_exists      = exists.

endmethod.


method CREATEIXMLDOCFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of the SAPlink-plugins.                         |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

data _bspName TYPE O2APPLNAME.
data bspApp type ref to CL_O2_API_APPLICATION.
data bspAttributes type O2APPLATTR.
data rc type sysubrc.
data sourceString type string.
data navGraph type o2applgrap_table.
data navLine type O2APPLGRAP.

*xml nodes
data rootNode type ref to if_ixml_element.
data navNode type ref to if_ixml_element.
data pageNode type ref to if_ixml_element.
data layoutNode type ref to if_ixml_element.
data eventNode type ref to if_ixml_element.
data parameterNode type ref to if_ixml_element.
data typeDefNode type ref to if_ixml_element.

*page data
data bspPages type O2PAGELIST.
data bspPage type O2PAGATTR.
data pageKey type O2PAGKEY.
data pageAPI type ref to CL_O2_API_PAGES.
data pageAttributes type O2PAGATTR.
data pageContent TYPE O2PAGELINE_TABLE.
data pageXML TYPE XSTRING.
data pageGUIDS TYPE BSP_GUIDS.
data eventHandlers type O2PAGEVH_TABLETYPE.
data eventHandler type O2PAGEVHS.
data eventAttributes type O2PAGEVH.
data eventContent type RSWSOURCET.
data typeDefinitionSource type RSWSOURCET.
data pageParameters TYPE O2PAGPAR_TABLETYPE.
data pageParameter type O2PAGPARS.

  _bspName = objName.

  call method CL_O2_API_APPLICATION=>Load
    exporting
      P_APPLICATION_NAME = _bspName
    importing
      P_APPLICATION = bspApp
    exceptions
      OBJECT_NOT_EXISTING = 1
      PERMISSION_FAILURE  = 2
      ERROR_OCCURED       = 3.
  if sy-subrc <> 0.
    case sy-subrc.
      when 1.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_found.
      when 2.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_authorized.
      when 3.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>system_error.
    endcase.
  endif.

  call method bspApp->get_attributes
    exporting
      p_version = 'A'
    importing
      p_attributes = bspAttributes.

* Create parent node
  data _objType type string.
*  _objType = objType.
  _objType = getObjectType( ).
  rootNode = xmlDoc->create_element( _objType ).
  setAttributesFromStructure( node = rootNode structure = bspAttributes
  ).

* navigation data
  CALL METHOD bspApp->get_navgraph
    EXPORTING
      P_VERSION      = 'A'
    IMPORTING
      P_NAVGRAPH     = navGraph
    EXCEPTIONS
      OBJECT_INVALID = 1
      OBJECT_DELETED = 2
      ERROR_OCCURED  = 3
      others         = 4
        .

  loop at navGraph into navLine.
    navNode = xmlDoc->create_element( 'navgraph' ).
    setAttributesFromStructure( node = navNode structure = navLine ).
*   add navigation graph entry
    rc = rootNode->append_child( navNode ).
  endloop.

*/-----------------------Create nodes for pages-----------------------\
* Get BSP pages
  call method CL_O2_API_PAGES=>GET_ALL_PAGES
    exporting
      P_APPLNAME = _bspName
      P_VERSION = 'A'
    importing
      P_PAGES = bspPages.

  loop at bspPages into bspPage.
    pageNode = xmlDoc->create_element( 'page' ).

*   page header data
    pageKey-APPLNAME = _bspName.
    pageKey-PAGEKEY = bspPage-PAGEKEY.
    clear pageAPI.
    call method CL_O2_API_PAGES=>load
          exporting
            P_PAGEKEY = pageKey
          importing
            P_PAGE = pageAPI.

*   page attributes
    CALL METHOD pageAPI->get_attrs
      IMPORTING
        P_ATTRS      = pageAttributes
      EXCEPTIONS
        PAGE_DELETED = 1
        others       = 2
        .
    setAttributesFromStructure( node = pageNode structure =
    pageAttributes ).

    if pageAttributes-pagetype = 'C'.  "controller pages
*get/set controller class here if needed (pageAttributes-implclass)
    else.                              "non-controller pages
*     page layout
      call method pageAPI->GET_PAGE
        importing
          P_CONTENT = pageContent
          P_XML_SOURCE = pageXML
          P_OTR_GUIDS = pageGUIDS.

      if pageContent is not initial.
        layoutNode = xmlDoc->create_element( 'layout' ).
        sourceString = buildSourceString( pageTable = pageContent ).
        if sourceString is not initial.
          rc = layoutNode->IF_IXML_NODE~SET_VALUE( sourceString ).
        endif.
        rc = pageNode->append_child( layoutNode ).
      endif.

*     event handlers
      call method pageAPI->GET_EVENT_HANDLERS
        importing
          P_EV_HANDLER = eventHandlers.

      loop at eventHandlers into eventHandler.
        eventNode = xmlDoc->create_element( 'event' ).
        move-corresponding eventHandler to eventAttributes.
        setAttributesFromStructure( node = eventNode structure =
        eventAttributes ).

        sourceString = buildSourceString( sourceTable =
        eventHandler-source ).
        if sourceString is not initial.
          rc = eventNode->IF_IXML_NODE~SET_VALUE( sourceString ).
        endif.
        rc = pageNode->append_child( eventNode ).
      endloop.

*     page parameters
      refresh pageParameters.
      CALL METHOD pageAPI->get_parameters
        IMPORTING
          P_PARAMETERS = pageParameters
        EXCEPTIONS
          PAGE_DELETED = 1
          INVALID_CALL = 2
          others       = 3.

      loop at pageParameters into pageParameter.
        parameterNode = xmlDoc->create_element( 'parameter' ).
        setAttributesFromStructure( node = parameterNode structure =
        pageParameter ).
        rc = pageNode->append_child( parameterNode ).
      endloop.

*     type definitions
      CALL METHOD pageAPI->get_type_source
        IMPORTING
          P_SOURCE     = typeDefinitionSource
        EXCEPTIONS
          PAGE_DELETED = 1
          INVALID_CALL = 2
          others       = 3
              .

      if typeDefinitionSource is not initial.
        typeDefNode = xmlDoc->create_element( 'typedef' ).
        sourceString = buildSourceString( sourceTable =
        typeDefinitionSource ).
        if sourceString is not initial.
          rc = typeDefNode->IF_IXML_NODE~SET_VALUE( sourceString ).
        endif.
        rc = pageNode->append_child( typeDefNode ).
      endif.
    endif.                             "controller/non-controller pages

*   add page node
    rc = rootNode->append_child( pageNode ).
  endloop.
*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootNode ).
  ixmlDocument = xmlDoc.
endmethod.


method CREATEOBJECTFROMIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of the SAPlink-plugins.                         |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  "data _bspName TYPE O2APPLNAME.
  data bspAttributes type O2APPLATTR.
  data flag type char1.
  data transReq type trkorr.
  data nodes type o2applnode_table.
  data navGraph type o2applgrap_table.
  data navLine type O2APPLGRAP.
  data application type ref to cl_o2_api_application.
  data nameSpace type skwf_urlp.
  data folderName type string.
  data folderDesc type sdok_descr.
  data rootNode type ref to if_ixml_element.
  data pageNode type ref to if_ixml_element.
  data node type ref to if_ixml_element.
  data filter type ref to if_ixml_node_filter.
  data iterator type ref to if_ixml_node_iterator.
  data pageIterator type ref to if_ixml_node_iterator.
  data ixmlNode type ref to if_ixml_node.
  data source type string.
  data trobjName type trobj_name.
  data objectName type SEU_OBJKEY.
  data obj_name type E071-OBJ_NAME.
  data l_nodeguid type ICFNODGUID.
  data l_icfdocu  type ICF_DOCU.
  data l_icfname  type ICFNAME.
  data _devclass type devclass.
  data checkExists type flag.

  " page data
  data bspPages type O2PAGELIST.
  data bspPage type O2PAGATTR.
  data pageAttributes type O2PAGATTR.
  data newPage TYPE REF TO CL_O2_API_PAGES.
  data pageContent TYPE O2PAGELINE_TABLE.
  data eventHandlers type O2PAGEVH_TABLETYPE.
  data eventHandler type O2PAGEVHS.
  data eventAttributes type O2PAGEVH.
  data pageParameters TYPE O2PAGPAR_TABLETYPE.
  data pageParameter type O2PAGPARS.
  data typeDefinitionSource type RSWSOURCET.
  data _objType type string.

  _devclass = devclass.
  _objType = getObjectType( ).

  xmlDoc = ixmlDocument.
  rootNode = xmlDoc->find_from_name( _objType ).

  call method GETSTRUCTUREFROMATTRIBUTES
        exporting
          node = rootNode
        changing
          structure = bspAttributes.

  objName = bspAttributes-applname.
  bspAttributes-devclass = _devclass.
** check bsp does not exist
*  _bspName = bspAttributes-applname.
*  call method cl_o2_api_application=>check_exist
*    exporting
*      p_application = _bspName
*    importing
*      p_exists      = flag.
*  if flag is not initial and overwrite <> 'X'.
*    raise exception type zcx_saplink
*      exporting textid = zcx_saplink=>existing.
*  endif.
  checkExists = checkexists( ).
  if checkExists is not initial.
    if overwrite is initial.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>existing.
    else.
*     delete object for new install
      deleteobject( ).
    endif.
  endif.

* retrieve navigation modeler details
  free: filter, iterator, node.
  filter = xmlDoc->create_filter_name( 'navgraph' ).
  iterator = xmlDoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  while node is not initial.
    clear navLine.
    call method GETSTRUCTUREFROMATTRIBUTES
          exporting
            node = node
          changing
            structure = navLine.
    append navLine to navGraph.
    node ?= iterator->get_next( ).
  endwhile.

****************Create application****************
* create bsp
  call method cl_o2_api_application=>create_new
    EXPORTING
      p_application_data      = bspAttributes
      p_nodes                 = nodes
      p_navgraph              = navGraph
    IMPORTING
      p_application           = application
    EXCEPTIONS
      object_already_existing = 1
      object_just_created     = 2
      not_authorized          = 3
      undefined_name          = 4
      author_not_existing     = 5
      action_cancelled        = 6
      error_occured           = 7
      invalid_parameter       = 8.
  if sy-subrc <> 0.
    case sy-subrc.
      when 1.
        if overwrite <> 'X'.
          raise exception type zcx_saplink
            exporting textid = zcx_saplink=>existing.
        endif.
      when 3.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_authorized.
      when others.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>system_error.
    endcase.
  endif.

* save bsp
  call method application->save
    IMPORTING
      p_devclass            = _devclass
    CHANGING
      p_transport_request   = transReq
    EXCEPTIONS
*      object_invalid        = 1
*      object_not_changeable = 2
*      action_cancelled      = 3
      permission_failure    = 4.
*      not_changed           = 5
*      error_occured         = 6.

  if sy-subrc <> 0.
    case sy-subrc.
      when 4.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_authorized.
      when others.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>system_error.
    endcase.
  endif.

* put bsp in worklist
  trobjName = objName.
  call function 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = 'WAPD'
      obj_name          = trobjName
    EXCEPTIONS
      wrong_object_name = 1.
  if sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  endif.

* reset
  call method application->set_changeable
    EXPORTING
      p_changeable                = ' '
      p_complete_application      = 'X'
    EXCEPTIONS
      action_cancelled            = 1
      object_locked_by_other_user = 2
      permission_failure          = 3
      object_already_changeable   = 4
      object_already_unlocked     = 5
      object_just_created         = 6
      object_deleted              = 7
      object_modified             = 8
      object_not_existing         = 9
      object_invalid              = 10
      error_occured               = 11.
  if sy-subrc ne 0.
    case sy-subrc.
      when 3.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_authorized.
      when others.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>system_error.
    endcase.
  endif.

* create application folder in mime repository
  nameSpace = 'SAP'.
  folderName = bspAttributes-applext.
  folderDesc = bspAttributes-text.

  call method cl_wb_mime_repository=>create_appl_folder
    EXPORTING
      folder_name          = folderName
      folder_desc          = folderDesc
      name_space           = nameSpace
    CHANGING
      dev_package          = _devclass
      corr_number          = transReq
    EXCEPTIONS
      no_folder_name       = 1
      folder_exists        = 0
      name_space_not_found = 3
      error_occured        = 4
      others               = 5.
** if this doesnt work, no biggie...it will create when BSP opened
*  if sy-subrc <> 0.
**raise error
*    exit.
*  endif.

****************Create pages****************
  free: filter, iterator, node.
  filter = xmlDoc->create_filter_name( 'page' ).
  pageIterator = xmlDoc->create_iterator_filtered( filter ).
  pageNode ?= pageIterator->get_next( ).

  while pageNode is not initial.
    free newPage.
*   get page data
    clear pageAttributes.
    call method GETSTRUCTUREFROMATTRIBUTES
          exporting
            node = pageNode
          changing
            structure = pageAttributes.

*   create new page
    CALL METHOD cl_o2_api_pages=>create_new_page
      EXPORTING
        p_pageattrs           = pageAttributes
      IMPORTING
        P_PAGE                = newPage.
*      EXCEPTIONS
*        OBJECT_ALREADY_EXISTS = 1
*        INVALID_NAME          = 2
*        ERROR_OCCURED         = 3
*        O2APPL_NOT_EXISTING   = 4
*        others                = 5
*            .
*    if sy-subrc <> 0.
**raise error
*      exit.
*    endif.

    if pageAttributes-pagetype = 'C'.  "controller pages
*get/set controller class here if needed (pageAttributes-implclass)
    else.                              "non-controller pages
*     get page content
      refresh pageContent.
      free ixmlNode. clear source.
      ixmlNode = pageNode->find_from_name( 'layout' ).
      if ixmlNode is not initial.
        source = ixmlNode->get_value( ).
        pageContent = BUILDTABLEFROMSTRING( source ).
      endif.

*     create page content
      if pageContent is not initial.
        CALL METHOD newPage->set_page
          EXPORTING
            p_content    = pageContent.
*          EXCEPTIONS
*            PAGE_DELETED = 1
*            INVALID_CALL = 2
*            others       = 3
*                .
*        if sy-subrc <> 0.
**    raise error
*          exit.
*        endif.
      endif.

*     get event handlers
      free: filter, iterator, node.
      filter = pageNode->create_filter_name( 'event' ).
      iterator = pageNode->create_iterator_filtered( filter ).
      node ?= iterator->get_next( ).

      refresh eventHandlers.
      while node is not initial.
        clear eventHandler.
        call method GETSTRUCTUREFROMATTRIBUTES
              exporting
                node = node
              changing
                structure = eventAttributes.
        move-corresponding eventAttributes to eventHandler.
        source = node->get_value( ).
        if source is not initial.
          eventHandler-source = BUILDTABLEFROMSTRING( source ).
        endif.
        append eventHandler to eventHandlers.
        node ?= iterator->get_next( ).
      endwhile.

*     create event handlers
      if eventHandlers is not initial.
        CALL METHOD newPage->set_event_handlers
          EXPORTING
            p_ev_handler = eventHandlers.
*          EXCEPTIONS
*            PAGE_DELETED = 1
*            INVALID_CALL = 2
*            others       = 3
*                .
*        if sy-subrc <> 0.
**    raise error
*          exit.
*        endif.
      endif.

*     get page parameters
      free: filter, iterator, node.
      filter = pageNode->create_filter_name( 'parameter' ).
      iterator = pageNode->create_iterator_filtered( filter ).
      node ?= iterator->get_next( ).

      refresh pageParameters.
      while node is not initial.
        clear pageParameter.
        call method GETSTRUCTUREFROMATTRIBUTES
              exporting
                node = node
              changing
                structure = pageParameter.

        append pageParameter to pageParameters.
        node ?= iterator->get_next( ).
      endwhile.

*     create page parameters
      if pageParameters is not initial.
        CALL METHOD newPage->set_parameters
          EXPORTING
            p_parameters      = pageParameters.
*          EXCEPTIONS
*            PAGE_DELETED      = 1
*            INVALID_PARAMETER = 2
*            INVALID_CALL      = 3
*            others            = 4
*                .
*        if sy-subrc <> 0.
**    raise error
*          exit.
*        endif.
      endif.

*     get type definitions
      refresh typeDefinitionSource.
      free ixmlNode. clear source.
      ixmlNode = pageNode->find_from_name( 'typedef' ).
      if ixmlNode is not initial.
        source = ixmlNode->get_value( ).
        typeDefinitionSource = BUILDTABLEFROMSTRING( source ).
      endif.

*     create type definition source code
      if typeDefinitionSource is not initial.
        CALL METHOD newPage->set_type_source
          EXPORTING
            p_source     = typeDefinitionSource.
*          EXCEPTIONS
*            PAGE_DELETED = 1
*            INVALID_CALL = 2
*            others       = 3
*                .
*        if sy-subrc <> 0.
**    raise error
*          exit.
*        endif.
      endif.
    endif.                             "controller/non-controller pages

*   put page in worklist
    clear: objectName, obj_name.
    objectName = cl_wb_object_type=>get_concatenated_key_from_id(
           p_key_component1 = pageAttributes-applname
           p_key_component2 = pageAttributes-pagekey
           p_external_id    = 'WG ' ).

    obj_name = objectName.
    translate obj_name to upper case.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
        EXPORTING
          OBJECT                  = 'WAPP'
          OBJ_NAME                = obj_name.
*        EXCEPTIONS
*          OTHERS                  = 1.

    CALL FUNCTION 'RS_WORKING_AREA_INIT'.

*   save page
    CALL METHOD newPage->save
      EXPORTING
*        P_SAVE_ACTIVE      = 'X'
        P_WITH_ALL_TEXTS   = 'X'.
*      EXCEPTIONS
*        NOT_CHANGED        = 1
*        ERROR_OCCURED      = 2
*        INVALID_PARAMETERS = 3
*        others             = 4
*            .
*    if sy-subrc <> 0.
**raise error
*      exit.
*    endif.

    pageNode ?= pageIterator->get_next( ).
  endwhile. "pageNode

****************Create SICF node****************
  CALL FUNCTION 'HTTP_GET_NODEGUID'
    EXPORTING
      parguid        = '5MR7OICIELJY8B1AFT2X1CCKV'
      "GUID of node "/sap/bc/bsp"
      nodename       = 'SAP'
    IMPORTING
      nodeguid       = l_nodeguid
    EXCEPTIONS
      node_not_found = 1
      OTHERS         = 2.

  l_icfname = bspAttributes-applname.
  l_icfdocu = bspAttributes-text.
  translate l_icfname to lower case.

  CALL FUNCTION 'HTTPTREE_INSERT_NODE'
    EXPORTING
      p_icf_name                      = l_icfname
      p_icfparguid                    = l_nodeguid
      p_icfdocu                       = l_icfdocu
      p_doculang                      = sy-langu
      p_icfactive                     = 'X'         "activate service
      p_package                       = devclass
   EXCEPTIONS
      NO_NEW_VIRTUAL_HOST             = 1
      SPECIAL_SERVICE_ERROR           = 2
      PARENT_NOT_EXISTING             = 3
      ENQUEUE_ERROR                   = 4
      NODE_ALREADY_EXISTING           = 5
      EMPTY_DOCU                      = 6
      DOCULANG_NOT_INSTALLED          = 7
      SECURITY_INFO_ERROR             = 8
      USER_PASSWORD_ERROR             = 9
      PASSWORD_ENCRYPTION_ERROR       = 10
      INVALID_URL                     = 11
      INVALID_OTR_CONCEPT             = 12
      FORMFLG401_ERROR                = 13
      HANDLER_ERROR                   = 14
      TRANSPORT_ERROR                 = 15
      TADIR_ERROR                     = 16
      PACKAGE_NOT_FOUND               = 17
      OTHERS                          = 18
            .

* i guess if we made it this far, we will assume success
* successful install
  name = objName.

endmethod.


method DELETEOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of the SAPlink-plugins.                         |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

data bspName type o2applname.
data bspAttributes type O2APPLATTR.
data bspExt type o2applext.
data bspApp type ref to cl_o2_api_application.
data nameSpace type skwf_urlp.
data folderName type string.
data objectName type SEU_OBJKEY.
data obj_name type E071-OBJ_NAME.
data l_nodeguid type ICFNODGUID.
data l_icfname  type ICFNAME.

data bspPages type o2pagename_table.
data bspPage type o2pagename.
data pagekey type o2pagkey.

  bspName = objName.

* save external name for later
  call method cl_o2_api_application=>get_application_external_name
    EXPORTING
      p_application_key   = bspName
      p_version           = 'A'
    IMPORTING
      p_application_name  = bspExt
    EXCEPTIONS
      object_not_existing = 1.
  if sy-subrc <> 0.
    bspExt = bspName.
  endif.

* load bsp
  call method cl_o2_api_application=>load
    EXPORTING
      p_application_name  = bspName
    IMPORTING
      p_application       = bspApp
    EXCEPTIONS
      object_not_existing = 1
      permission_failure  = 2
      error_occured       = 3.
  if sy-subrc <> 0.
    case sy-subrc.
      when 3.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_authorized.
      when 9.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_found.
      when others.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>system_error.
    endcase.
  endif.

* set bsp changeable
  call method bspApp->set_changeable
    EXPORTING
      p_changeable                = 'X'
      p_complete_application      = 'X'
    EXCEPTIONS
      action_cancelled            = 1
      object_locked_by_other_user = 2
      permission_failure          = 3
      object_already_changeable   = 0
      object_already_unlocked     = 5
      object_just_created         = 6
      object_deleted              = 7
      object_modified             = 8
      object_not_existing         = 9
      object_invalid              = 10
      error_occured               = 11.
  if sy-subrc <> 0.
    case sy-subrc.
      when 2.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>locked.
      when 3.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_authorized.
      when 9.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_found.
      when others.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>system_error.
    endcase.
  endif.

* delete BSP pages
  call method cl_o2_api_application=>get_all_pages_static
    EXPORTING
      p_application_key = bspName
    IMPORTING
      p_pages           = bspPages
    EXCEPTIONS
      object_not_found  = 1.

  loop at bspPages into bspPage.
    pageKey-applname = bspPage-applname.
    pageKey-pagekey  = bspPage-pagekey.
    call method cl_o2_page=>delete_page_for_application
      EXPORTING
        p_pagekey           = pageKey
      EXCEPTIONS
        object_not_existing = 1
        error_occured       = 2.
  endloop.

* delete bsp
  call method bspApp->delete
    EXCEPTIONS
      object_not_empty      = 1
      object_not_changeable = 2
      object_invalid        = 3
      action_cancelled      = 4
      permission_failure    = 5
      error_occured         = 6.
   if sy-subrc <> 0.
    case sy-subrc.
      when 5.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>not_authorized.
      when others.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>system_error.
    endcase.
  endif.

* reset lock
  objectName = bspName.
  call method cl_o2_api_application=>call_access_permission
    EXPORTING
      p_mode                 = 'FREE'
      p_object               = objectName
      p_complete_application = 'X'
    EXCEPTIONS
      action_canceled        = 1
      enqueued_by_user       = 2
      locked_by_author       = 3
      permission_failure     = 4
      error_occured          = 5.

* remove from working area
  obj_name = objectName.
  call function 'RS_DELETE_FROM_WORKING_AREA'
    EXPORTING
      object    = 'WAPD'
      obj_name  = obj_name
      immediate = 'X'.

* delete folder in mime repository
  nameSpace = 'SAP'.
  folderName = bspExt.

  call method cl_wb_mime_repository=>delete_appl_folder
    EXPORTING
      folder_name          = folderName
      name_space           = nameSpace
    EXCEPTIONS
      no_folder_name       = 1
      folder_not_found     = 2
      name_space_not_found = 3
      error_occured        = 4
      others               = 5.

****************Delete SICF node****************
  CALL FUNCTION 'HTTP_GET_NODEGUID'
    EXPORTING
      parguid        = '5MR7OICIELJY8B1AFT2X1CCKV'
      "GUID of node "/sap/bc/bsp"
      nodename       = 'SAP'
    IMPORTING
      nodeguid       = l_nodeguid
    EXCEPTIONS
      node_not_found = 1
      OTHERS         = 2.

  l_icfname = bspName.
  translate l_icfname to lower case.

  call function 'HTTPTREE_DELETE_NODE'
    exporting
      p_icf_name                        = l_icfname
      p_icfparguid                      = l_nodeguid
    exceptions
      no_virtual_host_delete            = 1
      special_service_error             = 2
      enqueue_error                     = 3
      node_not_existing                 = 4
      node_has_childs                   = 5
      node_is_aliased                   = 6
      node_not_in_original_system       = 7
      transport_error                   = 8
      tadir_error                       = 9
      db_error                          = 10
      others                            = 11.
endmethod.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of the SAPlink-plugins.                         |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  objecttype = 'WAPA'. "BSP Application

endmethod.
ENDCLASS.
