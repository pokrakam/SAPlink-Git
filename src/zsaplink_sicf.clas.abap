class ZSAPLINK_SICF definition
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



CLASS ZSAPLINK_SICF IMPLEMENTATION.


method CHECKEXISTS .
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

data _objDirEntry type string.
data _icfName type ICFNAME.
data _parentNode type ICFPARGUID.
data l_nodeGuid type ICFNODGUID.
data tempstring type string.

  _objDirEntry = objName.
  split _objDirEntry at space into _icfName tempString.
  condense: _icfName, tempString.
  _parentNode = tempString.

  CALL FUNCTION 'HTTP_GET_NODEGUID'
  EXPORTING
    parguid        = _parentNode
    nodename       = _icfName
  IMPORTING
    nodeguid       = l_nodeguid
  EXCEPTIONS
    node_not_found = 1
    OTHERS         = 2.

  if sy-subrc = 0.
    exists = 'X'.
  endif.

endmethod.


method CREATEIXMLDOCFROMOBJECT .
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

data rc type sysubrc.
data rootNode type ref to if_ixml_element.
data _objDirEntry type string.
data _icfName type ICFNAME.
data _parentNode type ICFPARGUID.
*data l_nodeGuid type ICFNODGUID.
data icfAttributes type icfservice.
data tempString type string.

data docNode type ref to if_ixml_element.
data handlerNode type ref to if_ixml_element.
data wa_icfdocu type icfdocu.
data wa_icfhandler type icfhandler.
data it_icfhandler type table of icfhandler.

  _objDirEntry = objName.
  split _objDirEntry at space into _icfName tempString.
  condense: _icfName, tempString.
  _parentNode = tempString.

*  CALL FUNCTION 'HTTP_GET_NODEGUID'
*  EXPORTING
*    parguid        = _parentNode
*    nodename       = _icfName
*  IMPORTING
*    nodeguid       = l_nodeguid
*  EXCEPTIONS
*    node_not_found = 1
*    OTHERS         = 2.
*
*  if sy-subrc <> 0.
*    case sy-subrc.
*      when 1.
*        raise exception type zcx_saplink
*          exporting textid = zcx_saplink=>not_found.
*      when others.
*        raise exception type zcx_saplink
*          exporting textid = zcx_saplink=>system_error.
*    endcase.
*  endif.

  select single * from icfservice into icfAttributes
    where icf_name = _icfName
    and   icfparguid = _parentNode.
*    and   icfnodguid = l_nodeGuid.

  if sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>not_found.
  endif.

* Create parent node
  data _objType type string.
  _objType = getObjectType( ).
  rootNode = xmlDoc->create_element( _objType ).
  setAttributesFromStructure( node = rootNode structure = icfAttributes
  ).
*  rc = rootNode->if_ixml_node~set_value( _objDirEntry ).

* icfdocu info
  select single * from icfdocu into wa_icfdocu
    where icf_name = _icfName
    and   icfparguid = _parentNode.

  if sy-subrc = 0.
    docNode = xmlDoc->create_element( 'icfdocu' ).
    setAttributesFromStructure( node = docNode structure = wa_icfdocu ).
    rc = rootNode->append_child( docNode ).
  endif.

* icfhandler entries
  select * from icfhandler into table it_icfhandler
    where icf_name = _icfName
    and   icfparguid = _parentNode.

  loop at it_icfhandler into wa_icfhandler.
    handlerNode = xmlDoc->create_element( 'icfhandler' ).
    setAttributesFromStructure( node = handlerNode structure = wa_icfhandler ).
    rc = rootNode->append_child( handlerNode ).
  endloop.

* append root to xml and return
  rc = xmldoc->append_child( rootNode ).
  ixmlDocument = xmlDoc.
endmethod.


method CREATEOBJECTFROMIXMLDOC .
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

*data _bspName TYPE O2APPLNAME.
*data bspAttributes type O2APPLATTR.
*data flag type char1.
*data transReq type trkorr.
*data nodes type o2applnode_table.
*data navGraph type o2applgrap_table.
*data navLine type O2APPLGRAP.
*data application type ref to cl_o2_api_application.
*data nameSpace type skwf_urlp.
*data folderName type string.
*data folderDesc type sdok_descr.
*data rootNode type ref to if_ixml_element.
*data pageNode type ref to if_ixml_element.
data node type ref to if_ixml_element.
data filter type ref to if_ixml_node_filter.
data iterator type ref to if_ixml_node_iterator.
*data pageIterator type ref to if_ixml_node_iterator.
*data ixmlNode type ref to if_ixml_node.
*data source type string.
*data trobjName type trobj_name.
*data objectName type SEU_OBJKEY.
*data obj_name type E071-OBJ_NAME.
*data l_nodeguid type ICFNODGUID.
*data l_icfdocu  type ICF_DOCU.
*data l_icfname  type ICFNAME.
data _devclass type devclass.
data checkExists type flag.
*
**page data
*data bspPages type O2PAGELIST.
*data bspPage type O2PAGATTR.
*data pageAttributes type O2PAGATTR.
*data newPage TYPE REF TO CL_O2_API_PAGES.
*data pageContent TYPE O2PAGELINE_TABLE.
*data eventHandlers type O2PAGEVH_TABLETYPE.
*data eventHandler type O2PAGEVHS.
*data eventAttributes type O2PAGEVH.
*data pageParameters TYPE O2PAGPAR_TABLETYPE.
*data pageParameter type O2PAGPARS.
*data typeDefinitionSource type RSWSOURCET.
data _objType type string.

data rc type sysubrc.
data rootNode type ref to if_ixml_element.
data _objDirEntry type SOBJ_NAME.
data _icfName type ICFNAME.
data _parentNode type ICFPARGUID.
*data l_nodeGuid type ICFNODGUID.
data icfAttributes type icfservice.
data _icfdocu  type ICF_DOCU.
data wa_icfHandler type icfHandler.
data wa_icfdocu type icfdocu.
data tempString type string.

  _devclass = devclass.
  _objType = getObjectType( ).

  xmlDoc = ixmlDocument.
  rootNode = xmlDoc->find_from_name( _objType ).

  call method GETSTRUCTUREFROMATTRIBUTES
        exporting
          node = rootNode
        changing
          structure = icfAttributes.

  _objDirEntry = icfAttributes-icf_name.
  _objDirEntry+15 = icfAttributes-icfparguid.

  objName = _objDirEntry.

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

* get documentation info
  free node.
  node = rootnode->find_from_name( 'icfdocu' ).
  if node IS NOT INITIAL.
    call method GETSTRUCTUREFROMATTRIBUTES
          exporting
            node = node
          changing
            structure = wa_icfdocu.
  endif.

****************Create SICF node****************
*  _icfdocu = wa_icfdocu-icf_docu.

  CALL FUNCTION 'HTTPTREE_INSERT_NODE'
    EXPORTING
      p_icf_name                      = icfAttributes-icf_Name
      p_icfparguid                    = icfAttributes-icfparguid
      p_icfdocu                       = wa_icfdocu-icf_docu
      p_doculang                      = wa_icfdocu-icf_langu
*      p_icfactive                     = 'X'     "activate service
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

  if sy-subrc <> 0.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  endif.

* insert handler data
  free: filter, iterator, node.
  filter = xmlDoc->create_filter_name( 'icfhandler' ).
  iterator = xmlDoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  while node is not initial.
    call method GETSTRUCTUREFROMATTRIBUTES
          exporting
            node = node
          changing
            structure = wa_icfHandler.

    if wa_icfHandler is not initial.
      insert icfHandler from wa_icfHandler.
    endif.
    node ?= iterator->get_next( ).
  endwhile.

* successful install
  name = objName.

endmethod.


method DELETEOBJECT .
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

data _objDirEntry type string.
data _icfName type ICFNAME.
data _parentNode type ICFPARGUID.
data tempstring type string.

  _objDirEntry = objName.
  split _objDirEntry at space into _icfName tempString.
  condense: _icfName, tempString.
  _parentNode = tempString.

****************Delete SICF node****************
  translate _icfname to lower case.

  call function 'HTTPTREE_DELETE_NODE'
    exporting
      p_icf_name                        = _icfname
      p_icfparguid                      = _parentNode
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


method GETOBJECTTYPE .
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

  objecttype = 'SICF'. "SICF Node

endmethod.
ENDCLASS.
