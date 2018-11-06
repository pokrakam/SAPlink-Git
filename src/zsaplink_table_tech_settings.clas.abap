class ZSAPLINK_TABLE_TECH_SETTINGS definition
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
protected section.

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
ENDCLASS.



CLASS ZSAPLINK_TABLE_TECH_SETTINGS IMPLEMENTATION.


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

  DATA: l_name TYPE ddobjname,
        dd09l_wa type dd09l.
  l_name = objname.

  CALL FUNCTION 'DDIF_TABT_GET'
    EXPORTING
      name                = l_name
    IMPORTING
      DD09l_WA            = DD09l_WA
    EXCEPTIONS
      ILLEGAL_INPUT       = 1
      OTHERS              = 2.
  IF sy-subrc = 0 AND dd09l_wa-TABNAME IS NOT INITIAL.
    exists = 'X'.
  ENDIF.


ENDMETHOD.


METHOD CREATEIXMLDOCFROMOBJECT.
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

  DATA: gotstate TYPE ddgotstate,
        dd09l_wa TYPE dd09l.

*xml nodes
  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA rc         TYPE sysubrc.
  DATA _tabtname  TYPE ddobjname.
  _tabtname = objname.

  CALL FUNCTION 'DDIF_TABT_GET'
    EXPORTING
      name          = _tabtname
    IMPORTING
      gotstate      = gotstate
      dd09l_wa      = dd09l_wa
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0 OR dd09l_wa-tabname IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.

* Create parent node
  DATA _objtype TYPE string.
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = dd09l_wa ).

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
ENDMETHOD.


METHOD CREATEOBJECTFROMIXMLDOC.
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

  DATA: gotstate TYPE ddgotstate,
        dd09l_wa TYPE dd09l.

*xml nodes
  DATA rootnode    TYPE REF TO if_ixml_element.
  DATA rc          TYPE sysubrc.
  DATA _tabtname   TYPE ddobjname.
  DATA _devclass   TYPE devclass.
  DATA checkexists TYPE flag.
  DATA _objtype    TYPE string.

  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = dd09l_wa.

  objname = dd09l_wa-tabname.

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

  DATA : l_pgmid         TYPE tadir-pgmid,
           l_object      TYPE tadir-object,
           l_obj_name    TYPE tadir-obj_name,
           l_dd_objname  TYPE ddobjname,
           l_srcsystem   TYPE tadir-srcsystem,
           l_author      TYPE tadir-author,
           l_devclass    TYPE tadir-devclass,
           l_masterlang  TYPE tadir-masterlang.


  l_pgmid      = 'R3TR'.
  l_object     = _objtype.
  l_obj_name   = objname.
  l_dd_objname = objname.
  l_srcsystem  = sy-sysid.
  l_author     = sy-uname.
  l_devclass   = _devclass.
  l_masterlang = sy-langu.

  DATA: itadir TYPE tadir.
  itadir-pgmid      = l_pgmid.
  itadir-object     = l_object.
  itadir-obj_name   = l_obj_name.
  itadir-srcsystem  = l_srcsystem.
  itadir-author     = l_author.
  itadir-devclass   = l_devclass.
  itadir-masterlang = l_masterlang.
  MODIFY tadir FROM itadir.

  CALL FUNCTION 'TR_TADIR_INTERFACE'
    EXPORTING
      wi_test_modus                  = ' '
      wi_delete_tadir_entry          = 'X'
      wi_tadir_pgmid                 = l_pgmid
      wi_tadir_object                = l_object
      wi_tadir_obj_name              = l_obj_name
      wi_tadir_srcsystem             = l_srcsystem
      wi_tadir_author                = l_author
      wi_tadir_devclass              = l_devclass
      wi_tadir_masterlang            = l_masterlang
      iv_set_edtflag                 = ''
    EXCEPTIONS
      tadir_entry_not_existing       = 1
      tadir_entry_ill_type           = 2
      no_systemname                  = 3
      no_systemtype                  = 4
      original_system_conflict       = 5
      object_reserved_for_devclass   = 6
      object_exists_global           = 7
      object_exists_local            = 8
      object_is_distributed          = 9
      obj_specification_not_unique   = 10
      no_authorization_to_delete     = 11
      devclass_not_existing          = 12
      simultanious_set_remove_repair = 13
      order_missing                  = 14
      no_modification_of_head_syst   = 15
      pgmid_object_not_allowed       = 16
      masterlanguage_not_specified   = 17
      devclass_not_specified         = 18
      specify_owner_unique           = 19
      loc_priv_objs_no_repair        = 20
      gtadir_not_reached             = 21
      object_locked_for_order        = 22
      change_of_class_not_allowed    = 23
      no_change_from_sap_to_tmp      = 24
      OTHERS                         = 25.
  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
      WHEN 11 OR 23 OR 24.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_authorized.
      WHEN 22.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>locked.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'DDIF_TABT_PUT'
    EXPORTING
      name              = l_dd_objname
      dd09L_wa          = dd09l_wa
    EXCEPTIONS
      tabt_not_found    = 1
      name_inconsistent = 2
      tabt_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>system_error.
  ENDIF.

  DATA: trobjtype  TYPE trobjtype,
        trobj_name TYPE trobj_name.
  trobjtype  = l_object.
  trobj_name = l_obj_name.
  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = trobjtype
      obj_name          = trobj_name
    EXCEPTIONS
      wrong_object_name = 1.

  name = objName.
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
  objecttype = 'TABT'.  "Table Technical Settings
endmethod.
ENDCLASS.
