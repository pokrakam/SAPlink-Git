class ZSAPLINK_SMARTFORM definition
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



CLASS ZSAPLINK_SMARTFORM IMPLEMENTATION.


METHOD checkexists .
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

  SELECT SINGLE formname FROM stxfadm INTO objname WHERE formname = objname.
  IF sy-subrc = 0.
    exists = 'X'.
  ENDIF.

ENDMETHOD.


METHOD createixmldocfromobject .
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
  DATA rootnode     TYPE REF TO if_ixml_element.
  DATA sourcenode   TYPE REF TO if_ixml_element.
  DATA rc           TYPE sysubrc.
  DATA sourcestring TYPE string.
  DATA _objtype     TYPE string.

  DATA: l_filename          TYPE string,
        l_file_filter       TYPE string,
        l_user_action       TYPE i.
  DATA: wa_node             type ssfgnode,
        l_element           TYPE REF TO if_ixml_element.
  DATA: l_language_str      TYPE string,
        l_language(2)       TYPE c.
  DATA: l_lines             TYPE i,
        l_splitted_name_tab TYPE TABLE OF string.
  DATA: l_stylename         TYPE tdssname,
        l_stylevari         TYPE tdvariant,
        l_save_style        TYPE tdssname.

  TYPES: t_raw(250) TYPE x.

  CONSTANTS: c_xml_ns_uri_sf(255)  TYPE c
      VALUE 'urn:sap-com:SmartForms:2000:internal-structure',"#EC NOTEXT
             c_xml_ns_uri_ifr(255) TYPE c
      VALUE 'urn:sap-com:sdixml-ifr:2000'.                  "#EC NOTEXT

  DATA: g_ixml               TYPE REF TO if_ixml,
        xml_macro_rc         TYPE i,
        xml_document         TYPE REF TO if_ixml_document,
        xml_ns_prefix_sf     TYPE string,
        xml_ns_uri_sf        TYPE string,
        xml_ns_uri_ifr       TYPE string,
        xml_document_size    TYPE i,
        xml_xtable           TYPE TABLE OF t_raw,
        xml_xtable2           TYPE TABLE OF string,
        sform_name           TYPE tdsfname.

  DATA ref_ssf TYPE REF TO cl_ssf_fb_smart_form.

  sform_name = objname.

  IF g_ixml IS INITIAL.
    g_ixml          = cl_ixml=>create( ).
  ENDIF.
  xml_document      = g_ixml->create_document( ).
  xml_ns_prefix_sf  = 'sf'.
  xml_ns_uri_sf     = c_xml_ns_uri_sf.
  xml_ns_uri_ifr    = c_xml_ns_uri_ifr.
  CLEAR: xml_document_size, xml_xtable[], xml_xtable2[].

  CREATE OBJECT ref_ssf.

  TRY.
      CALL METHOD ref_ssf->load
        EXPORTING
          im_formname = sform_name.

      ref_ssf->xml_init( ).

      CALL METHOD ref_ssf->xml_download
        EXPORTING
          parent   = xml_document
        CHANGING
          document = xml_document.

* namespace
      l_element  = xml_document->get_root_element( ).
      l_element->set_attribute( name      = xml_ns_prefix_sf
                                namespace = 'xmlns'
                                value     = xml_ns_uri_sf ).
      l_element->set_attribute( name  = 'xmlns'
                                value = xml_ns_uri_ifr ).


* language
      WRITE sy-langu TO l_language.
      l_language_str = l_language.
      xml_macro_rc = l_element->set_attribute(
                          name      = 'language'
                          namespace = xml_ns_prefix_sf
                          value     = l_language_str ).

* convert DOM to xml
      CALL FUNCTION 'SDIXML_DOM_TO_XML'
        EXPORTING
          document     = xml_document
        IMPORTING
          size         = xml_document_size
        TABLES
          xml_as_table = xml_xtable
        EXCEPTIONS
          OTHERS       = 1.
      CHECK sy-subrc EQ 0.

      _objtype = getobjecttype( ).
      rootnode = xmldoc->create_element( _objtype ).

      DATA: wa_stxfadm TYPE stxfadm.

      SELECT SINGLE * FROM stxfadm INTO wa_stxfadm WHERE formname = objname.

      setattributesfromstructure( node = rootnode structure =  wa_stxfadm
      ).
      sourcenode = xmldoc->create_element( 'smartform' ).

      xml_xtable2 = xml_xtable[].

      sourcestring = buildsourcestring( sourcetable = xml_xtable2[] ).

      rc = sourcenode->if_ixml_node~set_value( sourcestring ).
      rc = rootnode->append_child( sourcenode ).
      rc = xmldoc->append_child( rootnode ).
      ixmldocument = xmldoc.

      FREE: xml_document, xml_xtable[], xml_document_size.


    CATCH cx_ssf_fb .
      CLEAR ixmldocument.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>not_found.
  ENDTRY.

ENDMETHOD.


METHOD createobjectfromixmldoc .
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

  TYPES: t_raw(250)     TYPE x.

  DATA rootnode         TYPE REF TO if_ixml_element.
  DATA progattribs      TYPE trdir.
  DATA sourcenode       TYPE REF TO if_ixml_element.
  DATA l_xml_node       TYPE REF TO if_ixml_element.
  DATA source           TYPE string.
  DATA sourcetable      TYPE table_of_strings.
  DATA _objtype         TYPE string.
  DATA checkexists      TYPE flag.

  DATA: wa_stxfadm      TYPE stxfadm,
        formname        TYPE tdsfname,
        master_language TYPE sylangu,
        lv_devclass     TYPE devclass,
        korrnum         TYPE trkorr,
        modif_language  TYPE sylangu.

  DATA: g_ixml               TYPE REF TO if_ixml,
        xml_macro_rc         TYPE i,
        xml_document         TYPE REF TO if_ixml_document,
        l_element            TYPE REF TO if_ixml_element,
        xml_ns_prefix_sf     TYPE string,
        xml_ns_uri_sf        TYPE string,
        xml_ns_uri_ifr       TYPE string,
        xml_document_size    TYPE i,
        xml_xtable           TYPE TABLE OF t_raw,
        l_ns_uri             TYPE string,
        l_name               TYPE string,
        l_language           TYPE string,
        p_dequeue            TYPE tdbool,
        l_cancel             TYPE tdsfflag,
        sf_exception         TYPE REF TO cx_ssf_fb.

  DATA: ref_ssf TYPE REF TO cl_ssf_fb_smart_form,
        l_upload_smartform TYPE REF TO cl_ssf_fb_smart_form.


  _objtype = getobjecttype( ).
  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = wa_stxfadm.
  objname = wa_stxfadm-formname.

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

  sourcenode = rootnode->find_from_name( 'smartform' ).
  source = sourcenode->get_value( ).
  sourcetable = buildtablefromstring( source ).

  xml_xtable = sourcetable.
  xml_document_size = STRLEN( source ).

  CREATE OBJECT ref_ssf.

  formname = objname.

* Check access permission and enqueue smart form
  master_language = sy-langu.
  TRY.
      CALL METHOD ref_ssf->enqueue
        EXPORTING
          suppress_corr_check   = space
          language_upd_exit     = ' '
          master_language       = master_language
          mode                  = 'INSERT'
          formname              = formname
        IMPORTING
          devclass              = lv_devclass
          new_master_language   = master_language
          korrnum               = korrnum
          modification_language = modif_language.
    CATCH cx_ssf_fb INTO sf_exception.
      CASE sf_exception->textid.
        WHEN cx_ssf_fb=>enqueued_by_user OR cx_ssf_fb=>enqueue_system_failure.
          RAISE EXCEPTION TYPE zcx_saplink
               EXPORTING msg = 'Enqueued by user'.
        WHEN cx_ssf_fb=>no_modify_permission OR cx_ssf_fb=>no_show_permission.
          RAISE EXCEPTION TYPE zcx_saplink
               EXPORTING msg = 'Permission Error'.
        WHEN cx_ssf_fb=>permission_failure.
          EXIT.
        WHEN cx_ssf_fb=>request_language_denied.
          RAISE EXCEPTION TYPE zcx_saplink
               EXPORTING msg = 'Language request denied'.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
  ENDTRY.

  CALL FUNCTION 'SDIXML_XML_TO_DOM'
    EXPORTING
      xml      = xml_xtable[]
      size     = xml_document_size
    IMPORTING
      document = xml_document
    EXCEPTIONS
      OTHERS   = 1.

  l_xml_node  = xml_document->get_root_element( ).
  l_ns_uri    = l_xml_node->get_namespace_uri( ).
  l_name      = l_xml_node->get_name( ).
  l_element  ?= l_xml_node->query_interface( ixml_iid_element ).
  l_language  = l_element->get_attribute( name = 'language'
                                          namespace = xml_ns_prefix_sf ).

  CREATE OBJECT l_upload_smartform.
  CALL METHOD l_upload_smartform->xml_upload
    EXPORTING
      dom      = l_xml_node
      formname = formname
      language = master_language
    CHANGING
      sform    = ref_ssf.
  ref_ssf = l_upload_smartform.

  PERFORM save_form IN PROGRAM saplstxb
              USING
                 ' ' 'X'
              CHANGING
                 ref_ssf
                 l_cancel.

  FREE: xml_document.

* dequeue form
  ref_ssf->dequeue( formname = formname ).

* successful install
  name = objname.

ENDMETHOD.


METHOD deleteobject .
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
    CALL FUNCTION 'FB_DELETE_FORM'
     EXPORTING
       I_FORMNAME                  = OBJNAME
*       I_FORMTYPE                  = ' '
*       I_WITH_DIALOG               = 'X'
*       I_WITH_CONFIRM_DIALOG       = 'X'
*     IMPORTING
*       O_FORMNAME                  =
     EXCEPTIONS
       NO_NAME                     = 1
       NO_FORM                     = 2
       FORM_LOCKED                 = 3
       NO_ACCESS_PERMISSION        = 4
       ILLEGAL_LANGUAGE            = 5
       ILLEGAL_FORMTYPE            = 6
       OTHERS                      = 7
              .
    IF sy-subrc <> 0.

    ENDIF.

  ENDMETHOD.                    "createobjectfromixmldoc


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
  objectType = 'SSFO'. "SAP Smartforms
endmethod.
ENDCLASS.
