class ZSAPLINK_MIME definition
  public
  inheriting from ZSAPLINK
  create public .

public section.
  type-pools ABAP .
  type-pools SEOP .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .
  type-pools SEOX .

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

  types:
    BEGIN OF gt_mime,
          path TYPE string,
          id TYPE sdok_docid,
          class TYPE sdok_class,
          name TYPE string,
          description TYPE string,
          type TYPE w3conttype,
          size TYPE i,
          language TYPE spras,
         END OF gt_mime .

  constants GC_DOC_CLASS_IMAGE_LOG type SDOK_CLASS value 'M_IMAGE_L' ##NO_TEXT.
  constants GC_PROP_FOLDER_ID type STRING value 'KW_PARENT_FOLDER_ID' ##NO_TEXT.
  constants GC_PROP_FOLDER_CLASS type STRING value 'KW_PARENT_FOLDER_CLASS' ##NO_TEXT.
  constants GC_PROP_URL type STRING value 'KW_RELATIVE_URL' ##NO_TEXT.
  constants GC_OBJECT_TYPE type STRING value 'SMIM' ##NO_TEXT.
  data GV_MIME_PATH type STRING .
  data GR_MIME_REPOSITORY type ref to IF_MR_API .
  data GC_DOC_CLASS_FOLDER_LOG type STRING value 'M_FOLDER' ##NO_TEXT.

  methods GET_MIME_PATH
    importing
      !IV_CLASS type SDOK_CLASS
      !IV_OBJID type SDOK_DOCID
    returning
      value(RV_PATH) type STRING
    raising
      ZCX_SAPLINK .
  methods GET_MIME_DESCRIPTION
    importing
      !IV_CLASS type SDOK_CLASS
      !IV_OBJID type SDOK_DOCID
    returning
      value(RV_DESCRIPTION) type STRING .
  methods MIME_TO_XML
    returning
      value(RR_MIME_NODE) type ref to IF_IXML_ELEMENT .
  methods XML_TO_MIME
    importing
      !IV_XML type ref to IF_IXML_DOCUMENT
      !IV_DEV_PACKAGE type DEVCLASS default '$TMP'
      !IV_OVERWRITE type FLAG
    raising
      ZCX_SAPLINK .
  methods GET_MIME_CLASS
    importing
      !IV_OBJID type SDOK_DOCID
    returning
      value(RV_CLASS) type SDOK_CLASS .
ENDCLASS.



CLASS ZSAPLINK_MIME IMPLEMENTATION.


METHOD checkexists.
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
  "
  " References
  DATA lr_mime_repository TYPE REF TO if_mr_api.
  "
  "  Variables
  DATA lv_path TYPE string.

  CLEAR exists.

  " Create MIME repository instance
  lr_mime_repository = cl_mime_repository_api=>get_api( ).

  lr_mime_repository->get( EXPORTING
                             i_url = objname
                           EXCEPTIONS
                             parameter_missing = 1
                             error_occured = 2
                             not_found = 3
                             permission_failure = 4 ).

  IF sy-subrc = 0.
    " MIME exists
    exists = abap_true.
  ELSE.
    " MIME does not exist
    exists = abap_false.
  ENDIF.
ENDMETHOD.


METHOD createixmldocfromobject.
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
  "
  " References
  DATA lr_mime_node TYPE REF TO if_ixml_element.
  "
  "
  DATA lv_return_code TYPE sysubrc.
  "
  " MIME to XML
  lr_mime_node = me->mime_to_xml( ).

  " Append MIME node to xmldoc
  lv_return_code = xmldoc->append_child( lr_mime_node ).

  " Return xml
  ixmldocument = xmldoc.
ENDMETHOD.


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

  xml_to_mime( iv_xml = ixmldocument
               iv_overwrite = overwrite ).

  name = objname.
ENDMETHOD.


METHOD deleteobject.
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
  "
  " References
  DATA lr_mime_repository TYPE REF TO if_mr_api.
  "
  " Variables
  DATA lv_textid TYPE sotr_conc.

  " Create instance of MIME repository
  lr_mime_repository = cl_mime_repository_api=>get_api( ).

  " Delete MIME object
  lr_mime_repository->delete( EXPORTING
                                i_url = objname
                                i_delete_children = 'X'
                              EXCEPTIONS
                                parameter_missing = 1
                                error_occured = 2
                                cancelled = 3
                                permission_failure = 4
                                not_found = 5 ).

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 4.
        lv_textid = zcx_saplink=>not_authorized.
      WHEN 5.
        lv_textid = zcx_saplink=>not_found.
    ENDCASE.

    RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = lv_textid.
  ENDIF.
ENDMETHOD.


METHOD getobjecttype.
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
  "
  "Return MIME object type SMIM
  objecttype = me->gc_object_type.
ENDMETHOD.


METHOD get_mime_class.
  CLEAR rv_class.
  "
  " Get mime class
  SELECT SINGLE lo_class
    INTO rv_class
    FROM smimloio
    WHERE loio_id = iv_objid.
ENDMETHOD.


METHOD get_mime_description.
  "
  " Tables
  DATA lt_object_list TYPE TABLE OF sdokobject.
  DATA lt_descriptions TYPE TABLE OF sdokdesc.
  "
  " Structures
  DATA ls_object_list TYPE sdokobject.
  DATA ls_description TYPE sdokdesc.
  "
  " Variables
  DATA lv_description TYPE string.

  CLEAR rv_description.

  ls_object_list-class = iv_class.
  ls_object_list-objid = iv_objid.

  CALL FUNCTION 'SDOK_LOIO_DESCRIPTIONS_GET'
    EXPORTING
      object_id    = ls_object_list
    TABLES
      descriptions = lt_descriptions
    exceptions
    NOT_EXISTING  Object Does Not Exist
NOT_AUTHORIZED  No Authorization  .
  "
  " Description
  READ TABLE lt_descriptions INTO ls_description WITH KEY langu = sy-langu.

  IF sy-subrc = 0.
    lv_description = ls_description-descript.
  ENDIF.

  rv_description = lv_description.
ENDMETHOD.


METHOD GET_MIME_PATH.
  "
  " Tables
  DATA lt_object_list TYPE TABLE OF sdokobject.
  DATA lt_properties TYPE TABLE OF sdokproptl.
  "
  " Structures
  DATA ls_object_list TYPE sdokobject.
  DATA ls_properties TYPE sdokproptl.
  "
  " Variables
  DATA lv_path TYPE string.

  CLEAR rv_path.

  ls_object_list-class = iv_class.
  ls_object_list-objid = iv_objid.
  APPEND ls_object_list TO lt_object_list.

  WHILE lt_object_list IS NOT INITIAL.

    CALL FUNCTION 'SDOK_LOIOS_PROPERTIES_GET'
      TABLES
        object_list        = lt_object_list
        properties         = lt_properties.


    CLEAR ls_object_list.
    REFRESH lt_object_list.
    "
    " KW_RELATIVE_URL
    READ TABLE lt_properties INTO ls_properties WITH KEY name = me->gc_prop_url.

    IF sy-subrc = 0.
      " Build path
      CONCATENATE '/'
                  ls_properties-value
                  lv_path
             INTO lv_path.
    ENDIF.

    "
    "KW_PARENT_FOLDER_CLASS
    READ TABLE lt_properties INTO ls_properties WITH KEY name = me->gc_prop_folder_class.
    ls_object_list-class = ls_properties-value.

    " KW_PARENT_FOLDER_ID
    READ TABLE lt_properties INTO ls_properties WITH KEY name = me->gc_prop_folder_id.
    ls_object_list-objid = ls_properties-value.

    IF ls_object_list-class IS NOT INITIAL AND
       ls_object_list-objid IS NOT INITIAL.
      APPEND ls_object_list TO lt_object_list.
    ENDIF.
  ENDWHILE.

  if lv_path is initial.
    "Not found
    RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_found.
  endif.

  rv_path = lv_path.
ENDMETHOD.


METHOD mime_to_xml.
  "
  " References
  DATA lr_mime_node TYPE REF TO if_ixml_element.
  DATA lr_mime_repository TYPE REF TO if_mr_api.
  DATA lr_zip TYPE REF TO cl_abap_zip.
  "
  " Structures
  DATA ls_mime TYPE gt_mime.
  DATA ls_loio TYPE skwf_io.
  "
  " Variables
  DATA lv_object_type TYPE string.
  DATA lv_return_code TYPE sysubrc.
  DATA lv_mime_xstring TYPE xstring.
  DATA lv_zip_xstring TYPE xstring.
  DATA lv_string TYPE string.
  DATA lv_loio TYPE sdok_docid.
  DATA lv_class TYPE sdok_class.

  CLEAR rr_mime_node.

  try.
      " Object type = SMIM
      lv_object_type = getobjecttype( ).

      " Node SMIM
      lr_mime_node = xmldoc->create_element( lv_object_type ).

      " Get logical id of info object
      lv_loio = objname.

      " MIME class
      lv_class = me->get_mime_class( iv_objid = lv_loio ).

      " Build path
      ls_mime-path = me->get_mime_path( iv_class = lv_class
                               iv_objid = lv_loio ).

      " Get description
      ls_mime-description = me->get_mime_description( iv_class = lv_class
                                                iv_objid = lv_loio ).

      " MIME repository reference
      lr_mime_repository = cl_mime_repository_api=>get_api( ).

      " Get properties from MIME
      CALL METHOD lr_mime_repository->properties
        EXPORTING
          i_url              = ls_mime-path
        IMPORTING
          e_name             = ls_mime-name
          e_size             = ls_mime-size
          e_loio             = ls_loio
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          not_found          = 3
          permission_failure = 4.

      ls_mime-id = ls_loio-objid.
      ls_mime-class = ls_loio-class.

      " Get file from MIME
      CALL METHOD lr_mime_repository->get
        EXPORTING
          i_url              = ls_mime-path
        IMPORTING
          e_content          = lv_mime_xstring
          e_mime_type        = ls_mime-type
        CHANGING
          c_language         = ls_mime-language
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          not_found          = 3
          permission_failure = 4.

      " Create zip file from MIME file
      CREATE OBJECT lr_zip.

      " Add MIME to zip
      lr_zip->add(
       EXPORTING name = ls_mime-name
              content = lv_mime_xstring ).

      "
      lr_zip->save( RECEIVING zip = lv_zip_xstring ).

      " Encode xstring BASE64
      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = lv_zip_xstring
        IMPORTING
          output = lv_string.

      IF sy-subrc = 0.
        " Set node attributes from structure
        setattributesfromstructure( node = lr_mime_node
                                    structure = ls_mime
                                    ).

        " Add BASE64 string to xml value
        lv_return_code = lr_mime_node->if_ixml_node~set_value( lv_string ).

        " Return xml node
        rr_mime_node = lr_mime_node.
      ENDIF.
    CATCH zcx_saplink.
    CATCH cx_root.
  ENDTRY.
ENDMETHOD.


METHOD xml_to_mime.
  "
  " References
  DATA lr_rootnode TYPE REF TO if_ixml_element.
  DATA lr_filter TYPE REF TO if_ixml_node_filter.
  DATA lr_iterator TYPE REF TO if_ixml_node_iterator.
  DATA lr_node TYPE REF TO if_ixml_element.
  DATA lr_mime_repository TYPE REF TO if_mr_api.
  DATA lr_zip TYPE REF TO cl_abap_zip.
  "
  " Structures
  DATA ls_mime TYPE gt_mime.
  "
  " Variables
  DATA lv_string TYPE string.
  DATA lv_zip_xstring TYPE xstring.
  DATA lv_mime_xstring TYPE xstring.
  DATA lv_is_existing TYPE flag.
  DATA lv_is_folder TYPE flag.

  xmldoc = iv_xml.

  lr_filter = xmldoc->create_filter_name( me->gc_object_type ).
  lr_iterator = xmldoc->create_iterator_filtered( lr_filter ).
  lr_node ?= lr_iterator->get_next( ).

  WHILE lr_node IS NOT INITIAL.
    getstructurefromattributes( EXPORTING
                                  node = lr_node
                                CHANGING
                                  structure = ls_mime ).

    lv_is_existing = checkexists( ).

    IF lv_is_existing IS NOT INITIAL.
      IF iv_overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>existing.
      ELSE.
        " Delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

    TRY.
        " MIME repository API instance
        lr_mime_repository = cl_mime_repository_api=>get_api( ).

        IF ls_mime-class = me->gc_doc_class_folder_log.
          "
          " Create folder
          lr_mime_repository->create_folder(
            i_url = ls_mime-path
            i_description = ls_mime-description
            i_language = ls_mime-language
            i_dev_package = iv_dev_package
          ).
        ELSE.
          lv_string = lr_node->get_value( ).

          CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
            EXPORTING
              input  = lv_string
            IMPORTING
              output = lv_zip_xstring.

          "
          " Unzip
          CREATE OBJECT lr_zip.

          "
          lr_zip->load( zip = lv_zip_xstring ).

          " Get MIME from Zip
          lr_zip->get( EXPORTING
                         name = ls_mime-name
                       IMPORTING
                         content = lv_mime_xstring
                                  ).
          "
          " Create MIME
          lr_mime_repository->put(
            EXPORTING
              i_url = ls_mime-path
              i_content = lv_mime_xstring
              i_description = ls_mime-description
              i_language = ls_mime-language
              i_dev_package = iv_dev_package
            EXCEPTIONS
              parameter_missing         = 1
              error_occured             = 2
              cancelled                 = 3
              permission_failure        = 4
              data_inconsistency        = 5
              new_loio_already_exists   = 6
              is_folder                 = 7
              OTHERS                    = 8
          ).
        ENDIF.
      CATCH zcx_saplink.
        RAISE EXCEPTION TYPE zcx_saplink
           EXPORTING textid = zcx_saplink=>cx_root.
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_saplink
           EXPORTING textid = zcx_saplink=>cx_root.
    ENDTRY.

    lr_node ?= lr_iterator->get_next( ).
  ENDWHILE.
ENDMETHOD.
ENDCLASS.
