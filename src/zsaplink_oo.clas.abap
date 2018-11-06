class ZSAPLINK_OO definition
  public
  inheriting from ZSAPLINK
  abstract
  create public .

public section.
  type-pools ABAP .
  type-pools SEOP .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .
  type-pools SEOX .

  constants C_XML_KEY_FRIENDS type STRING value 'friends' ##NO_TEXT.
  constants C_XML_KEY_INHERITANCE type STRING value 'inheritance' ##NO_TEXT.
  constants C_XML_KEY_SOTR type STRING value 'sotr' ##NO_TEXT.
  constants C_XML_KEY_SOTRTEXT type STRING value 'sotrText' ##NO_TEXT.
protected section.

  constants C_XML_KEY_ALIAS_METHOD type STRING value 'aliasMethod' ##NO_TEXT.
  constants C_XML_KEY_CLSDEFERRD type STRING value 'typeClasDef' ##NO_TEXT.
  constants C_XML_KEY_FORWARDDECLARATION type STRING value 'forwardDeclaration' ##NO_TEXT.
  constants C_XML_KEY_INTDEFERRD type STRING value 'typeIntfDef' ##NO_TEXT.
  constants C_XML_KEY_TYPEPUSAGE type STRING value 'typeUsage' ##NO_TEXT.

  methods CREATE_ALIAS_METHOD
    changing
      !XT_ALIASES_METHOD type SEOO_ALIASES_R .
  methods CREATE_CLSDEFERRD
    changing
      !XT_CLSDEFERRDS type SEOT_CLSDEFERRDS_R .
  methods CREATE_INTDEFERRD
    changing
      !XT_INTDEFERRDS type SEOT_INTDEFERRDS_R .
  methods CREATE_OTR
    importing
      value(NODE) type ref to IF_IXML_ELEMENT
      !DEVCLASS type DEVCLASS default '$TMP'
    exporting
      !CONCEPT type SOTR_TEXT-CONCEPT
    raising
      ZCX_SAPLINK .
  methods CREATE_TYPEPUSAGE
    changing
      !XT_TYPEPUSAGES type SEOT_TYPEPUSAGES_R .
  methods GET_ALIAS_METHOD
    importing
      !IT_METHODS type ABAP_METHDESCR_TAB
    changing
      !XO_ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_CLSDEFERRD
    changing
      !XO_ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_INTDEFERRD
    changing
      !XO_ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_OTR
    importing
      !OTRGUID type SOTR_CONC
    returning
      value(NODE) type ref to IF_IXML_ELEMENT .
  methods GET_TYPEPUSAGE
    changing
      !XO_ROOTNODE type ref to IF_IXML_ELEMENT .
private section.
ENDCLASS.



CLASS ZSAPLINK_OO IMPLEMENTATION.


method CREATE_ALIAS_METHOD.
  DATA: filter TYPE REF TO if_ixml_node_filter,
        iterator TYPE REF TO if_ixml_node_iterator,
        node TYPE REF TO if_ixml_element.

  DATA: ls_alias_method  LIKE LINE OF xt_aliases_method.


  filter = xmldoc->create_filter_name( c_xml_key_alias_method ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR ls_alias_method.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_alias_method.
    INSERT ls_alias_method INTO TABLE xt_aliases_method.
    node ?= iterator->get_next( ).
  ENDWHILE.

endmethod.


method CREATE_CLSDEFERRD.
  DATA: filter TYPE REF TO if_ixml_node_filter,
        iterator TYPE REF TO if_ixml_node_iterator,
        node TYPE REF TO if_ixml_element.

  DATA: ls_clsdeferrd  LIKE LINE OF xt_clsdeferrds.


  filter   = xmldoc->create_filter_name( c_xml_key_clsdeferrd ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_clsdeferrd.
    APPEND ls_clsdeferrd TO xt_clsdeferrds.
    node ?= iterator->get_next( ).
  ENDWHILE.

endmethod.


method CREATE_INTDEFERRD.
  DATA: filter TYPE REF TO if_ixml_node_filter,
        iterator TYPE REF TO if_ixml_node_iterator,
        node TYPE REF TO if_ixml_element.

  DATA: ls_intdeferrd  LIKE LINE OF xt_intdeferrds.


  filter   = xmldoc->create_filter_name( c_xml_key_intdeferrd ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_intdeferrd.
    APPEND ls_intdeferrd TO xt_intdeferrds.
    node ?= iterator->get_next( ).
  ENDWHILE.

endmethod.


method CREATE_OTR.
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
  DATA txtnode TYPE REF TO if_ixml_element.
  DATA filter TYPE REF TO if_ixml_node_filter.
  DATA iterator TYPE REF TO if_ixml_node_iterator.

  DATA sotrheader TYPE sotr_head.
  DATA sotrtextline TYPE sotr_text.
  DATA sotrtexttable TYPE TABLE OF sotr_text.
  DATA sotrpaket TYPE sotr_pack.

* get OTR header info
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = node
    CHANGING
      structure = sotrheader.

* get OTR text info
  filter = node->create_filter_name( c_xml_key_sotrText ).
  iterator = node->create_iterator_filtered( filter ).
  txtnode ?= iterator->get_next( ).

  WHILE txtnode IS NOT INITIAL.
    CLEAR sotrtextline.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = txtnode
      CHANGING
        structure = sotrtextline.
    CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
    APPEND sotrtextline TO sotrtexttable.
    txtnode ?= iterator->get_next( ).
  ENDWHILE.

* ewH:issue 33--> in 6.40 and above, you cannot pass a default concept
*  (otr) guid, so we will always create new
*  CALL FUNCTION 'SOTR_GET_CONCEPT'
*    EXPORTING
*      concept              = sotrHeader-concept
**   IMPORTING
**     HEADER               =
**   TABLES
**     ENTRIES              =
*   EXCEPTIONS
*     NO_ENTRY_FOUND       = 1
*     OTHERS               = 2
*            .
*  IF sy-subrc <> 1.
**   delete OTR if exists already
*    CALL FUNCTION 'SOTR_DELETE_CONCEPT'
*      EXPORTING
*        concept                     = sotrHeader-concept
*     EXCEPTIONS
*       NO_AUTHORIZATION            = 1
*       NO_ENTRY_FOUND              = 2. "who cares
**       CONCEPT_USED                = 3
**       NO_MASTER_LANGUAGE          = 4
**       NO_SOURCE_SYSTEM            = 5
**       NO_TADIR_ENTRY              = 6
**       ERROR_IN_CORRECTION         = 7
**       USER_CANCELLED              = 8
**       OTHERS                      = 9
**              .
*    if sy-subrc = 1.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>not_authorized.
*    endif.
*  ENDIF.


  DATA objecttable TYPE sotr_objects.
  DATA objecttype TYPE LINE OF sotr_objects.
* Retrieve object type of OTR
  CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
    EXPORTING
      object_vector    = sotrheader-objid_vec
    IMPORTING
      OBJECTS          = objecttable
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  READ TABLE objecttable INTO objecttype INDEX 1.

* create OTR
  sotrpaket-paket = devclass.
  CALL FUNCTION 'SOTR_CREATE_CONCEPT'
    EXPORTING
      paket                               = sotrpaket
      crea_lan                            = sotrheader-crea_lan
      alias_name                          = sotrheader-alias_name
*      CATEGORY                            =
      object                              = objecttype
      entries                             = sotrtexttable
*     FLAG_CORRECTION_ENTRY               =
*     IN_UPDATE_TASK                      =
*      CONCEPT_DEFAULT                     = sotrHeader-concept "ewH:33
    IMPORTING
      concept                             = concept         "ewH:33
    EXCEPTIONS
      package_missing                     = 1
      crea_lan_missing                    = 2
      object_missing                      = 3
      paket_does_not_exist                = 4
      alias_already_exist                 = 5
      object_type_not_found               = 6
      langu_missing                       = 7
      identical_context_not_allowed       = 8
      text_too_long                       = 9
      error_in_update                     = 10
      no_master_langu                     = 11
      error_in_concept_id                 = 12
      alias_not_allowed                   = 13
      tadir_entry_creation_failed         = 14
      internal_error                      = 15
      error_in_correction                 = 16
      user_cancelled                      = 17
      no_entry_found                      = 18
      OTHERS                              = 19
            .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

endmethod.


method CREATE_TYPEPUSAGE.
  DATA: filter   TYPE REF TO if_ixml_node_filter,
        iterator TYPE REF TO if_ixml_node_iterator,
        node     TYPE REF TO if_ixml_element,
        source   TYPE string.


  DATA: ls_typepusage  LIKE LINE OF xt_typepusages.

*rrq comments Forward nodes are created in an old version of the
*create XML from object.  In that node, the only attribute set
*is the "TypeGroup".  All other attributes are hard coded on the
*create Object from XML .  To fix this and make it transparent to
*users, "forwaredDeclaration" nodes will be supported, and a new
*node will be added.
*if it is an old version XML document, forwardDeclarations nodes
*if it is a new version XML document, typeUsages nodes

  filter   = xmldoc->create_filter_name( c_xml_key_typepusage ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_typepusage.
    APPEND ls_typepusage TO xt_typepusages.
    node ?= iterator->get_next( ).
  ENDWHILE.

* only check forwardDeclaration if typeUsages does not exist
* later version this will be removed
  IF xt_typepusages IS INITIAL.
    filter = xmldoc->create_filter_name( c_xml_key_forwarddeclaration ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CLEAR ls_typepusage.
      source = node->get_value( ).
      ls_typepusage-clsname = objname.
      ls_typepusage-version = '0'.
      ls_typepusage-tputype = '0'.
      ls_typepusage-explicit =  'X'.
      ls_typepusage-implicit = ''.
      ls_typepusage-typegroup = source.
      APPEND ls_typepusage TO xt_typepusages.
      node ?= iterator->get_next( ).
    ENDWHILE.
  ENDIF.

endmethod.


method GET_ALIAS_METHOD.
  DATA lo_alias  TYPE REF TO if_ixml_element.
  DATA ls_alias  TYPE seoaliases.
  DATA: l_rc     TYPE sy-subrc,
        ls_method LIKE LINE OF it_methods,
        ls_clsmethkey TYPE seocmpkey.

  LOOP AT it_methods INTO ls_method.
    ls_clsmethkey-clsname = objname.
    ls_clsmethkey-cmpname = ls_method-name.
    CLEAR ls_alias.
    CALL FUNCTION 'SEO_ALIAS_GET'
     EXPORTING
       cmpkey             = ls_clsmethkey
*       VERSION            = SEOC_VERSION_INACTIVE
     IMPORTING
       alias              = ls_alias
     EXCEPTIONS
       not_existing       = 1
       deleted            = 2
       OTHERS             = 3
             .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      lo_alias = xmldoc->create_element( c_xml_key_alias_method ).
      setattributesfromstructure( node      = lo_alias
                                  structure = ls_alias ).
      l_rc = xo_rootnode->append_child( lo_alias ).
    ENDIF.
  ENDLOOP.

endmethod.


method GET_CLSDEFERRD.
  DATA: lt_clsdeferrds     TYPE seot_clsdeferrds_r,
        lo_clsdeferrds     TYPE REF TO if_ixml_element,
        ls_clsdeferrd      TYPE seot_typepusage_r.

  DATA: l_rc               TYPE sy-subrc,
        ls_classkey        TYPE seoclskey.

  ls_classkey-clsname = objname.

  CALL FUNCTION 'SEO_CLSDEFERRD_READ_ALL'
    EXPORTING
      cifkey            = ls_classkey
      version           = seoc_version_active
    IMPORTING
      CLASSDEFERREDS    = lt_clsdeferrds
    EXCEPTIONS
      clif_not_existing = 1
      OTHERS            = 2.

  LOOP AT lt_clsdeferrds INTO ls_clsdeferrd.
    lo_clsdeferrds = xmldoc->create_element( c_xml_key_clsdeferrd ).
    setattributesfromstructure( node      = lo_clsdeferrds
                                structure = ls_clsdeferrd ).
    l_rc = xo_rootnode->append_child( lo_clsdeferrds ).
  ENDLOOP.
endmethod.


method GET_INTDEFERRD.
  DATA: lt_intdeferrds     TYPE seot_intdeferrds_r,
        lo_intdeferrds     TYPE REF TO if_ixml_element,
        ls_intdeferrd      TYPE seot_intdeferrd_r.

  DATA: l_rc               TYPE sy-subrc,
        ls_classkey        TYPE seoclskey.

  ls_classkey-clsname = objname.

  CALL FUNCTION 'SEO_INTDEFERRD_READ_ALL'
    EXPORTING
      cifkey             = ls_classkey
      version            = seoc_version_active
    IMPORTING
      interfacedeferreds = lt_intdeferrds
    EXCEPTIONS
      clif_not_existing  = 1
      OTHERS             = 2.

  LOOP AT lt_intdeferrds INTO ls_intdeferrd.
    lo_intdeferrds = xmldoc->create_element( c_xml_key_intdeferrd ).
    setattributesfromstructure( node      = lo_intdeferrds
                                structure = ls_intdeferrd ).
    l_rc = xo_rootnode->append_child( lo_intdeferrds ).
  ENDLOOP.

endmethod.


method GET_OTR.
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
  DATA txtnode TYPE REF TO if_ixml_element.
  DATA rc TYPE sysubrc.

  DATA sotrheader TYPE sotr_head.
  DATA sotrtextline TYPE sotr_text.
  DATA sotrtexttable TYPE TABLE OF sotr_text.

  DATA _ixml TYPE REF TO if_ixml.
  DATA _xmldoc TYPE REF TO if_ixml_document.

  CALL FUNCTION 'SOTR_GET_CONCEPT'
    EXPORTING
      concept        = otrguid
    IMPORTING
      header         = sotrheader
    TABLES
      entries        = sotrtexttable
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  sotrheader-paket = '$TMP'. "change devclass to $TMP for exports

* Create xml doc
*  _ixml = cl_ixml=>create( ).
*  _xmldoc = _ixml->create_document( ).
*  streamfactory = _ixml->create_stream_factory( ).

* Create parent node
  rootnode = xmldoc->create_element( c_xml_key_sotr ). "OTR object type
  CLEAR sotrheader-concept.                                 "ewH:33
  setattributesfromstructure( node = rootnode structure = sotrheader ).

* Create nodes for texts
  LOOP AT sotrtexttable INTO sotrtextline.
    txtnode = xmldoc->create_element( c_xml_key_sotrtext ).
    CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
    setattributesfromstructure(
      node = txtnode structure = sotrtextline ).
    rc = rootnode->append_child( txtnode ).
  ENDLOOP.

  node = rootnode.

endmethod.


method GET_TYPEPUSAGE.
  DATA: lt_typepusages     TYPE seot_typepusages_r,
        lo_typepusages     TYPE REF TO if_ixml_element,
        ls_typepusage      TYPE seot_typepusage_r.

  DATA: l_rc               TYPE sy-subrc,
        l_string           TYPE string,
        ls_classkey        TYPE seoclskey.

  ls_classkey-clsname = objname.

  CALL FUNCTION 'SEO_TYPEPUSAGE_READ_ALL'
    EXPORTING
      cifkey            = ls_classkey
      version           = seoc_version_active
    IMPORTING
      typepusages       = lt_typepusages
    EXCEPTIONS
      clif_not_existing = 1
      OTHERS            = 2.

  LOOP AT lt_typepusages INTO ls_typepusage.
    lo_typepusages = xmldoc->create_element( c_xml_key_typepusage ).
    setattributesfromstructure( node      = lo_typepusages
                                structure = ls_typepusage ).
    l_rc = xo_rootnode->append_child( lo_typepusages ).
  ENDLOOP.

*ewH: for version 0.1.3, we will continue to generate both nodes
* in order for upgradeability of saplink itself.  For version
* 2.0, forwardDeclaration node generations will be deprecated.
  LOOP AT lt_typepusages INTO ls_typepusage.
    lo_typepusages = xmldoc->create_element( c_xml_key_forwarddeclaration ).
    l_string       = ls_typepusage-typegroup.
    l_rc = lo_typepusages->if_ixml_node~set_value( l_string ).
    l_rc = xo_rootnode->append_child( lo_typepusages ).
  ENDLOOP.

endmethod.
ENDCLASS.
