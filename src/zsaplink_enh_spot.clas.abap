class ZSAPLINK_ENH_SPOT definition
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

  data BADIDEF_TOOL type ref to CL_ENH_TOOL_BADI_DEF .
  data SHORTTEXT type STRING .
  constants SHORTTEXT_ID type STRING value 'shorttext' ##NO_TEXT.
  constants SPOT_NAME_ID type STRING value 'spot_name' ##NO_TEXT.

  methods GET_BADIDEF_TOOL
    importing
      !I_LOCK type BOOLEAN optional
    raising
      ZCX_SAPLINK .
  methods GET_IMPLEMENTATIONS
    returning
      value(R_DEFINITIONS) type ENH_BADI_DATA_IT
    raising
      ZCX_SAPLINK .
ENDCLASS.



CLASS ZSAPLINK_ENH_SPOT IMPLEMENTATION.


method CHECKEXISTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  DATA cx  TYPE REF TO cx_root.

  TRY.
      me->get_badidef_tool( ).
      exists = 'X'.
    CATCH cx_root INTO cx.
  ENDTRY.

endmethod.


method CREATEIXMLDOCFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  " General DATA types
  DATA _objtype TYPE string.
  DATA rc TYPE sysubrc.
  DATA cx TYPE REF TO cx_enh_root.
  " XML related DATA types
  DATA rootnode TYPE REF TO if_ixml_element.
  " Object specific DATA types
  DATA impls      TYPE enh_badi_data_it.
  DATA impls_xml  TYPE string.
  DATA impls_ixml TYPE REF TO if_ixml_document.
  DATA impls_root TYPE REF TO if_ixml_element.
  DATA value TYPE string.
  FIELD-SYMBOLS <impl> LIKE LINE OF impls.

  " Set root object
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  rootnode->set_attribute(
    EXPORTING
      name      = me->spot_name_id " NAME
      value     = objname          " VALUE
  ).

  me->get_badidef_tool( ).

  " Read Shorttext
  me->shorttext = me->badidef_tool->if_enh_object_docu~get_shorttext( ).
  rootnode->set_attribute(
    EXPORTING
      name      = me->shorttext_id " NAME
      value     = me->shorttext       " VALUE
  ).

  " Read implementations and transform to XML
  impls = me->get_implementations( ).
  CALL TRANSFORMATION (`ID`)
    SOURCE impls = impls
    RESULT XML impls_xml.

  impls_ixml = zsaplink=>convertstringtoixmldoc( xmlstring = impls_xml ).
  impls_root = impls_ixml->get_root_element( ).

  rootnode->append_child( new_child = impls_root ).

* append root node to xmldoc
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.

endmethod.


method CREATEOBJECTFROMIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  DATA cx  TYPE REF TO cx_root.
  DATA: msg      TYPE string,
        msg_long TYPE string.

  DATA _objtype TYPE string.
  DATA rootnode TYPE REF TO if_ixml_element.
  DATA ixml TYPE REF TO if_ixml.
  DATA impls_root TYPE REF TO if_ixml_node.
  DATA impls_ixml TYPE REF TO if_ixml_document.
  DATA impls_xml  TYPE string.
  DATA impls      TYPE enh_badi_data_it.
  DATA interface_not_active TYPE boolean.

  FIELD-SYMBOLS: <impl> LIKE LINE OF impls.

  DATA: error_list TYPE REF TO cl_wb_checklist,
        error_tab  TYPE swbme_error_tab.
  FIELD-SYMBOLS: <error> LIKE LINE OF error_tab.
  FIELD-SYMBOLS: <mtext> LIKE LINE OF <error>-mtext.


  DATA:
  e_devclass               TYPE devclass,
  e_overwrite              TYPE seox_boolean.

  e_devclass = devclass.
  _objtype = getobjecttype( ).
  e_overwrite = overwrite.
  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).
  me->objname = rootnode->get_attribute( me->spot_name_id ).
  me->shorttext = rootnode->get_attribute( me->shorttext_id ).

  " check if object exists
  TRY.
      me->get_badidef_tool( 'X' ).
    CATCH cx_root INTO cx.
  ENDTRY.

  IF me->badidef_tool IS BOUND AND
     overwrite         IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>existing.
  ENDIF.
  " Read BAdI Definitionss from Slinkee
  impls_root = rootnode->get_first_child( ).

  ixml = cl_ixml=>create( ).
  impls_ixml = ixml->create_document( ).
  impls_ixml->append_child( new_child = impls_root ).

  impls_xml = zsaplink=>convertixmldoctostring( ixmldocument = impls_ixml ).

  CALL TRANSFORMATION (`ID`)
    SOURCE XML impls_xml
    RESULT impls = impls.

  " Create Implementation
  " try create
  DATA spot_name  TYPE enhspotname.
  DATA enhcomp   TYPE enhcompositename.
  DATA enhref    TYPE REF TO if_enh_spot_tool.
  " Delete existing object
  IF me->badidef_tool IS BOUND.
    me->deleteobject( ).
  ENDIF.

  spot_name = me->objname.
  " Now create the new implementation
  TRY.
      cl_enh_factory=>create_enhancement_spot(
        EXPORTING
          spot_name     = spot_name    " Name (ID) of an Enhancement Spot
          tooltype      = cl_enh_tool_badi_def=>tooltype   " Enhancement Spot Tool
          compositename = enhcomp
        IMPORTING
          spot          = enhref   " SAP Enhancement Tool Root Interface
        CHANGING
*          trkorr        = trkorr    " Request/Task
          devclass      = e_devclass
      ).
    CATCH cx_enh_root INTO cx.
      msg = cx->get_text( ).
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = msg.
  ENDTRY.

  " enhancement is of type BAdI, so convert
  me->badidef_tool ?= enhref.
  " Set Enhancement Spot Name

  me->badidef_tool->if_enh_object_docu~set_shorttext( me->shorttext ).

  LOOP AT impls ASSIGNING <impl>.
    " The implementation can only be created when the
    " implementing class is active
    CALL FUNCTION 'SEO_INTERFACE_EXISTENCE_CHECK'
      EXPORTING
        intkey        = <impl>-interface_name
      IMPORTING
        not_active    = interface_not_active
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_class      = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF interface_not_active = abap_true.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = 'Classes for Enhancement Implementations must be active'.
    ENDIF.
    TRY.
      badidef_tool->add_badi_def( <impl> ).
      CATCH cx_enh_root INTO cx.
        msg = cx->get_text( ).
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
    ENDTRY.
  ENDLOOP.

  TRY.
      " Check if the object has still errors which would avoid saving
      me->badidef_tool->if_enh_object~check(
        EXPORTING
          version                = cl_enh_tool_badi_impl=>inactive   " ABAP: Program Status (Active, Saved, Transported...)
        CHANGING
          error_list             = error_list    " List of All Error Messages from a Syntax Check
      ).
      error_list->get_error_messages(
        IMPORTING
          p_error_tab = error_tab    " Error Message Table
      ).
      IF error_tab IS NOT INITIAL.
        LOOP AT error_tab ASSIGNING <error> WHERE mtype = 'E'.
          LOOP AT <error>-mtext ASSIGNING <mtext>.
            CONCATENATE msg <mtext> ';' INTO msg.
          ENDLOOP.
        ENDLOOP.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
      ENDIF.

      " Save
      me->badidef_tool->if_enh_object~save( ).
      " Unlock
      me->badidef_tool->if_enh_object~unlock( ).
    CATCH cx_enh_root INTO cx.
      " Unlock
      me->badidef_tool->if_enh_object~unlock( ).
      msg = cx->get_text( ).
      msg_long = cx->get_longtext( ).
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = msg.
  ENDTRY.
  " successful install
  name = me->objname.

endmethod.


method DELETEOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  DATA: implementations TYPE enh_badi_data_it.
  FIELD-SYMBOLS: <impl> LIKE LINE OF implementations.
  " First delete implementation
  implementations = me->get_implementations( ).

  IF me->badidef_tool->if_enh_object~is_locked( ) NE abap_true.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'Enhancement Object is not locked'.
  ENDIF.

  " And now the Enhancement itself
  me->badidef_tool->if_enh_object~delete(
    EXPORTING
      nevertheless_delete = 'X'   " Enhancement Boolean
      run_dark            = 'X'    " Enhancement Boolean
  ).
  me->badidef_tool->if_enh_object~unlock( ).

endmethod.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  objecttype = 'ENHS'.  " Enhancement Spot

endmethod.


method GET_BADIDEF_TOOL.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  DATA cx  TYPE REF TO cx_enh_root.
  DATA msg TYPE string.

  DATA spot_name     TYPE enhspotname.
  DATA enhref        TYPE REF TO IF_ENH_SPOT_TOOL.

  spot_name = objname.

  " FREE: enhref, me->badiimpl_tool.

  TRY.
      enhref = cl_enh_factory=>get_enhancement_spot(
                 spot_name      = spot_name
                 lock           = i_lock
               ).
      " Check that is is realy a BAdI
      IF enhref->get_tool( ) <> cl_enh_tool_badi_def=>tooltype.
        CONCATENATE 'The enhancement' spot_name 'is not a BAdI Definition' INTO msg SEPARATED BY space.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
      ENDIF.

    CATCH cx_enh_root INTO cx.
      msg = cx->get_text( ).
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found
          object = objname.
  ENDTRY.

  " enhancement is of type BAdI, so convert
  me->badidef_tool ?= enhref.

endmethod.


method GET_IMPLEMENTATIONS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  r_definitions = me->badidef_tool->get_badi_defs( im_version = 'A' ).

endmethod.
ENDCLASS.
