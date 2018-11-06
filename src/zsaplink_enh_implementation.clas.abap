class ZSAPLINK_ENH_IMPLEMENTATION definition
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

  data SHORTTEXT type STRING .
  data SPOT_NAME type ENHSPOTNAME .
  constants ENHANCEMENT_ID type STRING value 'enhancement_id' ##NO_TEXT.
  constants SHORTTEXT_ID type STRING value 'shorttext' ##NO_TEXT.
  constants SPOT_NAME_ID type STRING value 'spot_name' ##NO_TEXT.
  data BADIIMPL_TOOL type ref to CL_ENH_TOOL_BADI_IMPL .
  data HOOKIMPL_TOOL type ref to CL_ENH_TOOL_HOOK_IMPL .
  data ENHTOOLTYPE type ENHTOOLTYPE value 'Tool Type' ##NO_TEXT.
  constants ENHTOOLTYPE_ID type STRING value 'tooltype_id' ##NO_TEXT.
  data PGMID type PGMID .
  data OBJ_NAME type TROBJ_NAME .
  data OBJ_TYPE type TROBJTYPE .
  data PROGRAM type PROGNAME .
  constants ORIG_OBJ type STRING value 'orig_object' ##NO_TEXT.

  methods GET_IMPL_TOOL
    importing
      !I_LOCK type BOOLEAN optional
    raising
      ZCX_SAPLINK .
  methods GET_IMPLEMENTATIONS
    exporting
      value(R_IMPLEMENTATIONS) type ANY
    raising
      ZCX_SAPLINK .
  methods GET_SHORTTEXT .
  methods SAVE_BADI_IMPL
    importing
      value(ENHREF) type ref to IF_ENH_TOOL
      value(BADI_IMPLS) type ENH_BADI_IMPL_DATA_IT
    raising
      ZCX_SAPLINK .
  methods SAVE_HOOK_IMPL
    importing
      value(ENHREF) type ref to IF_ENH_TOOL
      value(HOOK_IMPLS) type ENH_HOOK_IMPL_IT
    raising
      ZCX_SAPLINK .
ENDCLASS.



CLASS ZSAPLINK_ENH_IMPLEMENTATION IMPLEMENTATION.


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
      me->get_impl_tool( ).
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
  DATA badi_impls      TYPE enh_badi_impl_data_it.
  DATA hook_impls      TYPE ENH_HOOK_IMPL_IT.
  DATA impls_xml  TYPE string.
  DATA impls_ixml TYPE REF TO if_ixml_document.
  DATA impls_root TYPE REF TO if_ixml_element.
  DATA value TYPE string.
  DATA enh_include TYPE progname.
  DATA extension TYPE enhincludeextension.
  DATA enhobj    TYPE enhobj.
  FIELD-SYMBOLS <fs_impls> TYPE any.

  " Set root object
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  rootnode->set_attribute(
    EXPORTING
      name      = me->enhancement_id " NAME
      value     = objname          " VALUE
  ).

  TRY.
    me->get_impl_tool( ).
    CATCH cx_enh_root INTO cx.
  ENDTRY.
  " Read Shorttext of implementation
  me->get_shorttext( ).
  rootnode->set_attribute(
    EXPORTING
      name      = me->shorttext_id " NAME
      value     = me->shorttext       " VALUE
  ).

  " set enhancement tool type
  value = enhtooltype.
  rootnode->set_attribute(
      EXPORTING
        name      = me->enhtooltype_id
        value     = value
    ).

  if enhtooltype eq cl_enh_tool_badi_impl=>tooltype.  " BADI implementation
    " Enhancement Spot
    value = me->badiimpl_tool->get_spot_name( ).
    rootnode->set_attribute(
      EXPORTING
        name      = me->spot_name_id
        value     = value
    ).
    ASSIGN badi_impls to <fs_impls>.

  else. "hook implementation

    ASSIGN hook_impls to <fs_impls>.
    CALL METHOD me->hookimpl_tool->get_original_object
      EXPORTING
        VERSION   = 'I'
      IMPORTING
        PGMID     = me->pgmid
        OBJ_NAME  = me->obj_name
        OBJ_TYPE  = me->obj_type
        PROGRAM   = me->program.

     CONCATENATE me->pgmid    me->obj_name
                 me->obj_type me->program
     INTO value  SEPARATED BY '-'.

    rootnode->set_attribute(
      EXPORTING
        name      = me->orig_obj
        value     = value
    ).

  endif.

  " Read implementations and transform to XML
  CALL METHOD ME->GET_IMPLEMENTATIONS
    IMPORTING
      R_IMPLEMENTATIONS = <fs_impls>.

  CALL TRANSFORMATION (`ID`)
  SOURCE impls = <fs_impls>
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
  DATA: msg      TYPE string.

  DATA _objtype TYPE string.
  DATA rootnode TYPE REF TO if_ixml_element.
  DATA ixml TYPE REF TO if_ixml.
  DATA impls_root TYPE REF TO if_ixml_node.
  DATA impls_ixml TYPE REF TO if_ixml_document.
  DATA impls_xml  TYPE string.
  DATA badi_impls      TYPE enh_badi_impl_data_it.
  DATA hook_impls      TYPE ENH_HOOK_IMPL_IT.
  DATA tool_type       TYPE enhtooltype.
  DATA class_not_active TYPE boolean.
  DATA enhobj TYPE string.
  DATA enhname   TYPE enhname.
  DATA enhcomp   TYPE enhcompositename.
  DATA enhref    TYPE REF TO if_enh_tool.

  FIELD-SYMBOLS: <badiimpl> LIKE LINE OF badi_impls.
  FIELD-SYMBOLS: <hookimpl> LIKE LINE OF hook_impls.
  FIELD-SYMBOLS <fs_impls> TYPE any.

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

  me->objname = rootnode->get_attribute( me->enhancement_id ).
  me->shorttext = rootnode->get_attribute( me->shorttext_id ).
  me->spot_name = rootnode->get_attribute( me->spot_name_id ).
  me->enhtooltype = tool_type = rootnode->get_attribute( me->enhtooltype_id ).
  enhobj = rootnode->get_attribute( me->orig_obj ).

  SPLIT enhobj AT '-'
   into me->pgmid    me->obj_name
        me->obj_type me->program.

  " check if object exists
  TRY.
      me->get_impl_tool( 'X' ).
    CATCH cx_root INTO cx.
  ENDTRY.

  IF me->badiimpl_tool IS BOUND   OR
     me->hookimpl_tool IS BOUND.
   IF overwrite         IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
   ELSE.
" Delete existing object
        me->deleteobject( ).
   ENDIF.
  ENDIF.
  " Read BAdI/HOOK Implementations from Slinkee
  impls_root = rootnode->get_first_child( ).
  ixml = cl_ixml=>create( ).
  impls_ixml = ixml->create_document( ).
  impls_ixml->append_child( new_child = impls_root ).

  impls_xml = zsaplink=>convertixmldoctostring( ixmldocument = impls_ixml ).

  if tool_type eq cl_enh_tool_badi_impl=>tooltype.
     ASSIGN badi_impls to <fs_impls>.
  else.
    ASSIGN hook_impls to <fs_impls>.
  endif.

  CALL TRANSFORMATION (`ID`)
    SOURCE XML impls_xml
    RESULT impls = <fs_impls>.

  " Create Implementation
  " try create
  enhname = me->objname.
  " Now create the new implementation
  TRY.
      CALL METHOD cl_enh_factory=>create_enhancement
        EXPORTING
          enhname       = enhname
          enhtype       = cl_abstract_enh_tool_redef=>credefinition
          enhtooltype   = tool_type
          compositename = enhcomp
        IMPORTING
          enhancement   = enhref
        CHANGING
*         TRKORR        =
          devclass      = e_devclass.
    CATCH cx_enh_root INTO cx.
      msg = cx->get_text( ).
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = msg.
  ENDTRY.

  if tool_type eq cl_enh_tool_badi_impl=>tooltype.
    " enhancement is of type BAdI
    CALL METHOD ME->SAVE_BADI_IMPL
      EXPORTING
        ENHREF     = enhref
        BADI_IMPLS = <fs_impls>.
  else.
    " enhancement is of type hook
    CALL METHOD ME->SAVE_HOOK_IMPL
      EXPORTING
        ENHREF     = enhref
        HOOK_IMPLS = <fs_impls>.
  endif.
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

  DATA: badi_implementations TYPE enh_badi_impl_data_it.
  DATA: hook_implementations TYPE enh_hook_impl_it.

  if enhtooltype eq cl_enh_tool_badi_impl=>tooltype.
    IF me->badiimpl_tool->if_enh_object~is_locked( ) NE abap_true.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = 'Enhancement Object is not locked'.
    ENDIF.
    me->badiimpl_tool->if_enh_object~delete(
      EXPORTING
        nevertheless_delete = 'X'   " Enhancement Boolean
        run_dark            = 'X'    " Enhancement Boolean
    ).
    me->badiimpl_tool->if_enh_object~unlock( ).

  else.
    IF me->hookimpl_tool->if_enh_object~is_locked( ) NE abap_true.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = 'Enhancement Object is not locked'.
    ENDIF.
    me->hookimpl_tool->if_enh_object~delete(
      EXPORTING
        nevertheless_delete = 'X'   " Enhancement Boolean
        run_dark            = 'X'    " Enhancement Boolean
    ).
    me->hookimpl_tool->if_enh_object~unlock( ).
  endif.

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

  objecttype = 'ENHO'.  " Enhancement Implementation

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


  if enhtooltype eq cl_enh_tool_badi_impl=>tooltype.
    CALL METHOD me->badiimpl_tool->get_implementations
      EXPORTING
        version            = 'I'
      RECEIVING
        re_implementations = r_implementations.
  else.
    CALL METHOD me->hookimpl_tool->get_hook_impls
      EXPORTING
        version            = 'I'
      RECEIVING
        enhancements = r_implementations.
  endif.

endmethod.


method GET_IMPL_TOOL.
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

  DATA enhname   TYPE enhname.
  DATA enhspot   TYPE enhspotname.
  DATA enhref    TYPE REF TO if_enh_tool.
  DATA classname TYPE enhtoolclassname.

  enhname = objname.
  enhspot = objname.

  " FREE: enhref, me->badiimpl_tool.

  TRY.
      enhref = cl_enh_factory=>get_enhancement(
                 enhancement_id = enhname
                 lock           = i_lock
               ).

      " Check that is is realy a BAdI or Hook
      enhtooltype = enhref->get_tool( ).
      if enhtooltype ne cl_enh_tool_badi_impl=>tooltype and
         enhtooltype ne cl_enh_tool_hook_impl=>tooltype.
         CONCATENATE 'The enhancement' enhname 'is not a BAdI/Hook Implementation' INTO msg SEPARATED BY space.
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

 " convert based on type of enhancement
  if enhtooltype eq cl_enh_tool_badi_impl=>tooltype.
    me->badiimpl_tool ?= enhref.
  else.
    me->hookimpl_tool ?= enhref.
  endif.

endmethod.


method GET_SHORTTEXT.
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

  if enhtooltype eq cl_enh_tool_badi_impl=>tooltype.
    me->shorttext = me->badiimpl_tool->if_enh_object_docu~get_shorttext( ).
  else.
    me->shorttext = me->hookimpl_tool->if_enh_object_docu~get_shorttext( ).
  endif.

endmethod.


method SAVE_BADI_IMPL.
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

  DATA  class_not_active TYPE boolean.
  DATA: error_list TYPE REF TO cl_wb_checklist.
  DATA error_tab  TYPE swbme_error_tab.

  DATA  cx  TYPE REF TO cx_root.
  DATA: msg      TYPE string,
        msg_long TYPE string.

  FIELD-SYMBOLS: <badiimpl> LIKE LINE OF badi_impls.
  FIELD-SYMBOLS: <error> LIKE LINE OF error_tab.
  FIELD-SYMBOLS: <mtext> LIKE LINE OF <error>-mtext.

  me->badiimpl_tool ?= enhref.
  " Set Enhancement Spot Name
  me->badiimpl_tool->set_spot_name( spot_name = me->spot_name ).

  me->badiimpl_tool->if_enh_object_docu~set_shorttext( me->shorttext ).

  LOOP AT badi_impls ASSIGNING <badiimpl>.
    " The implementation can only be created when the
    " implementing class is active
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = <badiimpl>-impl_class
      IMPORTING
        not_active    = class_not_active
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF class_not_active = abap_true.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = 'Classes for Enhancement Implementations must be active'.
    ENDIF.
    TRY.
        CALL METHOD badiimpl_tool->add_implementation
          EXPORTING
            im_implementation = <badiimpl>.
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
      me->badiimpl_tool->if_enh_object~check(
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
      me->badiimpl_tool->if_enh_object~save( run_dark = 'X' ).
      " Unlock
      me->badiimpl_tool->if_enh_object~unlock( ).
    CATCH cx_enh_root INTO cx.
      " Unlock
      me->badiimpl_tool->if_enh_object~unlock( ).
      msg = cx->get_text( ).
      msg_long = cx->get_longtext( ).
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = msg.
  ENDTRY.

endmethod.


method SAVE_HOOK_IMPL.
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

  DATA  class_not_active TYPE boolean.

  DATA  cx  TYPE REF TO cx_root.
  DATA: msg      TYPE string,
        msg_long TYPE string.

  DATA  include TYPE progname.
  DATA  extension TYPE enhincludeextension.

  FIELD-SYMBOLS: <hookimpl> LIKE LINE OF hook_impls.

  me->hookimpl_tool ?= enhref.
  " saving the enhancement include?
  CALL METHOD me->hookimpl_tool->get_hook_impls_include
    IMPORTING
      include   = include
      extension = extension.

  if include is initial or
     extension is initial.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'Enhancement include not saved'.
  endif.

  TRY.
      " Set orig object name
      CALL METHOD ME->HOOKIMPL_TOOL->SET_ORIGINAL_OBJECT
        EXPORTING
          PGMID    = me->pgmid
          OBJ_NAME = me->obj_name
          OBJ_TYPE = me->obj_type
          PROGRAM  = me->program.

    CATCH cx_enh_root INTO cx.
      msg = cx->get_text( ).
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = msg.
  ENDTRY.

  me->hookimpl_tool->if_enh_object_docu~set_shorttext( me->shorttext ).

  LOOP AT hook_impls ASSIGNING <hookimpl>.
    TRY.
        me->hookimpl_tool->add_hook_impl(
          overwrite = <hookimpl>-overwrite
          method =    <hookimpl>-method
          enhmode =   <hookimpl>-enhmode
          full_name = <hookimpl>-full_name
          source =    <hookimpl>-source
         ).

      CATCH cx_enh_root INTO cx.
        msg = cx->get_text( ).
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
    ENDTRY.
  ENDLOOP.

  TRY.
      " Save
      me->hookimpl_tool->if_enh_object~save( run_dark = 'X' ).
      " Unlock
      me->hookimpl_tool->if_enh_object~unlock( ).
    CATCH cx_enh_root INTO cx.
      " Unlock
      me->hookimpl_tool->if_enh_object~unlock( ).
      msg = cx->get_text( ).
      msg_long = cx->get_longtext( ).
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = msg.
  ENDTRY.

endmethod.
ENDCLASS.
