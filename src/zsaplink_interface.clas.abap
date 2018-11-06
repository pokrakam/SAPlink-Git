class ZSAPLINK_INTERFACE definition
  public
  inheriting from ZSAPLINK
  create public .

public section.
  type-pools ABAP .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .

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

  constants XML_KEY_METHOD type STRING value 'method' ##NO_TEXT.
  constants XML_KEY_PARAMETER type STRING value 'parameter' ##NO_TEXT.
  constants XML_KEY_EXCEPTION type STRING value 'exception' ##NO_TEXT.
  constants XML_KEY_EVENTS type STRING value 'events' ##NO_TEXT.
  constants XML_KEY_ATTRIBUTE type STRING value 'attribute' ##NO_TEXT.
  constants XML_KEY_TYPEUSAGE type STRING value 'typeUsage' ##NO_TEXT.
  constants XML_KEY_TYPES type STRING value 'types' ##NO_TEXT.
  constants XML_KEY_INCLUDE type STRING value 'include' ##NO_TEXT.

  methods EXPORT_INTERFACES
    importing
      !CLASSKEY type SEOCLSKEY
    exporting
      !RC type SYSUBRC
    changing
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods IMPORT_METHODS
    changing
      !CH_METHODS type SEOO_METHODS_R
      !CH_PARAMETERS type SEOS_PARAMETERS_R
      !CH_EXCEPS type SEOS_EXCEPTIONS_R .
  methods IMPORT_ATTRIBUTES
    changing
      !CH_ATTRIBUTES type SEOO_ATTRIBUTES_R
    raising
      ZCX_SAPLINK .
  methods IMPORT_INTERFACES
    changing
      !CH_COMPRISINGS type SEOR_COMPRISINGS_R .
  methods IMPORT_TYPES
    changing
      !CH_TYPEUSAGES type SEOT_TYPEPUSAGES_R
      !CH_TYPES type SEOO_TYPES_R .
  methods IMPORT_EVENTS
    changing
      !CH_EVENTS type SEOO_EVENTS_R
      !CH_PARAMETERS type SEOS_PARAMETERS_R .
  methods EXPORT_METHODS
    importing
      !INTFDESCR type ref to CL_ABAP_INTFDESCR
      !CLASSNAME type SEOCLSNAME
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods EXPORT_EVENTS
    importing
      !CLASSKEY type SEOCLSKEY
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods EXPORT_ATTRIBUTES
    importing
      !INTFDESCR type ref to CL_ABAP_INTFDESCR
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods EXPORT_TYPES
    importing
      !CLASSKEY type SEOCLSKEY
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods EXPORT_TYPEGROUPS
    importing
      !CLASSKEY type SEOCLSKEY
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
ENDCLASS.



CLASS ZSAPLINK_INTERFACE IMPLEMENTATION.


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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

data intkey type SEOCLSKEY.
data not_active TYPE  char1.

  intkey-clsName = objname.

  CALL FUNCTION 'SEO_INTERFACE_EXISTENCE_CHECK'
    EXPORTING
      intkey              = intkey
    IMPORTING
      not_active    = not_active
    EXCEPTIONS
*     NOT_SPECIFIED       = 1
     NOT_EXISTING        = 2
*     IS_CLASS            = 3
*     NO_TEXT             = 4
*     INCONSISTENT        = 5
*     OTHERS              = 6
            .
  IF sy-subrc <> 2.
    exists = 'X'.
  endif.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data intfsection type ref to if_ixml_element.
  data rootnode type ref to if_ixml_element.
  data _classname type seoclsname.
  data rc type sysubrc.
  data intfdescr type ref to cl_abap_intfdescr.
  data typedescr type ref to cl_abap_typedescr.
  data classkey type seoclskey.
  data intproperties type vseointerf.
  data _objtype type string.

  _classname = objname.
  classkey-clsname = objname.

  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  call function 'SEO_INTERFACE_GET'
    EXPORTING
      intkey       = classkey
      version      = '1'
    IMPORTING
      interface    = intproperties
    EXCEPTIONS
      not_existing = 1
      deleted      = 2
      is_class     = 3
      model_only   = 4
      others       = 5.

  if sy-subrc <> 0.
    case sy-subrc.
      when 1.
        raise exception type zcx_SAPlink
          exporting textid = zcx_SAPlink=>not_found.
      when 2.
        raise exception type zcx_SAPlink
          exporting
            textid = zcx_SAPlink=>error_message
            msg = 'interface deleted'.
      when 3.
        raise exception type zcx_SAPlink
          exporting
            textid = zcx_SAPlink=>error_message
            msg = 'classes not supported'.
      when 4.
        raise exception type zcx_SAPlink
          exporting
            textid = zcx_SAPlink=>error_message
            msg = 'interface is modeled only'.
    endcase.
  endif.

  setattributesfromstructure( node = rootnode
                         structure = intproperties ).

  try.
      call method cl_abap_intfdescr=>describe_by_name
        EXPORTING
          p_name         = objname
        RECEIVING
          p_descr_ref    = typedescr
        EXCEPTIONS
          type_not_found = 1.
      if sy-subrc = 0.
        intfdescr ?= typedescr.
      endif.
    catch cx_root.
      raise exception type zcx_SAPlink
        exporting textid = zcx_SAPlink=>system_error.
  endtry.

*Add included interfaces to the xml document
  CALL METHOD ME->EXPORT_INTERFACES
    EXPORTING
      CLASSKEY = classkey
    IMPORTING
      RC       = rc
    CHANGING
      ROOTNODE = rootnode.

*Add types to the xml document
  CALL METHOD me->EXPORT_TYPES
    EXPORTING
      CLASSKEY = classkey
    CHANGING
      RC       = rc
      ROOTNODE = rootnode.

*Add typegroups to the xml document
  CALL METHOD me->EXPORT_TYPEGROUPS
    EXPORTING
      CLASSKEY = classkey
    CHANGING
      RC       = rc
      ROOTNODE = rootnode.

*Add events to the xml document
  CALL METHOD me->EXPORT_EVENTS
    EXPORTING
      CLASSKEY = classkey
    CHANGING
      RC       = rc
      ROOTNODE = rootnode.

*Add attributes to the xml document
  CALL METHOD me->EXPORT_ATTRIBUTES
    EXPORTING
      INTFDESCR = intfdescr
    CHANGING
      RC        = rc
      ROOTNODE  = rootnode.

*Add methods to the xml document
  CALL METHOD me->EXPORT_METHODS
    EXPORTING
      INTFDESCR = intfdescr
      CLASSNAME = _CLASSNAME
    CHANGING
      RC        = rc
      ROOTNODE  = rootnode.

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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data rootnode type ref to if_ixml_element.
  data classkey type seoclskey.
  data not_active type boolean.
  data _devclass type devclass.
  data _objtype type string.
  data checkexists type flag.

  data: e_corrnr                 type TRKORR,
        e_devclass               type DEVCLASS,
        e_version                type SEOVERSION,
        e_genflag                type GENFLAG,
        e_authority_check        type SEOX_BOOLEAN,
        e_overwrite              type SEOX_BOOLEAN.

  data: i_korrnr type trkorr.

  data: ch_interface type VSEOINTERF,
        ch_comprisings type SEOR_COMPRISINGS_R,
        ch_attributes type SEOO_ATTRIBUTES_R,
        ch_methods type SEOO_METHODS_R,
        ch_events type SEOO_EVENTS_R,
        ch_parameters type SEOS_PARAMETERS_R,
        ch_exceps type SEOS_EXCEPTIONS_R,
        ch_typeusages type SEOT_TYPEPUSAGES_R,
        ch_types type SEOO_TYPES_R.

  call function 'SEO_BUFFER_INIT'.

  e_devclass = devclass.
  _objtype = getobjecttype( ).
  e_overwrite = overwrite.

  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  call method getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = ch_interface.

  checkexists = checkexists( ).
  if checkexists is not initial.
    if overwrite is initial.
      raise exception type zcx_SAPlink
        exporting textid = zcx_SAPlink=>existing.
    else.
*     delete object for new install
      deleteobject( ).
    endif.
  endif.

  if sy-subrc <> 0.
    case sy-subrc.
      when 1.
        raise exception type zcx_SAPlink
          exporting textid = zcx_SAPlink=>not_authorized.
      when others.
        raise exception type zcx_SAPlink
          exporting textid = zcx_SAPlink=>system_error.
    endcase.
  endif.

*Add attributes to new interface
  call method import_attributes
    CHANGING
      ch_attributes = ch_attributes.

*Add includes
  call method import_interfaces
    CHANGING
      ch_comprisings = ch_comprisings.

*Add types and type groups
  call method import_types
    CHANGING
      ch_types      = ch_types
      ch_typeusages = ch_typeusages.

*Add events and event parameters
  call method import_events
    CHANGING
      ch_events     = ch_events
      ch_parameters = ch_parameters.

*Add methods, method parameters and method exceptions
  call method import_methods
    CHANGING
      ch_methods    = ch_methods
      ch_parameters = ch_parameters
      ch_exceps     = ch_exceps.

*Create the interface
  CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
   EXPORTING
    CORRNR                             = e_corrnr
    DEVCLASS                           = e_devclass
    VERSION                            = e_version
    GENFLAG                            = e_genflag
    AUTHORITY_CHECK                    = e_authority_check
    OVERWRITE                          = e_overwrite
*     SUPPRESS_REFACTORING_SUPPORT       = SEOX_TRUE
   IMPORTING
    KORRNR                             = i_korrnr
* TABLES
*   CLASS_DESCRIPTIONS                 =
*   COMPONENT_DESCRIPTIONS             =
*   SUBCOMPONENT_DESCRIPTIONS          =
   CHANGING
    INTERFACE                          = ch_interface
    COMPRISINGS                        = ch_comprisings
    ATTRIBUTES                         = ch_attributes
    METHODS                            = ch_methods
    EVENTS                             = ch_events
    PARAMETERS                         = ch_parameters
    EXCEPS                             = ch_exceps
*   ALIASES                            =
    TYPEPUSAGES                        = ch_typeusages
*   CLSDEFERRDS                        =
*   INTDEFERRDS                        =
    TYPES                              = ch_types
   EXCEPTIONS
    EXISTING                           = 1
    IS_CLASS                           = 2
    DB_ERROR                           = 3
    COMPONENT_ERROR                    = 4
    NO_ACCESS                          = 5
    OTHER                              = 6
    OTHERS                             = 7.

  case sy-subrc.
    when '0'.
** i guess if we made it this far, we will assume
** successful install
      name = objname.
    when '1'.
      RAISE EXCEPTION TYPE zcx_SAPlink
        EXPORTING textid = zcx_SAPlink=>existing.
    when others.
      raise exception type zcx_SAPlink
        exporting textid = zcx_SAPlink=>system_error.
  endcase.

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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  DATA clskey TYPE seoclskey.
  clskey-clsname = objname.

  CALL FUNCTION 'SEO_INTERFACE_GET'
    EXPORTING
      INTKEY       = clskey
      VERSION      = SEOC_VERSION_INACTIVE
      STATE        = '0'
    EXCEPTIONS
      NOT_EXISTING = 1
      DELETED      = 2
      IS_CLASS     = 3
      MODEL_ONLY   = 4
      OTHERS       = 5.

  IF SY-SUBRC <> 0.
    RAISE EXCEPTION TYPE zcx_SAPlink
      EXPORTING
        textid = zcx_SAPlink=>error_message
        msg = 'interface not deleted'.
  ENDIF.

  CALL FUNCTION 'SEO_INTERFACE_DELETE_W_DEPS'
    EXPORTING
      intkey       = clskey
      save         = ' '
    EXCEPTIONS
      not_existing = 1
      is_class     = 2
      not_deleted  = 3
      db_error     = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_SAPlink
          EXPORTING textid = zcx_SAPlink=>not_found.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_SAPlink
          EXPORTING
            textid = zcx_SAPlink=>error_message
            msg = 'class not supported'.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_SAPlink
          EXPORTING
            textid = zcx_SAPlink=>error_message
            msg = 'interface not deleted'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_SAPlink
          EXPORTING textid = zcx_SAPlink=>system_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
    EXPORTING
      CIFKEY        = clskey
*      CHANGING
*        CORRNR        = corrnr
    EXCEPTIONS
      NOT_EXISTING  = 1
      NOTHING_TO_DO = 2
      ACCESS_ERROR  = 3
      DB_ERROR      = 4
      OTHERS        = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

endmethod.


method EXPORT_ATTRIBUTES.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: attribkey type seocmpkey,
        attribdescr type abap_attrdescr,
        attribnode type ref to if_ixml_element,
        attribproperties type vseoattrib,
        _otrguid type sotr_conc,
        otrnode type ref to if_ixml_element.

  attribkey-clsname = objname.
  loop at intfdescr->attributes into attribdescr where is_inherited =
  abap_false.
    attribnode = xmldoc->create_element( XML_KEY_ATTRIBUTE ).
    attribkey-cmpname = attribdescr-name.
    call function 'SEO_ATTRIBUTE_GET'
      EXPORTING
        attkey    = attribkey
      IMPORTING
        attribute = attribproperties.

*   include OTR if necessary (for exception classes)
    if attribproperties-type = 'SOTR_CONC' and attribproperties-attvalue
    is not initial.
      _otrguid = attribproperties-attvalue+1(32).
      otrnode = createnodefromotr( _otrguid ).
      if otrnode is bound.
        rc = attribnode->append_child( otrnode ).
      endif.
    endif.

*   append attribute node to parent node
    setattributesfromstructure( node = attribnode structure =
    attribproperties ).
    rc = rootnode->append_child( attribnode ).
  endloop.

endmethod.


method EXPORT_EVENTS.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  DATA: events      TYPE SEOO_EVENTS_R,
        wa_event    LIKE LINE OF events,
        eventkey    type SEOCMPKEY,
        eventparams type seos_parameters_r,
        wa_params   type seos_parameter_r,
        event_node  TYPE REF TO if_ixml_element,
        parameternode type ref to if_ixml_element.

  CALL FUNCTION 'SEO_EVENT_READ_ALL'
    EXPORTING
      cifkey            = classkey
      version           = 1
    IMPORTING
      events            = events
    EXCEPTIONS
      clif_not_existing = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT events INTO wa_event.
    eventkey-clsname = wa_event-clsname.
    eventkey-cmpname = wa_event-CMPNAME.
    event_node = xmldoc->create_element( XML_KEY_EVENTS ).
    setattributesfromstructure( node = event_node structure =
    wa_event ).
    CALL FUNCTION 'SEO_EVENT_SIGNATURE_GET'
      EXPORTING
        EVTKEY     = eventkey
      IMPORTING
        PARAMETERS = eventparams.
*   event parameters
    loop at eventParams into wa_params.

      parameternode = xmldoc->create_element( XML_KEY_PARAMETER ).
      setattributesfromstructure( node = parameternode
      structure = wa_params ).
      rc = event_node->append_child( parameternode ).
    ENDLOOP.
    rc = rootnode->append_child( event_node ).
  ENDLOOP.

endmethod.


method EXPORT_INTERFACES.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  DATA: it_vseocompri type table of VSEOCOMPRI,
        wa_vseocompri like line of it_vseocompri,
        implementingNode TYPE REF TO if_ixml_element,
        objname type VRSD-OBJNAME.

  objname = classkey.

  CALL FUNCTION 'SVRS_GET_VERSION_INTF_40'
    EXPORTING
*   DESTINATION                        =
      OBJECT_NAME                        = objname
      VERSNO                             = '00000'
*   IV_NO_RELEASE_TRANSFORMATION       =
* IMPORTING
*   INFO_LINE                          =
  TABLES
*   VSMODISRC                          =
*   PSEOALIASES                        =
*   PVSEOATTRIB                        =
      PVSEOCOMPRI                        = it_vseocompri
*   PVSEOEVENT                         =
*   PVSEOEXCEP                         =
*   PVSEOINTERF                        =
*   PSMODILOG                          =
*   PVSEOMETHOD                        =
*   PVSEOPARAM                         =
*   PPOOL_SOURCE                       =
*   PSOURCE                            =
*   PTRDIR                             =
*   TYPE_TAB                           =
*   PSEOTYPEPLS                        =
  EXCEPTIONS
    NO_VERSION                         = 1
    SYSTEM_FAILURE                     = 2
    COMMUNICATION_FAILURE              = 3
    OTHERS                             = 4
            .
  IF SY-SUBRC <> 0.
  ENDIF.

  LOOP AT it_vseocompri INTO wa_vseocompri.
    implementingNode = xmldoc->create_element( XML_KEY_INCLUDE ).
    setattributesfromstructure( node = implementingNode structure =
    wa_vseocompri ).
    rc = rootnode->append_child( implementingNode ).
  ENDLOOP.

endmethod.


method EXPORT_METHODS.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: methoddescr type abap_methdescr,
        methodkey type seocpdkey,
        clsmethkey type seocmpkey,
        methodproperties type vseomethod,
        paramdescr type abap_parmdescr,
        paramproperties type vseoparam,
        paramkey type seoscokey,
        exceptionlist type seos_exceptions_r,
        anexception type vseoexcep,
        exceptionnode type ref to if_ixml_element,
        parameternode type ref to if_ixml_element,
        methodnode type ref to if_ixml_element.

  loop at intfdescr->methods into methoddescr where
  not ( is_inherited = 'X' and is_redefined is initial ).
    methodkey-clsname = classname.
    methodkey-cpdname = methoddescr-name.

    clsmethkey-clsname = classname.
    clsmethkey-cmpname = methoddescr-name.
    clear methodproperties.

    call function 'SEO_METHOD_GET'
      EXPORTING
        mtdkey       = clsmethkey
      IMPORTING
        method       = methodproperties
      EXCEPTIONS
        not_existing = 1.
    if sy-subrc = 0.
      methodnode = xmldoc->create_element( XML_KEY_METHOD ).
      setattributesfromstructure( node = methodnode structure =
      methodproperties ).

*add method parameters only when not an alias
      if methoddescr-alias_for eq ''.

        loop at methoddescr-parameters into paramdescr.
          clear paramproperties.
          parameternode = xmldoc->create_element( XML_KEY_PARAMETER ).
          paramkey-cmpname = clsmethkey-cmpname.
          paramkey-sconame = paramdescr-name.
          paramkey-clsname = objname.
          call function 'SEO_PARAMETER_GET'
            EXPORTING
              parkey    = paramkey
              version   = '1'
            IMPORTING
              parameter = paramproperties.
          setattributesfromstructure( node = parameternode
          structure = paramproperties ).
          rc = methodnode->append_child( parameternode ).
        endloop.

      endif.

*add method exceptions
      call function 'SEO_METHOD_SIGNATURE_GET'
        EXPORTING
          mtdkey  = clsmethkey
          version = '1'
        IMPORTING
          exceps  = exceptionlist.
      loop at exceptionlist into anexception.
        exceptionnode = xmldoc->create_element( XML_KEY_EXCEPTION ).
        setattributesfromstructure( node = exceptionnode
        structure = anexception ).
        rc = methodnode->append_child( exceptionnode ).
      endloop.
    endif. "method found
    rc = rootnode->append_child( methodnode ).
  endloop.

endmethod.


method EXPORT_TYPEGROUPS.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: forwarddeclarationlist type seot_typepusages_r,
        forwarddeclaration type ref to if_ixml_element,
        forwarddeclarationrow type seot_typepusage_r.

  call function 'SEO_TYPEPUSAGE_READ_ALL'
    EXPORTING
      cifkey      = classkey
      version     = '1'
    IMPORTING
      typepusages = forwarddeclarationlist.

*comment rrq old way....forwardDeclarations the only attribute
*set was the "TypeGroup"
*the new way....the entire structure is passed as in node typeUsage
  LOOP AT forwarddeclarationlist INTO forwarddeclarationrow.
    forwarddeclaration = xmldoc->create_element( XML_KEY_TYPEUSAGE ).
    setattributesfromstructure( node = forwarddeclaration structure =
    forwarddeclarationrow ).
    rc = rootnode->append_child( forwarddeclaration ).
  ENDLOOP.

endmethod.


method EXPORT_TYPES.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: types      type seoo_types_r,
      wa_type    like line of types,
      types_node type ref to if_ixml_element.

  call function 'SEO_TYPE_READ_ALL'
    EXPORTING
      cifkey            = classkey
      version           = 1
    IMPORTING
      types             = types
    EXCEPTIONS
      clif_not_existing = 1
      others            = 2.

  if sy-subrc <> 0.
  endif.

  loop at types into wa_type.
    types_node = xmldoc->create_element( XML_KEY_TYPES ).
    setattributesfromstructure( node = types_node structure =
    wa_type ).
    rc = rootnode->append_child( types_node ).
  endloop.

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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  objecttype = 'INTF'.  "Interface

endmethod.


method IMPORT_ATTRIBUTES.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: otrConcept type SOTR_TEXT-CONCEPT,
        wa_attributes like line of ch_attributes,
        filter type ref to if_ixml_node_filter,
        iterator type ref to if_ixml_node_iterator,
        node type ref to if_ixml_element,
        otrnode type ref to if_ixml_element.

  filter = xmldoc->create_filter_name( XML_KEY_ATTRIBUTE ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
*   create OTR texts if necessary (for exception classes)
    clear otrConcept.
    otrnode = node->find_from_name( 'sotr' ).
    IF otrnode IS NOT INITIAL.
      me->createotrfromnode(
        exporting node = otrnode
        importing concept = otrConcept ).
    ENDIF.
    clear wa_attributes.
*   create attribute
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_attributes.
    wa_attributes-version = '0'.
*   ewH:issue33-->6.40 and above, must create new concept
    if otrConcept is not initial.
      concatenate `'` otrConcept `'` into wa_attributes-attvalue.
    endif.
    append wa_attributes to ch_attributes.
    node ?= iterator->get_next( ).
  ENDWHILE.

endmethod.


method IMPORT_EVENTS.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: event_filter type ref to if_ixml_node_filter,
        parameter_filter type ref to if_ixml_node_filter,
        event_iterator type ref to if_ixml_node_iterator,
        parameter_iterator type ref to if_ixml_node_iterator,
        event_node type ref to if_ixml_element,
        parameter_node type ref to if_ixml_element,
        wa_events like line of ch_events,
        wa_parameters like line of ch_parameters.

  event_filter = xmldoc->create_filter_name( XML_KEY_EVENTS ).
  event_iterator = xmldoc->create_iterator_filtered( event_filter ).
  event_node ?= event_iterator->get_next( ).
  WHILE event_node IS NOT INITIAL.
    CLEAR wa_events.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = event_node
      CHANGING
        structure = wa_events.
    append wa_events to ch_events.
    parameter_filter = event_node->create_filter_name( XML_KEY_PARAMETER ).
    parameter_iterator = event_node->create_iterator_filtered( parameter_filter ).
    parameter_node ?= parameter_iterator->get_next( ).
    WHILE parameter_node IS NOT INITIAL.
      CLEAR wa_parameters.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = parameter_node
        CHANGING
          structure = wa_parameters.
      append wa_parameters to ch_parameters.
      parameter_node ?= parameter_iterator->get_next( ).
    endwhile.
    event_node ?= event_iterator->get_next( ).
  ENDWHILE.

endmethod.


method IMPORT_INTERFACES.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: filter type ref to if_ixml_node_filter,
        iterator type ref to if_ixml_node_iterator,
        node type ref to if_ixml_element,
        wa_comprisings like line of ch_comprisings.

  filter = xmldoc->create_filter_name( XML_KEY_INCLUDE ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR wa_comprisings.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_comprisings.
    wa_comprisings-version = '0'.
    append wa_comprisings to ch_comprisings.
    node ?= iterator->get_next( ).
  ENDWHILE.

endmethod.


method IMPORT_METHODS.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: filter type ref to if_ixml_node_filter,
        filter2 type ref to if_ixml_node_filter,
        iterator type ref to if_ixml_node_iterator,
        iterator2 type ref to if_ixml_node_iterator,
        node type ref to if_ixml_element,
        node2 type ref to if_ixml_element,
        wa_parameters like line of ch_parameters,
        wa_methods like line of ch_methods,
        wa_exceps like line of ch_exceps.

*Add methods to new interface
  filter = xmldoc->create_filter_name( XML_KEY_METHOD ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR wa_methods.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_methods.

*Add parameters
    filter2 = node->create_filter_name( XML_KEY_PARAMETER ).
    iterator2 = node->create_iterator_filtered( filter2 ).
    node2 ?= iterator2->get_next( ).
    WHILE node2 IS NOT INITIAL.
      CLEAR wa_parameters.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node2
        CHANGING
          structure = wa_parameters.
      append wa_parameters to ch_parameters.
      node2 ?= iterator2->get_next( ).
    ENDWHILE.
*Add exceptions
    filter2 = node->create_filter_name( XML_KEY_EXCEPTION ).
    iterator2 = node->create_iterator_filtered( filter2 ).
    node2 ?= iterator2->get_next( ).
    WHILE node2 IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node2
        CHANGING
          structure = wa_exceps.
      append wa_exceps to ch_exceps.
      node2 ?= iterator2->get_next( ).
    ENDWHILE.
    append wa_methods to ch_methods.
    node ?= iterator->get_next( ).
  endwhile.

endmethod.


method IMPORT_TYPES.
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

*Plugin created by Thomas Ritter
*Based on work done by: Michael Yakovlev, Edward Herrmann
*and other SAPlink contributors

  data: filter type ref to if_ixml_node_filter,
        iterator type ref to if_ixml_node_iterator,
        node type ref to if_ixml_element,
        wa_types like line of ch_types,
        wa_typeusages like line of ch_typeusages.

  filter = xmldoc->create_filter_name( XML_KEY_TYPES ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR wa_types.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_types.
    wa_types-version = '0'.
    append wa_types to ch_types.
    node ?= iterator->get_next( ).
  ENDWHILE.

*ewH: for version 0.1.3, we will continue to generate both nodes
* in order for upgradeability of SAPlink itself.  For version
* 2.0, forwardDeclaration node generations will be deprecated.
  filter = xmldoc->create_filter_name( XML_KEY_TYPEUSAGE ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_typeUsages.
    APPEND wa_typeusages TO ch_typeusages.
    node ?= iterator->get_next( ).
  ENDWHILE.

endmethod.
ENDCLASS.
