*"* use this source file for any type declarations (class
*"* definitions, interfaces or data types) you need for method
*"* implementation or private method's signature
TYPES: BEGIN OF ts_objects,
         nugget_level TYPE int4,
         sort         TYPE sy-tabix,
         xmldocument  TYPE REF TO if_ixml_document,
       END OF ts_objects,
       tt_objects TYPE SORTED TABLE OF ts_objects WITH UNIQUE KEY nugget_level sort.
