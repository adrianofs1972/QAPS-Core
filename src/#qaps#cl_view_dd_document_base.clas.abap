class /QAPS/CL_VIEW_DD_DOCUMENT_BASE definition
  public
  abstract
  create public .

public section.

  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER
      !IV_TITLE type SDYDO_TEXT_ELEMENT .
  methods UPDATE
    importing
      !IR_DATA type ref to DATA .
  methods CONSTRUCTOR .
protected section.

  data MO_DOCUMENT type ref to CL_DD_DOCUMENT .
  data MO_CONTAINER type ref to CL_GUI_CONTAINER .

  methods LINK_CLICKED
    for event CLICKED of CL_DD_LINK_ELEMENT
    importing
      !SENDER .
  methods BUTTON_CLICKED
    for event CLICKED of CL_DD_BUTTON_ELEMENT
    importing
      !SENDER .
  methods DISPLAY_DOCUMENT .
  methods ENTERED
    for event ENTERED of CL_DD_INPUT_ELEMENT
    importing
      !SENDER .
  methods HELP_F1
    for event HELP_F1 of CL_DD_INPUT_ELEMENT
    importing
      !SENDER .
  methods RESOURCES_CHANGED
    for event RESOURCES_CHANGED of CL_GUI_RESOURCES .
  methods SELECTED
    for event SELECTED of CL_DD_SELECT_ELEMENT
    importing
      !SENDER .
  methods SET_CONTENT
    importing
      !IV_TEXT type SDYDO_TEXT_ELEMENT .
  methods SET_TITLE
    importing
      !IV_TITLE type SDYDO_TEXT_ELEMENT
      !IV_ICON type ICONNAME optional .
  methods ITEM_SELECTED
    for event ITEM_SELECTED of CL_DD_FORM_ELEMENT
    importing
      !ID .
  methods INPUT_ENTERED
    for event INPUT_ENTERED of CL_DD_FORM_ELEMENT
    importing
      !ID .
  methods INPUT_HELPF1
    for event INPUT_HELPF1 of CL_DD_FORM_ELEMENT
    importing
      !ID .
  methods INPUT_HELPF4
    for event INPUT_HELPF4 of CL_DD_FORM_ELEMENT
    importing
      !ID .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_DD_DOCUMENT_BASE IMPLEMENTATION.


  method BUTTON_CLICKED.
  endmethod.


  METHOD constructor.
    mo_document = NEW cl_dd_document( ).
  ENDMETHOD.


  METHOD display_document.

    mo_document->display_document(
      EXPORTING
        reuse_control      = 'X'    " HTML Control Reused
*        reuse_registration =     " Event Registration Reused
*        container          =     " Name of Container (New Container Object Generated)
        parent             =  mo_container   " Contain Object Already Exists
*      EXCEPTIONS
*        html_display_error = 1
*        others             = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  method ENTERED.
  endmethod.


  method HELP_F1.
  endmethod.


  METHOD initialize.

    mo_container = io_container.

    set_title( iv_title ).
*    set_text( iv_text =  ).

    display_document( ).

  ENDMETHOD.


  method INPUT_ENTERED.
  endmethod.


  method INPUT_HELPF1.
  endmethod.


  method INPUT_HELPF4.
  endmethod.


  method ITEM_SELECTED.
  endmethod.


  method LINK_CLICKED.
  endmethod.


  method RESOURCES_CHANGED.
  endmethod.


  method SELECTED.
  endmethod.


  method SET_CONTENT.
  endmethod.


  METHOD set_title.

    mo_document->line_with_layout( start            =  'X' ).

    mo_document->add_gap( width  = 3  ).

    IF NOT iv_icon IS INITIAL.
      mo_document->add_icon( sap_icon         = iv_icon
                             sap_size = 'LARGE'  ).

      mo_document->add_gap( width  = 3  ).

    ENDIF.

    mo_document->add_text( text      = iv_title
                           sap_style = 'heading' ).

    mo_document->line_with_layout( end =  'X' ).

    mo_document->new_line( ).
    mo_document->underline( ).

  ENDMETHOD.


  method UPDATE.
  endmethod.
ENDCLASS.
