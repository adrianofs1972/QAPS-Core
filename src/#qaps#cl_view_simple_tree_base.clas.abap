CLASS /qaps/cl_view_simple_tree_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    EVENTS on_function_selected
      EXPORTING
        VALUE(iv_function) TYPE ui_func
        VALUE(iv_xml_data) TYPE string .
    EVENTS on_node_double_click
      EXPORTING
        VALUE(iv_node_key) TYPE tm_nodekey
        VALUE(iv_xml_data) TYPE string OPTIONAL
        VALUE(iv_source) TYPE string OPTIONAL
        VALUE(iv_guid) TYPE guid16 OPTIONAL
        VALUE(iv_texto) TYPE string OPTIONAL
        VALUE(iv_additional_data) TYPE string OPTIONAL .

    METHODS get_selected_node
      RETURNING
        VALUE(return) TYPE string .
    METHODS update
      IMPORTING
        !ir_data     TYPE REF TO data
        !ir_expanded TYPE REF TO data OPTIONAL .
    METHODS initialize
      IMPORTING
        !io_container TYPE REF TO cl_gui_container
        !iv_root_text TYPE tm_nodetxt DEFAULT 'Root'
        !iv_toolbar   TYPE abap_bool DEFAULT abap_false .
    METHODS remove_node
      IMPORTING
        !ir_line TYPE REF TO data .
    METHODS add_new_child_node
      IMPORTING
        !ir_line TYPE REF TO data .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ts_line_key,
        key                TYPE lvc_nkey,
        guid               TYPE guid_16,
        text               TYPE lvc_value,
        is_root            TYPE abap_bool,
        content            TYPE string,
        content_additional TYPE string,
        source             TYPE string,
        tipo               TYPE char1,
        node_key           TYPE tm_nodekey,
        no_event           TYPE abap_bool,
        text_node          TYPE tm_nodetxt,
        trigger_event      TYPE abap_bool,
      END OF ts_line_key .
    TYPES:
      tt_line_key TYPE TABLE OF ts_line_key .
    TYPES:
      BEGIN OF ts_tree_line,
        node_key   TYPE tm_nodekey,
        parent_key TYPE tm_nodekey,
        text       TYPE tm_nodetxt,
        image      TYPE tv_image,
        source     TYPE string,
        no_event   TYPE abap_bool,
        grid       TYPE guid16,
      END  OF ts_tree_line .
    TYPES:
      tt_tree_line TYPE TABLE OF ts_tree_line .

    DATA mv_root_text TYPE tm_nodetxt .
    DATA mo_splitter_toolbar TYPE REF TO cl_gui_splitter_container .
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar .
    DATA mo_tree TYPE REF TO cl_simple_tree_model .
    DATA mt_nodes TYPE tt_line_key .

    METHODS drag
          FOR EVENT drag OF cl_simple_tree_model
      IMPORTING
          !node_key
          !drag_drop_object .
    METHODS drop
          FOR EVENT drop OF cl_simple_tree_model
      IMPORTING
          !node_key
          !drag_drop_object .
    METHODS node_context_menu_request
          FOR EVENT node_context_menu_request OF cl_simple_tree_model
      IMPORTING
          !node_key
          !menu .
    METHODS node_context_menu_select
          FOR EVENT node_context_menu_select OF cl_simple_tree_model
      IMPORTING
          !node_key
          !fcode .
    METHODS add_child_node
      IMPORTING
        !ir_line          TYPE REF TO data
        !iv_expand_parent TYPE abap_bool DEFAULT abap_true .
    METHODS add_root_node
      IMPORTING
        !iv_root_text TYPE tm_nodetxt .
    METHODS customize_toolbar
      IMPORTING
        !co_toolbar TYPE REF TO cl_gui_toolbar .
    METHODS function_selected
          FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
          !fcode .
    METHODS init_without_toolbar
      IMPORTING
        !io_container TYPE REF TO cl_gui_container
        !iv_root_text TYPE tm_nodetxt .
    METHODS init_with_toolbar
      IMPORTING
        !io_container TYPE REF TO cl_gui_container
        !iv_root_text TYPE tm_nodetxt .
    METHODS node_double_click
          FOR EVENT node_double_click OF cl_simple_tree_model
      IMPORTING
          !node_key .
    METHODS set_events .
    METHODS drop_complete
          FOR EVENT drop_complete OF cl_simple_tree_model
      IMPORTING
          !node_key
          !drag_drop_object .
  PRIVATE SECTION.
ENDCLASS.



CLASS /qaps/cl_view_simple_tree_base IMPLEMENTATION.


  METHOD add_child_node.
  ENDMETHOD.


  METHOD add_new_child_node.
  ENDMETHOD.


  METHOD add_root_node.

    mo_tree->add_node( EXPORTING node_key = 'Root'          "#EC NOTEXT
                                 isfolder = 'X'
                                 text = iv_root_text  ).

    APPEND VALUE ts_line_key(
        key     = 'Root'
        text    = iv_root_text
        is_root = abap_true ) TO mt_nodes.



  ENDMETHOD.


  METHOD customize_toolbar.

    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'INSERT'
        icon      = icon_insert_row
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Add Lista de Custos' ).                "#EC NOTEXT

* add Dropdown Button to toolbar (for Insert Line)
    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'REMOVE'
        icon      = icon_delete_row
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Remove Lista de Custos' ).             "#EC NOTEXT

    mo_toolbar->add_button(
      EXPORTING
        fcode     = '&SEP'
        icon = ''
        butn_type = cntb_btype_sep
        text      = ''  ).                                  "#EC NOTEXT

    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'ACTIVE'
        icon      = icon_activate
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Ativar Lista de Custos' ).             "#EC NOTEXT

    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'INACTIVE'
        icon      = icon_deactivate
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Desativar Lista de Custos' ).          "#EC NOTEXT

  ENDMETHOD.


  METHOD drag.
  ENDMETHOD.


  METHOD drop.
  ENDMETHOD.


  METHOD drop_complete.
  ENDMETHOD.


  METHOD function_selected.
  ENDMETHOD.


  METHOD get_selected_node.

    mo_tree->get_selected_node(
      IMPORTING
        node_key                   =  DATA(lv_node_key)   " Key of Selected Node
      EXCEPTIONS
        control_not_existing       = 1
        control_dead               = 2
        cntl_system_error          = 3
        failed                     = 4
        single_node_selection_only = 5
        OTHERS                     = 6
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF NOT lv_node_key IS INITIAL.
      DATA(ls_node) = VALUE #( mt_nodes[ node_key = lv_node_key ] OPTIONAL ).
      return = ls_node-content.
    ENDIF.

  ENDMETHOD.


  METHOD initialize.

    IF iv_toolbar = abap_false.
      init_without_toolbar( EXPORTING io_container = io_container
                                      iv_root_text = iv_root_text  ).
    ELSEIF iv_toolbar = abap_true.
      init_with_toolbar( EXPORTING io_container = io_container
                                   iv_root_text = iv_root_text  ).
    ENDIF.

    mv_root_text = iv_root_text.

  ENDMETHOD.


  METHOD init_without_toolbar.

    mo_tree = NEW cl_simple_tree_model( node_selection_mode  = cl_simple_tree_model=>node_sel_mode_single
*                                    hide_selection              =
    ).

    mo_tree->create_tree_control( parent =  io_container   ).

    set_events( ).

    add_root_node( iv_root_text ).

  ENDMETHOD.


  METHOD init_with_toolbar.
*    BREAK-POINT.
    mo_splitter_toolbar = NEW cl_gui_splitter_container(
        parent                  = io_container
        rows                    = 2
        columns                 = 1
        ).

    mo_splitter_toolbar->set_border( border = '' ).
    mo_splitter_toolbar->set_row_sash(
      EXPORTING
        id                = 1    " Row Splitter Bar ID
        type              = cl_gui_splitter_container=>type_sashvisible    " Attribute
        value             = cl_gui_splitter_container=>false  ).

    "Toolbar
    DATA(lo_toolbar) = mo_splitter_toolbar->get_container( row = 1 column = 1 ).
    mo_toolbar = NEW cl_gui_toolbar( parent  = lo_toolbar ).

    customize_toolbar( mo_toolbar ).

    mo_splitter_toolbar->set_row_height( id = 1 height = 3 ).

    "Tree
    DATA(lo_tree) = mo_splitter_toolbar->get_container( row = 2 column = 1 ).
    mo_tree = NEW cl_simple_tree_model( node_selection_mode  = cl_simple_tree_model=>node_sel_mode_single
*                                    hide_selection              =
   ).

    mo_tree->create_tree_control( parent =  lo_tree   ).

    set_events( ).

    add_root_node( iv_root_text ).

  ENDMETHOD.


  METHOD node_context_menu_request.



  ENDMETHOD.


  METHOD node_context_menu_select.

  ENDMETHOD.


  METHOD node_double_click.
  ENDMETHOD.


  METHOD remove_node.
  ENDMETHOD.


  METHOD set_events.

    DATA: event  TYPE cntl_simple_event,
          events TYPE cntl_simple_events.

    DATA: lt_toolbar_event TYPE cntl_simple_events,
          ls_toolbar_event TYPE cntl_simple_event.

    event-eventid    = cl_simple_tree_model=>eventid_node_double_click.
    event-appl_event = 'X'.              " process PAI if event occurs
    APPEND event TO events.

    event-eventid    = cl_simple_tree_model=>eventid_node_context_menu_req.
    event-appl_event = 'X'.              " process PAI if event occurs
    APPEND event TO events.

*    event-eventid    = cl_simple_tree_model=>eventid_.
*    event-appl_event = 'X'.              " process PAI if event occurs
*    APPEND event TO events.

    mo_tree->set_registered_events( EXPORTING events  = events ).

    SET HANDLER: node_double_click FOR mo_tree,
                 node_context_menu_request FOR mo_tree,
                 node_context_menu_select  FOR mo_tree.

    CHECK mo_toolbar IS BOUND.

    ls_toolbar_event-eventid = cl_gui_toolbar=>m_id_function_selected.
    ls_toolbar_event-appl_event = 'X'.
    APPEND ls_toolbar_event TO lt_toolbar_event.

    mo_toolbar->set_registered_events( EXPORTING events = lt_toolbar_event ).
    SET HANDLER me->function_selected FOR mo_toolbar.

    "Drag and drop events
    SET HANDLER: drag          FOR mo_tree,
                 drop_complete FOR mo_tree,
                 drop          FOR mo_tree.


  ENDMETHOD.


  METHOD update.

    DATA lr_line TYPE REF TO data.
    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    mo_tree->delete_all_nodes( ).

    add_root_node( iv_root_text = mv_root_text ).

    ASSIGN ir_data->* TO <ft>.

    LOOP AT <ft> ASSIGNING FIELD-SYMBOL(<fs_line>).
      lr_line = REF #( <fs_line> ).
      add_child_node( lr_line ).
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
