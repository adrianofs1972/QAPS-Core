CLASS /qaps/cl_view_tree_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    EVENTS on_function_selected
      EXPORTING
        VALUE(iv_function) TYPE ui_func
        VALUE(it_checked_items) TYPE lvc_t_chit .

    METHODS update
      IMPORTING
        !ir_data TYPE REF TO data .
    METHODS initialize
      IMPORTING
        !ir_outtab    TYPE REF TO data
        !io_container TYPE REF TO cl_gui_container
        !iv_type      TYPE tabname
        !iv_action    TYPE c .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ts_line_key,
        key     TYPE lvc_nkey,
        text    TYPE lvc_value,
        is_root TYPE abap_bool,
      END OF ts_line_key .
    TYPES:
      tt_line_key TYPE TABLE OF ts_line_key .

    DATA mv_type TYPE tabname .
    DATA mv_root_key TYPE lvc_nkey .
    DATA ms_hierarchy_header TYPE treev_hhdr .
    DATA mv_action TYPE c .
    DATA ms_catalog_structure TYPE tabname .
    DATA mr_outtab TYPE REF TO data .
    DATA mo_tree TYPE REF TO cl_gui_alv_tree .
    DATA mo_container TYPE REF TO cl_gui_container .
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar .
    DATA mt_nodes TYPE tt_line_key .

    METHODS add_child_node
      IMPORTING
        !ir_line TYPE REF TO data .
    METHODS add_root_node .
    METHODS display_tree
      IMPORTING
        VALUE(is_header)  TYPE treev_hhdr
        VALUE(it_catalog) TYPE lvc_t_fcat
        VALUE(iv_type)    TYPE tabname .
    METHODS set_events .
    METHODS customize_catalog
      CHANGING
        !ct_catalog TYPE lvc_t_fcat .
    METHODS customize_hierarchy_header
      CHANGING
        !cs_data TYPE treev_hhdr .
    METHODS get_catalog
      IMPORTING
        !iv_type      TYPE tabname
      RETURNING
        VALUE(return) TYPE lvc_t_fcat .
    METHODS get_hierarchy_header
      RETURNING
        VALUE(return) TYPE treev_hhdr .
    METHODS function_selected
          FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
          !fcode .
    METHODS create_instance
      IMPORTING
        !io_container TYPE REF TO cl_gui_container .
    METHODS dropdown_clicked
          FOR EVENT dropdown_clicked OF cl_gui_toolbar
      IMPORTING
          !fcode
          !posx
          !posy .
    METHODS button_click
        FOR EVENT button_click OF cl_gui_alv_tree .
    METHODS checkbox_change
        FOR EVENT checkbox_change OF cl_gui_alv_tree .
    METHODS expand_nc
        FOR EVENT expand_nc OF cl_gui_alv_tree .
    METHODS header_click
        FOR EVENT header_click OF cl_gui_alv_tree .
    METHODS header_context_menu
        FOR EVENT header_context_menu OF cl_gui_alv_tree .
    METHODS item_context_menu_request
        FOR EVENT item_context_menu_request OF cl_gui_alv_tree .
    METHODS item_context_menu_selected
        FOR EVENT item_context_menu_selected OF cl_gui_alv_tree .
    METHODS item_double_click
        FOR EVENT item_double_click OF cl_gui_alv_tree .
    METHODS item_keypress
        FOR EVENT item_keypress OF cl_gui_alv_tree .
    METHODS link_click
        FOR EVENT link_click OF cl_gui_alv_tree .
    METHODS node_context_menu_request
        FOR EVENT node_context_menu_request OF cl_gui_alv_tree .
    METHODS node_context_menu_selected
        FOR EVENT node_context_menu_selected OF cl_gui_alv_tree .
    METHODS node_double_click
        FOR EVENT node_double_click OF cl_gui_alv_tree .
    METHODS on_drag
        FOR EVENT on_drag OF cl_gui_alv_tree .
    METHODS on_drag_multiple
        FOR EVENT on_drag_multiple OF cl_gui_alv_tree .
    METHODS on_drop
        FOR EVENT on_drop OF cl_gui_alv_tree .
    METHODS on_drop_complete
        FOR EVENT on_drop_complete OF cl_gui_alv_tree .
    METHODS on_drop_complete_multiple
        FOR EVENT on_drop_complete_multiple OF cl_gui_alv_tree .
    METHODS on_drop_get_flavor
        FOR EVENT on_drop_get_flavor OF cl_gui_alv_tree .
    METHODS selection_changed
        FOR EVENT selection_changed OF cl_gui_alv_tree .
    METHODS node_keypress
        FOR EVENT node_keypress OF cl_gui_alv_tree .
    METHODS on_drop_external_files
        FOR EVENT on_drop_external_files OF cl_gui_alv_tree .
  PRIVATE SECTION.
ENDCLASS.



CLASS /qaps/cl_view_tree_base IMPLEMENTATION.


  METHOD add_child_node.
  ENDMETHOD.


  METHOD add_root_node.

    DATA: l_node_text TYPE lvc_value.
*          ls_sflight  TYPE sflight.

    FIELD-SYMBOLS <fs> TYPE any.

    DATA: go_struct   TYPE REF TO cl_abap_structdescr,
          go_new_type TYPE REF TO cl_abap_structdescr,
          go_new_str  TYPE REF TO cl_abap_structdescr,
          gt_comp     TYPE cl_abap_structdescr=>component_table,
          go_data     TYPE REF TO data.

    go_struct ?= cl_abap_typedescr=>describe_by_name( p_name = mv_type ).
    gt_comp = go_struct->get_components( ).
    go_new_type = cl_abap_structdescr=>create( gt_comp ).
    go_new_str = cl_abap_structdescr=>create(
                 p_components          = gt_comp
*                 p_strict              = TRUE
             ).

    CREATE DATA go_data TYPE HANDLE go_new_str.

    ASSIGN go_data->* TO <fs>.

* set item-layout
    DATA: lt_item_layout TYPE lvc_t_layi,
          ls_item_layout TYPE lvc_s_layi.

    ls_item_layout-fieldname = mo_tree->c_hierarchy_column_name.
    ls_item_layout-style     = cl_gui_column_tree=>style_default.
    APPEND ls_item_layout TO lt_item_layout.

* add node
    l_node_text =  'Root'.

    DATA: ls_node TYPE lvc_s_layn.
    ls_node-n_image   = space.
    ls_node-exp_image = space.
    ls_node-isfolder = 'X'.
    ls_node-expander = 'X'.

    CALL METHOD mo_tree->add_node
      EXPORTING
        i_relat_node_key = ''
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
        is_outtab_line   = <fs>
        is_node_layout   = ls_node
        it_item_layout   = lt_item_layout
      IMPORTING
        e_new_node_key   = mv_root_key.

    mo_tree->update_calculations( ).
    mo_tree->frontend_update( ).

  ENDMETHOD.


  METHOD button_click.
  ENDMETHOD.


  METHOD checkbox_change.
  ENDMETHOD.


  METHOD create_instance.


    mo_tree = NEW cl_gui_alv_tree(
*        lifetime                    =
        parent                      =  io_container
*        shellstyle                  =
*        node_selection_mode         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
*        hide_selection              =
*        item_selection              = 'X'
        no_toolbar                  = ''
        no_html_header              = 'X'
*        i_print                     =
*        i_fcat_complete             =
*        i_model_mode                =
    ).

    mo_tree->get_toolbar_object(
      IMPORTING
        er_toolbar = mo_toolbar
    ).

    CHECK NOT mo_toolbar IS INITIAL.

    mo_toolbar->delete_all_buttons(
*      EXCEPTIONS
*        cntl_error = 1
*        others     = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* add seperator to toolbar
    CALL METHOD mo_toolbar->add_button
      EXPORTING
        fcode     = ''
        icon      = ''
        butn_type = cntb_btype_sep
        text      = ''
        quickinfo = 'This is a Seperator'.


    CALL METHOD mo_toolbar->add_button
      EXPORTING
        fcode     = 'INSERT_LC'
        icon      = '@17@'
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Adicionar Lista'.

* add Standard Button to toolbar (for Delete Subtree)
    CALL METHOD mo_toolbar->add_button
      EXPORTING
        fcode     = 'DELETE'
        icon      = '@18@'
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Excluir Lista'.



  ENDMETHOD.


  METHOD customize_catalog.
  ENDMETHOD.


  METHOD customize_hierarchy_header.
  ENDMETHOD.


  METHOD display_tree.

    DATA: lt_data TYPE REF TO data.

*************************************    ]
    DATA: go_struct   TYPE REF TO cl_abap_structdescr,
          go_new_type TYPE REF TO cl_abap_structdescr,
          go_new_tab  TYPE REF TO cl_abap_tabledescr,
          gt_comp     TYPE cl_abap_structdescr=>component_table,
          go_data     TYPE REF TO data.


    go_struct ?= cl_abap_typedescr=>describe_by_name( p_name = iv_type ).
    gt_comp = go_struct->get_components( ).
    go_new_type = cl_abap_structdescr=>create( gt_comp ).
    go_new_tab = cl_abap_tabledescr=>create( p_line_type = go_new_type
                                             p_table_kind = cl_abap_tabledescr=>tablekind_std
                                             p_unique = abap_false ).

    CREATE DATA go_data TYPE HANDLE go_new_tab.

    ASSIGN go_data->* TO FIELD-SYMBOL(<fs_any>).

    mo_tree->set_table_for_first_display(
            EXPORTING
              is_hierarchy_header = is_header
*              it_list_commentary  = lt_list_commentary
*              i_logo              = l_logo
*              i_background_id     = 'ALV_BACKGROUND'
*              i_save              = 'A'
*              is_variant          = ls_variant
            CHANGING
              it_outtab           = <fs_any>
              it_fieldcatalog     = it_catalog ).


  ENDMETHOD.


  METHOD dropdown_clicked.
  ENDMETHOD.


  METHOD expand_nc.
  ENDMETHOD.


  METHOD function_selected.

    mo_tree->get_checked_items( IMPORTING et_checked_items = DATA(lt_checked_items) ).

    RAISE EVENT on_function_selected
      EXPORTING
        iv_function = fcode
        it_checked_items = lt_checked_items.

  ENDMETHOD.


  METHOD get_catalog.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = iv_type
      CHANGING
        ct_fieldcat      = return.

  ENDMETHOD.


  METHOD get_hierarchy_header.

    return-heading = '<Redefinir Método>'.
    return-tooltip = '<Redefinir Método>'.
    return-width = 30.
    return-width_pix = ''.

  ENDMETHOD.


  METHOD header_click.
  ENDMETHOD.


  METHOD header_context_menu.
  ENDMETHOD.


  METHOD initialize.

    mr_outtab = ir_outtab.
    mv_action = iv_action.
    mv_type = iv_type.

    "Cria instânica
    create_instance( io_container ).

*    "Catálogo de Campo
    DATA(lt_catalog) = get_catalog( iv_type ).
    customize_catalog( CHANGING ct_catalog = lt_catalog ).

    DATA(ls_header) = get_hierarchy_header( ).
    customize_hierarchy_header( CHANGING cs_data = ls_header ).

    "Eventos
    set_events( ).
*
*    "Display ALV
    display_tree( is_header  = ls_header
                  it_catalog = lt_catalog
                  iv_type = iv_type ).

    add_root_node( ).


  ENDMETHOD.


  METHOD item_context_menu_request.
  ENDMETHOD.


  METHOD item_context_menu_selected.
  ENDMETHOD.


  METHOD item_double_click.
  ENDMETHOD.


  METHOD item_keypress.
  ENDMETHOD.


  METHOD link_click.
  ENDMETHOD.


  METHOD node_context_menu_request.
  ENDMETHOD.


  METHOD node_context_menu_selected.
  ENDMETHOD.


  METHOD node_double_click.
  ENDMETHOD.


  METHOD node_keypress.
  ENDMETHOD.


  METHOD on_drag.
  ENDMETHOD.


  METHOD on_drag_multiple.
  ENDMETHOD.


  METHOD on_drop.
  ENDMETHOD.


  METHOD on_drop_complete.
  ENDMETHOD.


  METHOD on_drop_complete_multiple.
  ENDMETHOD.


  METHOD on_drop_external_files.
  ENDMETHOD.


  METHOD on_drop_get_flavor.
  ENDMETHOD.


  METHOD selection_changed.
  ENDMETHOD.


  METHOD set_events.

    DATA lt_events TYPE cntl_simple_events.

    APPEND VALUE cntl_simple_event( eventid = cl_simple_tree_model=>eventid_node_double_click
                                    ) TO lt_events.

    mo_tree->set_registered_events( EXPORTING events = lt_events  ).

    "Tree Events
    SET HANDLER:
*                  button_click FOR mo_tree,
*                  checkbox_change FOR mo_tree,
*                  expand_nc FOR mo_tree,
*                  header_click FOR mo_tree,
*                  header_context_menu FOR mo_tree,
*                  item_context_menu_request FOR mo_tree,
*                  item_context_menu_selected FOR mo_tree,
*                  item_double_click FOR mo_tree,
*                  item_keypress FOR mo_tree,
*                  link_click FOR mo_tree,
*                  node_context_menu_request FOR mo_tree,
*                  node_context_menu_selected FOR mo_tree,
                  node_double_click FOR mo_tree,
*                  on_drag FOR mo_tree,
*                  on_drag_multiple FOR mo_tree,
*                  on_drop FOR mo_tree,
*                  on_drop_complete FOR mo_tree,
*                  on_drop_complete_multiple FOR mo_tree,
*                  on_drop_get_flavor FOR mo_tree,
*                  selection_changed FOR mo_tree,
*                  node_keypress FOR mo_tree,
                  on_drop_external_files FOR mo_tree.

    SET HANDLER: dropdown_clicked FOR mo_toolbar,
                 function_selected FOR mo_toolbar.

  ENDMETHOD.


  METHOD update.

    DATA lr_line TYPE REF TO data.
    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <ft>.

    LOOP AT <ft> REFERENCE INTO lr_line.
      add_child_node( lr_line ).
    ENDLOOP.

    mo_tree->update_calculations( ).
    mo_tree->frontend_update( ).

  ENDMETHOD.
ENDCLASS.
