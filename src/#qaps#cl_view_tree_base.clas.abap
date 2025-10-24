class /QAPS/CL_VIEW_TREE_BASE definition
  public
  abstract
  create public .

public section.

  events ON_FUNCTION_SELECTED
    exporting
      value(IV_FUNCTION) type UI_FUNC
      value(IT_CHECKED_ITEMS) type LVC_T_CHIT .

  methods UPDATE
    importing
      !IR_DATA type ref to DATA .
  methods INITIALIZE
    importing
      !IR_OUTTAB type ref to DATA
      !IO_CONTAINER type ref to CL_GUI_CONTAINER
      !IV_TYPE type TABNAME
      !IV_ACTION type C .
protected section.

  types:
    BEGIN OF ts_line_key,
           key     TYPE lvc_nkey,
           text    TYPE lvc_value,
           is_root TYPE abap_bool,
         END OF ts_line_key .
  types:
    tt_line_key TYPE TABLE OF ts_line_key .

  data MV_TYPE type TABNAME .
  data MV_ROOT_KEY type LVC_NKEY .
  data MS_HIERARCHY_HEADER type TREEV_HHDR .
  data MV_ACTION type C .
  data MS_CATALOG_STRUCTURE type TABNAME .
  data MR_OUTTAB type ref to DATA .
  data MO_TREE type ref to CL_GUI_ALV_TREE .
  data MO_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MT_NODES type TT_LINE_KEY .

  methods ADD_CHILD_NODE
    importing
      !IR_LINE type ref to DATA .
  methods ADD_ROOT_NODE .
  methods DISPLAY_TREE
    importing
      value(IS_HEADER) type TREEV_HHDR
      value(IT_CATALOG) type LVC_T_FCAT
      value(IV_TYPE) type TABNAME .
  methods SET_EVENTS .
  methods CUSTOMIZE_CATALOG
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_HIERARCHY_HEADER
    changing
      !CS_DATA type TREEV_HHDR .
  methods GET_CATALOG
    importing
      !IV_TYPE type TABNAME
    returning
      value(RETURN) type LVC_T_FCAT .
  methods GET_HIERARCHY_HEADER
    returning
      value(RETURN) type TREEV_HHDR .
  methods FUNCTION_SELECTED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods CREATE_INSTANCE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  methods DROPDOWN_CLICKED
    for event DROPDOWN_CLICKED of CL_GUI_TOOLBAR
    importing
      !FCODE
      !POSX
      !POSY .
  methods BUTTON_CLICK
    for event BUTTON_CLICK of CL_GUI_ALV_TREE .
  methods CHECKBOX_CHANGE
    for event CHECKBOX_CHANGE of CL_GUI_ALV_TREE .
  methods EXPAND_NC
    for event EXPAND_NC of CL_GUI_ALV_TREE .
  methods HEADER_CLICK
    for event HEADER_CLICK of CL_GUI_ALV_TREE .
  methods HEADER_CONTEXT_MENU
    for event HEADER_CONTEXT_MENU of CL_GUI_ALV_TREE .
  methods ITEM_CONTEXT_MENU_REQUEST
    for event ITEM_CONTEXT_MENU_REQUEST of CL_GUI_ALV_TREE .
  methods ITEM_CONTEXT_MENU_SELECTED
    for event ITEM_CONTEXT_MENU_SELECTED of CL_GUI_ALV_TREE .
  methods ITEM_DOUBLE_CLICK
    for event ITEM_DOUBLE_CLICK of CL_GUI_ALV_TREE .
  methods ITEM_KEYPRESS
    for event ITEM_KEYPRESS of CL_GUI_ALV_TREE .
  methods LINK_CLICK
    for event LINK_CLICK of CL_GUI_ALV_TREE .
  methods NODE_CONTEXT_MENU_REQUEST
    for event NODE_CONTEXT_MENU_REQUEST of CL_GUI_ALV_TREE .
  methods NODE_CONTEXT_MENU_SELECTED
    for event NODE_CONTEXT_MENU_SELECTED of CL_GUI_ALV_TREE .
  methods NODE_DOUBLE_CLICK
    for event NODE_DOUBLE_CLICK of CL_GUI_ALV_TREE .
  methods ON_DRAG
    for event ON_DRAG of CL_GUI_ALV_TREE .
  methods ON_DRAG_MULTIPLE
    for event ON_DRAG_MULTIPLE of CL_GUI_ALV_TREE .
  methods ON_DROP
    for event ON_DROP of CL_GUI_ALV_TREE .
  methods ON_DROP_COMPLETE
    for event ON_DROP_COMPLETE of CL_GUI_ALV_TREE .
  methods ON_DROP_COMPLETE_MULTIPLE
    for event ON_DROP_COMPLETE_MULTIPLE of CL_GUI_ALV_TREE .
  methods ON_DROP_GET_FLAVOR
    for event ON_DROP_GET_FLAVOR of CL_GUI_ALV_TREE .
  methods SELECTION_CHANGED
    for event SELECTION_CHANGED of CL_GUI_ALV_TREE .
  methods NODE_KEYPRESS
    for event NODE_KEYPRESS of CL_GUI_ALV_TREE .
  methods ON_DROP_EXTERNAL_FILES
    for event ON_DROP_EXTERNAL_FILES of CL_GUI_ALV_TREE .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_TREE_BASE IMPLEMENTATION.


  method ADD_CHILD_NODE.
  endmethod.


  METHOD add_root_node.

    DATA: l_node_text TYPE lvc_value.
*          ls_sflight  TYPE sflight.

    FIELD-SYMBOLS <fs> type any.

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


  method BUTTON_CLICK.
  endmethod.


  method CHECKBOX_CHANGE.
  endmethod.


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


  method CUSTOMIZE_CATALOG.
  endmethod.


  method CUSTOMIZE_HIERARCHY_HEADER.
  endmethod.


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


  method DROPDOWN_CLICKED.
  endmethod.


  method EXPAND_NC.
  endmethod.


  METHOD function_selected.

    mo_tree->get_checked_items( IMPORTING et_checked_items = data(lt_checked_items) ).

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


  method HEADER_CLICK.
  endmethod.


  method HEADER_CONTEXT_MENU.
  endmethod.


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


  method ITEM_CONTEXT_MENU_REQUEST.
  endmethod.


  method ITEM_CONTEXT_MENU_SELECTED.
  endmethod.


  method ITEM_DOUBLE_CLICK.
  endmethod.


  method ITEM_KEYPRESS.
  endmethod.


  method LINK_CLICK.
  endmethod.


  method NODE_CONTEXT_MENU_REQUEST.
  endmethod.


  method NODE_CONTEXT_MENU_SELECTED.
  endmethod.


  method NODE_DOUBLE_CLICK.
  endmethod.


  method NODE_KEYPRESS.
  endmethod.


  method ON_DRAG.
  endmethod.


  method ON_DRAG_MULTIPLE.
  endmethod.


  method ON_DROP.
  endmethod.


  method ON_DROP_COMPLETE.
  endmethod.


  method ON_DROP_COMPLETE_MULTIPLE.
  endmethod.


  method ON_DROP_EXTERNAL_FILES.
  endmethod.


  method ON_DROP_GET_FLAVOR.
  endmethod.


  method SELECTION_CHANGED.
  endmethod.


  METHOD set_events.

    DATA lt_events TYPE cntl_simple_events.

    append VALUE cntl_simple_event( eventid = cl_simple_tree_model=>eventid_node_double_click
                                    ) to lt_events.

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
