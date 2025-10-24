class /QAPS/CL_VIEW_ALV_BASE definition
  public
  abstract
  create public .

public section.

  events ON_BUTTON_CLICK
    exporting
      value(IS_COL_ID) type LVC_S_COL optional
      value(IS_ROW_NO) type LVC_S_ROID optional
      value(IR_DATA) type ref to DATA optional .
  events ON_HOTSPOT_CLICK
    exporting
      value(IV_SOURCE) type CHAR20
      value(IS_ROW_ID) type LVC_S_ROW
      value(IS_COLUMN_ID) type LVC_S_COL
      value(IS_ROW_NO) type LVC_S_ROID
      value(IR_DATA) type ref to DATA
      value(IV_XML_DATA) type STRING optional
      value(IV_ADDITIONAL_DATA_1) type STRING optional
      value(IV_ADDITIONAL_DATA_2) type STRING optional
      value(IV_ADDITIONAL_DATA_3) type STRING optional
      value(IV_ADDITIONAL_DATA_4) type STRING optional .
  events ON_MENU_BUTTON
    exporting
      value(E_OBJECT) type ref to CL_CTMENU
      value(E_UCOMM) type SY-UCOMM .
  events HAS_CHANGES
    exporting
      value(LT_MODIFICATION) type LVC_T_MODI .
  events ON_USER_COMMAND
    exporting
      value(IV_UCOMM) type SYUCOMM
      value(IV_SOURCE) type CHAR20
      value(IV_ACTION) type CHAR1
      value(IV_XML_DATA) type STRING optional
      value(IV_ADDTIONAL_DATA_1) type STRING optional
      value(IV_ADDTIONAL_DATA_2) type STRING optional
      value(IV_ADDTIONAL_DATA_3) type STRING optional
      value(IV_ADDTIONAL_DATA_4) type STRING optional .
  events ON_DOUBLE_CLICK
    exporting
      value(E_ROW) type LVC_S_ROW optional
      value(E_COLUMN) type LVC_S_COL optional
      value(ES_ROW_NO) type LVC_S_ROID optional
      value(IV_ADDITIONAL_DATA_1) type STRING optional
      value(IV_ADDITIONAL_DATA_2) type STRING optional
      value(IV_ADDITIONAL_DATA_3) type STRING optional
      value(IV_ADDITIONAL_DATA_4) type STRING optional .

  methods DISABLE_ALV .
  methods GET_SELECTED_LINES
    returning
      value(RETURN) type LVC_T_ROW .
  methods INITIALIZE
    importing
      !IR_OUTTAB type ref to DATA
      !IO_CONTAINER type ref to CL_GUI_CONTAINER
      !IS_CATALOG_STRUCTURE type TABNAME
      !IV_ACTION type C .
  methods RESET .
  methods SET_DATA
    importing
      !IR_OUTTAB type ref to DATA
      !IV_SOFT_REFRESH type ABAP_BOOL default ABAP_TRUE
      !IR_PARENT type ref to DATA optional
      !IV_SOURCE type STRING optional .
  methods SET_GRID_TITLE
    importing
      !IV_TITLE type LVC_TITLE .
protected section.

  data MV_SOURCE type CHAR20 .
  data MV_ACTION type C .
  data MS_CATALOG_STRUCTURE type TABNAME .
  data MR_OUTTAB type ref to DATA .
  data MO_ALV type ref to /QAPS/CL_GUI_ALV_GRID .
  data MO_CONTAINER type ref to CL_GUI_CONTAINER .
  data MS_CONTENT type /QAPS/S_XML_TYPE_CONTENT .

  methods GET_CONTENT
    changing
      !CR_DATA type ref to DATA .
  methods GET_CONTENT_TYPE
    returning
      value(RETURN) type TYPENAME .
  methods BUTTON_CLICK
    for event BUTTON_CLICK of CL_GUI_ALV_GRID
    importing
      !SENDER
      !ES_COL_ID
      !ES_ROW_NO .
  methods DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO .
  methods GET_STRUCTURE_NAME
    importing
      !IR_TABLE type ref to DATA
    returning
      value(RETURN) type TABNAME .
  methods HOTSPOT_CLICK
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods CREATE_INSTANCE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  methods CUSTOMIZE_CATALOG
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods DATA_CHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
    importing
      !E_MODIFIED
      !ET_GOOD_CELLS .
  methods DISPLAY_ALV
    importing
      value(IS_LAYOUT) type LVC_S_LAYO
      value(IT_CATALOG) type LVC_T_FCAT
      value(IT_SORT) type LVC_T_SORT .
  methods GET_CATALOG
    importing
      !IV_TYPE type TABNAME
    returning
      value(RETURN) type LVC_T_FCAT .
  methods GET_LAYOUT
    returning
      value(RETURN) type LVC_S_LAYO .
  methods GET_SORT
    returning
      value(RETURN) type LVC_T_SORT .
  methods MENU_BUTTON
    for event MENU_BUTTON of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_UCOMM .
  methods SET_EVENTS .
  methods TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods SET_CONTENT
    importing
      !IR_DATA type ref to DATA .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_ALV_BASE IMPLEMENTATION.


  METHOD button_click.

  ENDMETHOD.


  METHOD create_instance.
    mo_alv = NEW /qaps/cl_gui_alv_grid(
*      i_shellstyle            = 0
*      i_lifetime              =
      i_parent = io_container
*     i_appl_events           = space
*     i_parentdbg             =
*     i_applogparent          =
*     i_graphicsparent        =
*     i_name   =
*     i_fcat_complete         = space
*     o_previous_sral_handler =
*     i_use_one_ux_appearance = abap_false
    ).

  ENDMETHOD.


  METHOD customize_catalog.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = 'MESSAGE'
                      OR fieldname = 'STYLE'
                      OR fieldname = 'COLOR'
                      OR fieldname = 'ORD'.

    loop at ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      check <fs>-datatype = 'CURR' or <fs>-datatype = 'QUAN'.
      <fs>-no_zero = 'X'.
    endloop.

  ENDMETHOD.


  method DATA_CHANGED_FINISHED.
  endmethod.


  method DISABLE_ALV.
    mo_alv->set_frontend_layout( is_layout = value lvc_s_layo( edit = '' ) ).
  endmethod.


  METHOD display_alv.

    ASSIGN mr_outtab->* TO FIELD-SYMBOL(<fs>).

    mo_alv->set_table_for_first_display(
      EXPORTING
*       i_buffer_active =
*       i_bypassing_buffer            =
*       i_consistency_check           =
*       i_structure_name              =
*       is_variant      =
*       i_save          =
*       i_default       = 'X'
        is_layout       = is_layout
*       is_print        =
*       it_special_groups             =
*       it_toolbar_excluding          =
*       it_hyperlink    =
*       it_alv_graphics =
*       it_except_qinfo =
*       ir_salv_adapter =
      CHANGING
        it_outtab       = <fs>
        it_fieldcatalog = it_catalog
       it_sort         = it_sort
*       it_filter       =
*      EXCEPTIONS
*       invalid_parameter_combination = 1
*       program_error   = 2
*       too_many_lines  = 3
*       others          = 4
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDMETHOD.


  method DOUBLE_CLICK.
  endmethod.


  METHOD get_catalog.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = iv_type
      CHANGING
        ct_fieldcat      = return.

  ENDMETHOD.


  METHOD get_content.

*    DATA: lr_data       TYPE REF TO data,
*          lo_type_descr TYPE REF TO cl_abap_tabledescr.
*
*    FIELD-SYMBOLS: <fs> TYPE ANY TABLE.
*
*    lo_type_descr ?= cl_abap_tabledescr=>describe_by_name( ms_content-data_type ).
*
*    CREATE DATA lr_data TYPE HANDLE lo_type_descr.

    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_content-xml
                                         CHANGING  cr_data = cr_data    ).

*    ASSIGN lr_data->* TO <fs>.
*    .
*    BREAK c060863.

  ENDMETHOD.


  METHOD get_content_type.
    return = ms_content-data_type.
  ENDMETHOD.


  method GET_LAYOUT.
    return-zebra = 'X'.
    return-ctab_fname = 'COLOR'.
    return-stylefname = 'STYLE'.
  endmethod.


  METHOD get_selected_lines.
    mo_alv->get_selected_rows( IMPORTING et_index_rows = return ).
  ENDMETHOD.


  method GET_SORT.
  endmethod.


  METHOD get_structure_name.

    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                   <fs_comp>  TYPE abap_componentdescr.

    DATA: lr_line       TYPE REF TO data,
          lo_sdescr     TYPE REF TO cl_abap_structdescr.

    ASSIGN ir_table->* TO <fs_table>.
    CREATE DATA lr_line LIKE LINE OF <fs_table>.

    lo_sdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_line ).
    return = lo_sdescr->get_relative_name( ).

  ENDMETHOD.


  method HOTSPOT_CLICK.
  endmethod.


  METHOD initialize.

    mr_outtab = ir_outtab.
    mv_action = iv_action.

    "Cria instânica
    create_instance( io_container ).

    "Catálogo de Campo
    DATA(lt_catalog) = get_catalog( is_catalog_structure ).
    customize_catalog( CHANGING ct_catalog = lt_catalog ).

    "Layout
    DATA(ls_layo) = get_layout( ).

    "Sort
    DATA(lt_sort) = get_sort( ).

    "Eventos
    set_events( ).

    "Display ALV
    display_alv( is_layout  = ls_layo
                 it_catalog = lt_catalog
                 it_sort    = lt_sort ).

  ENDMETHOD.


  method MENU_BUTTON.
  endmethod.


  METHOD reset.
    FIELD-SYMBOLS <fs> TYPE ANY TABLE.
    ASSIGN mr_outtab->* TO <fs>.
    REFRESH <fs>.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  METHOD set_content.

    DATA lo_table TYPE REF TO cl_abap_tabledescr.

    lo_table ?= cl_abap_tabledescr=>describe_by_data_ref( ir_data ).

    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_data  ).

    ms_content-xml = lv_xml.
    ms_content-data_type = lo_table->get_relative_name( ).

  ENDMETHOD.


  METHOD set_data.
    mr_outtab = ir_outtab.

    mv_source = iv_source.

    IF iv_soft_refresh = abap_true.
      mo_alv->refresh_table_display(
        EXPORTING
          is_stable      = VALUE lvc_s_stbl( row = 'X' col = 'X' )
          i_soft_refresh = abap_true
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).
    ELSE.
      mo_alv->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_events.

    mo_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
    mo_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).

    SET HANDLER: data_changed_finished FOR mo_alv,
                 button_click          FOR mo_alv,
                 menu_button           FOR mo_alv,
                 toolbar               FOR mo_alv,
                 hotspot_click         FOR mo_alv,
                 user_command          FOR mo_alv,
                 double_click          FOR mo_alv.

  ENDMETHOD.


  METHOD set_grid_title.
    mo_alv->set_gridtitle( i_gridtitle = iv_title ).
  ENDMETHOD.


  method TOOLBAR.
  endmethod.


  method USER_COMMAND.
  endmethod.
ENDCLASS.
