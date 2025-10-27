CLASS /qaps/cl_view_alv_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    EVENTS on_button_click
      EXPORTING
        VALUE(is_col_id) TYPE lvc_s_col OPTIONAL
        VALUE(is_row_no) TYPE lvc_s_roid OPTIONAL
        VALUE(ir_data) TYPE REF TO data OPTIONAL .
    EVENTS on_hotspot_click
      EXPORTING
        VALUE(iv_source) TYPE char20
        VALUE(is_row_id) TYPE lvc_s_row
        VALUE(is_column_id) TYPE lvc_s_col
        VALUE(is_row_no) TYPE lvc_s_roid
        VALUE(ir_data) TYPE REF TO data
        VALUE(iv_xml_data) TYPE string OPTIONAL
        VALUE(iv_additional_data_1) TYPE string OPTIONAL
        VALUE(iv_additional_data_2) TYPE string OPTIONAL
        VALUE(iv_additional_data_3) TYPE string OPTIONAL
        VALUE(iv_additional_data_4) TYPE string OPTIONAL .
    EVENTS on_menu_button
      EXPORTING
        VALUE(e_object) TYPE REF TO cl_ctmenu
        VALUE(e_ucomm) TYPE sy-ucomm .
    EVENTS has_changes
      EXPORTING
        VALUE(lt_modification) TYPE lvc_t_modi .
    EVENTS on_user_command
      EXPORTING
        VALUE(iv_ucomm) TYPE syucomm
        VALUE(iv_source) TYPE char20
        VALUE(iv_action) TYPE char1
        VALUE(iv_xml_data) TYPE string OPTIONAL
        VALUE(iv_addtional_data_1) TYPE string OPTIONAL
        VALUE(iv_addtional_data_2) TYPE string OPTIONAL
        VALUE(iv_addtional_data_3) TYPE string OPTIONAL
        VALUE(iv_addtional_data_4) TYPE string OPTIONAL .
    EVENTS on_double_click
      EXPORTING
        VALUE(e_row) TYPE lvc_s_row OPTIONAL
        VALUE(e_column) TYPE lvc_s_col OPTIONAL
        VALUE(es_row_no) TYPE lvc_s_roid OPTIONAL
        VALUE(iv_additional_data_1) TYPE string OPTIONAL
        VALUE(iv_additional_data_2) TYPE string OPTIONAL
        VALUE(iv_additional_data_3) TYPE string OPTIONAL
        VALUE(iv_additional_data_4) TYPE string OPTIONAL .

    METHODS disable_alv .
    METHODS get_selected_lines
      RETURNING
        VALUE(return) TYPE lvc_t_row .
    METHODS initialize
      IMPORTING
        !ir_outtab            TYPE REF TO data
        !io_container         TYPE REF TO cl_gui_container
        !is_catalog_structure TYPE tabname
        !iv_action            TYPE c .
    METHODS reset .
    METHODS set_data
      IMPORTING
        !ir_outtab       TYPE REF TO data
        !iv_soft_refresh TYPE abap_bool DEFAULT abap_true
        !ir_parent       TYPE REF TO data OPTIONAL
        !iv_source       TYPE string OPTIONAL .
    METHODS set_grid_title
      IMPORTING
        !iv_title TYPE lvc_title .
  PROTECTED SECTION.

    DATA mv_source TYPE char20 .
    DATA mv_action TYPE c .
    DATA ms_catalog_structure TYPE tabname .
    DATA mr_outtab TYPE REF TO data .
    DATA mo_alv TYPE REF TO /qaps/cl_gui_alv_grid .
    DATA mo_container TYPE REF TO cl_gui_container .
    DATA ms_content TYPE /qaps/s_xml_type_content .

    METHODS get_content
      CHANGING
        !cr_data TYPE REF TO data .
    METHODS get_content_type
      RETURNING
        VALUE(return) TYPE typename .
    METHODS button_click
          FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING
          !sender
          !es_col_id
          !es_row_no .
    METHODS double_click
          FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
          !e_row
          !e_column
          !es_row_no .
    METHODS get_structure_name
      IMPORTING
        !ir_table     TYPE REF TO data
      RETURNING
        VALUE(return) TYPE tabname .
    METHODS hotspot_click
          FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
          !e_row_id
          !e_column_id
          !es_row_no .
    METHODS create_instance
      IMPORTING
        !io_container TYPE REF TO cl_gui_container .
    METHODS customize_catalog
      CHANGING
        !ct_catalog TYPE lvc_t_fcat .
    METHODS data_changed_finished
          FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
          !e_modified
          !et_good_cells .
    METHODS display_alv
      IMPORTING
        VALUE(is_layout)  TYPE lvc_s_layo
        VALUE(it_catalog) TYPE lvc_t_fcat
        VALUE(it_sort)    TYPE lvc_t_sort .
    METHODS get_catalog
      IMPORTING
        !iv_type      TYPE tabname
      RETURNING
        VALUE(return) TYPE lvc_t_fcat .
    METHODS get_layout
      RETURNING
        VALUE(return) TYPE lvc_s_layo .
    METHODS get_sort
      RETURNING
        VALUE(return) TYPE lvc_t_sort .
    METHODS menu_button
          FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING
          !e_object
          !e_ucomm .
    METHODS set_events .
    METHODS toolbar
          FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
          !e_object
          !e_interactive .
    METHODS user_command
          FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
          !e_ucomm .
    METHODS set_content
      IMPORTING
        !ir_data TYPE REF TO data .
  PRIVATE SECTION.
ENDCLASS.



CLASS /qaps/cl_view_alv_base IMPLEMENTATION.


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

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CHECK <fs>-datatype = 'CURR' OR <fs>-datatype = 'QUAN'.
      <fs>-no_zero = 'X'.
    ENDLOOP.

  ENDMETHOD.


  METHOD data_changed_finished.
  ENDMETHOD.


  METHOD disable_alv.
    mo_alv->set_frontend_layout( is_layout = VALUE lvc_s_layo( edit = '' ) ).
  ENDMETHOD.


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


  METHOD double_click.
  ENDMETHOD.


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


  METHOD get_layout.
    return-zebra = 'X'.
    return-ctab_fname = 'COLOR'.
    return-stylefname = 'STYLE'.
  ENDMETHOD.


  METHOD get_selected_lines.
    mo_alv->get_selected_rows( IMPORTING et_index_rows = return ).
  ENDMETHOD.


  METHOD get_sort.
  ENDMETHOD.


  METHOD get_structure_name.

    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                   <fs_comp>  TYPE abap_componentdescr.

    DATA: lr_line   TYPE REF TO data,
          lo_sdescr TYPE REF TO cl_abap_structdescr.

    ASSIGN ir_table->* TO <fs_table>.
    CREATE DATA lr_line LIKE LINE OF <fs_table>.

    lo_sdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_line ).
    return = lo_sdescr->get_relative_name( ).

  ENDMETHOD.


  METHOD hotspot_click.
  ENDMETHOD.


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


  METHOD menu_button.
  ENDMETHOD.


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


  METHOD toolbar.
  ENDMETHOD.


  METHOD user_command.
  ENDMETHOD.
ENDCLASS.
