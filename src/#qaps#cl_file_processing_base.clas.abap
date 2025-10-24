CLASS /qaps/cl_file_processing_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS execute
      IMPORTING
        !is_simulacao TYPE /qaps/s_simulacao OPTIONAL
        !ir_data      TYPE REF TO data
      RETURNING
        VALUE(return) TYPE REF TO data
      RAISING
        /qaps/cx_pricing_error .
  PROTECTED SECTION.

    DATA mr_result TYPE REF TO data .
    DATA mv_result TYPE abap_bool .
    DATA mt_mapping TYPE /qaps/t_file_mapping .

    METHODS export_log
      IMPORTING
        !ir_data TYPE REF TO data .
    METHODS prepare_catalog .
    METHODS process_sheet
      IMPORTING
        !ir_line TYPE REF TO data
      RAISING
        /qaps/cx_pricing_error .
    METHODS processing .
    METHODS pre_processing .
    METHODS show_log
      RETURNING
        VALUE(return) TYPE abap_bool .
    METHODS mapping_columns
      IMPORTING
        !ir_data               TYPE REF TO data
        !is_target_type_struct TYPE typename .
    METHODS generic_to_typed_data
      IMPORTING
        !ir_data TYPE REF TO data .
  PRIVATE SECTION.
ENDCLASS.



CLASS /qaps/cl_file_processing_base IMPLEMENTATION.


  METHOD execute.

    "Mapear colunas
    mapping_columns( ir_data = ir_data
                     is_target_type_struct = '/QAPS/S_FILE_CST_ELEM_LOG' ).

    "Populate Data
    generic_to_typed_data( ir_data ).

    "Processar dados
    pre_processing( ).

    "Exibir dados a efetivar
    DATA(lv_return) = show_log( ).

    IF lv_return = abap_true.
      processing( ).
    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD export_log.

*    DATA lt_download TYPE /qaps/t_file_upload_multitab.
*
*    APPEND INITIAL LINE TO lt_download ASSIGNING FIELD-SYMBOL(<fs_download>).
*    <fs_download>-sheet_name = 'LOG'.
*    <fs_download>-data = ir_data.

    TRY.
        DATA(lo_file) = NEW /qaps/cl_helper_file( ).
        lo_file->file_download(
          EXPORTING
            ir_data             =  ir_data   " QAPS: File Upload
            iv_filename         = `LOG_` && sy-datum && `_` && sy-uzeit
            iv_call_type        = if_fdt_doc_spreadsheet=>gc_call_message_area
        ).
*          CATCH /qaps/cx_file_error.    "
*        lo_file->file_download_multi_tab( it_data = lt_download
*                                          iv_filename = `LOG_` && sy-datum && `_` && sy-uzeit ).
      CATCH /qaps/cx_file_error.    "
    ENDTRY.

  ENDMETHOD.


  METHOD generic_to_typed_data.
  ENDMETHOD.


  METHOD mapping_columns.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <ft>.

    "header
    LOOP AT <ft> ASSIGNING FIELD-SYMBOL(<fs>).

      CHECK sy-tabix = 1.

      DATA(lo_desc_src) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( <fs> ) ).
      DATA(lo_desc_trg) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( is_target_type_struct ) ).

      DATA(lt_components_trg) = lo_desc_trg->get_components( ).

      LOOP AT lo_desc_src->get_components( ) INTO DATA(ls_components_src).

*        ASSIGN COMPONENT ls_components-name OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_SRC>).

*        CHECK <fv> IS ASSIGNED.
        DATA(ls_components_trg) = lt_components_trg[ sy-tabix ].

        APPEND VALUE /qaps/s_file_mapping(
            excel_column = ls_components_src-name
            table_column = ls_components_trg-name ) TO mt_mapping.

*        UNASSIGN <fv>.

      ENDLOOP.

      EXIT.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_catalog.
  ENDMETHOD.


  METHOD pre_processing.

    BREAK-POINT.

  ENDMETHOD.


  METHOD processing.
  ENDMETHOD.


  METHOD process_sheet.

    "Mapear colunas
    mapping_columns( ir_data = ir_line
                     is_target_type_struct = VALUE #( ) ).

    "Populate Data
    generic_to_typed_data( ir_line ).

    "Processar dados
    pre_processing( ).



  ENDMETHOD.


  METHOD show_log.



  ENDMETHOD.
ENDCLASS.
