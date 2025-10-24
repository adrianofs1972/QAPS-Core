class /QAPS/CL_FILE_PROCESSING_BASE definition
  public
  abstract
  create public .

public section.

  methods EXECUTE
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO optional
      !IR_DATA type ref to DATA
    returning
      value(RETURN) type ref to DATA
    raising
      /QAPS/CX_PRICING_ERROR .
protected section.

  data MR_RESULT type ref to DATA .
  data MV_RESULT type ABAP_BOOL .
  data MT_MAPPING type /QAPS/T_FILE_MAPPING .

  methods EXPORT_LOG
    importing
      !IR_DATA type ref to DATA .
  methods PREPARE_CATALOG .
  methods PROCESS_SHEET
    importing
      !IR_LINE type ref to DATA
    raising
      /QAPS/CX_PRICING_ERROR .
  methods PROCESSING .
  methods PRE_PROCESSING .
  methods SHOW_LOG
    returning
      value(RETURN) type ABAP_BOOL .
  methods MAPPING_COLUMNS
    importing
      !IR_DATA type ref to DATA
      !IS_TARGET_TYPE_STRUCT type TYPENAME .
  methods GENERIC_TO_TYPED_DATA
    importing
      !IR_DATA type ref to DATA .
private section.
ENDCLASS.



CLASS /QAPS/CL_FILE_PROCESSING_BASE IMPLEMENTATION.


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


  method PREPARE_CATALOG.
  endmethod.


  METHOD pre_processing.

    BREAK-POINT.

  ENDMETHOD.


  method PROCESSING.
  endmethod.


  METHOD process_sheet.

    "Mapear colunas
    mapping_columns( ir_data = ir_line
                     is_target_type_struct = value #( ) ).

    "Populate Data
    generic_to_typed_data( ir_line ).

    "Processar dados
    pre_processing( ).



  ENDMETHOD.


  METHOD show_log.



  ENDMETHOD.
ENDCLASS.
