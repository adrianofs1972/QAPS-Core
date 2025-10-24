FUNCTION fm_file_upload_parallel.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_FILE_DATA) TYPE  /QAPS/S_FILE_DATA
*"  EXPORTING
*"     VALUE(CT_FILE_UPLOAD) TYPE  /QAPS/T_FILE_UPLOAD_XML_DATA
*"----------------------------------------------------------------------

  DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .
  DATA : lv_filename      TYPE string,
         lv_headerxstring TYPE xstring.

  FIELD-SYMBOLS <gt_data> TYPE ANY TABLE.



*  DATA(ls_file_data) = get_file_data( lv_filename ).

  TRY .
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet( document_name = VALUE #( ) "lv_filename
                                                xdocument     = is_file_data-header ) .

      lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
        IMPORTING
          worksheet_names = DATA(lt_worksheets) ).

      IF NOT lt_worksheets IS INITIAL.
*          BREAK c060863.
        LOOP AT lt_worksheets INTO DATA(lv_woksheetname).

          APPEND INITIAL LINE TO ct_file_upload ASSIGNING FIELD-SYMBOL(<fs_return>).
          <fs_return>-sheet_name = lv_woksheetname.

          DATA(lr_data) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_woksheetname ).

          <fs_return>-xml = /qaps/cl_serialization=>serialize( lr_data ).
          data(lo_table_type) = cast cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( lr_data ) ).
          <fs_return>-type = /qaps/cl_serialization=>serialize( ref #( lo_table_type ) ).

        ENDLOOP.

      ENDIF.
*      CATCH cx_fdt_excel.
*        BREAK-POINT.
    CATCH cx_fdt_excel_core INTO DATA(lx_excep).
      DATA(lv_message) = lx_excep->get_text( ).
      MESSAGE lv_message TYPE 'E'.
    CLEANUP.
      FREE lo_excel_ref.
  ENDTRY .

ENDFUNCTION.
