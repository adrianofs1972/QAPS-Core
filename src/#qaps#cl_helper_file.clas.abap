class /QAPS/CL_HELPER_FILE definition
  public
  final
  create public .

public section.

  data MT_FILE_XML_CONTENT type /QAPS/T_FILE_UPLOAD_XML_DATA .

  methods ON_RECEIVE_FILE_XML
    importing
      !P_TASK type CLIKE .
  methods FILE_DOWNLOAD_MULTI_TAB
    importing
      value(IT_DATA) type /QAPS/T_FILE_UPLOAD_MULTITAB
      !IV_FILENAME type STRING default 'Export'
    raising
      /QAPS/CX_FILE_ERROR .
  methods FILE_DOWNLOAD
    importing
      value(IR_DATA) type ref to DATA
      !IV_FILENAME type STRING default 'Export'
      !IV_CALL_TYPE type I default 4
    raising
      /QAPS/CX_FILE_ERROR .
  methods FILE_UPLOAD_PARALLEL_PROCESS
    importing
      !IV_FILENAME type STRING optional
    returning
      value(RETURN) type /QAPS/T_FILE_UPLOAD
    raising
      /QAPS/CX_FILE_ERROR .
  methods FILE_UPLOAD
    importing
      !IV_FILENAME type STRING optional
    returning
      value(RETURN) type /QAPS/T_FILE_UPLOAD
    raising
      /QAPS/CX_FILE_ERROR .
protected section.
private section.

  data MV_EXECUTED type ABAP_BOOL .

  methods FILE_OPEN
    returning
      value(RETURN) type STRING
    raising
      /QAPS/CX_FILE_ERROR .
  methods GET_FILE_DATA
    importing
      !IV_FILENAME type STRING
    returning
      value(RETURN) type /QAPS/S_FILE_DATA .
ENDCLASS.



CLASS /QAPS/CL_HELPER_FILE IMPLEMENTATION.


  METHOD file_download.

    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet,
           lt_columns   TYPE if_fdt_doc_spreadsheet=>t_column,
           lr_line      TYPE REF TO data.

    DATA: lv_action   TYPE i,
          lv_filename TYPE string,
          lv_fullpath TYPE string,
          lv_path     TYPE string.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <ft>.

    "Colunas
    DATA(lo_table) = cl_abap_tabledescr=>describe_by_data( <ft> ).
    CREATE DATA lr_line LIKE LINE OF  <ft>.
    DATA(lo_desc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( lr_line ) ).

    DATA(lt_components) = lo_desc->get_components( ).

    IF line_exists( lt_components[ as_include = 'X' ] ).
      DATA(lo_struct) = CAST cl_abap_structdescr( lt_components[ as_include = 'X' ]-type ).
      LOOP AT lo_desc->get_components( ) INTO DATA(ls_comp_include).
        CHECK ls_comp_include-as_include = 'X'.
        DATA(lo_struct_include) = CAST cl_abap_structdescr( ls_comp_include-type ).
        DATA(lt_comp_include) = lo_struct_include->get_components( ).
      ENDLOOP.

    ENDIF.

    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<fs_column>).

      CHECK <fs_column> IS ASSIGNED.
      IF <fs_column>-as_include IS INITIAL.
        CHECK NOT line_exists( lt_columns[ name = <fs_column>-name ] ).

        DATA(lv_lines) = lines( lt_columns ) + 1.
        APPEND VALUE if_fdt_doc_spreadsheet=>s_column(
            id           = lv_lines
            name         = <fs_column>-name
            display_name = <fs_column>-name
            is_result    = abap_true
            type         = <fs_column>-type ) TO lt_columns.
      ELSE.
        LOOP AT lt_comp_include ASSIGNING FIELD-SYMBOL(<fs_comp_include>).
          CHECK <fs_comp_include>-as_include IS INITIAL.
          CHECK NOT line_exists( lt_columns[ name         = <fs_comp_include>-name ] ).

          lv_lines = lines( lt_columns ) + 1.
          APPEND VALUE if_fdt_doc_spreadsheet=>s_column(
            id           = lv_lines
            name         = <fs_comp_include>-name
            display_name = <fs_comp_include>-name
            is_result    = abap_true
            type         = <fs_comp_include>-type ) TO lt_columns.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    TRY.
        /qaps/cl_fdt_xl_spreadsheet=>set_version( iv_version = '2' ).
        DATA(lv_content) = /qaps/cl_fdt_xl_spreadsheet=>if_fdt_doc_spreadsheet~create_document(
                                            columns            = lt_columns
                                            itab               = ir_data
                                            iv_call_type       = iv_call_type "if_fdt_doc_spreadsheet=>gc_call_trace
                                            ).

      CATCH cx_fdt_excel_core.
    ENDTRY.

    IF xstrlen( lv_content ) > 0.

      cl_gui_frontend_services=>file_save_dialog(
        EXPORTING
          window_title              = 'Exportação'
          default_extension         = 'xlsx'
          default_file_name         = iv_filename
          file_filter               = |Excel File (*.xlsx)\|*.xlsx|
          prompt_on_overwrite       = 'X'
        CHANGING
          filename                  = lv_filename    " File Name to Save
          path                      = lv_path
          fullpath                  = lv_fullpath
          user_action               = lv_action
        EXCEPTIONS
          cntl_error                = 1
          error_no_gui              = 2
          not_supported_by_gui      = 3
          invalid_default_file_name = 4
          OTHERS                    = 5
      ).

      IF lv_action = cl_gui_frontend_services=>action_ok.

        DATA(lt_raw_data) = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_content ).
        cl_gui_frontend_services=>gui_download( EXPORTING filename = lv_fullpath
                                                          filetype = 'BIN'
                                                          bin_filesize = xstrlen( lv_content )
                                                CHANGING data_tab = lt_raw_data ).


      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD file_download_multi_tab.

    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet,
           lt_columns   TYPE if_fdt_doc_spreadsheet=>t_column,
           lr_line      TYPE REF TO data.

    DATA: lv_action   TYPE i,
          lv_filename TYPE string,
          lv_fullpath TYPE string,
          lv_path     TYPE string.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      <fs_data>-sheet_id = sy-tabix.

      REFRESH lt_columns.

      ASSIGN <fs_data>-data->* TO <ft>.

      "Colunas
      DATA(lo_table) = cl_abap_tabledescr=>describe_by_data( <ft> ).
      CREATE DATA lr_line LIKE LINE OF  <ft>.
      DATA(lo_desc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( lr_line ) ).

      LOOP AT lo_desc->get_components( ) ASSIGNING FIELD-SYMBOL(<fs_column>).

        CHECK <fs_column> IS ASSIGNED.
        DATA(lv_name) = <fs_column>-type->get_relative_name( ).

        IF lines( <fs_data>-display_name ) > 0.

          DATA(ls_display) = VALUE #( <fs_data>-display_name[ name = <fs_column>-name ] OPTIONAL ).
          data(lv_display_name) = ls_display-display_name.

          IF lv_display_name IS INITIAL.
            lv_display_name = <fs_column>-name.
          ENDIF.

          DATA(lv_width) = ls_display-width.

        ELSE.
          lv_display_name = <fs_column>-name.
        ENDIF.

        if lv_width is INITIAL.
          if <fs_column>-type->length < 255.
            lv_width = <fs_column>-type->length.
          else.
            lv_width = 255.
          endif.
        endif.

        APPEND VALUE /qaps/s_column(
            id           = sy-tabix
            name         = <fs_column>-name
            display_name = lv_display_name
            is_result    = abap_true
            type         = <fs_column>-type
            width        = lv_width  ) TO <fs_data>-columns.
      ENDLOOP.

    ENDLOOP.

    TRY.

        DATA(lv_content) = /qaps/cl_fdt_xl_spreadsheet=>create_document_multi_tab(
                                            it_data            = it_data
                                            columns            = lt_columns
*                                            itab               = ir_data "REF #( it_data )
                                            is_alv_document    = abap_true
                                            iv_call_type       = if_fdt_doc_spreadsheet=>gc_call_trace
                                            ).

      CATCH cx_fdt_excel_core.
    ENDTRY.

    IF xstrlen( lv_content ) > 0.

      cl_gui_frontend_services=>file_save_dialog(
        EXPORTING
          window_title              = 'Exportação'
          default_extension         = 'xlsx'
          default_file_name         = iv_filename
          file_filter               = |Excel File (*.xlsx)\|*.xlsx|
          prompt_on_overwrite       = 'X'
        CHANGING
          filename                  = lv_filename    " File Name to Save
          path                      = lv_path
          fullpath                  = lv_fullpath
          user_action               = lv_action
        EXCEPTIONS
          cntl_error                = 1
          error_no_gui              = 2
          not_supported_by_gui      = 3
          invalid_default_file_name = 4
          OTHERS                    = 5
      ).

      IF lv_action = cl_gui_frontend_services=>action_ok.

        DATA(lt_raw_data) = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_content ).
        cl_gui_frontend_services=>gui_download( EXPORTING filename = lv_fullpath
                                                          filetype = 'BIN'
                                                          bin_filesize = xstrlen( lv_content )
                                                CHANGING data_tab = lt_raw_data ).


      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD file_open.

    DATA : lv_filename      TYPE string,
           lt_records       TYPE solix_tab,
           lv_headerxstring TYPE xstring,
           lv_filelength    TYPE i,
           lv_rc            TYPE i.

    FIELD-SYMBOLS <gt_data> TYPE ANY TABLE.

    DATA lt_filetable TYPE filetable.
    DATA lv_user_action TYPE i.
    DATA lv_enconding TYPE abap_encoding.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Escolher Arquivo'
        default_extension       = '*.xlsx'   " Default Extension
        default_filename        = '*.xlsx'    " Default File Name
        file_filter             = '*.xlsx'    " File Extension Filter String
*        with_encoding           =     " File Encoding
*        initial_directory       =     " Initial Directory
*        multiselection          =     " Multiple selections poss.
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc
        user_action             = lv_user_action
        file_encoding           = lv_enconding
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).

    IF lv_user_action = cl_gui_frontend_services=>action_ok.
      return = lt_filetable[ 1 ].
    ELSE.
      RAISE EXCEPTION TYPE /qaps/cx_file_error
        EXPORTING
          message = 'Operação cancelada pelo usuário'.
    ENDIF.

  ENDMETHOD.


  METHOD file_upload.

    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .
    DATA : lv_filename      TYPE string,
           lv_headerxstring TYPE xstring.

    FIELD-SYMBOLS <gt_data> TYPE ANY TABLE.

    IF iv_filename IS INITIAL.
      lv_filename = file_open( ).
    ELSE.
      lv_filename = iv_filename.
    ENDIF.

    DATA(ls_file_data) = get_file_data( lv_filename ).

    TRY .
        lo_excel_ref = NEW cl_fdt_xl_spreadsheet( document_name = lv_filename
                                                  xdocument     = ls_file_data-header ) .

        lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
          IMPORTING
            worksheet_names = DATA(lt_worksheets) ).

        IF NOT lt_worksheets IS INITIAL.
*          BREAK c060863.
          LOOP AT lt_worksheets INTO DATA(lv_woksheetname).

            APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs_return>).
            <fs_return>-sheet_name = lv_woksheetname.
            <fs_return>-data = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                                    lv_woksheetname ).
          ENDLOOP.

        ENDIF.
      CATCH cx_sy_generate_subpool_full.
        BREAK-POINT.
      CATCH cx_fdt_excel_core INTO DATA(lx_excep).
        DATA(lv_message) = lx_excep->get_text( ).
        MESSAGE lv_message TYPE 'E'.
      CLEANUP.
        FREE lo_excel_ref.
    ENDTRY .

  ENDMETHOD.


  METHOD file_upload_parallel_process.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    DATA : lo_excel_ref   TYPE REF TO cl_fdt_xl_spreadsheet,
           lo_table_descr TYPE REF TO cl_abap_tabledescr,
           lr_data type ref to data.

    DATA : lv_filename      TYPE string,
           lv_headerxstring TYPE xstring.

    FIELD-SYMBOLS <gt_data> TYPE ANY TABLE.

    IF iv_filename IS INITIAL.
      lv_filename = file_open( ).
    ELSE.
      lv_filename = iv_filename.
    ENDIF.

    DATA(ls_file_data) = get_file_data( lv_filename ).

    CALL FUNCTION 'FM_FILE_UPLOAD_PARALLEL'
      DESTINATION 'NONE'
      STARTING NEW TASK 'TSK_FILE'
      CALLING on_receive_file_xml ON END OF TASK
      EXPORTING
        is_file_data          = ls_file_data
      EXCEPTIONS " failure when calling RFC
        communication_failure = 1
        system_failure        = 2
        resource_failure      = 3.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL mv_executed = abap_true.

    BREAK-POINT.
    LOOP AT mt_file_xml_content INTO DATA(ls_file_xml_content).

      APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-sheet_name = ls_file_xml_content-sheet_name.

      ASSIGN <fs>-data->* TO <ft>.
*      lo_table_descr = new cl_abap_tabledescr( ).
      lr_data = ref #( lo_table_descr ).
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ls_file_xml_content-type
                                           CHANGING  cr_data = lr_data ).

*      <fs>-data

    ENDLOOP.
*    return = mt_file_xml_content.

  ENDMETHOD.


  METHOD get_file_data.

    DATA :
      lt_records       TYPE solix_tab,
      lv_headerxstring TYPE xstring,
      lv_filelength    TYPE i,
      lv_rc            TYPE i.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = iv_filename
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_filelength
        header                  = lv_headerxstring
      TABLES
        data_tab                = lt_records
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.

    IF sy-subrc NE 0.
      DATA(lv_message) = /qaps/cl_helper=>get_formatted_message( ).
      RAISE EXCEPTION TYPE /qaps/cx_file_error
        EXPORTING
          message = lv_message.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = lv_headerxstring
      TABLES
        binary_tab   = lt_records
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc NE 0.
      lv_message = /qaps/cl_helper=>get_formatted_message( ).
      RAISE EXCEPTION TYPE /qaps/cx_file_error
        EXPORTING
          message = lv_message.
    ENDIF.

    return-header = lv_headerxstring.
    return-data = lt_records.

  ENDMETHOD.


  METHOD on_receive_file_xml.

    RECEIVE RESULTS FROM FUNCTION 'FM_FILE_UPLOAD_PARALLEL'
      IMPORTING
        ct_file_upload = mt_file_xml_content
      EXCEPTIONS
        resource_failure      = 1
        system_failure        = 2
        communication_failure = 3.

    mv_executed = abap_true.

  ENDMETHOD.
ENDCLASS.
