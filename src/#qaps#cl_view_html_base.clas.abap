class /QAPS/CL_VIEW_HTML_BASE definition
  public
  abstract
  create public .

public section.

  types:
    BEGIN OF ts_html,
        line TYPE char255,
      END OF ts_html .
  types:
    tt_html TYPE TABLE OF ts_html .

  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  methods REFRESH .
  methods UPDATE
    importing
      !IR_DATA type ref to DATA
      !IR_ADDTIONAL_DATA type ref to DATA optional .
protected section.

  data MO_HTML type ref to CL_GUI_HTML_VIEWER .

  methods MERGE_DATA_TO_HTML
    importing
      !IR_DATA type ref to DATA
      !IR_ADDTIONAL_DATA type ref to DATA optional
    exporting
      value(ET_HTML) type TT_HTML .
  methods DISPLAY_HTML_DATA
    importing
      value(IT_DATA) type TT_HTML .
  methods GET_HTML_CONTENT
    exporting
      !ET_HTML type TT_HTML .
  methods SAPEVENT
    for event SAPEVENT of CL_GUI_HTML_VIEWER .
  methods CUSTOMIZE_HTML_CONTENT
    changing
      !CT_DATA type TT_HTML .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_HTML_BASE IMPLEMENTATION.


  method CUSTOMIZE_HTML_CONTENT.
  endmethod.


  METHOD display_html_data.

    DATA : lv_url(1024)  TYPE c.

    lv_url = 'tp_lista.htm'.

    mo_html->load_data(
      EXPORTING
        url                    = lv_url
*        type                   = 'text'    " Type of a MIME Object
*        subtype                = 'html'    " Subtype of a MIME Object
*        size                   = 0    " Length of Data
*        encoding               =     " Encoding for MIME Object
*        charset                =     " Encoding for MIME Object
*        needfiltering          = 0    " If it is 1 or 2, content is filtered, else no filter
*        language               =
*        i_tidyt                =     " For special calls only
      IMPORTING
        assigned_url           = lv_url
      CHANGING
        data_table             = it_data    " data table
*        iscontentchanged       =     " Sets to 1 if the content is filtered else it is 0
*      EXCEPTIONS
*        dp_invalid_parameter   = 1
*        dp_error_general       = 2
*        cntl_error             = 3
*        html_syntax_notcorrect = 4
*        others                 = 5
    ).

    mo_html->show_url( EXPORTING url = lv_url ).

  ENDMETHOD.


  METHOD get_html_content.

    data ls_html type ts_html.

    DEFINE add_to_html.
      APPEND &1 TO et_html.
    END-OF-DEFINITION.

    ls_html-line = '<html><body></body></html>'.
    add_to_html ls_html.

  ENDMETHOD.


  METHOD initialize.

    mo_html = NEW cl_gui_html_viewer(
*        shellstyle               =
        parent                   = io_container
*        lifetime                 = LIFETIME_DEFAULT
*        saphtmlp                 =
*        uiflag                   =
*        end_session_with_browser = 0
*        name                     =
*        saphttp                  =
*        query_table_disabled     = ''
    ).

    get_html_content( IMPORTING et_html = DATA(lt_html) ).

    customize_html_content( CHANGING ct_data = lt_html ).

    display_html_data( lt_html ).

  ENDMETHOD.


  method MERGE_DATA_TO_HTML.
  endmethod.


  METHOD refresh.

    DATA lt_html TYPE tt_html.

    APPEND VALUE ts_html( line = '<html><body></body></html>' ) TO lt_html.
    display_html_data( lt_html ).

  ENDMETHOD.


  method SAPEVENT.
  endmethod.


  METHOD update.

    merge_data_to_html( EXPORTING ir_data = ir_data
                                  ir_addtional_data = ir_addtional_data
                        IMPORTING et_html = DATA(lt_html) ).

    display_html_data( lt_html ).

  ENDMETHOD.
ENDCLASS.
