class /QAPS/CL_HELPER definition
  public
  final
  create public .

public section.

  class-methods QUESTION
    importing
      !IV_TITLE type CHAR30
      !IV_QUESTION type BAPI_MSG
    returning
      value(RETURN) type ABAP_BOOL .
  class-methods GET_FORMATTED_MESSAGE
    returning
      value(RETURN) type STRING .
  class-methods GET_GRUPO_APS_DESCRIPTION
    importing
      !IV_GRKEY type /QAPS/ZMIGRKEY
    returning
      value(RETURN) type /QAPS/ZMIGRDES .
  class-methods GET_MARC
    importing
      !IV_MATNR type MATNR
      !IV_WERKS type WERKS_D
    returning
      value(RETURN) type MARC .
  class-methods GET_MATERIAL_DESCRIPTION
    importing
      !IV_MATNR type MATNR
    returning
      value(RETURN) type MAKTX .
  class-methods GET_VENDOR_DESCRIPTION
    importing
      !IV_LIFNR type LIFNR
    returning
      value(RETURN) type NAME1_GP .
  class-methods NUMBER_TREATMENT
    importing
      !IV_NUMBER type STRING
    returning
      value(RETURN) type MENGE_D .
  class-methods ROUND_WITH_LIMIT
    importing
      !IV_VALUE type /QAPS/ZMIDFGET-/QAPS/GEMNG1
    returning
      value(RETURN) type CHAR10 .
  class-methods GET_MAIL_FROM_USER
    importing
      !IV_UNAME type UNAME
    returning
      value(RETURN) type ADR6-SMTP_ADDR .
  class-methods SEND_MAIL
    importing
      !IV_SUBJECT type SO_OBJ_DES
      !IT_BODY type BCSY_TEXT
      !IV_SENDER type ADR6-SMTP_ADDR
      !IV_TYPE type SO_OBJ_TP default 'RAW'
      !IT_MAIL_TO type PIQT_MAILADDR
      !IT_MAIL_TO_CC type PIQT_MAILADDR optional
      !IT_MAIL_TO_BCC type PIQT_MAILADDR optional
      !IV_COMMIT type ABAP_BOOL default ABAP_TRUE .
  class-methods SEND_MAIL_WITH_ATTACHMENTS .
  class-methods ADD_TIME_TO_DATE
    importing
      !IV_DATUM type SY-DATUM
      !IV_UZEIT type SY-UZEIT
      !IV_HOUR_ADD type I
    exporting
      !EV_DATUM type SY-DATUM
      !EV_UZEIT type SY-UZEIT .
  class-methods MATERIAL_INPUT
    importing
      !IV_MATNR type MATNR
    returning
      value(RETURN) type MATNR .
  class-methods FORMAT_DATE
    importing
      !IV_DATUM type DATUM
    returning
      value(RETURN) type CHAR10 .
  class-methods IS_PRODUCAO_INTERNA
    importing
      !IV_MATNR type MATNR
      !IV_WERKS type WERKS_D
    returning
      value(RETURN) type ABAP_BOOL .
  class-methods GET_MATERIAL_TYPE
    importing
      !IV_MATNR type MATNR
    returning
      value(RETURN) type MTART .
protected section.
private section.

  class-methods GET_USER_DATA
    importing
      !IV_UNAME type UNAME optional
    exporting
      !ES_DESCRIPTION type BAPIUSDESC
      !ES_LOGONDATA type BAPILOGOND
      !ES_DEFAULTS type BAPIDEFAUL
      !ES_ADDRESS type BAPIADDR3
      !ES_COMPANY type BAPIUSCOMP
      !ES_SNC type BAPISNCU
      !ES_REF_USER type BAPIREFUS
      !ES_ALIAS type BAPIALIAS
      !ES_UCLASS type BAPIUCLASS
      !ES_LASTMODIFIED type BAPIMODDAT
      !ES_ISLOCKED type BAPISLOCKD
      !ES_IDENTITY type BAPIIDENTITY
      !ES_ADMINDATA type BAPIUSERADMIN .
ENDCLASS.



CLASS /QAPS/CL_HELPER IMPLEMENTATION.


  METHOD add_time_to_date.

    DATA: lv_seconds TYPE p,
          lv_datum   TYPE sy-datum,
          lv_uzeit   TYPE sy-uzeit.

    lv_seconds = iv_hour_add * 3600.

    CALL FUNCTION 'C14Z_CALC_DATE_TIME'
      EXPORTING
        i_add_seconds = lv_seconds
        i_uzeit       = iv_uzeit
        i_datum       = iv_datum
      IMPORTING
        e_datum       = lv_datum
        e_uzeit       = lv_uzeit.

    ev_datum = lv_datum.
    ev_uzeit = lv_uzeit.

  ENDMETHOD.


  method FORMAT_DATE.

     CONCATENATE iv_datum+6(2) iv_datum+4(2) iv_datum(4)
        into return
        SEPARATED BY '.'.

  endmethod.


  METHOD get_formatted_message.

    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = sy-msgid
*       LANG      = '-D'
        no        = sy-msgno
        v1        = sy-msgv1
        v2        = sy-msgv2
        v3        = sy-msgv3
        v4        = sy-msgv4
      IMPORTING
        msg       = return
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

  ENDMETHOD.


  METHOD get_grupo_aps_description.

    SELECT SINGLE /qaps/grdes
      FROM /qaps/zmig0
      WHERE /qaps/grkey = @iv_grkey
      INTO @return.

  ENDMETHOD.


  METHOD get_mail_from_user.

    get_user_data(
      EXPORTING
        iv_uname        = iv_uname    " Nome do usuário
      IMPORTING
*        es_description  =     " User: Transfer Structure Description
*        es_logondata    =     " Usuário: estrut.transferênc.dds.logon
*        es_defaults     =     " Usuário: estrut.transferênc.val.fixo
        es_address      = DATA(ls_address)    " Estrutura de referência BAPI p/endereços (pessoa de contato)
*        es_company      =     " Firma à qual está atribuído um usuário
*        es_snc          =     " Características SNC de um usuário
*        es_ref_user     =     " Nome do usuário
*        es_alias        =     " Alias para nome de usuário
*        es_uclass       =     " Classificação do usuário relativa à licença
*        es_lastmodified =     " Usuário: última modificação (data e hora)
*        es_islocked     =     " Bloqueio do usuário
*        es_identity     =     " Person Assignment of an Identity
*        es_admindata    =  DATA(LS_ADMINDATA)   " User: Administration Data
    ).

    return = ls_address-e_mail.

  ENDMETHOD.


  METHOD get_marc.

    SELECT SINGLE *
      FROM marc
      WHERE matnr = @iv_matnr
      AND   werks = @iv_werks
      INTO @return.

  ENDMETHOD.


  method GET_MATERIAL_DESCRIPTION.

    data lv_matnr type matnr.

    lv_matnr = |{ iv_matnr ALPHA = IN WIDTH = 18 }|.

    select single maktx
      from makt
      where matnr = @lv_matnr
      and spras = @sy-langu
      into @return.

  endmethod.


  METHOD get_material_type.

    DATA lv_matnr TYPE matnr.

    lv_matnr = material_input( iv_matnr ).

    SELECT SINGLE mtart
      INTO return
      FROM mara
      WHERE matnr = lv_matnr.

  ENDMETHOD.


  METHOD get_user_data.

    DATA lt_return TYPE bapiret2_t.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username     = iv_uname
*       CACHE_RESULTS        = 'X'
      IMPORTING
        logondata    = es_logondata
        defaults     = es_defaults
        address      = es_address
        company      = es_company
        snc          = es_snc
        ref_user     = es_ref_user
        alias        = es_alias
        uclass       = es_uclass
        lastmodified = es_lastmodified
        islocked     = es_islocked
        identity     = es_identity
        admindata    = es_admindata
        description  = es_description
      TABLES
*       PARAMETER    =
*       PROFILES     =
*       ACTIVITYGROUPS       =
        return       = lt_return
*       ADDTEL       =
*       ADDFAX       =
*       ADDTTX       =
*       ADDTLX       =
*       ADDSMTP      =
*       ADDRML       =
*       ADDX400      =
*       ADDRFC       =
*       ADDPRT       =
*       ADDSSF       =
*       ADDURI       =
*       ADDPAG       =
*       ADDCOMREM    =
*       PARAMETER1   =
*       GROUPS       =
*       UCLASSSYS    =
*       EXTIDHEAD    =
*       EXTIDPART    =
*       SYSTEMS      =
      .


  ENDMETHOD.


  METHOD get_vendor_description.

    DATA lv_lifnr TYPE lifnr.

    lv_lifnr = |{ iv_lifnr ALPHA = IN WIDTH = 10 }|.

    SELECT SINGLE name1
      FROM lfa1
      WHERE lifnr = @lv_lifnr
      into @return.

  ENDMETHOD.


  method IS_PRODUCAO_INTERNA.
  endmethod.


  METHOD material_input.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = iv_matnr
      IMPORTING
        output = return
*   EXCEPTIONS
*       LENGTH_ERROR       = 1
*       OTHERS = 2
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD number_treatment.

    DATA: lv_number TYPE cha_class_data-sollwert,
          lv_result TYPE cha_class_view-sollwert.

    DATA : lv_numberofcolumns   TYPE i,
           lv_date_string       TYPE string,
           lv_target_date_field TYPE datum,
           lv_dec_val           TYPE p DECIMALS 5,
           lv_int_exp           TYPE i.

    SPLIT iv_number AT 'E-' INTO DATA(lv_inteiro) DATA(lv_expoente).
    SPLIT lv_inteiro AT '.' INTO DATA(lv_int_part) DATA(lv_decimals).

    DATA(lv_strlen) = strlen( lv_decimals ).

    IF lv_strlen < 5.
      lv_number = lv_inteiro && `E-` && lv_expoente.
    ELSE.
      lv_number = lv_inteiro(5) && `E-` && lv_expoente.
    ENDIF.

    CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
      EXPORTING
        i_number_of_digits = 3
        i_fltp_value       = lv_number
*       I_VALUE_NOT_INITIAL_FLAG       = 'X'
*       I_SCREEN_FIELDLENGTH           = 16
      IMPORTING
        e_char_field       = lv_result.


*    BREAK-POINT.
    TRANSLATE lv_result USING ',.'.
    return = lv_result.


  ENDMETHOD.


  METHOD question.

    DATA lv_answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = iv_question
        display_cancel_button = ''
      IMPORTING
        answer                = lv_answer
*     TABLES
*       PARAMETER             =
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF lv_answer = '1'.
      return = abap_true.
    ELSE.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD round_with_limit.

    DATA lv_var1 TYPE p DECIMALS 0.

    lv_var1 = iv_value.

    return = lv_var1 + '0.999'.
    translate return USING ',.'.
    condense return NO-GAPS.

    return = `'` && return && `'`.

  ENDMETHOD.


  METHOD send_mail.

    DATA: lo_send_request  TYPE REF TO cl_bcs,
          lo_document      TYPE REF TO cl_document_bcs,
          lo_sender        TYPE REF TO cl_cam_address_bcs,
          lo_recipient_to  TYPE REF TO cl_cam_address_bcs,
          lo_recipient_cc  TYPE REF TO cl_cam_address_bcs,
          lo_recipient_bcc TYPE REF TO cl_cam_address_bcs,
          lo_bcs_exception TYPE REF TO cx_bcs.

    TRY.

        lo_send_request = cl_bcs=>create_persistent( ).


        lo_document = cl_document_bcs=>create_document(
         i_type = iv_type
         i_text = it_body
         i_subject = iv_subject ).

        lo_send_request->set_document( lo_document ).

        lo_sender = cl_cam_address_bcs=>create_internet_address( iv_sender ).
        lo_send_request->set_sender( lo_sender ).

        LOOP AT it_mail_to INTO DATA(lv_mail).
          lo_recipient_to = cl_cam_address_bcs=>create_internet_address( lv_mail ).
          lo_send_request->add_recipient( i_recipient = lo_recipient_to ).
        ENDLOOP.

        LOOP AT it_mail_to INTO lv_mail.
          lo_recipient_cc = cl_cam_address_bcs=>create_internet_address( lv_mail ).
          lo_send_request->add_recipient( i_recipient = lo_recipient_cc
          i_copy = 'X' ).
        ENDLOOP.

        LOOP AT it_mail_to INTO lv_mail.
          lo_recipient_bcc = cl_cam_address_bcs=>create_internet_address( lv_mail ).
          lo_send_request->add_recipient( i_recipient = lo_recipient_bcc
           i_blind_copy = 'X' ).
        ENDLOOP.

        DATA(lv_sent_to_all) = lo_send_request->send( ).
        IF lv_sent_to_all = 'X'.
*          WRITE 'Email sent to all recipients'.
        ELSE.
*          WRITE 'Email could not be sent to all recipients!'.
        ENDIF.

        IF iv_commit = abap_true.
          COMMIT WORK.
        ENDIF.

      CATCH cx_bcs INTO lo_bcs_exception.

*        WRITE: 'Error occurred while sending email: Error Type', bcs_exception->error_type.

    ENDTRY.
  ENDMETHOD.


  method SEND_MAIL_WITH_ATTACHMENTS.

*&---------------------------------------------------------------------*
*& Data Declaration
*&---------------------------------------------------------------------*
DATA : lo_mime_helper TYPE REF TO cl_gbt_multirelated_service,
       lo_bcs         TYPE REF TO cl_bcs,
       lo_doc_bcs     TYPE REF TO cl_document_bcs,
       lo_recipient   TYPE REF TO if_recipient_bcs,
       lt_soli        TYPE TABLE OF soli,
       ls_soli        TYPE soli,
       lv_status      TYPE bcs_rqst.

*&---------------------------------------------------------------------*
*& Creation of the mail
*&---------------------------------------------------------------------*

" Create the main object of the mail.
CREATE OBJECT lo_mime_helper.

" Create the mail content.-----"CLASSIC WAY"
*ls_soli-line = '<!DOCTYPE html PUBLIC “-//IETF//DTD HTML 5.0//EN">'.
*APPEND ls_soli TO lt_soli.

*ls_soli-line = '<HTML>'.
*APPEND ls_soli TO lt_soli.

*ls_soli-line = '<BODY>'.
*APPEND ls_soli TO lt_soli.

*ls_soli-line = 'Hi Dear,<P>Content Section!</P>'.
*APPEND ls_soli TO lt_soli.

*ls_soli-line = '</BODY>'.
*APPEND ls_soli TO lt_soli.

*ls_soli-line = '</HTML>'.
*APPEND ls_soli TO lt_soli.

" Create the mail content.-----"NEW WAY"
DATA(string) = '<!DOCTYPE html PUBLIC “-//IETF//DTD HTML 5.0//EN">'
            && '<HTML><BODY>Hi Dear,<P>Content Section!</P></BODY></HTML>'.

lt_soli = CL_DOCUMENT_BCS=>STRING_TO_SOLI( string ).

" Set the HTML body of the mail
CALL METHOD lo_mime_helper->set_main_html
  EXPORTING
    content     = lt_soli
    description = 'Test Email'.

* Set the subject of the mail.
lo_doc_bcs = cl_document_bcs=>create_from_multirelated(
                i_subject          = 'Subject of our email'
                i_importance       = '9'                " 1~High Priority  5~Average priority 9~Low priority
                i_multirel_service = lo_mime_helper ).

lo_bcs = cl_bcs=>create_persistent( ).

lo_bcs->set_document( i_document = lo_doc_bcs ).

* Set the email address
lo_recipient = cl_cam_address_bcs=>create_internet_address(
                  i_address_string =  'test@test12.com' ).

lo_bcs->add_recipient( i_recipient = lo_recipient ).

* Change the status.
lv_status = 'N'.
CALL METHOD lo_bcs->set_status_attributes
  EXPORTING
    i_requested_status = lv_status.

*&---------------------------------------------------------------------*
*& Send the email
*&---------------------------------------------------------------------*
TRY.
  lo_bcs->send( ).
  COMMIT WORK.
CATCH cx_bcs INTO DATA(lx_bcs).
  ROLLBACK WORK.
ENDTRY.

  endmethod.
ENDCLASS.
