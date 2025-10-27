class /QAPS/CL_HELPER_TEXT definition
  public
  abstract
  final
  create public .

public section.

  class-methods GET_WAERS_TEXT
    importing
      !IV_WAERS type WAERS
    returning
      value(RETURN) type KTEXT_CURT
    raising
      /QAPS/CX_GENERAL .
  class-methods GET_DATA_ELEMENT_TEXT
    importing
      !IV_DATA_ELEMENT type ROLLNAME
    returning
      value(RETURN) type DD04T .
  class-methods GET_DOMAIN_TEXT
    importing
      !IV_DOMAIN type DOMNAME
      !IV_VALUE type DOMVALUE_L
    returning
      value(RETURN) type DDTEXT .
  class-methods GET_DOMAIN_VALUES
    importing
      !IV_DOMAIN type DOMNAME
    returning
      value(RETURN) type DD07VTAB .
  class-methods GET_USER_FULLNAME
    importing
      !IV_USERNAME type XUBNAME
    returning
      value(RETURN) type AD_NAMTEXT .
  class-methods GET_MATERIAL_TEXT
    importing
      !IV_MATNR type MATNR
    returning
      value(RETURN) type MAKTX
    raising
      /QAPS/CX_GENERAL .
  class-methods GET_GRUPO_APS_TEXT
    importing
      !IV_GRKEY type /QAPS/ZMIGRKEY
    returning
      value(RETURN) type /QAPS/ZMIGRDES
    raising
      /QAPS/CX_GENERAL .
  class-methods GET_LIFNR_TEXT
    importing
      !IV_LIFNR type LIFNR
    returning
      value(RETURN) type NAME1_GP
    raising
      /QAPS/CX_GENERAL .
  class-methods GET_KUNNR_TEXT
    importing
      !IV_KUNNR type KUNNR
    returning
      value(RETURN) type NAME1_GP
    raising
      /QAPS/CX_GENERAL .
  class-methods GET_WERKS_TEXT
    importing
      !IV_WERKS type WERKS_D
    returning
      value(RETURN) type NAME1
    raising
      /QAPS/CX_GENERAL .
  class-methods GET_VERSAO_TEXT
    importing
      !IV_VERSAO type /QAPS/ZMIVERSAO
    returning
      value(RETURN) type /QAPS/ZMIDESCRV
    raising
      /QAPS/CX_GENERAL .
  class-methods GET_MEINS_TEXT
    importing
      !IV_MEINS type MEINS
    returning
      value(RETURN) type MSEHT
    raising
      /QAPS/CX_GENERAL .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF ts_material,
      matnr TYPE matnr,
      maktx TYPE maktx,
    END OF ts_material .
  TYPES:
    tt_material TYPE TABLE OF ts_material .

  TYPES:
    BEGIN OF ts_fornecedor,
      lifnr TYPE lifnr,
      name1 TYPE lfa1-name1,
    END OF ts_fornecedor.
  TYPES:
    tt_fornecedor TYPE TABLE OF ts_fornecedor.

  TYPES:
    BEGIN OF ts_cliente,
      kunnr TYPE kunnr,
      name1 TYPE kna1-name1,
    END OF ts_cliente.
  TYPES:
    tt_cliente TYPE TABLE OF ts_cliente.

  CLASS-DATA: mt_material TYPE tt_material,
              mt_fornecedor type tt_fornecedor,
              mt_cliente  type tt_cliente.

  CLASS-METHODS raise_general_exception
    IMPORTING
      !iv_type    TYPE bapi_mtype
      !iv_message TYPE bapi_msg
    RAISING
      /qaps/cx_general .
ENDCLASS.



CLASS /QAPS/CL_HELPER_TEXT IMPLEMENTATION.


  METHOD get_data_element_text.

    SELECT SINGLE *
      FROM dd04t
      WHERE rollname = @iv_data_element
      AND ddlanguage = @sy-langu
      INTO @return.

  ENDMETHOD.


  METHOD get_domain_text.

    CALL FUNCTION 'DOMAIN_VALUE_GET'
      EXPORTING
        i_domname  = iv_domain
        i_domvalue = iv_value
      IMPORTING
        e_ddtext   = return
      EXCEPTIONS
        not_exist  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.


  METHOD get_domain_values.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = iv_domain
*       TEXT       = 'X'
*       FILL_DD07L_TAB        = ' '
      TABLES
        values_tab = return
*       VALUES_DD07L          =
*     EXCEPTIONS
*       NO_VALUES_FOUND       = 1
*       OTHERS     = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD get_grupo_aps_text.

    CHECK NOT iv_grkey IS INITIAL.

    SELECT SINGLE /qaps/grdes
      FROM  /qaps/zmig0
      WHERE /qaps/grkey = @iv_grkey
      INTO @return.

    CHECK sy-subrc NE 0.

    raise_general_exception( iv_type          =  'E'
                             iv_message       =  TEXT-e02 ).

  ENDMETHOD.


  METHOD get_kunnr_text.

    DATA lv_kunnr TYPE kunnr.

    CHECK NOT iv_kunnr IS INITIAL.

    lv_kunnr = |{ iv_kunnr ALPHA = IN WIDTH = 10 }|.

    IF NOT line_exists( mt_cliente[ kunnr = lv_kunnr ] ).

      SELECT SINGLE name1
        FROM  kna1
        WHERE kunnr = @lv_kunnr
        INTO @return.

      IF NOT return IS INITIAL.
        APPEND VALUE ts_cliente( kunnr = lv_kunnr
                                 name1 = return ) TO mt_cliente.
      ENDIF.

    ELSE.
      return =  mt_cliente[ kunnr = lv_kunnr ]-name1.
    ENDIF.

    CHECK sy-subrc NE 0.

    raise_general_exception( iv_type          =  'E'
                             iv_message       =  TEXT-e04 ).

  ENDMETHOD.


  METHOD get_lifnr_text.

    data lv_lifnr type lifnr.

    CHECK NOT iv_lifnr IS INITIAL.

    lv_lifnr = |{ iv_lifnr ALPHA = in WIDTH = 10 }|.

    if not line_exists( mt_fornecedor[ lifnr = lv_lifnr ] ).

    SELECT SINGLE name1
      FROM  lfa1
      WHERE lifnr = @lv_lifnr
      INTO @return.

      if not return is INITIAL.
        append value ts_fornecedor( lifnr = lv_lifnr
                                    name1 = return ) to mt_fornecedor.
      endif.

    else.
      return = mt_fornecedor[ lifnr = lv_lifnr ]-name1.
    endif.

    CHECK return is INITIAL.

    raise_general_exception( iv_type          =  'E'
                             iv_message       =  TEXT-e04 ).

  ENDMETHOD.


  METHOD get_material_text.

    DATA lv_matnr TYPE matnr.

    CHECK NOT iv_matnr IS INITIAL.

    lv_matnr = |{ iv_matnr ALPHA = IN WIDTH = 18 }|.

    IF NOT line_exists( mt_material[ matnr = lv_matnr ] ).

      SELECT SINGLE maktx
        FROM makt
        WHERE matnr = @lv_matnr
        AND   spras = @sy-langu
        INTO @return.

      IF NOT return IS INITIAL.
        APPEND VALUE ts_material( matnr = lv_matnr
                                  maktx = return ) TO mt_material.
      ENDIF.

    ELSE.
      return = mt_material[ matnr = lv_matnr ]-maktx.
    ENDIF.

    CHECK return IS INITIAL.

    raise_general_exception( iv_type          =  'E'
                             iv_message       =  TEXT-e01 ).


  ENDMETHOD.


  METHOD get_meins_text.

    SELECT SINGLE t006~msehi,mseht
      FROM t006
      INNER JOIN t006a
      ON t006~msehi = t006a~msehi
      WHERE spras = @sy-langu
      AND kzkeh   = 'X'
      AND dimid   = 'MASS'
      AND t006~msehi = @iv_meins
      INTO @DATA(ls_data).

    IF sy-subrc IS INITIAL.
      return = ls_data-mseht.
    ELSE.
      raise_general_exception( iv_type          =  'E'
                               iv_message       =  TEXT-e05 ).

    ENDIF.

  ENDMETHOD.


  METHOD get_user_fullname.

    DATA: lt_ret     TYPE bapiret2_t,
          ls_address TYPE bapiaddr3.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_username
*       CACHE_RESULTS        = 'X'
      IMPORTING
*       LOGONDATA            =
*       DEFAULTS =
        address  = ls_address
*       COMPANY  =
*       SNC      =
*       REF_USER =
*       ALIAS    =
*       UCLASS   =
*       LASTMODIFIED         =
*       ISLOCKED =
*       IDENTITY =
*       ADMINDATA            =
*       DESCRIPTION          =
      TABLES
*       PARAMETER            =
*       PROFILES =
*       ACTIVITYGROUPS       =
        return   = lt_ret
*       ADDTEL   =
*       ADDFAX   =
*       ADDTTX   =
*       ADDTLX   =
*       ADDSMTP  =
*       ADDRML   =
*       ADDX400  =
*       ADDRFC   =
*       ADDPRT   =
*       ADDSSF   =
*       ADDURI   =
*       ADDPAG   =
*       ADDCOMREM            =
*       PARAMETER1           =
*       GROUPS   =
*       UCLASSSYS            =
*       EXTIDHEAD            =
*       EXTIDPART            =
*       SYSTEMS  =
      .

    return = ls_address-fullname.


  ENDMETHOD.


  METHOD get_versao_text.

*    CHECK NOT iv_versao IS INITIAL.
*
*    SELECT SINGLE /qaps/descrv
*      FROM  /qaps/zmifpvx
*      WHERE /qaps/versao = @iv_versao
*      INTO @return.
*
*    CHECK sy-subrc NE 0.
*
*    raise_general_exception( iv_type          =  'E'
*                             iv_message       =  TEXT-e03 ).

  ENDMETHOD.


  METHOD get_waers_text.

    SELECT SINGLE ktext
      FROM tcurt
      WHERE spras = @sy-langu
      AND waers = @iv_waers
      INTO @return.

    CHECK sy-subrc NE 0.

    raise_general_exception( iv_type          =  'E'
                             iv_message       =  TEXT-e06 ).

  ENDMETHOD.


  METHOD get_werks_text.

    CHECK NOT iv_werks IS INITIAL.

    SELECT SINGLE name1
      FROM  t001w
      WHERE werks = @iv_werks
      INTO @return.

    CHECK sy-subrc NE 0.

    raise_general_exception( iv_type          =  'E'
                             iv_message       =  TEXT-e04 ).

  ENDMETHOD.


  METHOD raise_general_exception.

    RAISE EXCEPTION TYPE /qaps/cx_general
      EXPORTING
*       textid  =
*       previous =
        message = VALUE bapiret2( type = iv_type
                                  message = iv_message ).

  ENDMETHOD.
ENDCLASS.
