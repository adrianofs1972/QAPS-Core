class /QAPS/CL_MODEL_BASE definition
  public
  abstract
  create public .

public section.

  methods DOCKING_SIZE_CONTROL
    for event SIZE_CONTROL of CL_GUI_DOCKING_CONTAINER .
  methods CONSTRUCTOR
    importing
      !IV_ACTION type /QAPS/PROCESS_ACTION optional .
protected section.

  data MV_ACTION type CHAR1 .
  constants MC_GUID_NULL type GUID16 value '00000000000000000000000000000000' ##NO_TEXT.

  methods PROGRESS
    importing
      !IV_TEXTO type STRING .
  methods GET_MATERIAL_DESCRIPTION
    importing
      !IV_MATNR type MATNR
      !IV_SPRAS type SPRAS default SY-LANGU
    returning
      value(RETURN) type MAKTX .
  methods GET_USER_FULLNAME
    importing
      !IV_UNAME type XUBNAME
    returning
      value(RETURN) type AD_NAMTEXT .
  methods PREENCHER_DADOS_CONTROLE
    changing
      !CR_DATA type ref to DATA .
  methods VALIDATE_MATERIAL
    importing
      !IV_MATNR type MATNR
    raising
      /QAPS/CX_GENERAL .
  methods VALIDATE_MATERIAL_CENTRO
    importing
      !IV_MATNR type MATNR
      !IV_WERKS type WERKS_D
    raising
      /QAPS/CX_GENERAL .
  methods VALIDATE_CENTRO
    importing
      !IV_WERKS type WERKS_D
    raising
      /QAPS/CX_GENERAL .
  methods COMPONENT_TO_COLOR
    importing
      !IV_STRUCTURE type ANY
    returning
      value(RETURN) type LVC_T_SCOL .
  methods COMPONENT_TO_STYLE
    importing
      !IV_STRUCTURE type ANY
    returning
      value(RETURN) type LVC_T_STYL .
private section.
ENDCLASS.



CLASS /QAPS/CL_MODEL_BASE IMPLEMENTATION.


  METHOD component_to_color.

    DATA: lo_typedescr TYPE REF TO cl_abap_structdescr.
    DATA ls_scol TYPE lvc_s_scol.

    lo_typedescr ?= cl_abap_typedescr=>describe_by_name( iv_structure ).

    DATA(lt_comp) = lo_typedescr->get_components( ).

    SORT lt_comp BY name.

    LOOP AT lt_comp INTO DATA(ls_comp).

      CHECK NOT ls_comp-name IS INITIAL.
      ls_scol-fname = ls_comp-name.
      APPEND ls_scol TO return.
    ENDLOOP.

  ENDMETHOD.


  METHOD component_to_style.

    DATA: lo_typedescr TYPE REF TO cl_abap_structdescr.
    DATA ls_style TYPE lvc_s_styl.

    lo_typedescr ?= cl_abap_typedescr=>describe_by_name( iv_structure ).

    DATA(lt_comp) = lo_typedescr->get_components( ).

    SORT lt_comp BY name.

    LOOP AT lt_comp INTO DATA(ls_comp).

      CHECK NOT ls_comp-name IS INITIAL.
      ls_style-fieldname = ls_comp-name.
      APPEND ls_style TO return.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    mv_action = iv_action.
  ENDMETHOD.


  METHOD docking_size_control.
  ENDMETHOD.


  METHOD get_material_description.

    SELECT SINGLE *
      FROM /qaps/v_material
      WHERE matnr = @iv_matnr
      AND spras = @iv_spras
      INTO @DATA(ls_data).

    return = ls_data-maktx.

  ENDMETHOD.


  METHOD get_user_fullname.

    DATA: ls_address TYPE bapiaddr3,
          lt_return  TYPE bapiret2_t.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_uname
*       CACHE_RESULTS        = 'X'
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    return = ls_address-fullname.

  ENDMETHOD.


  METHOD preencher_dados_controle.

    /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = cr_data ).
*    ASSIGN cr_data->* TO FIELD-SYMBOL(<fs_data>).
*
*    ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_uname>).
*
*    IF <fs_uname> IS ASSIGNED.
*      IF <fs_uname> IS INITIAL.
*        ASSIGN COMPONENT 'CREATED_IN' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_datum>).
*        ASSIGN COMPONENT 'CREATED_ON' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_uzeit>).
*
*        <fs_uname> = sy-uname.
*        <fs_datum> = sy-datum.
*        <fs_uzeit> = sy-uzeit.
*      ELSE.
*        ASSIGN COMPONENT 'MODIFIED_BY' OF STRUCTURE <fs_data> TO <fs_uname>.
*        ASSIGN COMPONENT 'MODIFIED_IN' OF STRUCTURE <fs_data> TO <fs_datum>.
*        ASSIGN COMPONENT 'MODIFIED_ON' OF STRUCTURE <fs_data> TO <fs_uzeit>.
*
*        <fs_uname> = sy-uname.
*        <fs_datum> = sy-datum.
*        <fs_uzeit> = sy-uzeit.
*      ENDIF.
*
*    ENDIF.


  ENDMETHOD.


  METHOD progress.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = iv_texto.

  ENDMETHOD.


  METHOD validate_centro.

    SELECT SINGLE *
     FROM t001w
     WHERE werks = @iv_werks
     INTO @DATA(ls_mara).

    CHECK sy-subrc NE 0.

    DATA(lv_message) = |Centro { iv_werks } não existe |.

    RAISE EXCEPTION TYPE /qaps/cx_general
      EXPORTING
        message = VALUE bapiret2( type = 'E'
                                  message = lv_message ).

  ENDMETHOD.


  METHOD validate_material.

    SELECT SINGLE *
     FROM mara
     WHERE matnr = @iv_matnr
     INTO @DATA(ls_mara).

    CHECK sy-subrc NE 0.

    DATA(lv_message) = |Material { iv_matnr ALPHA = OUT } não existe |.

    RAISE EXCEPTION TYPE /qaps/cx_general
      EXPORTING
        message = VALUE bapiret2( type = 'E'
                                  message = lv_message ).

  ENDMETHOD.


  METHOD validate_material_centro.

    SELECT SINGLE *
     FROM marc
     WHERE werks = @iv_werks
     AND matnr = @iv_matnr
     INTO @DATA(ls_marc).

    CHECK sy-subrc NE 0.

    DATA(lv_message) = |Material { iv_matnr ALPHA = OUT } não existe no centro { iv_werks }|.

    RAISE EXCEPTION TYPE /qaps/cx_general
      EXPORTING
        message = VALUE bapiret2( type = 'E'
                                  message = lv_message ).

  ENDMETHOD.
ENDCLASS.
