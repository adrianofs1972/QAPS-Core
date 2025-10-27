class /QAPS/CL_HELPER_DATA definition
  public
  abstract
  final
  create public .

public section.

  class-methods MATERIAL_TO_INPUT
    importing
      !IV_MATNR type MATNR
    returning
      value(RETURN) type MATNR .
  class-methods PREENCHER_DADOS_CONTROLE
    changing
      !CR_DATA type ref to DATA .
protected section.
private section.
ENDCLASS.



CLASS /QAPS/CL_HELPER_DATA IMPLEMENTATION.


  METHOD material_to_input.
    return = |{ iv_matnr ALPHA = IN WIDTH = 18 }|.
  ENDMETHOD.


  METHOD preencher_dados_controle.

    ASSIGN cr_data->* TO FIELD-SYMBOL(<fs_data>).

    ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_uname>).

    IF <fs_uname> IS ASSIGNED.
      IF <fs_uname> IS INITIAL.
        ASSIGN COMPONENT 'CREATED_IN' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_datum>).
        ASSIGN COMPONENT 'CREATED_ON' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_uzeit>).

        <fs_uname> = sy-uname.
        <fs_datum> = sy-datum.
        <fs_uzeit> = sy-uzeit.
      ELSE.
        ASSIGN COMPONENT 'MODIFIED_BY' OF STRUCTURE <fs_data> TO <fs_uname>.
        ASSIGN COMPONENT 'MODIFIED_IN' OF STRUCTURE <fs_data> TO <fs_datum>.
        ASSIGN COMPONENT 'MODIFIED_ON' OF STRUCTURE <fs_data> TO <fs_uzeit>.

        <fs_uname> = sy-uname.
        <fs_datum> = sy-datum.
        <fs_uzeit> = sy-uzeit.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
