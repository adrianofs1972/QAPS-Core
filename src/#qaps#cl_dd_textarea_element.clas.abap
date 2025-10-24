class /QAPS/CL_DD_TEXTAREA_ELEMENT definition
  public
  inheriting from CL_DD_INPUT_ELEMENT
  final
  create public .

public section.

  methods SET_VALUE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /QAPS/CL_DD_TEXTAREA_ELEMENT IMPLEMENTATION.


  METHOD set_value.

    data lt_text_area type SDYDO_HTML_TABLE.
    DATA initial_position TYPE i.
    DATA position TYPE i.
    DATA l_id TYPE sdydo_element_name.

    CONCATENATE 'name="' id '"' INTO l_id.
    SEARCH formarea->html_table FOR l_id.
    IF sy-subrc = 0.
      me->value = value.

      initial_position = sy-tabix.

      LOOP AT formarea->html_table FROM initial_position INTO DATA(ls_html).

        IF ls_html-line CS '</textarea>'.
          position = sy-tabix - 1.
          exit.
        ENDIF.

      ENDLOOP.

      line = escape( val    = value
                     format = cl_abap_format=>e_xss_ml ).
      CONCATENATE '"' line '"' INTO line.
      MODIFY formarea->html_table FROM line INDEX position.
      mark_changed( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
