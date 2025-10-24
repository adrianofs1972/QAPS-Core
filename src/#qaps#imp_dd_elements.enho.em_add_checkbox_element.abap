METHOD add_checkbox_element .

  DATA position TYPE i.
  DATA sign(15).
  DATA element_id TYPE sdydo_element_name.
  DATA size_char(10).
  DATA maxlength_char(10).
  DATA info TYPE string.
  DATA l_html_table TYPE sdydo_html_table.
  DATA tooltip_str TYPE string.
  DATA tabindex_str TYPE string.
  DATA hotkey_str     TYPE string.                    " A11Y
  DATA hotkey_tooltip TYPE string.                    " A11Y
  DATA label_str      TYPE string.                    " A11Y

* build id and create object
  ADD 1 TO element_count.
  sign = element_count.
  SHIFT sign LEFT DELETING LEADING space.
  CONCATENATE id 'I' sign INTO element_id.

  CREATE OBJECT input_element EXPORTING id = element_id.
  input_element->is_input = 'X'.
  APPEND input_element TO table_of_elements.
  input_element->formarea = me.
  input_element->value = value.
  input_element->a11y_turned = me->a11y_turned.

* handle name
  IF NOT name IS INITIAL.
    info = escape( val    = name
                   format = cl_abap_format=>e_xss_ml ).
  ENDIF.
  CONCATENATE element_id 'NAME:' info INTO info.
  TRANSLATE info TO UPPER CASE.
* handle tabindex ( - A11Y / Section508 )
  handle_tabindex( EXPORTING tabindex     = tabindex
                   IMPORTING tabindex_str = tabindex_str ).
* handel keyboard shortcut ( - A11Y / Section508 )
  handle_hotkey( EXPORTING hotkey         = hotkey
                 IMPORTING hotkey_str     = hotkey_str
                           hotkey_tooltip = hotkey_tooltip ).
* handle ToolTips ( + A11Y tooltip )
  handle_tooltip( EXPORTING info_text      = tooltip
                            hotkey_tooltip = hotkey_tooltip
                  IMPORTING tooltip_str    = tooltip_str ).
* set event handler
  SET HANDLER input_element->on_entered FOR formelement.
  SET HANDLER input_element->on_helpf1  FOR formelement.
  SET HANDLER input_element->on_helpf4  FOR formelement.
* handle maxlength
  IF maxlength IS INITIAL.
    maxlength_char = '250'.
  ELSE.
    maxlength_char = maxlength.
  ENDIF.
  SHIFT maxlength_char LEFT DELETING LEADING space.

* handle A11Y label for input field
  IF NOT a11y_label IS INITIAL AND a11y_turned = 'Y'.
    label_str = escape( val    = a11y_label
                        format = cl_abap_format=>e_xss_ml ).
    CONCATENATE '<label style="display:none" for="'
                info '">' label_str '</label>'
             INTO label_str.
  ENDIF.

* insert INPUT-tag into HTML-Table
  IF NOT sub_area IS INITIAL.
    l_html_table = sub_area->html_table.
  ELSE.
    l_html_table = html_table.
  ENDIF.
  SEARCH l_html_table FOR cursor.
  IF sy-subrc = 0.
    position = sy-tabix.
    IF is_line_with_layout = 'X'.
      html_str = '<td>'.
    ELSE.
      CLEAR html_str.
    ENDIF.
    size_char = size.
    SHIFT size_char LEFT DELETING LEADING space.

    CONCATENATE html_str label_str INTO html_str.
    INSERT html_str INTO l_html_table INDEX position.
    ADD 1 TO position.

* CONCATENATE html_str
    CONCATENATE
     '<input type="checkbox" id="' info '"'
                ' name="' element_id '"'
*                ' size="' size_char '"'
*                ' maxlength="' maxlength_char '"'

                ' style="display:    "'
                tabindex_str
                hotkey_str INTO html_str.
    INSERT html_str INTO l_html_table INDEX position.
*    ADD 1 TO position.
*    CONCATENATE tooltip_str ' value=' INTO html_str.
*    INSERT html_str INTO l_html_table INDEX position.
    ADD 1 TO position.
*   Input value is kept in an extra row of html_table.
    html_str = escape( val    = value
                       format = cl_abap_format=>e_xss_ml ).
    CONCATENATE '"' html_str '"' INTO html_str.
    INSERT html_str INTO l_html_table INDEX position.
    ADD 1 TO position.
    CONCATENATE ' onHelp="InputHelpF1(this.form,this);"'
                ' onClick="javascript:submitForm(this.form,this);"'
                ' onKeyDown="InputKeyDown(this.form,this);"'
              INTO html_str.

     if disabled = 'X'.
      html_str = html_str && ` disabled = "true" `.
    endif.

    if checked = 'X'.
      html_str = html_str && ` checked >`.
    else.
      html_str = html_str && `>`.
    endif.

    CONCATENATE html_str
                '<label for="vehicle1">'
                value
                '</label><br>'
            INTO html_str.
    IF is_line_with_layout = 'X'.
      CONCATENATE html_str '</td>' INTO html_str.
    ENDIF.
    INSERT html_str INTO l_html_table INDEX position.
  ENDIF.
  IF NOT sub_area IS INITIAL.
    sub_area->html_table = l_html_table.
  ELSE.
    html_table = l_html_table.
  ENDIF.



ENDMETHOD.
