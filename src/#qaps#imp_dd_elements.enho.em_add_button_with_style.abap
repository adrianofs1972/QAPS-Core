METHOD add_button_with_style .

  DATA position TYPE i.
  DATA sign(15).
  DATA element_id TYPE sdydo_element_name.
  DATA label_str TYPE string.
  DATA info TYPE string.
  DATA tooltip_str TYPE string.
  DATA on_click TYPE string.
  DATA local_line_with_layout.
  DATA l_html_table TYPE sdydo_html_table.
  DATA tabindex_str TYPE string.
  DATA a11y_tip TYPE string.
  DATA hotkey_str TYPE string.
  DATA hotkey_tooltip TYPE string.

  DATA lc__line_with_layout_start TYPE string VALUE '<table><tr><td>'.
  DATA lc__line_with_layout_end   TYPE string VALUE '</td></tr></table>'.

* build name and create object
  ADD 1 TO element_count.
  sign = element_count.
  SHIFT sign LEFT DELETING LEADING space.
  CONCATENATE id 'B' sign INTO element_id.

  CREATE OBJECT button EXPORTING id = element_id.
  button->is_button = 'X'.
  APPEND button TO table_of_elements.
  button->formarea = me.
  button->action_name = element_id.
  button->a11y_turned = me->a11y_turned.

* set event handler
  SET HANDLER button->on_clicked FOR formelement.
* handle label
  IF NOT label IS INITIAL.
    label_str = escape( val    = label
                        format = cl_abap_format=>e_xss_ml ).

    IF NOT sap_icon IS INITIAL.
*      IF style IS INITIAL.
        CONCATENATE '<span class="imgButtonLabel">' label_str '</span>' INTO label_str.
*      ELSE.
*        CONCATENATE '<span style="' style '">' label_str '</span>' INTO label_str.
*      ENDIF.
    ELSE.
      CONCATENATE '<span>' label_str '</span>' INTO label_str.
    ENDIF.
  ENDIF.
* handle tabindex ( - A11Y / Section508 )
  handle_tabindex( EXPORTING tabindex     = tabindex
                   IMPORTING tabindex_str = tabindex_str ).
* handel keyboard shortcut ( - A11Y / Section508 )
  handle_hotkey( EXPORTING hotkey         = hotkey
                 IMPORTING hotkey_str     = hotkey_str
                           hotkey_tooltip = hotkey_tooltip ).
* handle ToolTips ( + A11Y tooltip )
  a11y_tip = 'Button'(001).
  handle_tooltip( EXPORTING element_name   = a11y_tip
                            element_label  = label
                            info_text      = tooltip
                            hotkey_tooltip = hotkey_tooltip
                  IMPORTING tooltip_str    = tooltip_str ).
* handle name
  info = element_id.
  IF NOT name IS INITIAL.
    line = escape( val    = name
                   format = cl_abap_format=>e_xss_ml ).
    CONCATENATE info 'NAME:' line INTO info.
    TRANSLATE info TO UPPER CASE.
  ENDIF.
* handle on click
  CONCATENATE ' onClick="ButtonClick(' id ',this);"' INTO on_click.

* create tags, insert HTML
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
      CLEAR is_line_with_layout.
      local_line_with_layout = 'X'.
    ELSE.
      CLEAR html_str.
    ENDIF.

*   A11Y: enter event for button
    DATA on_keydown TYPE string.
    CONCATENATE ' onKeypress="if(event.keycode=13) ButtonClick('
                id ',this);"'
                INTO on_keydown.                            "#EC NOTEXT

    IF local_line_with_layout IS INITIAL.
      CONCATENATE html_str lc__line_with_layout_start
                                                     INTO html_str.
    ENDIF.
    if style is INITIAL.
      CONCATENATE html_str '<button id="' info
                '" type="button"' tabindex_str
                hotkey_str ' style="display:    "'   INTO html_str.
    else.
      CONCATENATE html_str '<button id="' info
                '" type="button"' tabindex_str
                hotkey_str ' style="display:;' style '"'   INTO html_str.
    endif.
    INSERT html_str INTO l_html_table INDEX position.
    ADD 1 TO position.
    CONCATENATE tooltip_str on_click
                on_keydown '>'                       INTO html_str.
*   new line necesary because of DISABLE/ENABLE Method.
    INSERT html_str INTO l_html_table INDEX position.
*    ADD 1 TO position.
    IF NOT sub_area IS INITIAL.
      sub_area->html_table = l_html_table.
    ELSE.
      html_table = l_html_table.
    ENDIF.

    IF NOT sap_icon IS INITIAL.
*     suppress tooltip for icon
      DATA: flag TYPE sdydo_flag.
*     render icon
      IF NOT sub_area IS INITIAL.
        flag = sub_area->a11y_turned.
        sub_area->a11y_turned = 'N'.
        sub_area->add_icon( sap_icon         = sap_icon
                            alternative_text = tooltip
                            sap_size = '' ).
        sub_area->a11y_turned = flag.
      ELSE.
        flag = me->a11y_turned.
        me->a11y_turned = 'N'.
        me->add_icon_v2( sap_icon         = sap_icon
                      alternative_text = tooltip
                      sap_size = '' ).
        me->a11y_turned = flag.
      ENDIF.
    ENDIF.
*   render label
    CONCATENATE label_str '</button>'                INTO html_str.
    IF local_line_with_layout IS INITIAL.
      CONCATENATE  html_str lc__line_with_layout_end INTO html_str.
    ENDIF.
    IF local_line_with_layout = 'X'.
      CONCATENATE  html_str '</td>' INTO html_str.
      is_line_with_layout = 'X'.
    ENDIF.
    IF NOT sub_area IS INITIAL.
      SEARCH sub_area->html_table FOR cursor.
      position = sy-tabix.
      INSERT html_str INTO sub_area->html_table INDEX position.
    ELSE.
      SEARCH html_table FOR cursor.
      position = sy-tabix.
      INSERT html_str INTO html_table INDEX position.
    ENDIF.
  ENDIF.


ENDMETHOD.
