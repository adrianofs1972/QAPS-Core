class /QAPS/CL_DD_CHECK_ELEMENT definition
  public
  inheriting from CL_DD_FORM_ELEMENT
  create public .

*"* public components of class /QAPS/CL_DD_CHECK_ELEMENT
*"* do not include other source files here!!!
public section.

  data VALUE type SDYDO_VALUE .

  events ENTERED .
  events HELP_F1 .
  events HELP_F4 .

  methods ON_ENTERED
    for event INPUT_ENTERED of CL_DD_FORM_ELEMENT
    importing
      !ID .
  methods ON_HELPF1
    for event INPUT_HELPF1 of CL_DD_FORM_ELEMENT
    importing
      !ID .
  methods ON_HELPF4
    for event INPUT_HELPF4 of CL_DD_FORM_ELEMENT
    importing
      !ID .
  methods SET_VALUE
    importing
      !VALUE type SDYDO_VALUE optional .
  methods SET_DISPLAY
    importing
      !SIZE type I optional
      !MAXLENGTH type I optional
      !SUPPRESS_DISPLAY type SDYDO_FLAG optional
      !READ_ONLY type SDYDO_FLAG optional
      !DISABLED type SDYDO_FLAG optional .
  methods SET_FOCUS .
protected section.
*" protected components of class CL_DD_INPUT_ELEMENT
*" do not include other source files here!!!

private section.
*" private components of class CL_DD_INPUT_ELEMENT
*" do not include other source files here!!!

ENDCLASS.



CLASS /QAPS/CL_DD_CHECK_ELEMENT IMPLEMENTATION.


METHOD ON_ENTERED.

  IF id = me->id.
    RAISE EVENT entered.
  ENDIF.

ENDMETHOD.


METHOD ON_HELPF1.

  IF id = me->id.
    RAISE EVENT help_f1.
  ENDIF.

ENDMETHOD.


METHOD ON_HELPF4.

  IF id = me->id.
    RAISE EVENT help_f4.
  ENDIF.

ENDMETHOD.


METHOD SET_DISPLAY.

  DATA position TYPE i.
  DATA pos_tooltip TYPE i.
  DATA size_char(10).
  DATA wa TYPE sdydo_html_line.
  DATA waste TYPE string.
  DATA l_id TYPE sdydo_element_name.

  DATA lc__marker_display TYPE string VALUE ' style="display:'.
  DATA lc__marker_maxlength TYPE string VALUE ' maxlength='.
  DATA lc__marker_size TYPE string VALUE ' size='.
  DATA lc__marker_style TYPE string VALUE ' style='.
  DATA lc__marker_value TYPE string VALUE '" value='.

  CONCATENATE 'name="' id '"' INTO l_id.
  SEARCH formarea->html_table FOR l_id.
  IF sy-subrc = 0.
    position = sy-tabix.
    IF NOT size IS INITIAL.
      size_char = size.
      SHIFT size_char LEFT DELETING LEADING space.
      READ TABLE formarea->html_table INDEX position INTO wa.
      SPLIT wa AT lc__marker_size INTO wa line.
      CONCATENATE wa lc__marker_size size_char INTO wa.
      SPLIT line AT lc__marker_maxlength INTO waste line.
      CONCATENATE wa lc__marker_maxlength line INTO line.
      MODIFY formarea->html_table FROM line INDEX position.
      mark_changed( incl_self = abap_true ).
    ENDIF.
    IF NOT maxlength IS INITIAL.
      size_char = maxlength.
      SHIFT size_char LEFT DELETING LEADING space.
      READ TABLE formarea->html_table INDEX position INTO wa.
      SPLIT wa AT lc__marker_maxlength INTO wa line.
      CONCATENATE wa lc__marker_maxlength size_char INTO wa.
      SPLIT line AT lc__marker_style INTO waste line.
      CONCATENATE wa lc__marker_style line INTO line.
      MODIFY formarea->html_table FROM line INDEX position.
      mark_changed( incl_self = abap_true ).
    ENDIF.
    IF disabled = 'X' AND disabled_active IS INITIAL.
      size_char = ' disabled'.
      READ TABLE formarea->html_table INDEX position INTO wa.
      SPLIT wa AT lc__marker_display INTO wa line.
      CONCATENATE wa size_char lc__marker_display line INTO line.
      MODIFY formarea->html_table FROM line INDEX position.

*     A11Y: modify tooltip of inputfield ( insert 'disabled' )
      IF me->a11y_turned = 'Y'.
        pos_tooltip = position + 1.
        READ TABLE formarea->html_table INDEX pos_tooltip INTO wa.
        SPLIT wa AT lc__marker_value INTO wa line.
        CONCATENATE wa 'Deaktiviert'(001) INTO wa
                   SEPARATED BY space.
        CONCATENATE wa lc__marker_value line INTO wa.
        MODIFY formarea->html_table FROM wa INDEX pos_tooltip.
      ENDIF.

      mark_changed( incl_self = abap_true ).
      disabled_active = 'X'.
    ENDIF.
    IF disabled IS INITIAL AND disabled_active = 'X'.
      READ TABLE formarea->html_table INDEX position INTO wa.
      SPLIT wa AT 'disabled' INTO wa line.
      CONCATENATE wa line INTO line.
      MODIFY formarea->html_table FROM line INDEX position.

*     A11Y: modify tooltip of inputfield ( delete 'disabled' )
      IF me->a11y_turned = 'Y'.
        pos_tooltip = position + 1.
        READ TABLE formarea->html_table INDEX pos_tooltip INTO wa.
        SPLIT wa AT 'Deaktiviert'(001) INTO wa line.
        CONCATENATE wa line INTO wa.
        MODIFY formarea->html_table FROM wa INDEX pos_tooltip.
      ENDIF.

      mark_changed( incl_self = abap_true ).
      CLEAR disabled_active.
    ENDIF.
    IF read_only = 'X' AND readonly_active IS INITIAL.
      size_char = ' readonly'.
      READ TABLE formarea->html_table INDEX position INTO wa.
      SPLIT wa AT lc__marker_display INTO wa line.
      CONCATENATE wa size_char lc__marker_display line INTO line.
      MODIFY formarea->html_table FROM line INDEX position.

*     A11Y: modify tooltip of inputfield ( insert 'readonly' )
      IF me->a11y_turned = 'Y'.
        pos_tooltip = position + 1.
        READ TABLE formarea->html_table INDEX pos_tooltip INTO wa.
        SPLIT wa AT lc__marker_value INTO wa line.
        CONCATENATE wa 'Nur lesen'(002) INTO wa
                   SEPARATED BY space.
        CONCATENATE wa lc__marker_value line INTO wa.
        MODIFY formarea->html_table FROM wa INDEX pos_tooltip.
      ENDIF.

      mark_changed( incl_self = abap_true ).
      readonly_active = 'X'.
    ENDIF.
    IF read_only IS INITIAL AND readonly_active = 'X'.
      READ TABLE formarea->html_table INDEX position INTO wa.
      SPLIT wa AT 'readonly' INTO wa line.
      CONCATENATE wa line INTO line.
      MODIFY formarea->html_table FROM line INDEX position.

*     A11Y: modify tooltip of inputfield ( delete 'readonly' )
      IF me->a11y_turned = 'Y'.
        pos_tooltip = position + 1.
        READ TABLE formarea->html_table INDEX pos_tooltip INTO wa.
        SPLIT wa AT 'Nur lesen'(002) INTO wa line.
        CONCATENATE wa line INTO wa.
        MODIFY formarea->html_table FROM wa INDEX pos_tooltip.
      ENDIF.

      mark_changed( incl_self = abap_true ).
      CLEAR readonly_active.
    ENDIF.
    IF suppress_display = 'X' AND display_suppressed IS INITIAL.
      size_char = 'none'.
      READ TABLE formarea->html_table INDEX position INTO wa.
      SPLIT wa AT lc__marker_display INTO wa line.
      CONCATENATE wa lc__marker_display size_char INTO wa.
      SPLIT line AT '"' INTO waste line.
      CONCATENATE wa '"' line INTO line.
      MODIFY formarea->html_table FROM line INDEX position.
      mark_changed( incl_self = abap_true ).
      display_suppressed = 'X'.
    ENDIF.
    IF suppress_display IS INITIAL AND display_suppressed = 'X'.
      size_char = '    '.
      READ TABLE formarea->html_table INDEX position INTO wa.
      SPLIT wa AT lc__marker_display INTO wa line.
      CONCATENATE wa lc__marker_display size_char INTO wa.
      SPLIT line AT '"' INTO waste line.
      CONCATENATE wa '"' line INTO line.
      MODIFY formarea->html_table FROM line INDEX position.
      mark_changed( incl_self = abap_true ).
      CLEAR display_suppressed.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD SET_FOCUS.

* note 0434375
* -------------------------------------------------- *
* --  obsolet wegen Scriptfehler im WebGUI ab IE5 -- *
* -------------------------------------------------- *
  IF cl_gui_object=>www_active <> 'X'. " not WebGUI
    CONCATENATE formarea->id '.' id INTO focus_path.
    changed = 'X'.
    formarea->changed = 'X'.
    formarea->formelement->changed = 'X'.
  ENDIF.

ENDMETHOD.                    "SET_FOCUS


METHOD SET_VALUE.

  DATA position TYPE i.
  DATA l_id TYPE sdydo_element_name.

  CONCATENATE 'name="' id '"' INTO l_id.
  SEARCH formarea->html_table FOR l_id.
  IF sy-subrc = 0.
    me->value = value.

    position = sy-tabix + 2.
    line = escape( val    = value
                   format = cl_abap_format=>e_xss_ml ).
    CONCATENATE '"' line '"' INTO line.
    MODIFY formarea->html_table FROM line INDEX position.
    mark_changed( ).
  ENDIF.

ENDMETHOD.
ENDCLASS.
