METHOD add_icon_v2 .

  DATA icon_key     TYPE sdydo_key.
  DATA icon_name    TYPE sdydo_url.
  DATA icon_path    TYPE sdydo_url.
  DATA style_str    TYPE string.
  DATA alt_txt_str  TYPE string.
  DATA tabindex_str TYPE string.
  DATA tmp_str      TYPE string.

* handle styles
  IF ( NOT sap_style IS INITIAL OR NOT sap_color IS INITIAL ).
    handle_styles( EXPORTING sap_style  = sap_style
                             sap_color  = sap_color
                   IMPORTING style_buff = style_str ).
  ENDIF.
* handle alternative text and tab index ( A11Y )
  IF NOT alternative_text IS INITIAL.
    IF a11y_turned = 'Y'.
      alt_txt_str = 'Ikone'(011).
      handle_tabindex( EXPORTING tabindex     = tabindex
                       IMPORTING tabindex_str = tabindex_str ).
    ENDIF.
    tmp_str = escape( val    = alternative_text
                      format = cl_abap_format=>e_xss_ml ).
    CONCATENATE alt_txt_str ` ` tmp_str     INTO alt_txt_str.
    CONCATENATE ' alt="'    alt_txt_str
                '" title="' alt_txt_str '"' INTO alt_txt_str.
  ELSE.
    alt_txt_str = ' alt="" title=""'.                       "#EC NOTEXT
  ENDIF.

  IF NOT sap_icon IS INITIAL.
    icon_key = sap_icon.
  ELSE.
    icon_key = 'ICON_DUMMY'.
    alt_txt_str = 'kein Symbol angegeben'(003).
    CONCATENATE ' alt="'    alt_txt_str
                '" title="' alt_txt_str '"' INTO alt_txt_str.
  ENDIF.

  get_image_relpath( EXPORTING image_id   = icon_key
                               is_icon    = 'X'
                     IMPORTING image_name = icon_name
                               image_path = icon_path ).

  IF icon_path IS NOT INITIAL.
*   build and insert <img> into HTML_Table
    CLEAR tmp_str.
    CASE sap_size.
      WHEN cl_dd_area=>large.
        tmp_str = ' width="20"'.
      WHEN cl_dd_area=>extra_large.
        tmp_str = ' width="30"'.
    ENDCASE.
    CONCATENATE '<img src="' icon_path '"' alt_txt_str
                    tabindex_str style_str tmp_str
                    ' align="top">'  INTO line.

    SEARCH html_table FOR cursor.
    IF sy-subrc = 0.
      DATA position     TYPE i.
      DATA picture_info TYPE sdydo_picture.

      position = sy-tabix.
      IF is_line_with_layout = 'X'.
        CONCATENATE '<td>' line '</td>' INTO line.
      ENDIF.
      internal_insert_html( EXPORTING html_str = line
                             CHANGING position = position ).

*     append to table_of_pictures
      picture_info-object_id       = icon_key.
      picture_info-object_internal = icon_name.
      COLLECT picture_info INTO table_of_pictures.
    ENDIF.
  ENDIF.


ENDMETHOD.
