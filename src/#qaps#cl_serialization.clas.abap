class /QAPS/CL_SERIALIZATION definition
  public
  abstract
  final
  create public .

public section.

  class-methods DESERIALIZE
    importing
      !IV_XML type STRING
    changing
      !CR_DATA type ref to DATA .
  class-methods SERIALIZE
    importing
      !IR_DATA type ref to DATA
    returning
      value(RETURN) type STRING .
protected section.
private section.
ENDCLASS.



CLASS /QAPS/CL_SERIALIZATION IMPLEMENTATION.


  METHOD deserialize.

    ASSIGN cr_data->* TO FIELD-SYMBOL(<fs>).

    CALL TRANSFORMATION id
      SOURCE XML iv_xml
      RESULT field = <fs>.

  ENDMETHOD.


  METHOD serialize.

    ASSIGN ir_data->* TO FIELD-SYMBOL(<fs>).

    CALL TRANSFORMATION id
      SOURCE field = <fs>
      RESULT XML return.

  ENDMETHOD.
ENDCLASS.
